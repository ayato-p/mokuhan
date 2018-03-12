(ns mokuhan.parser-test
  (:require [clojure.test :as t]
            [mokuhan.parser :as sut]))

(t/deftest parse-variables-test
  (t/testing "escaped variables"
    (t/is
     (= [[:escaped-variable [:name "x"]]]
        (sut/parse* "{{x}}")
        (sut/parse* "{{ x }}")
        (sut/parse* "{{\tx\t}}")
        (sut/parse* "{{\nx\n}}")))

    (t/is
     (= [[:escaped-variable [:name "x" "y"]]]
        (sut/parse* "{{x.y}}")
        (sut/parse* "{{ x.y }}")
        (sut/parse* "{{\tx.y\t}}")
        (sut/parse* "{{\nx.y\n}}")))


    (t/are [src expected] (= expected (sut/parse* src))
      " {{ x }} "
      [[:whitespace " "] [:escaped-variable [:name "x"]] [:whitespace " "]]

      "--{{x}}--"
      [[:text "--"] [:escaped-variable [:name "x"]] [:text "--"]]

      "}}{{x}}--"
      [[:text "}}"] [:escaped-variable [:name "x"]] [:text "--"]]

      "--{{x}}{{"
      [[:text "--"] [:escaped-variable [:name "x"]] [:text "{{"]]

      "{{x}}}"
      [[:escaped-variable [:name "x"]] [:text "}"]]

      "{{{x}}"
      [[:text "{"] [:escaped-variable [:name "x"]]]

      "{{{{x}}}}"
      [[:text "{"] [:unescaped-variable [:name "x"]] [:text "}"]]))

  (t/testing "unescaped variables"
    (t/is
     (= [[:unescaped-variable [:name "x"]]]
        (sut/parse* "{{{x}}}")
        (sut/parse* "{{{ x }}}")
        (sut/parse* "{{{\tx\t}}}")
        (sut/parse* "{{{\nx\n}}}")))

    (t/is
     (= [[:unescaped-variable [:name "x" "y"]]]
        (sut/parse* "{{{x.y}}}")
        (sut/parse* "{{{ x.y }}}")
        (sut/parse* "{{{\tx.y\t}}}")
        (sut/parse* "{{{\nx.y\n}}}")))

    (t/is
     (= [[:unescaped-variable [:name "x"]]]
        (sut/parse* "{{&x}}")
        (sut/parse* "{{& x }}")
        (sut/parse* "{{&\tx\t}}")
        (sut/parse* "{{&\nx\n}}")))

    (t/is
     (= [[:unescaped-variable [:name "x" "y"]]]
        (sut/parse* "{{&x.y}}")
        (sut/parse* "{{& x.y }}")
        (sut/parse* "{{&\tx.y\t}}")
        (sut/parse* "{{&\nx.y\n}}")))

    (t/is
     (= [[:whitespace " "] [:unescaped-variable [:name "x"]] [:whitespace " "]]
        (sut/parse* " {{{x}}} ")
        (sut/parse* " {{&x}} ")))

    (t/is
     (= [[:text "--"] [:unescaped-variable [:name "x"]] [:text "--"]]
        (sut/parse* "--{{{x}}}--")
        (sut/parse* "--{{&x}}--")))

    (t/is
     (= [[:text "{"] [:unescaped-variable [:name "x"]] [:text "}"]]
        (sut/parse* "{{{{x}}}}")))

    (t/is
     (= [[:unescaped-variable [:name "&x"]]]
        (sut/parse* "{{{ &x }}}")
        (sut/parse* "{{{&x }}}")))

    (t/is
     (= [[:text "{"] [:unescaped-variable [:name "x"]] [:text "}"]]
        (sut/parse* "{{{& x }}}")))))

(t/deftest parse-name-test
  (letfn [(find-name [vec-or-any]
            (when (sequential? vec-or-any)
              (if (= :name (first vec-or-any))
                vec-or-any
                (reduce #(when-let [res (find-name %2)]
                           (reduced res))
                        nil
                        vec-or-any))))]
    (t/testing "single name"
      (t/are [src expected] (= expected (find-name (sut/parse* src)))
        "{{x}}" [:name "x"]
        " {{x}} " [:name "x"]
        "{{ x }}" [:name "x"]
        "{{\n\nx\n\n}}" [:name "x"]))

    (t/testing "dotted name"
      (t/are [src expected] (= expected (find-name (sut/parse* src)))
        "{{x.y}}" [:name "x" "y"]
        " {{x.y}} " [:name "x" "y"]
        "{{ x.y }}" [:name "x" "y"]
        "{{x.y.z}}" [:name "x" "y" "z"]
        "{{\n\nx.y.z\n\n}}" [:name "x" "y" "z"]))

    (t/testing "illegal names"
      (t/are [src expected] (= expected (sut/parse* src))
        "{{.x}}" [[:text "{{.x}}"]]
        "{{x.}}" [[:text "{{x.}}"]]
        "{{.x.}}" [[:text "{{.x.}}"]]
        "{{x . y}}" [[:text "{{x"] [:whitespace  " "] [:text  "."]
                     [:whitespace " "] [:text "y}}"]]
        "{{x. y}}" [[:text "{{x."] [:whitespace " "] [:text "y}}"]]
        "{{x .y}}" [[:text "{{x"] [:whitespace " "] [:text ".y}}"]]
        "{{x}} {{.y}}" [[:escaped-variable [:name "x"]]
                        [:whitespace  " "] [:text "{{.y}}"]]
        "{{.x}} {{y}}" [[:text "{{.x}}"] [:whitespace  " "]
                        [:escaped-variable [:name "y"]]]
        "{{x}} {{.y}} {{z}}" [[:escaped-variable [:name "x"]]
                              [:whitespace " "]
                              [:text "{{.y}}"] [:whitespace " "]
                              [:escaped-variable [:name "z"]]]
        "{{.x}} {{y}} {{.z}}" [[:text "{{.x}}"] [:whitespace " "]
                               [:escaped-variable [:name "y"]]
                               [:whitespace " "]
                               [:text "{{.z}}"]]))))

(t/deftest parse-section-test
  (t/testing "standard section"
    (t/is
     (= [[:open-section [:name "x"]]
         [:close-section [:name "x"]]]
        (sut/parse* "{{#x}}{{/x}}")))

    (t/is
     (= [[:open-section [:name "x" "y"]]
         [:close-section [:name "x" "y"]]]
        (sut/parse* "{{#x.y}}{{/x.y}}")))

    (t/is
     (= [[:open-section [:name "x"]]
         [:close-section [:name "y"]]]
        (sut/parse* "{{#x}}{{/y}}")))

    (t/is
     (= [[:open-section [:name "x"]]
         [:open-section [:name "y"]]
         [:close-section [:name "y"]]
         [:close-section [:name "x"]]]
        (sut/parse* "{{#x}}{{#y}}{{/y}}{{/x}}")))

    (t/is
     (= [[:open-section [:name "x"]]
         [:text "{"]
         [:text "{"]
         [:close-section [:name "x"]]]
        (sut/parse* "{{#x}}{{{{/x}}")))

    (t/is
     (= [[:open-section [:name "x"]]
         [:text "}}"]
         [:close-section [:name "x"]]]
        (sut/parse* "{{#x}}}}{{/x}}"))))

  (t/testing "inverted section"
    (t/is
     (= [[:open-inverted-section [:name "x"]]
         [:close-section [:name "x"]]]
        (sut/parse* "{{^x}}{{/x}}")))

    (t/is
     (= [[:open-inverted-section [:name "x" "y"]]
         [:close-section [:name "x" "y"]]]
        (sut/parse* "{{^x.y}}{{/x.y}}")))

    (t/is
     (= [[:open-section [:name "x"]]
         [:close-section [:name "y"]]]
        (sut/parse* "{{#x}}{{/y}}"))))

  (t/testing "unopened section"
    (t/is
     (= [[:close-section [:name "x"]]]
        (sut/parse* "{{/x}}")))

    (t/is
     (= [[:escaped-variable [:name "x"]]
         [:close-section [:name "x"]]]
        (sut/parse* "{{x}}{{/x}}"))))

  (t/testing "unclosed section"
    (t/is
     (= [[:open-section [:name "x"]]]
        (sut/parse* "{{#x}}")))

    (t/is
     (= [[:open-section [:name "x"]]
         [:escaped-variable [:name "x"]]]
        (sut/parse* "{{#x}}{{x}}")))

    (t/is
     (= [[:open-inverted-section [:name "x"]]]
        (sut/parse* "{{^x}}")))

    (t/is
     (= [[:open-inverted-section [:name "x"]]
         [:escaped-variable [:name "x"]]]
        (sut/parse* "{{^x}}{{x}}")))))

(t/deftest parse-comment-test
  (t/is
   (= [[:comment "x"]]
      (sut/parse* "{{!x}}")))

  (t/is
   (= [[:comment " x "]]
      (sut/parse* "{{! x }}")))

  (t/is
   (= [[:comment "\n\nx\n\n"]]
      (sut/parse* "{{!\n\nx\n\n}}")))

  (t/is
   (= [[:comment " x y z "]]
      (sut/parse* "{{! x y z }}")))

  (t/is
   (= [[:comment " x {{x"] [:text "}}"]]
      (sut/parse* "{{! x {{x}}}}")))

  (t/is
   (= [[:comment  "{{x"] [:whitespace " "] [:text "x}}"]]
      (sut/parse* "{{!{{x}} x}}"))))

(t/deftest parse-set-delimiter-test
  (t/is (= [[:set-delimiter [:new-open-delimiter "<<"] [:new-close-delimiter ">>"]]]
           (sut/parse* "{{=<< >>=}}")))

  (t/is (= [[:set-delimiter [:new-open-delimiter "{%"] [:new-close-delimiter "%}"]]]
           (sut/parse* "{{={% %}=}}")))

  (t/is (= [[:set-delimiter [:new-open-delimiter "%"] [:new-close-delimiter "%"]]]
           (sut/parse* "{{= % % =}}")))

  (t/is (= [[:set-delimiter [:new-open-delimiter "%"] [:new-close-delimiter "%"] [:rest "{{x}}"]]]
           (sut/parse* "{{=% %=}}{{x}}")))

  (t/is (= [[:set-delimiter [:new-open-delimiter "%"] [:new-close-delimiter "%"] [:rest "=}}"]]]
           (sut/parse* "{{=% %=}}=}}"))))

(t/deftest parse-newline-test
  (t/is (= [[:newline "\n"]]
           (sut/parse* "\n")))

  (t/is (= [[:newline "\r\n"]]
           (sut/parse* "\r\n"))))

(t/deftest parser-test
  (t/is
   (= #mokuhan.ast.Mustache
      {:contents
       (#mokuhan.ast.EscapedVariable{:path ["x"]}
        #mokuhan.ast.Whitespace{:content " "}
        #mokuhan.ast.EscapedVariable{:path ["y"]}
        #mokuhan.ast.Whitespace{:content " "}
        #mokuhan.ast.EscapedVariable{:path ["z"]})}
      (sut/parse "{{x}} {{y}} {{z}}")))

  (t/is
   (= #mokuhan.ast.Mustache
      {:contents
       (#mokuhan.ast.StandardSection
        {:path ["person"],
         :contents
         (#mokuhan.ast.Whitespace{:content " "}
          #mokuhan.ast.EscapedVariable{:path ["name"]}
          #mokuhan.ast.Whitespace{:content " "})})}
      (sut/parse "{{#person}} {{name}} {{/person}}")))

  (t/is
   (= (sut/parse "{{^person}} Nothing {{/person}}")
      #mokuhan.ast.Mustache
      {:contents
       (#mokuhan.ast.InvertedSection
        {:path ["person"],
         :contents
         (#mokuhan.ast.Whitespace{:content " "}
          #mokuhan.ast.Text{:content "Nothing"}
          #mokuhan.ast.Whitespace{:content " "})})}))

  (t/is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unopened section"
         (sut/parse "{{name}} {{/person}}")))

  (t/is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unclosed section"
         (sut/parse "{{#person}} {{name}}")))

  (t/is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unclosed section"
         (sut/parse "{{^person}} Nothing "))))
