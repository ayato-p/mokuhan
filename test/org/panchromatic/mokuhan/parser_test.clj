(ns org.panchromatic.mokuhan.parser-test
  (:require [clojure.test :as t]
            [fast-zip.core :as zip]
            [org.panchromatic.mokuhan.ast :as ast]
            [org.panchromatic.mokuhan.parser :as sut]))

(t/deftest parse-variables-test
  (t/testing "escaped variables"
    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{x}}")
        (sut/parse "{{ x }}")
        (sut/parse "{{\tx\t}}")
        (sut/parse "{{\nx\n}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["x" "y"]}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{x.y}}")
        (sut/parse "{{ x.y }}")
        (sut/parse "{{\tx.y\t}}")
        (sut/parse "{{\nx.y\n}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.Whitespace{:content " "}
          #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Whitespace{:content " "}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse " {{ x }} ")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.Text{:content "--"}
          #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Text{:content "--"}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "--{{x}}--")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.Text{:content "}}"}
          #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Text{:content "--"}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "}}{{x}}--")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.Text{:content "--"}
          #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Text{:content "{{"}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "--{{x}}{{")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Text{:content "}"}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{x}}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["{x"]}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{{x}}"))))


  (t/testing "unescaped variables"
    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.UnescapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{{x}}}")
        (sut/parse "{{{ x }}}")
        (sut/parse "{{{\tx\t}}}")
        (sut/parse "{{{\nx\n}}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.UnescapedVariable{:path ["x" "y"]}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{{x.y}}}")
        (sut/parse "{{{ x.y }}}")
        (sut/parse "{{{\tx.y\t}}}")
        (sut/parse "{{{\nx.y\n}}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.UnescapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{&x}}")
        (sut/parse "{{& x }}")
        (sut/parse "{{&\tx\t}}")
        (sut/parse "{{&\nx\n}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.UnescapedVariable{:path ["x" "y"]}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{&x.y}}")
        (sut/parse "{{& x.y }}")
        (sut/parse "{{&\tx.y\t}}")
        (sut/parse "{{&\nx.y\n}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.Whitespace{:content " "}
          #org.panchromatic.mokuhan.ast.UnescapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Whitespace{:content " "}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse " {{{x}}} ")
        (sut/parse " {{&x}} ")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.Text{:content "--"}
          #org.panchromatic.mokuhan.ast.UnescapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Text{:content "--"}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "--{{{x}}}--")
        (sut/parse "--{{&x}}--")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.UnescapedVariable{:path ["{x"]}
          #org.panchromatic.mokuhan.ast.Text{:content "}"}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{{{x}}}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.UnescapedVariable{:path ["&x"]}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{{ &x }}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.UnescapedVariable{:path ["&x"]}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{{ &x }}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.Text{:content "{"}
          #org.panchromatic.mokuhan.ast.UnescapedVariable{:path ["x"]}
          #org.panchromatic.mokuhan.ast.Text{:content "}"}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{{& x }}}")))))

(defn- find-nth-tag [ast n]
  (loop [loc (ast/ast-zip ast), n n]
    (when-not (zip/end? loc)
      (let [node (zip/node loc)]
        (if ((some-fn ast/variable? ast/section?) node)
          (if (zero? n)
            node
            (recur (zip/next loc) (dec n)))
          (recur (zip/next loc) n))))))

(defn- find-first-tag [ast]
  (find-nth-tag ast 0))

(t/deftest parse-name-test
  (t/testing "single name"
    (t/are [src expected] (= expected (.path (find-first-tag (sut/parse src))))
      "{{x}}" ["x"]
      " {{x}} " ["x"]
      "{{ x }}" ["x"]
      "{{\n\nx\n\n}}" ["x"]))

  (t/testing "dotted name"
    (t/are [src expected] (= expected (.path (find-first-tag (sut/parse src))))
      "{{x.y}}" ["x" "y"]
      " {{x.y}} " ["x" "y"]
      "{{ x.y }}" ["x" "y"]
      "{{x.y.z}}" ["x" "y" "z"]
      "{{\n\nx.y.z\n\n}}" ["x" "y" "z"]))

  (t/testing "current context"
    (t/are [src expected] (= expected (.path (find-first-tag (sut/parse src))))
      "{{.}}" ["."]
      " {{.}} " ["."]
      "{{ . }}" ["."]
      "{{\n.\n}}" ["."]))

  (t/testing "illegal names"
    (t/are [src] (nil? (find-first-tag (sut/parse* src)))
      "{{.x}}"
      "{{x.}}"
      "{{.x.}}"
      "{{x . y}}"
      "{{x. y}}"
      "{{x .y}}")

    (t/is
     (nil? (find-nth-tag (sut/parse "{{x}} {{.y}}") 1)))

    (t/is
     (not= ["x"] (.path (find-first-tag (sut/parse "{{.x}} {{y}}")))))

    (t/is (= [["x"] ["z"]]
             (as-> (sut/parse "{{x}} {{.y}} {{z}}") ast
               [(.path (find-nth-tag ast 0))
                (.path (find-nth-tag ast 1))])))

    (t/is (= [["y"] nil]
             (as-> (sut/parse "{{.x}} {{y}} {{.z}}") ast
               [(.path (find-nth-tag ast 0))
                (some-> (find-nth-tag ast 1) .path)])))))

(t/deftest parse-section-test
  (t/testing "standard section"
    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.StandardSection{:path ["x"], :contents nil}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{#x}}{{/x}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.StandardSection{:path ["x" "y"], :contents nil}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{#x.y}}{{/x.y}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.StandardSection
          {:path ["x"],
           :contents
           (#org.panchromatic.mokuhan.ast.StandardSection{:path ["y"], :contents nil})}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{#x}}{{#y}}{{/y}}{{/x}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.StandardSection
          {:path ["x"],
           :contents (#org.panchromatic.mokuhan.ast.Text{:content "}}"})}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{#x}}}}{{/x}}"))))

  (t/testing "inverted section"
    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.InvertedSection{:path ["x"], :contents nil}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{^x}}{{/x}}")))

    (t/is
     (= #org.panchromatic.mokuhan.ast.Mustache
        {:contents
         (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
          #org.panchromatic.mokuhan.ast.InvertedSection{:path ["x" "y"], :contents nil}
          #org.panchromatic.mokuhan.ast.Text{:content ""})}
        (sut/parse "{{^x.y}}{{/x.y}}"))))

  (t/testing "unopened section"
    (t/is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo #"Unopened section"
      (sut/parse "{{/x}}")))

    (t/is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo #"Unopened section"
      (sut/parse "{{x}}{{/x}}"))))

  (t/testing "unclosed section"
    (t/is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo #"Unclosed section"
      (sut/parse "{{#x}}")))

    (t/is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo #"Unclosed section"
      (sut/parse "{{#x}}{{x}}")))

    (t/is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo #"Unclosed section"
      (sut/parse "{{^x}}")))

    (t/is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo #"Unclosed section"
      (sut/parse "{{^x}}{{x}}")))))

(t/deftest parse-comment-test
  (t/is
   (= #org.panchromatic.mokuhan.ast.Mustache
      {:contents
       (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
        #org.panchromatic.mokuhan.ast.Comment{:content "x"}
        #org.panchromatic.mokuhan.ast.Text{:content ""})}
      (sut/parse "{{!x}}")))

  (t/is
   (= #org.panchromatic.mokuhan.ast.Mustache
      {:contents
       (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
        #org.panchromatic.mokuhan.ast.Comment{:content " x "}
        #org.panchromatic.mokuhan.ast.Text{:content ""})}
      (sut/parse "{{! x }}")))

  (t/is
   (= #org.panchromatic.mokuhan.ast.Mustache
      {:contents
       (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
        #org.panchromatic.mokuhan.ast.Comment{:content "\n\nx\n\n"}
        #org.panchromatic.mokuhan.ast.Text{:content ""})}
      (sut/parse "{{!\n\nx\n\n}}")))

  (t/is
   (=  #org.panchromatic.mokuhan.ast.Mustache
       {:contents
        (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
         #org.panchromatic.mokuhan.ast.Comment{:content " x y z " }
         #org.panchromatic.mokuhan.ast.Text{:content ""})}
       (sut/parse "{{! x y z }}")))

  (t/is
   (= #org.panchromatic.mokuhan.ast.Mustache
      {:contents
       (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
        #org.panchromatic.mokuhan.ast.Comment{:content " x {{x"}
        #org.panchromatic.mokuhan.ast.Text{:content "}}"}
        #org.panchromatic.mokuhan.ast.Text{:content ""})}
      (sut/parse "{{! x {{x}}}}")))

  (t/is
   (= #org.panchromatic.mokuhan.ast.Mustache
      {:contents
       (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
        #org.panchromatic.mokuhan.ast.Comment{:content "{{x"}
        #org.panchromatic.mokuhan.ast.Whitespace{:content " "}
        #org.panchromatic.mokuhan.ast.Text{:content "x}}"}
        #org.panchromatic.mokuhan.ast.Text{:content ""})}
      (sut/parse "{{!{{x}} x}}"))))

(t/deftest parse-set-delimiter-test
  (t/is (= #org.panchromatic.mokuhan.ast.Mustache
           {:contents
            (#org.panchromatic.mokuhan.ast.BeginningOfLine{})}
           (sut/parse "{{=<< >>=}}")))

  (t/is (= #org.panchromatic.mokuhan.ast.Mustache
           {:contents
            (#org.panchromatic.mokuhan.ast.BeginningOfLine{})}
           (sut/parse "{{={% %}=}}")))

  (t/is (= #org.panchromatic.mokuhan.ast.Mustache
           {:contents
            (#org.panchromatic.mokuhan.ast.BeginningOfLine{})}
           (sut/parse "{{= % % =}}")))

  (t/is (= #org.panchromatic.mokuhan.ast.Mustache
           {:contents
            (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
             #org.panchromatic.mokuhan.ast.Text{:content "{{x}}"}
             #org.panchromatic.mokuhan.ast.Text{:content ""})}
           (sut/parse "{{=% %=}}{{x}}")))

  (t/is (= #org.panchromatic.mokuhan.ast.Mustache
           {:contents
            (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
             #org.panchromatic.mokuhan.ast.Text{:content "=}}"}
             #org.panchromatic.mokuhan.ast.Text{:content ""})}
           (sut/parse "{{=% %=}}=}}")))

  (t/is (= #org.panchromatic.mokuhan.ast.Mustache
           {:contents
            (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
             #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["x"]}
             #org.panchromatic.mokuhan.ast.Text{:content ""})}
           (sut/parse "{{=% %=}}\n%x%")
           #org.panchromatic.mokuhan.ast.Mustache
           {:contents
            (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
             #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["x"]}
             #org.panchromatic.mokuhan.ast.Text{:content ""})})))

(t/deftest parse-newline-test
  (t/is (= #org.panchromatic.mokuhan.ast.Mustache
           {:contents
            (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
             #org.panchromatic.mokuhan.ast.Text{:content "\n"})}
           (sut/parse "\n")))

  (t/is (= #org.panchromatic.mokuhan.ast.Mustache
           {:contents
            (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
             #org.panchromatic.mokuhan.ast.Text{:content "\r\n"})}
           (sut/parse "\r\n"))))

(t/deftest parser-test
  (t/is
   (= #org.panchromatic.mokuhan.ast.Mustache
      {:contents
       (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
        #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["x"]}
        #org.panchromatic.mokuhan.ast.Whitespace{:content " "}
        #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["y"]}
        #org.panchromatic.mokuhan.ast.Whitespace{:content " "}
        #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["z"]}
        #org.panchromatic.mokuhan.ast.Text{:content ""})}
      (sut/parse "{{x}} {{y}} {{z}}")))

  (t/is
   (= #org.panchromatic.mokuhan.ast.Mustache
      {:contents
       (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
        #org.panchromatic.mokuhan.ast.StandardSection
        {:path ["person"],
         :contents
         (#org.panchromatic.mokuhan.ast.Whitespace{:content " "}
          #org.panchromatic.mokuhan.ast.EscapedVariable{:path ["name"]}
          #org.panchromatic.mokuhan.ast.Whitespace{:content " "})}
        #org.panchromatic.mokuhan.ast.Text{:content ""})}
      (sut/parse "{{#person}} {{name}} {{/person}}")))

  (mapcat #(map inc %) [(range 10)(range 10)])

  (t/is
   (= #org.panchromatic.mokuhan.ast.Mustache
      {:contents
       (#org.panchromatic.mokuhan.ast.BeginningOfLine{}
        #org.panchromatic.mokuhan.ast.InvertedSection
        {:path ["person"],
         :contents
         (#org.panchromatic.mokuhan.ast.Whitespace{:content " "}
          #org.panchromatic.mokuhan.ast.Text{:content "Nothing"}
          #org.panchromatic.mokuhan.ast.Whitespace{:content " "})}
        #org.panchromatic.mokuhan.ast.Text{:content ""})}
      (sut/parse "{{^person}} Nothing {{/person}}")))

  (t/is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unopened section"
         (sut/parse "{{name}} {{/person}}")))

  (t/is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unopened section"
         (sut/parse "{{#x}}{{/y}}")))

  (t/is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unclosed section"
         (sut/parse "{{#person}} {{name}}")))

  (t/is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unclosed section"
         (sut/parse "{{^person}} Nothing ")))

  (t/is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unclosed section"
         (sut/parse "{{#x}}{{{{/x}}")))

  )
