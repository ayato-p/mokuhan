(ns mustaclj.parser-test
  (:require [clojure.test :as t]
            [mustaclj.parser :as sut]))

(t/deftest parse-variables-test
  (t/testing "escaped variables"
    (t/are [src expected] (= expected (sut/parse src))
      "{{x}}"
      [[:escaped-variable [:open-delimiter "{{"] [:name "x"] [:close-delimiter "}}"]]]

      "{{ x }}"
      [[:escaped-variable [:open-delimiter "{{"] " " [:name "x"] " " [:close-delimiter "}}"]]]

      " {{ x }} "
      [" " [:escaped-variable [:open-delimiter "{{"] " " [:name "x"] " " [:close-delimiter "}}"]] " "]

      "--{{x}}--"
      ["--" [:escaped-variable [:open-delimiter "{{"] [:name "x"] [:close-delimiter "}}"]] "--"]

      "}}{{x}}--"
      ["}}" [:escaped-variable [:open-delimiter "{{"] [:name "x"] [:close-delimiter "}}"]] "--"]

      "{{\nx\n}}"
      [[:escaped-variable [:open-delimiter "{{"] "\n" [:name "x"] "\n" [:close-delimiter "}}"]]]))

  (t/testing "unescaped variables"
    (t/are [src expected] (= expected (sut/parse src))
      "{{{x}}}"
      [[:unescaped-variable [:open-triple-mustache "{{{"] [:name "x"] [:close-triple-mustache "}}}"]]]

      "{{{ x }}}"
      [[:unescaped-variable [:open-triple-mustache "{{{"] " " [:name "x"] " " [:close-triple-mustache "}}}"]]]

      "{{&x}}"
      [[:unescaped-variable [:open-delimiter "{{"] "&" [:name "x"] [:close-delimiter "}}"]]]

      "{{& x }}"
      [[:unescaped-variable [:open-delimiter "{{"] "&" " " [:name "x"] " " [:close-delimiter "}}"]]])))

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
      (t/are [src expected] (= expected (find-name (sut/parse src)))
        "{{x}}" [:name "x"]
        " {{x}} " [:name "x"]
        "{{ x }}" [:name "x"]
        "{{\n\nx\n\n}}" [:name "x"]))

    (t/testing "dotted name"
      (t/are [src expected] (= expected (find-name (sut/parse src)))
        "{{x.y}}" [:name "x" "." [:name "y"]]
        " {{x.y}} " [:name "x" "." [:name "y"]]
        "{{ x.y }}" [:name "x" "." [:name "y"]]
        "{{x.y.z}}" [:name "x" "." [:name "y" "." [:name "z"]]]
        "{{\n\nx.y.z\n\n}}" [:name "x" "." [:name "y" "." [:name "z"]]]))

    (t/testing "illegal names"
      (t/are [src expected] (= expected (sut/parse src))
        "{{.x}}" ["{{.x}}"]
        "{{x.}}" ["{{x.}}"]
        "{{.x.}}" ["{{.x.}}"]
        "{{x . y}}" ["{{x . y}}"]
        "{{x. y}}" ["{{x. y}}"]
        "{{x .y}}" ["{{x .y}}"]
        "{{x}} {{.y}}" [[:escaped-variable [:open-delimiter "{{"] [:name "x"] [:close-delimiter "}}"]]
                        " "
                        "{{.y}}"]
        "{{.x}} {{y}}" ["{{.x}} "
                        [:escaped-variable [:open-delimiter "{{"] [:name "y"] [:close-delimiter "}}"]]]
        "{{x}} {{.y}} {{z}}" [[:escaped-variable [:open-delimiter "{{"] [:name "x"] [:close-delimiter "}}"]]
                              " "
                              "{{.y}} "
                              [:escaped-variable [:open-delimiter "{{"] [:name "z"] [:close-delimiter "}}"]]]
        "{{.x}} {{y}} {{.z}}" ["{{.x}} "
                               [:escaped-variable [:open-delimiter "{{"] [:name "y"] [:close-delimiter "}}"]]
                               " "
                               "{{.z}}"]))))


(t/deftest parse-section-test
  (t/testing "standard section"
    (t/is
     (= [[:section
          [:open-section-tag [:open-delimiter "{{"] "#" [:name "x"] [:close-delimiter "}}"]]
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{#x}}{{/x}}")))

    (t/is
     (= [[:section
          [:open-section-tag [:open-delimiter "{{"] "#" [:name "x" "." [:name "y"]] [:close-delimiter "}}"]]
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "x" "." [:name "y"]] [:close-delimiter "}}"]]]]
        (sut/parse "{{#x.y}}{{/x.y}}")))

    (t/is
     (= [[:section
          [:open-section-tag [:open-delimiter "{{"] "#" [:name "x"] [:close-delimiter "}}"]]
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "y"] [:close-delimiter "}}"]]]]
        (sut/parse "{{#x}}{{/y}}")))

    (t/is
     (= [[:section
          [:open-section-tag [:open-delimiter "{{"] "#" [:name "x"] [:close-delimiter "}}"]]
          [:section
           [:open-section-tag [:open-delimiter "{{"] "#" [:name "y"] [:close-delimiter "}}"]]
           [:close-section-tag [:open-delimiter "{{"] "/" [:name "y"] [:close-delimiter "}}"]]]
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{#x}}{{#y}}{{/y}}{{/x}}")))

    (t/is
     (= [[:section
          [:open-section-tag [:open-delimiter "{{"] "#" [:name "x"] [:close-delimiter "}}"]]
          "{" "{"
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{#x}}{{{{/x}}")))

    (t/is
     (= [[:section
          [:open-section-tag [:open-delimiter "{{"] "#" [:name "x"] [:close-delimiter "}}"]]
          "}}"
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{#x}}}}{{/x}}"))))

  (t/testing "inverted section"
    (t/is
     (= [[:inverted-section
          [:open-inverted-section-tag [:open-delimiter "{{"] "^" [:name "x"] [:close-delimiter "}}"]]
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{^x}}{{/x}}")))

    (t/is
     (= [[:inverted-section
          [:open-inverted-section-tag [:open-delimiter "{{"] "^" [:name "x" "." [:name "y"]] [:close-delimiter "}}"]]
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "x" "." [:name "y"]] [:close-delimiter "}}"]]]]
        (sut/parse "{{^x.y}}{{/x.y}}")))

    (t/is
     (= [[:section
          [:open-section-tag [:open-delimiter "{{"] "#" [:name "x"] [:close-delimiter "}}"]]
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "y"] [:close-delimiter "}}"]]]]
        (sut/parse "{{#x}}{{/y}}"))))

  (t/testing "unopened section"
    (t/is
     (= [[:unopened-section
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{/x}}")))

    (t/is
     (= [[:escaped-variable
          [:open-delimiter "{{"] [:name "x"] [:close-delimiter "}}"]]
         [:unopened-section
          [:close-section-tag [:open-delimiter "{{"] "/" [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{x}}{{/x}}"))))

  (t/testing "unclosed section"
    (t/is
     (= [[:unclosed-section
          [:open-section-tag [:open-delimiter "{{"] "#" [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{#x}}")))

    (t/is
     (= [[:unclosed-section
          [:open-section-tag [:open-delimiter "{{"] "#" [:name "x"] [:close-delimiter "}}"]]
          [:escaped-variable [:open-delimiter "{{"] [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{#x}}{{x}}")))

    (t/is
     (= [[:unclosed-section
          [:open-inverted-section-tag [:open-delimiter "{{"] "^" [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{^x}}")))

    (t/is
     (= [[:unclosed-section
          [:open-inverted-section-tag [:open-delimiter "{{"] "^" [:name "x"] [:close-delimiter "}}"]]
          [:escaped-variable [:open-delimiter "{{"] [:name "x"] [:close-delimiter "}}"]]]]
        (sut/parse "{{^x}}{{x}}")))))

(t/deftest parse-comment-test
  (t/is
   (= [[:comment [:open-delimiter "{{"] "!" "x" [:close-delimiter "}}"]]]
      (sut/parse "{{!x}}")))

  (t/is
   (= [[:comment [:open-delimiter "{{"] "!" " x " [:close-delimiter "}}"]]]
      (sut/parse "{{! x }}")))

  (t/is
   (= [[:comment [:open-delimiter "{{"] "!" "\n\nx\n\n" [:close-delimiter "}}"]]]
      (sut/parse "{{!\n\nx\n\n}}")))

  (t/is
   (= [[:comment [:open-delimiter "{{"] "!" " x y z " [:close-delimiter "}}"]]]
      (sut/parse "{{! x y z }}")))

  (t/is
   (= [[:comment [:open-delimiter "{{"] "!" " x {{x" [:close-delimiter "}}"]] "}}"]
      (sut/parse "{{! x {{x}}}}")))

  (t/is
   (= [[:comment [:open-delimiter "{{"] "!" "{{x" [:close-delimiter "}}"]] " " "x}}"]
      (sut/parse "{{!{{x}} x}}"))))


(t/deftest set-delimiter-test
  (t/is (= [[:set-delimiter "{{=" [:new-open-delimiter "<<"] " " [:new-close-delimiter ">>"] "=}}"
             [:rest-of-mustache ""]]]
           (sut/parse "{{=<< >>=}}")))

  (t/is (= [[:set-delimiter "{{=" [:new-open-delimiter "{%"] " " [:new-close-delimiter "%}"] "=}}"
             [:rest-of-mustache ""]]]
           (sut/parse "{{={% %}=}}")))

  (t/is (= [[:set-delimiter "{{= " [:new-open-delimiter "%"] " " [:new-close-delimiter "%"] " =}}"
             [:rest-of-mustache ""]]]
           (sut/parse "{{= % % =}}")))

  (t/is (= [[:set-delimiter "{{=" [:new-open-delimiter "%"] " " [:new-close-delimiter "%"] "=}}"
             [:rest-of-mustache "{{x}}"]]]
           (sut/parse "{{=% %=}}{{x}}"))))
