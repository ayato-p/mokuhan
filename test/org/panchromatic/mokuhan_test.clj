(ns org.panchromatic.mokuhan-test
  (:require [cheshire.core :as c]
            [clj-http.client :as http]
            [clojure.set :as set]
            [clojure.template :as template]
            [clojure.test :as t]
            [clojure.walk :as walk]
            [org.panchromatic.mokuhan :as sut]
            [org.panchromatic.mokuhan.parser :as parser]))

(defn- get-spec [url]
  (walk/keywordize-keys (c/decode (:body (http/get url)))))

(defmacro generate-test-cases-from-spec [which-spec]
  (let [url (str "https://raw.githubusercontent.com/mustache/spec/master/specs/" which-spec ".json")
        spec (get-spec url)]
    `(t/deftest ~(symbol (str which-spec "-test"))
       ~@(for [test (:tests spec)]
           `(t/testing ~(str (:name test) " / " (:desc test))
              (t/is (= ~(:expected test)
                       (sut/render ~(:template test) ~(:data test)))))))))

(generate-test-cases-from-spec comments)

(generate-test-cases-from-spec delimiters)

(generate-test-cases-from-spec interpolation)

(generate-test-cases-from-spec inverted)

(comment
  (generate-test-cases-from-spec partials))

(generate-test-cases-from-spec sections)


;; https://github.com/mustache/spec/blob/master/specs/sections.json

;; https://github.com/mustache/spec/blob/master/specs/%7Elambdas.json
