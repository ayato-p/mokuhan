(ns org.panchromatic.mokuhan.test-helper
  #?(:clj (:require [cheshire.core :as c]
                    [clj-http.client :as http]
                    [clojure.walk :as walk])))

#?(:clj
   (defn- get-spec [url]
     (c/decode (:body (http/get url)) true)))

#?(:clj
   (defmacro generate-test-cases-from-spec [which-spec]
     (let [url (str "https://raw.githubusercontent.com/mustache/spec/master/specs/" which-spec ".json")
           spec (get-spec url)]
       `(t/deftest ~(symbol (str which-spec "-test"))
          ~@(for [test (:tests spec)]
              `(t/testing ~(str (:name test) " / " (:desc test))
                 (t/is (= ~(:expected test)
                          (sut/render ~(:template test) ~(:data test) ~(select-keys test [:partials]))))))))))
