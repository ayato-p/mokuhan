(ns mokuhan.renderer-test
  (:require [clojure.test :as t]
            [mokuhan.ast :as ast]
            [mokuhan.renderer :as sut]))


(t/deftest render-escaped-variable-test
  (t/testing "Single path"
    (let [v (ast/new-escaped-variable ["x"])]
      (t/testing "String"
        (t/is (= "Hi" (sut/render v {:x "Hi"} nil))))

      (t/testing "Integer"
        (t/is (= "42" (sut/render v {:x 42} nil))))

      (t/testing "HTML string"
        (t/is (= "&amp;&lt;&gt;&#39;&quot;" (sut/render v {:x "&<>'\""} nil))))

      (t/testing "nil"
        (t/is (= "" (sut/render v {:x nil} nil))))

      (t/testing "missing"
        (t/is (= "" (sut/render v {} nil)))))))
