(ns org.panchromatic.mokuhan.walker-test
  (:require [org.panchromatic.mokuhan.walker :as sut]
            [clojure.test :as t]))

(t/deftest traverse-test
  (t/testing "simple"
    (t/are [x path position expected] (= expected (sut/traverse x path position))
      {:x 42} ["x"] [] 42
      {:x 42} ["."] [["x"]] 42))

  (t/testing "nested map"
    (t/are [x path position expected] (= expected (sut/traverse x path position))
      {:x {:y 42}} ["y"] [["x"]] 42
      {:x {:y 42}} ["x" "y"] [] 42
      {:x {:y 42}} ["."] [["x" "y"]] 42
      {:x {:y 42}} ["."] [["x"] ["y"]] 42
      {:x {:y 42}} ["."] [["x"] ["."] ["y"]] 42
      {:x {:y 42}} ["."] [["x"] ["."] ["y"] ["."]] 42))

  (t/testing "found a key in a nested map"
    (t/are [x path position expected] (= expected (sut/traverse x path position))
      {:x {:y nil} :y 42} ["y"] [["x"]] nil
      {:x {:y nil} :y 42} ["x" "y"] [] nil
      {:x {:y nil} :y 42} ["."] [["x" "y"]] nil))

  (t/testing "traverser could not find a key in a nested map"
    (t/are [x path position expected] (= expected (sut/traverse x path position))
      {:x {} :y 42} ["y"] [["x"]] 42
      {:x {} :y 42} ["y"] [["x"] ["."]] 42
      {:x {} :y 42} ["."] [["x"] ["y"]] 42))

  (t/testing "with list"
    (t/are [x path position expected] (= expected (sut/traverse x path position))
      {:x [{:y 41} {:y 42} {:y 43}]} ["y"] [["x"] [1]] 42
      {:x [{:y 41} {:y 42} {:y 43}]} ["y"] [["x"] ["."] [1]] 42
      {:x [{:y 41} {:y 42} {:y 43}]} ["y"] [["x"] ["."] [1] ["."]] 42
      {:x [{:y 41} {:y 42} {:y 43}]} ["."] [["x"] ["."] [1] ["y"]] 42))

  (t/testing "with list and could not find a key in a nested map"
    (t/are [x path position expected] (= expected (sut/traverse x path position))
      {:x [{:z 1} {:z 2}] :y 42} ["y"] [["x"]] 42
      {:x [{:z 1} {:z 2}] :y 42} ["y"] [["x"] [0]] 42
      {:x [{:z 1} {:z 2}] :y 42} ["y"] [["x"] ["0"]] 42
      {:x [{:z 1} {:z 2}] :y 42} ["y"] [["x"] ["."] [0]] 42
      {:x [{:z 1} {:z 2}] :y 42} ["y"] [["x"] [0] ["."]] 42
      {:x [{:z 1} {:z 2}] :y 42} ["."] [["x"] [0] ["y"]] 42)))
