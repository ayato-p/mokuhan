(defproject org.panchromatic/mokuhan "0.1.0-SNAPSHOT"
  :description "Yet another implementation of Mustache in Clojure."
  :url "https://github.com/ayato-p/mokuhan"
  :license {:name "MIT License"
            :url "https://choosealicense.com/licenses/mit"}
  :dependencies [[fast-zip "0.7.0"]
                 [instaparse "1.4.8"]
                 [org.clojure/math.combinatorics "0.1.4"]]

  :profiles
  {:provided
   {:dependencies [[org.clojure/clojure "1.9.0"]]}

   :dev
   {:dependencies [[cheshire "5.8.0"]
                   [clj-http "3.8.0"]]}})
