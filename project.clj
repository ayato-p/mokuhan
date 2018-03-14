(defproject org.panchromatic/mokuhan "0.1.0-SNAPSHOT"
  :description "Yet another implementation of Mustache in Clojure."
  :url "https://github.com/ayato-p/mokuhan"
  :license {:name "MIT License"
            :url "https://choosealicense.com/licenses/mit"}
  :dependencies [[instaparse "1.4.8"]
                 [fast-zip "0.7.0"]]

  :profiles
  {:provided {:dependencies [[org.clojure/clojure "1.9.0"]]}})
