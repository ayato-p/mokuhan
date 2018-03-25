(defproject org.panchromatic/mokuhan "0.1.1-SNAPSHOT"
  :description "Yet another implementation of Mustache in Clojure."
  :url "https://github.com/ayato-p/mokuhan"
  :license {:name "MIT License"
            :url "https://choosealicense.com/licenses/mit"}

  :deploy-repositories [["releases" :clojars]]

  :dependencies [[fast-zip "0.7.0"]
                 [instaparse "1.4.8"]
                 [org.clojure/math.combinatorics "0.1.4"]]

  :profiles
  {:provided
   {:dependencies [[org.clojure/clojure "1.9.0"]
                   [org.clojure/clojurescript "1.9.946"]]}

   :dev
   {:dependencies [[cheshire "5.8.0"]
                   [clj-http "3.8.0"]
                   [com.cemerick/piggieback "0.2.2"]
                   [doo "0.1.10"]]}

   :cljstest
   [:plugin/cljsbuild :plugin/doo]

   :plugin/cljsbuild
   {:plugins [[lein-cljsbuild "1.1.7"]]
    :cljsbuild
    {:builds
     {:node-test {:source-paths ["src" "test"]
                  :compiler {:output-to "target/node-test/org/panchromatic/mokuhan-test.js"
                             :output-dir "target/node-test/org/panchromatic"
                             :main org.panchromatic.mokuhan.test-runner
                             :optimizations :advanced
                             :target :nodejs}}

      :phantom-test {:source-paths ["src" "test"]
                     :compiler {:output-to "target/phantom-test/org/panchromatic/mokuhan-test.js"
                                :output-dir "target/phantom-test/org/panchromatic"
                                :main org.panchromatic.mokuhan.test-runner
                                :optimizations :advanced}}}}}

   :plugin/doo
   {:plugins [[lein-doo "0.1.10"]]}}

  :aliases
  {"cljstest-node" ["doo" "node" "node-test" "once"]
   "cljstest-phantom" ["doo" "phantom" "phantom-test" "once"]
   "cljstest" ["with-profile" "+cljstest" "do" ["cljstest-node"] ["cljstest-phantom"]]
   "test-all" ["do" ["cljstest"] ["test"]]})
