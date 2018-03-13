(ns mokuhan.renderer
  (:require [clojure.string :as str]
            [mokuhan.ast :as ast]
            [mokuhan.walk :as walk])
  (:import [mokuhan.ast EscapedVariable InvertedSection Mustache StandardSection UnescapedVariable]))

(defn- escape-html [s]
  (-> s
      (str/replace #"&" "&amp;")
      (str/replace #"<" "&lt;")
      (str/replace #">" "&gt;")
      (str/replace #"\"" "&quot;")
      (str/replace #"'" "&#39;" #_"&apos;")))

(defprotocol Rendable
  (render [this data state]))

(defn- initial-state []
  {:position []})

(extend-protocol Rendable
  Mustache
  (render [mustache data state]
    (let [contents (ast/children mustache)]
      (reduce (fn [sb c]
                (->> (render c data (initial-state))
                     (.append sb)))
              (StringBuffer. contents)
              contents)))

  EscapedVariable
  (render [variable data state]
    (let [position (:position state)]
      (-> (traverse data (.path variable) position)
          str
          escape-html)))

  UnescapedVariable
  (render [variable data state]
    (let [position (:position state)]
      (-> (traverse data (.path variable) position)
          str)))

  StandardSection
  (render [section data state]))
