(ns mustaclj.renderer
  (:require [clojure.string :as str])
  (:import [mustaclj.ast EscapedVariable StandardSection UnescapedVariable]))

(defprotocol Traverse
  (traverse [this key]))

(extend-protocol Traverse
  clojure.lang.IPersistentMap
  (traverse [this key]
    (get this (keyword key))))

(defn- escape-html [s]
  (-> s
      (str/replace #"&" "&amp;")
      (str/replace #"<" "&lt;")
      (str/replace #">" "&gt;")
      (str/replace #"\"" "&quot;")
      (str/replace #"'" "&#39;" #_"&apos;")))

(defprotocol Rendable
  (render [this data]))

(extend-protocol Rendable
  EscapedVariable
  (render [variable data]
    (-> (reduce #(traverse %1 %2) data (.keys variable))
        str
        escape-html))

  UnescapedVariable
  (render [variable data]
    (-> (->> (.keys variable)
             (reduce #(traverse %1 %2) data))
        str))

  StandardSection
  (render [section data]))
