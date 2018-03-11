(ns mustaclj.renderer
  (:require [clojure.string :as str]
            [mustaclj.ast :as ast])
  (:import [mustaclj.ast EscapedVariable Mustache StandardSection UnescapedVariable]))

(defprotocol Traverse
  (traverse [this path] [this path position]))

(extend-protocol Traverse
  nil
  (traverse
    ([_ _])
    ([_ _ _]))

  clojure.lang.IPersistentMap
  (traverse
    ([m path]
     (let [[p & path] path]
       (cond-> (get m (keyword p))
         (seq path) (traverse path))))
    ([m path position]
     (loop [position position]
       (if (seq position)
         (or (-> (traverse m position)
                 (traverse path))
             (recur (pop position)))
         (traverse m path))))))

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
