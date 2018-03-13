(ns mokuhan.renderer
  (:require [clojure.string :as str]
            [mokuhan.ast :as ast]
            [mokuhan.walker :as walker])
  (:import [mokuhan.ast EscapedVariable InvertedSection Mustache StandardSection UnescapedVariable]))

(defn- escape-html [s]
  (-> s
      (str/replace #"&" "&amp;")
      (str/replace #"<" "&lt;")
      (str/replace #">" "&gt;")
      (str/replace #"\"" "&quot;")
      (str/replace #"'" "&#39;")))

(defprotocol Rendable
  (render [this context] [this context state]))

(def ^:private initial-state
  {:position []})

(extend-protocol Rendable
  nil
  (render
    ([_ _] "")
    ([_ _ _] ""))

  Object
  (render
    ([o _] (.toString o))
    ([o _ _] (.toString o)))

  Mustache
  (render
    ([mustache context]
     (render mustache context initial-state))
    ([mustache context state]
     (-> (reduce (fn [sb c]
                   (->> (render c context state)
                        (.append sb)))
                 (StringBuilder. 1024)
                 (ast/children mustache))
         (.toString))))

  EscapedVariable
  (render
    ([variable context]
     (render variable context initial-state))
    ([variable context state]
     (let [position (:position state)]
       (-> (walker/traverse context (.path variable) position)
           (render context state)
           escape-html))))

  UnescapedVariable
  (render
    ([variable context]
     (render variable context initial-state))
    ([variable context state]
     (let [position (:position state)]
       (-> (walker/traverse context (.path variable) position)
           (render context state)))))

  StandardSection
  (render [section context state]
    (let [path (.path section)
          contents (.contents section)
          position (:position state)
          o (walker/traverse context path position)]
      (cond
        ;; list
        (instance? java.util.List o)
        (->> (for [idx (range (count o)) ast contents] [idx ast])
             (reduce (fn [sb [idx ast]]
                       (->> (-> state
                                (update :position into path)
                                (update :position conj idx))
                            (render ast context)
                            (.append sb)))
                     (StringBuilder. 1024)))

        ;; truthy
        o
        (reduce (fn [sb ast]
                  (->> (update state :position into path)
                       (render ast context)
                       (.append sb)))
                (StringBuilder. 1024)
                contents)
        ;; falsy
        (not o)
        "")))

  InvertedSection
  (render [section context state]))
