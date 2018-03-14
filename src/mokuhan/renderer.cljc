(ns mokuhan.renderer
  (:require [clojure.string :as str]
            [mokuhan.ast :as ast
             #?@(:cljs [:refer [mokuhan.ast EscapedVariable InvertedSection Mustache StandardSection UnescapedVariable]])]
            mokuhan.renderer.platform
            [mokuhan.renderer.protocol :as proto]
            [mokuhan.util.stringbuilder :as sb]
            [mokuhan.walker :as walker])
  #?(:clj (:import [mokuhan.ast EscapedVariable InvertedSection Mustache StandardSection UnescapedVariable])))

(defn- escape-html [s]
  (-> s
      (str/replace #"&" "&amp;")
      (str/replace #"<" "&lt;")
      (str/replace #">" "&gt;")
      (str/replace #"\"" "&quot;")
      (str/replace #"'" "&#39;")))

(extend-protocol proto/Rendable
  Mustache
  (render
    ([mustache context state]
     (-> (reduce (fn [sb c]
                   (->> (proto/render c context state)
                        (sb/append sb)))
                 (sb/new-string-builder)
                 (ast/children mustache))
         (sb/to-string))))

  EscapedVariable
  (render
    ([variable context state]
     (let [position (:position state)]
       (-> (walker/traverse context (.path variable) position)
           (proto/render context state)
           escape-html))))

  UnescapedVariable
  (render
    ([variable context state]
     (let [position (:position state)]
       (-> (walker/traverse context (.path variable) position)
           (proto/render context state)))))

  StandardSection
  (render [section context state]
    (-> (walker/traverse context (.path section) (:position state))
        (proto/render-section section context state)))

  InvertedSection
  (render [section context state]
    (-> (walker/traverse context (.path section) (:position state))
        (proto/render-inverted-section section context state))))

(def ^:private initial-state
  {:position []})

(defn render [ast context]
  (proto/render ast context initial-state))
