(ns org.panchromatic.mokuhan.renderer
  (:require [clojure.string :as str]
            [org.panchromatic.mokuhan.ast :as ast
             #?@(:cljs [:refer [EscapedVariable InvertedSection Mustache StandardSection UnescapedVariable]])]
            [org.panchromatic.mokuhan.renderer.platform :as platform]
            [org.panchromatic.mokuhan.renderer.protocol :as proto]
            [org.panchromatic.mokuhan.util.stringbuilder :as sb]
            [org.panchromatic.mokuhan.walker :as walker])
  #?(:clj
     (:import [org.panchromatic.mokuhan.ast EscapedVariable InvertedSection Mustache StandardSection UnescapedVariable])))

;; don't remove platform ns via ns clean-up
::platform/require

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
  {:position []
   :render identity})

(defn render
  ([ast context]
   (render ast context initial-state))
  ([ast context state]
   (->> (merge initial-state state)
        (proto/render ast context))))
