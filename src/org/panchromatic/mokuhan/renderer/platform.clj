(ns org.panchromatic.mokuhan.renderer.platform
  (:require [org.panchromatic.mokuhan.renderer.protocol :as proto]
            [clojure.set :as set]))

(extend-type clojure.lang.AFunction
  proto/StandardSectionRenderer
  (render-section [f section context state]
    (f (proto/render-section-simply section context state))))

(extend-type java.util.List
  proto/StandardSectionRenderer
  (render-section [l section context state]
    (let [path (.path section)
          contents (.contents section)]
      (->> (for [idx (range (count l)) ast contents] [idx ast])
           (reduce (fn [sb [idx ast]]
                     (->> (-> state
                              (update :position conj path [idx]))
                          (proto/render ast context)
                          (.append sb)))
                   (StringBuilder. 1024))
           (.toString))))

  proto/InvertedSectionRenderer
  (render-inverted-section [l section context state]
    (if (.isEmpty l)
      (proto/render-section-simply section context state)
      "")))
