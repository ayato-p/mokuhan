(ns mokuhan.renderer.protocol
  (:require [mokuhan.util.misc :as misc]
            [mokuhan.util.stringbuilder :as sb]))

(defprotocol Rendable
  (render [this context state]))

(extend-protocol Rendable
  nil
  (render
    ([_ _ _] ""))

  Object
  (render
    ([o _ _] (.toString o))))


(defn render-section-simply [section context state]
  (let [path (.path section)
        contents (.contents section)]
    (->> contents
         (reduce (fn [sb ast]
                   (->> (update state :position into path)
                        (render ast context)
                        (sb/append sb)))
                 (sb/new-string-builder))
         (sb/to-string))))


(defprotocol StandardSectionRenderer
  (render-section [this section context state]))

(extend-protocol StandardSectionRenderer
  Object
  (render-section [o section context state]
    (if (misc/truthy? o)
      (render-section-simply section context state)
      "")))


(defprotocol InvertedSectionRenderer
  (render-inverted-section [this section context state]))

(extend-protocol InvertedSectionRenderer
  Object
  (render-inverted-section [o section context state]
    (if-not (misc/truthy? o)
      (render-section-simply section context state)
      "")))
