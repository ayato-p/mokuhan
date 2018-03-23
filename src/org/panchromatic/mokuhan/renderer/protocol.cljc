(ns org.panchromatic.mokuhan.renderer.protocol
  (:require [org.panchromatic.mokuhan.util.misc :as misc]
            [org.panchromatic.mokuhan.util.stringbuilder :as sb]))

(defprotocol Rendable
  (render [this context state]))

(extend-protocol Rendable
  nil
  (render
    ([_ _ _] ""))

  #?(:clj Object :cljs default)
  (render
    ([o _ _] (.toString o))))

(defn render-section-simply [section context state]
  (let [path #?(:clj (.path section) :cljs (:path section))
        contents #?(:clj (.contents section) :cljs (:contents section))]
    (->> contents
         (reduce (fn [sb ast]
                   (->> (update state :position conj path)
                        (render ast context)
                        (sb/append sb)))
                 (sb/new-string-builder))
         (sb/to-string))))


(defprotocol StandardSectionRenderer
  (render-section [this section context state]))

(extend-protocol StandardSectionRenderer
  nil
  (render-section [_ _ _ _]
    "")

  #?(:clj Object :cljs default)
  (render-section [o section context state]
    (if (misc/truthy? o)
      (render-section-simply section context state)
      "")))


(defprotocol InvertedSectionRenderer
  (render-inverted-section [this section context state]))

(extend-protocol InvertedSectionRenderer
  nil
  (render-inverted-section [_ section context state]
    (render-section-simply section context state))

  #?(:clj Object :cljs default)
  (render-inverted-section [o section context state]
    (if-not (misc/truthy? o)
      (render-section-simply section context state)
      "")))
