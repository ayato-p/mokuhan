(ns org.panchromatic.mokuhan.renderer.platform
  (:require [org.panchromatic.mokuhan.ast :as ast :refer [StandardSection]]
            [org.panchromatic.mokuhan.renderer.protocol :as proto]
            [org.panchromatic.mokuhan.util.stringbuilder :as sb]))

(extend-type function
  proto/Rendable
  (render [f data state]
    (let [render ((:render state identity))]
      (-> (f) (proto/render data state) render)))

  proto/StandardSectionRenderer
  (render-section [f section data state]
    (let [delimiters (ast/get-open-tag-delimiters section)
          render ((:render state identity) {:delimiters delimiters})]
      (-> (ast/children section)
          (->> (reduce #(sb/append %1 (.toString %2)) (sb/new-string-builder)))
          sb/to-string
          f
          (proto/render data state)
          render))))

(extend-type array
  proto/StandardSectionRenderer
  (render-section [ary ^StandardSection section data state]
    (let [path (.-path section)
          contents (.-contents section)]
      (->> (for [idx (range (.-length ary)) ast contents] [idx ast])
           (reduce (fn [sb [idx ast]]
                     (->> (-> state
                              (update :position (fnil conj []) path [idx]))
                          (proto/render ast data)
                          (sb/append sb)))
                   (sb/new-string-builder))
           (sb/to-string))))

  proto/InvertedSectionRenderer
  (render-inverted-section [ary section data state]
    (proto/render-section-simply section data state)))

(defn- render-section-seq [l-or-v ^StandardSection section data state]
  (let [path (.-path section)
        contents (.-contents section)]
    (->> (for [idx (range (count l-or-v)) ast contents] [idx ast])
         (reduce (fn [sb [idx ast]]
                   (->> (-> state
                            (update :position (fnil conj []) path [idx]))
                        (proto/render ast data)
                        (sb/append sb)))
                 (sb/new-string-builder))
         (sb/to-string))))

(extend-type cljs.core/EmptyList
  proto/StandardSectionRenderer
  (render-section [l section data state]
    "")

  proto/InvertedSectionRenderer
  (render-inverted-section [l section data state]
    (proto/render-section-simply section data state)))

(extend-type cljs.core/List
  proto/StandardSectionRenderer
  (render-section [l section data state]
    (render-section-seq l section data state))

  proto/InvertedSectionRenderer
  (render-inverted-section [l section data state]
    (if (empty? l)
      (proto/render-section-simply section data state)
      "")))

(extend-type cljs.core/PersistentVector
  proto/StandardSectionRenderer
  (render-section [v section data state]
    (render-section-seq v section data state))

  proto/InvertedSectionRenderer
  (render-inverted-section [v section data state]
    (if (empty? v)
      (proto/render-section-simply section data state)
      "")))
