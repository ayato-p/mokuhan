(ns org.panchromatic.mokuhan.renderer.platform
  (:require [org.panchromatic.mokuhan.ast :as ast]
            [org.panchromatic.mokuhan.renderer.protocol :as proto]
            [org.panchromatic.mokuhan.util.stringbuilder :as sb]))

(extend-type clojure.lang.AFunction
  proto/Rendable
  (render [f context state]
    (let [render ((:render state identity))]
      (-> (f) (proto/render context state) render)))

  proto/StandardSectionRenderer
  (render-section [f section context state]
    ;; Temporary fix
    (let [delimiters (ast/get-open-tag-delimiters section)
          render ((:render state) {:delimiters delimiters})]
      (-> (ast/children section)
          (->> (reduce #(sb/append %1 (.toString %2)) (sb/new-string-builder)))
          sb/to-string
          f
          (proto/render context state)
          render))))

(extend-type java.util.List
  proto/StandardSectionRenderer
  (render-section [l section context state]
    (let [path (.path section)
          contents (.contents section)]
      (->> (for [idx (range (count l)) ast contents] [idx ast])
           (reduce (fn [sb [idx ast]]
                     (->> (-> state
                              (update :position (fnil conj []) path [idx]))
                          (proto/render ast context)
                          (.append sb)))
                   (StringBuilder. 1024))
           (.toString))))

  proto/InvertedSectionRenderer
  (render-inverted-section [l section context state]
    (if (.isEmpty l)
      (proto/render-section-simply section context state)
      "")))
