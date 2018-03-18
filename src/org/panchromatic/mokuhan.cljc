(ns org.panchromatic.mokuhan
  (:require [org.panchromatic.mokuhan.parser :as parser]
            [org.panchromatic.mokuhan.renderer :as renderer]))

(defn render
  ([mustache data]
   (render mustache data {}))

  ([mustache data opts]
   (-> (parser/parse mustache)
       (renderer/render data {:render #(render % data opts)}))))
