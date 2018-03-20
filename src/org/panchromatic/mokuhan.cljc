(ns org.panchromatic.mokuhan
  (:require [org.panchromatic.mokuhan.parser :as parser]
            [org.panchromatic.mokuhan.renderer :as renderer]))

(defn render
  ([mustache data]
   (render mustache data {}))

  ([mustache data opts]
   (let [render' (fn [& [opts']] #(render % data (merge opts opts')))]
     (-> (parser/parse mustache opts)
         (renderer/render data (assoc opts :render render'))))))
