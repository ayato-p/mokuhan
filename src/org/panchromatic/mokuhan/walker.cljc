(ns org.panchromatic.mokuhan.walker
  (:require [org.panchromatic.mokuhan.walker.platform :as platform]
            [org.panchromatic.mokuhan.walker.protocol :as proto]))

;; don't remove platform ns via ns clean-up
::platform/require

(defn traverse
  ([x path]
   (let [x (proto/traverse x path)]
     (cond-> x
       (proto/found-key? x) (.value))))

  ([x path position]
   (let [x (loop [position position]
             (if (seq position)
               (or (-> (proto/traverse x position)
                       (proto/traverse path))
                   (recur (pop position)))
               (proto/traverse x path)))]
     (cond-> x (proto/found-key? x) (.value)))))
