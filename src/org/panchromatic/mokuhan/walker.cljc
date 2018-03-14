(ns org.panchromatic.mokuhan.walker
  (:require [org.panchromatic.mokuhan.walker.platform :as platform]
            [org.panchromatic.mokuhan.walker.protocol :as proto]))

;; don't remove platform ns via ns clean-up
::platform/require

(defn traverse
  ([x path]
   (proto/traverse x path))
  ([x path position]
   (loop [position position]
     (if (seq position)
       (or (-> (traverse x position)
               (traverse path))
           (recur (pop position)))
       (traverse x path)))))
