(ns mokuhan.walker
  (:require mokuhan.walker.platform
            [mokuhan.walker.protocol :as proto]))

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
