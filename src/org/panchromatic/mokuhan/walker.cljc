(ns org.panchromatic.mokuhan.walker
  (:require [org.panchromatic.mokuhan.walker.platform :as platform]
            [org.panchromatic.mokuhan.walker.protocol :as proto]))

;; don't remove platform ns via ns clean-up
::platform/require

(defn- path-candidates [path]
  (let [v (peek path)
        path (pop path)]
    (-> (->> (map #(subvec path % (count path)) (range (count path)))
             (mapcat #(let [length (count %)]
                        (->> (for [sep (reverse (range length))]
                               (for [p (if (zero? sep) (take 1 %) (subvec % sep length))]
                                 (conj (subvec % 0 sep) p)))
                             (apply concat))))
             (map #(conj % v)))
        (concat `([~v])))))

(defn traverse
  ([x path]
   (loop [[path & candidates] (path-candidates (vec path))]
     (when path
       (if-let [x (proto/traverse x path)]
         (cond-> x (proto/found-key? x) (.value))
         (recur candidates)))))

  ([x path position]
   (traverse x (into (vec position) path))))
