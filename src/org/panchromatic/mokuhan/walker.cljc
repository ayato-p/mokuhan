(ns org.panchromatic.mokuhan.walker
  (:require [clojure.math.combinatorics :as comb]
            [org.panchromatic.mokuhan.walker.platform :as platform]
            [org.panchromatic.mokuhan.walker.protocol :as proto]))

;; don't remove platform ns via ns clean-up
::platform/require

(defn- path-candidates [path]
  (if (>= 1 (count path))
    (list path)
    (let [v (peek path)
          path (map-indexed #(vector (inc %1) %2) (pop path))]
      (concat
       (map
        #(conj (nth % 1) v)
        (sort-by
         #(+ (nth % 0) (count (nth % 1)))
         >
         (reduce (fn [v i]
                   (->> (comb/combinations path i)
                        (map #(reduce (fn [w [j x]]
                                        (-> w (update 0 * j) (update 1 conj x)))
                                      [1 []]
                                      (sort-by first < %)))
                        (concat v)))
                 ()
                 (range 1 (inc (count path))))))
       (list [v])))))

(defn traverse* [x paths]
  (loop [[path & candidates] (path-candidates paths)]
    (when path
      (if-let [x (reduce #(cond-> %1
                            (not= (first %2) ".")
                            (proto/traverse %2))
                         x
                         path)]
        (cond-> x (proto/found-key? x) #?(:clj (.value) :cljs (.-value)))
        (recur candidates)))))

(defn traverse
  ([x path position]
   (->> (conj (vec position) path)
        (traverse* x))))
