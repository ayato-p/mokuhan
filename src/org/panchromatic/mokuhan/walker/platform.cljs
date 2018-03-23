(ns org.panchromatic.mokuhan.walker.platform
  (:require [goog.array :as ary]
            [goog.object :as o]
            [org.panchromatic.mokuhan.walker.protocol :as proto]))

(defn- traverse-map [m [p & path]]
  (let [p (cond-> p (not (string? p)) str)]
    (-> (if (= p ".")
          m
          (reduce (fn [_ k]
                    (when (contains? m k)
                      (reduced
                       (let [x (get m k)]
                         (or x (proto/found-key x))))))
                  nil
                  [p (keyword p) (symbol p)]))
        (cond-> (seq path) (proto/traverse path)))))

(extend-protocol proto/Traverser
  default
  (traverse [_ _] nil)

  object
  (traverse [o path]
    (let [[p & path] path]
      (-> (if (= p ".")
            o
            (when (o/containsKey o p)
              (let [x (o/get o p)]
                (or (cond-> x (fn? x) (.call))
                    (proto/found-key x)))))
          (cond-> (seq path) (proto/traverse path)))))

  array
  (traverse [ary path]
    (let [[p & path] path]
      (-> (if (= p ".")
            ary
            (let [p (if (integer? p)
                      p
                      (some->> p str (re-matches #"\d+") (js/parseInt)))
                  size (.-length ary)]
              (when (and p (<= p (dec size)))
                (let [x (aget ary p)]
                  (or x (proto/found-key x))))))
          (cond-> (seq path) (proto/traverse path)))))

  cljs.core/EmptyList
  (traverse [l path]
    (proto/traverse (into-array l) path))

  cljs.core/List
  (traverse [l path]
    (proto/traverse (into-array l) path))

  cljs.core/PersistentVector
  (traverse [v path]
    (let [[p & path] path]
      (-> (if (= p ".")
            v
            (let [p (if (integer? p)
                      p
                      (some->> p str (re-matches #"\d+") (js/parseInt)))
                  size (count v)]
              (when (and p (<= p (dec size)))
                (let [x (get v p)]
                  (or x (proto/found-key x))))))
          (cond-> (seq path) (proto/traverse path)))))


  cljs.core/PersistentHashMap
  (traverse [m path]
    (traverse-map m path))

  cljs.core/PersistentTreeMap
  (traverse [m path]
    (traverse-map m path))

  cljs.core/PersistentArrayMap
  (traverse [m path]
    (traverse-map m path)))
