(ns org.panchromatic.mokuhan.walker.platform
  (:require [clojure.reflect :as reflect]
            [org.panchromatic.mokuhan.walker.protocol :as proto]))

(defn- invokable-members [o]
  (let [{:keys [bases members]} (reflect/reflect o)
        xform (comp (filter #(empty? (:parameter-types %)))
                    (map :name)
                    (map str))]
    (->> (map resolve bases)
         (reduce #(into %1 (:members (reflect/reflect %2))) members)
         (into #{} xform))))

(defn- invoke-instance-member [^Object o ^String method-name]
  (try
    (clojure.lang.Reflector/invokeInstanceMember o method-name)
    (catch Exception e)))

(extend-protocol proto/Traverser
  Object
  (traverse
    ([o path]
     (let [[p & path] path]
       (-> (if (= p ".")
             o
             (when ((invokable-members o) p)
               (let [x (invoke-instance-member o p)]
                 (or x (proto/found-key x)))))
           (cond-> (seq path) (proto/traverse path))))))

  java.util.Map
  (traverse
    ([^java.util.Map m path]
     (let [[p & path] path]
       (-> (if (= p ".")
             m
             (reduce (fn [_ k]
                       (when (.containsKey m k)
                         (reduced
                          (let [x (.get m k)]
                            (or x (proto/found-key x))))))
                     nil
                     [p (keyword p) (symbol p)]))
           (cond-> (seq path) (proto/traverse path))))))

  java.util.List
  (traverse
    ([^java.util.List l path]
     (let [[p & path] path]
       (-> (if (= p ".")
             l
             (let [p (if (integer? p)
                       p
                       (some->> p str (re-matches #"\d+") (Long/parseLong)))
                   size (.size l)]
               (when (and p (<= p (dec size)))
                 (let [x (.get l p)]
                   (or x (proto/found-key x))))))
           (cond-> (seq path) (proto/traverse path)))))))
