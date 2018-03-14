(ns org.panchromatic.mokuhan.walker.platform
  (:require [org.panchromatic.mokuhan.walker.protocol :as proto]))

(defn- invoke-instance-method [^Object o ^String method-name]
  (try
    (clojure.lang.Reflector/invokeInstanceMethod o method-name (into-array []))
    (catch Exception _)))

(extend-protocol proto/Traverser
  Object
  (traverse
    ([o path]
     (let [[p & path] path]
       (if (= p ".")
         o
         (cond-> (invoke-instance-method o p)
           (seq path) (proto/traverse path))))))

  java.util.Map
  (traverse
    ([m path]
     (let [[p & path] path]
       (if (= p ".")
         m
         (cond-> (if (.containsKey m p)
                   (.get m p)
                   (.get m (keyword p)) )
           (seq path) (proto/traverse path))))))

  java.util.List
  (traverse
    ([l path]
     (let [[p & path] path]
       (if (= p ".")
         l
         (cond-> (.get l p)
           (seq path) (proto/traverse path)))))))
