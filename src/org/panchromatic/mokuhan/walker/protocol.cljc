(ns org.panchromatic.mokuhan.walker.protocol)

(defrecord FoundKey [value])

(defn found-key
  ([] (FoundKey. nil))
  ([value] (FoundKey. value)))

(defn found-key? [x]
  (instance? FoundKey x))

(defprotocol Traverser
  (traverse [this path]))

(extend-protocol Traverser
  nil
  (traverse [_ _])

  FoundKey
  (traverse [fk path]
    (traverse (.value fk) path)))
