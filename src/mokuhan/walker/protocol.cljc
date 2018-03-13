(ns mokuhan.walker.protocol)

(defprotocol Traverser
  (traverse [this path]))

(extend-protocol Traverser
  nil
  (traverse
    ([_ _])))
