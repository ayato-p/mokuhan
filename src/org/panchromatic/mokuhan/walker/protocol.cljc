(ns org.panchromatic.mokuhan.walker.protocol)

(defprotocol Traverser
  (traverse [this path]))

(extend-protocol Traverser
  nil
  (traverse
    ([_ _])))
