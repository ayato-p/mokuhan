(ns mokuhan.util.misc)

(defn truthy? [o]
  (and (not (false? o)) (some? o)))
