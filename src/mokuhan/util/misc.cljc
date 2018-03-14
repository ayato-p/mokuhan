(ns mokuhan.util.misc)

(defn truthy? [o]
  (and (not (false? o)) (some? o)))

(defn meta-without-qualifiers [x]
  (some->> (meta x)
           (reduce-kv #(assoc %1 (-> %2 name keyword) %3) {})))
