(ns mokuhan.util.stringbuilder)

#?(:clj
   (defn new-string-builder []
     (StringBuilder.))

   :cljs
   (defn new-string-builder []
     ;; TODO
     ))

#?(:clj
   (defn append [^StringBuilder sb ^String s]
     (.append sb s))

   :cljs
   (defn append [sb s]
     ;; TODO
     ))

#?(:clj
   (defn to-string [^StringBuilder sb]
     (.toString sb))

   :cljs
   (defn to-string [sb]
     ;; TODO
     ))
