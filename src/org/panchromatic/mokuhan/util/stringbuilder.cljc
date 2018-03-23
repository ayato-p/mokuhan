(ns org.panchromatic.mokuhan.util.stringbuilder
  #?(:cljs (:require [clojure.string :as str])))

#?(:clj
   (defn new-string-builder []
     (StringBuilder.))

   :cljs
   (defn new-string-builder []
     (atom [])))

#?(:clj
   (defn append [^StringBuilder sb ^String s]
     (.append sb s))

   :cljs
   (defn append [sb s]
     (swap! sb conj s)
     sb))

#?(:clj
   (defn to-string [^StringBuilder sb]
     (.toString sb))

   :cljs
   (defn to-string [sb]
     (str/join @sb)))
