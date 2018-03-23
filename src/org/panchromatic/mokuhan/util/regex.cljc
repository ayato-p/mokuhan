(ns org.panchromatic.mokuhan.util.regex
  #?(:clj (:import java.util.regex.Pattern)))

(defn re-quote [s]
  #?(:clj
     (re-pattern (Pattern/quote (str s)))
     :cljs
     (re-pattern (.replace s (js/RegExp. "\\W" "g") #(str "\\" %)))))

(defn source [regex]
  #?(:clj
     (.toString regex)
     :cljs
     (.-source regex)))
