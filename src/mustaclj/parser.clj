(ns mustaclj.parser
  (:import java.util.regex.Pattern)
  (:require [instaparse.core :as insta]))

(defn- re-quote [s]
  (re-pattern (Pattern/quote (str s))))

;;; {{name}} -> variable
;;; {{{name}}} -> unescaped variable
;;; {{#persone}} <-> {{/person}} -> section
;;;   false or empty list -> delete
;;;   non empty list -> repeat
;;;   lambda -> call function
;;;   non-false -> context
;;; {{^name}} <-> {{/name}} -> inverted variable
;;; {{! blah }} -> comment
;;; {{> box}} -> partial
;;; {{=<% %>=}} -> set delimiter

(def default-open-delimiter "{{")
(def default-close-delimiter "}}")

(defn gen-parser [open-delimiter* close-delimiter*]
  (let [open-delimiter (re-quote open-delimiter*)
        close-delimiter (re-quote close-delimiter*)]
    (-> (str
         "
<mustache> = *(tags / text / space)
<text> = !tags #'(?m)^.+?(?:(?=" open-delimiter ")|$)'
<space> = #'\\s+'

<tags> = (variables / sections / comment / set-delimiter)
open-delimiter = #'" open-delimiter "'
close-delimiter = #'" close-delimiter "'
name = #'(?!\\d)[\\w\\Q$%&*-_+=|?<>\\E][\\w\\Q!$%&*-_+=|:?<>\\E]*' *1 ('.' name)

<variables> = (escaped-variable / unescaped-variable)
escaped-variable = open-delimiter *1 space name *1 space close-delimiter
unescaped-variable = ( " (when (and (= default-open-delimiter open-delimiter*)
                                    (= default-close-delimiter close-delimiter*))
                           "open-triple-mustache *1 space name *1 space close-triple-mustache / ")
         "open-delimiter '&' *1 space name *1 space close-delimiter )
open-triple-mustache = #'\\Q{{{\\E'
close-triple-mustache = #'\\Q}}}\\E'

<sections> = (section / inverted-section / unopened-section / unclosed-section)
section = open-section-tag mustache close-section-tag
open-section-tag = open-delimiter '#' *1 space name *1 space close-delimiter
close-section-tag = open-delimiter '/' *1 space name *1 space close-delimiter

inverted-section = open-inverted-section-tag mustache close-section-tag
open-inverted-section-tag = open-delimiter '^' *1 space name *1 space close-delimiter

unopened-section = !(section / inverted-section) mustache close-section-tag
unclosed-section = !(section / inverted-section) (open-section-tag / open-inverted-section-tag) mustache

comment = open-delimiter '!' #'(?:.|\r?\n)*?(?=" close-delimiter ")' close-delimiter

set-delimiter = #'" open-delimiter "=\\s*' new-open-delimiter ' ' new-close-delimiter #'\\s*=" close-delimiter "' rest-of-mustache
new-open-delimiter = #'[^\\s]+'
new-close-delimiter = #'[^\\s]+?(?=\\s*" (re-quote (str "=" close-delimiter*)) ")'
rest-of-mustache = #'(?m)(.|\r?\n)*'
")
        (insta/parser :input-format :abnf))))

(def default-parser
  (gen-parser "{{" "}}"))

(defn parse
  ([mustache]
   (parse mustache {}))
  ([mustache opts]
   (->> (reduce-kv #(conj %1 %2 %3) [] opts)
        (apply insta/parse default-parser mustache))))
