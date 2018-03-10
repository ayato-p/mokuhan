(ns mustaclj.parser
  (:import java.util.regex.Pattern)
  (:require [instaparse.core :as insta]))

(defn- re-quote [s]
  (re-pattern (Pattern/quote (str s))))

;;; {{name}}   -> variable
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

(def default-delimiters
  {:open "{{" :close "}}"})

(defn generate-mustache-spec [{:keys [open close] :as delimiters}]
  (str "
<mustache> = *(tag / text)
text = !tag #'^.+?(?:(?=" (re-quote open)  ")|$)'
whitespace = #'\\s+'

<tag> = (variable / section / comment / set-delimiter)
<ident> = #'(?!\\d)[\\w\\Q$%&*-_+=|?<>\\E][\\w\\Q!$%&*-_+=|:?<>\\E]*'
name = ident *(<'.'> ident)

<variable> = !section !comment !set-delimiter (escaped-variable / unescaped-variable)
escaped-variable = !unescaped-variable <#'^" (re-quote open) "'> <*1 whitespace> name <*1 whitespace> <#'" (re-quote close) "'>
unescaped-variable = (!ampersand-unescaped-variable triple-mustache-unescaped-variable / ampersand-unescaped-variable)
<ampersand-unescaped-variable> = <#'^" (re-quote (str open "&")) "'> <*1 whitespace> name <*1 whitespace> <#'" (re-quote close) "'>
<triple-mustache-unescaped-variable> = <#'^" (re-quote "{{{") "'> <*1 whitespace> name <*1 whitespace> <#'" (re-quote "}}}") "'>

<section> = (open-section / close-section / open-inverted-section)
open-section = <#'^" (re-quote (str open "#")) "'> <*1 whitespace> name <*1 whitespace> <#'" (re-quote close) "'>
close-section = <#'^" (re-quote (str open "/")) "'> <*1 whitespace> name <*1 whitespace> <#'" (re-quote close) "'>
open-inverted-section = <#'^" (re-quote (str open "^")) "'> <*1 whitespace> name <*1 whitespace> <#'" (re-quote close) "'>

comment = <#'^" (re-quote (str open "!")) "'> #'(?:.|\\r?\\n)*?(?=" (re-quote close) ")' <#'" (re-quote close) "'>

set-delimiter = <#'^" (re-quote (str open "=")) "'> <*1 whitespace> new-open-delimiter <1* whitespace> new-close-delimiter <*1 whitespace> <#'" (re-quote (str "=" close)) "'> *rest
new-open-delimiter = #'[^\\s]+'
new-close-delimiter = #'[^\\s]+?(?=\\s*" (re-quote (str "=" close)) ")'
rest = #'(.|\\r?\\n)*$'
"))

(defn gen-parser [delimiters]
  (insta/parser (generate-mustache-spec delimiters) :input-format :abnf))

(def default-parser
  (gen-parser default-delimiters))

(defn parse
  ([mustache]
   (parse mustache {}))
  ([mustache opts]
   (->> (reduce-kv #(conj %1 %2 %3) [] opts)
        (apply insta/parse default-parser mustache))))
