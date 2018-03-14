(ns mokuhan.parser
  (:require [fast-zip.core :as zip]
            [instaparse.core :as insta]
            [mokuhan.ast :as ast]
            [mokuhan.util.misc :as misc])
  (:import java.util.regex.Pattern))

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
<mustache> = *(*(tag / text / whitespace) (newline / <eof>))
text = !tag #'^[^\\r\\n\\s]+?(?=(?:" (re-quote open)  "|\\r?\\n|\\s|\\z))'
whitespace = #'[^\\S\\r\\n]+'
newline = #'\\r?\\n'
eof = #'\\z'

<tag> = (set-delimiter / comment / section / variable)
<ident> = #'(?!\\d)[\\w\\Q$%&*-_+=|?<>\\E][\\w\\Q!$%&*-_+=|:?<>\\E]*?(?=\\s|\\.|" (re-quote close) ")'
name = ident *(<'.'> ident)

<variable> = !section !comment !set-delimiter (escaped-variable / unescaped-variable)
escaped-variable = !unescaped-variable <#'^" (re-quote open) "'> <*1 #'\\s+'> name <*1 #'\\s+'> <#'" (re-quote close) "'>
unescaped-variable = (!ampersand-unescaped-variable triple-mustache-unescaped-variable / ampersand-unescaped-variable)
<ampersand-unescaped-variable> = <#'^" (re-quote (str open "&")) "'> <*1 #'\\s+'> name <*1 #'\\s+'> <#'" (re-quote close) "'>
<triple-mustache-unescaped-variable> = <#'^" (re-quote "{{{") "'> <*1 #'\\s+'> name <*1 #'\\s+'> <#'" (re-quote "}}}") "'>

<section> = (open-section / close-section / open-inverted-section)
open-section = <#'^" (re-quote (str open "#")) "'> <*1 #'\\s+'> name <*1 #'\\s+'> <#'" (re-quote close) "'>
close-section = <#'^" (re-quote (str open "/")) "'> <*1 #'\\s+'> name <*1 #'\\s+'> <#'" (re-quote close) "'>
open-inverted-section = <#'^" (re-quote (str open "^")) "'> <*1 #'\\s+'> name <*1 #'\\s+'> <#'" (re-quote close) "'>

comment = <#'^" (re-quote (str open "!")) "'> #'(?:.|\\r?\\n)*?(?=" (re-quote close) ")' <#'" (re-quote close) "'>

set-delimiter = <#'^" (re-quote (str open "=")) "'> <*1 #'\\s+'> new-open-delimiter <1* #'\\s+'> new-close-delimiter <*1 #'\\s+'> <#'" (re-quote (str "=" close)) "'> *rest
new-open-delimiter = #'[^\\s]+'
new-close-delimiter = #'[^\\s]+?(?=\\s*" (re-quote (str "=" close)) ")'
rest = #'(.|\\r?\\n)*$'
"))

(defn gen-parser [delimiters]
  (insta/parser (generate-mustache-spec delimiters) :input-format :abnf))

(def default-parser
  (gen-parser default-delimiters))

(defn parse*
  ([mustache]
   (parse* mustache {}))
  ([mustache opts]
   (let [parser (:parser opts default-parser)]
     (->> (dissoc opts :parser)
          (reduce-kv #(conj %1 %2 %3) [])
          (apply insta/parse parser mustache)))))

(defn- vec->ast-node [v]
  (case (first v)
    :text (ast/new-text (second v))
    :whitespace (ast/new-whitespace (second v))
    :newline (ast/new-newline (second v))
    :comment (ast/new-comment (second v))
    :escaped-variable (ast/new-escaped-variable (drop 1 (second v)))
    :unescaped-variable (ast/new-unescaped-variable (drop 1 (second v)))
    :open-section (ast/new-standard-section (drop 1 (second v)))
    :open-inverted-section (ast/new-inverted-section (drop 1 (second v)))))

(defn- remove-left-whitespaces [loc]
  (let [cnt (count (zip/lefts loc))]
    (if (zero? cnt)
      loc
      (reduce (fn [loc n]
                (if (ast/newline? (zip/node loc))
                  (reduced (zip/rightmost loc))
                  (cond-> (zip/remove loc)
                    (zero? n) zip/down)))
              (zip/left loc)
              (range (dec cnt) -1 -1)))))

(defn parse
  ([mustache]
   (parse mustache {}))
  ([mustache opts]
   (loop [loc (ast/ast-zip)
          [elm & parsed] (parse* mustache opts)
          state {:stack [] ;; for section balance
                 :standalone? true ;; for standalone tag
                 :suppress-newline false ;; if true, remove content of newline
                 }]
     (let [standalone? (and (:standalone? state) (= :newline (ffirst parsed)))]
       (if (nil? elm)
         (if (zip/up loc)
           (throw (ex-info "Unclosed section"
                           {:type ::unclosed-section
                            :tag (second elm)
                            :meta (misc/meta-without-qualifiers elm)}))
           (zip/root loc))
         (case (first elm)
           (:open-section :open-inverted-section)
           (let [loc (-> loc (zip/append-child (vec->ast-node elm)) zip/down zip/rightmost)]
             (recur (cond-> loc standalone? remove-left-whitespaces)
                    parsed
                    (-> state
                        (update :stack conj (second elm))
                        (assoc :standalone? false)
                        (assoc :suppress-newline standalone?))))

           :close-section
           (if (= (peek (:stack state)) (second elm))
             (let [loc (-> loc
                           (zip/append-child nil) ;; This is just for anchor
                           zip/down zip/rightmost)]
               (recur (-> (cond-> loc standalone? remove-left-whitespaces) zip/up zip/up)
                      parsed
                      (-> state
                          (update :stack pop)
                          (assoc :standalone? false)
                          (assoc :suppress-newline standalone?))))
             (throw (ex-info "Unopened section"
                             {:type ::unopend-section
                              :tag (second elm)
                              :meta (misc/meta-without-qualifiers elm)})))

           :set-delimiter
           (let [[_ [_ open] [_ close] [_ rest-of-mustache]] elm
                 parser (gen-parser {:open open :close close})]
             (recur loc
                    (parse* rest-of-mustache {:parser parser})
                    state))


           (:whitespace :comment)
           (recur (-> loc (zip/append-child (vec->ast-node elm)))
                  parsed
                  ;; keep current state
                  state)

           :newline
           (let [suppress-newline (:suppress-newline state)]
             (recur (-> loc (zip/append-child (if suppress-newline (ast/new-newline "") (vec->ast-node elm))))
                    parsed
                    (-> state
                        (assoc :standalone? true)
                        (assoc :suppress-newline false))))

           (recur (-> loc (zip/append-child (vec->ast-node elm)))
                  parsed
                  (assoc state :standalone? false))))))))
