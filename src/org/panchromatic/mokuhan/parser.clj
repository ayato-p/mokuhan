(ns org.panchromatic.mokuhan.parser
  (:require [fast-zip.core :as zip]
            [instaparse.core :as insta]
            [org.panchromatic.mokuhan.util.misc :as misc]
            [org.panchromatic.mokuhan.ast :as ast]
            [clojure.string :as str])
  (:import java.util.regex.Pattern))

(defn- re-quote [s]
  (re-pattern (Pattern/quote (str s))))

;;; {{name}}   -> variable
;;; {{{name}}} -> unescaped variable
;;; {{&name}} -> unescaped variable
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

(def ^:private sigils ["\\&" "\\#" "\\/" "\\^" "\\>"])

(defn generate-mustache-spec [{:keys [open close] :as delimiters}]
  (let [mustache? (= default-delimiters delimiters)]
    (str "
<mustache> = *(beginning-of-line *(text / whitespace / tag) end-of-line)
beginning-of-line = <#'(?:^|\\A)'>
end-of-line = #'(?:\\r?\\n|\\z)'
text = !tag #'[^\\r\\n\\s]+?(?=(?:" (re-quote open)  "|\\r?\\n|\\s|\\z))'
whitespace = #'[^\\S\\r\\n]+'

<ident> = #'(?!(?:\\!|\\=))[^\\s\\.]+?(?=\\s|\\.|" (re-quote close) ")'
path = (ident *(<'.'> ident) / #'\\.')

<tag> = (comment-tag / set-delimiter-tag / standard-tag " (when mustache? " / triple-mustache-tag") ")
standard-tag = <#'" (re-quote open) "'> sigil <*1 #'\\s+'> path <*1 #'\\s+'> <#'" (re-quote close) "'>
sigil = #'(?:" (str/join "|" sigils) ")?'
" (when mustache?
    "triple-mustache-tag = <#'\\Q{{{\\E'> <*1 #'\\s+'> path <*1 #'\\s+'> <#'\\Q}}}\\E'>") "

comment-tag = <#'^" (re-quote (str open "!")) "'> #'(?:.|\\r?\\n)*?(?=" (re-quote close) ")' <#'" (re-quote close) "'>

set-delimiter-tag = <#'^" (re-quote (str open "=")) "'> <*1 #'\\s+'> new-open-delimiter <*1 #'\\s+'> new-close-delimiter <*1 #'\\s+'> <#'" (re-quote (str "=" close)) "'> rest
new-open-delimiter = #'[^\\s]+'
new-close-delimiter = #'[^\\s]+?(?=\\s*" (re-quote (str "=" close)) ")'
rest = #'(.|\\r?\\n)*$'")))

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
    :beginning-of-line (ast/new-beginning-of-line)
    (:text :end-of-line) (ast/new-text (second v))
    :whitespace (ast/new-whitespace (second v))
    :triple-mustache-tag (ast/new-unescaped-variable (drop 1 (second v)))
    :comment-tag (ast/new-comment (second v))
    :standard-tag
    (let [[_ [_ sigil] [_ & path]] v]
      (case sigil
        "" (ast/new-escaped-variable path)
        "&" (ast/new-unescaped-variable path)
        "#" (ast/new-standard-section path)
        ;; "/"
        "^" (ast/new-inverted-section path)
        ;; ">" partial
        ))))

(defn- remove-left-whitespaces [loc]
  (let [cnt (count (zip/lefts loc))]
    (if (zero? cnt)
      loc
      (reduce (fn [loc n]
                (if (ast/beginning-of-line? (zip/node loc))
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
                 }]
     (if (nil? elm)
       (if (zip/up loc)
         (throw (ex-info "Unclosed section"
                         {:type ::unclosed-section
                          :tag (peek (:stack state))
                          :meta (misc/meta-without-qualifiers elm)}))
         (zip/root loc))

       (case (first elm)
         :standard-tag
         (let [[_ [_ sigil] [_ & path]] elm]
           (case sigil
             ("#" "^") ;; open section
             (let [standalone? (and (:standalone? state) (= :end-of-line (ffirst parsed)))
                   loc (-> loc (zip/append-child (vec->ast-node elm)) zip/down zip/rightmost)]
               (recur (cond-> loc standalone? remove-left-whitespaces)
                      (cond->> parsed standalone? (drop 2)) ;; `drop 2` means remove EOL&BOL
                      (-> state
                          (update :stack conj path)
                          (assoc :standalone? false))))

             "/" ;; close secion
             (if (= (peek (:stack state)) path)
               (let [standalone? (and (:standalone? state) (= :end-of-line (ffirst parsed)))
                     loc (-> loc
                             (zip/append-child nil) ;; just for anchor
                             zip/down
                             zip/rightmost
                             (cond-> standalone? remove-left-whitespaces))
                     leftmost? (zero? (count (zip/lefts loc)))]
                 (recur (-> loc zip/remove (cond-> (not leftmost?) zip/up) zip/up)
                        (cond->> parsed standalone? (drop 2))
                        (-> state
                            (update :stack pop)
                            (assoc :standalone? false))))
               (throw (ex-info "Unopened section"
                               {:type ::unopend-section
                                :tag path
                                :meta (misc/meta-without-qualifiers elm)})))

             (recur (-> loc (zip/append-child (vec->ast-node elm)))
                    parsed
                    (assoc state :standalone? false))))

         :set-delimiter-tag
         (let [[_ [_ open] [_ close] [_ rest-of-mustache]] elm
               parser (gen-parser {:open open :close close})
               parsed (->> (parse* rest-of-mustache {:parser parser})
                           (drop 1) ;; don't need BOL
                           )
               standalone? (and (:standalone? state) (= :end-of-line (ffirst parsed)))]
           (recur (cond-> loc standalone? remove-left-whitespaces)
                  (cond->> parsed standalone? (drop 2))
                  state))

         (:whitespace :comment)
         (let [standalone? (and (:standalone? state) (= :end-of-line (ffirst parsed)))]
           (recur (-> loc (zip/append-child (vec->ast-node elm))
                      (cond-> standalone? remove-left-whitespaces))
                  (cond->> parsed standalone? (drop 2))
                  ;; keep current state
                  state))

         :beginning-of-line
         (recur (-> loc (zip/append-child (vec->ast-node elm)))
                parsed
                (assoc state :standalone? true))

         (recur (-> loc (zip/append-child (vec->ast-node elm)))
                parsed
                (assoc state :standalone? false)))))))
