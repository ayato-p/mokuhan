(ns org.panchromatic.mokuhan.parser
  (:require [clojure.string :as str]
            [fast-zip.core :as zip]
            [instaparse.core :as insta]
            [org.panchromatic.mokuhan.ast :as ast]
            [org.panchromatic.mokuhan.util.misc :as misc]
            [org.panchromatic.mokuhan.walker :as walker])
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
  (str "
<mustache> = *(beginning-of-line *(text / whitespace / tag) end-of-line)
beginning-of-line = <#'(?:^|\\A)'>
end-of-line = #'(?:\\r?\\n|\\z)'
text = !tag #'[^\\r\\n\\s]+?(?=(?:" (re-quote open)  "|\\r?\\n|\\s|\\z))'
whitespace = #'[^\\S\\r\\n]+'

<ident> = #'(?!(?:\\!|\\=))[^\\s\\.]+?(?=\\s|\\.|" (re-quote close) ")'
path = (ident *(<'.'> ident) / #'\\.')

<tag> = ( comment-tag / set-delimiter-tag / standard-tag / alt-unescaped-tag )
tag-open = #'" (re-quote open) "'
tag-close = #'" (re-quote close) "'
standard-tag = tag-open sigil <*1 #'\\s+'> path <*1 #'\\s+'> tag-close
sigil = #'(?:" (str/join "|" sigils) ")?'
alt-unescaped-tag = tag-open #'\\{' <*1 #'\\s+'> path <*1 #'\\s+'> #'\\}' tag-close

comment-tag = tag-open <#'!'> comment-content tag-close
comment-content = #'(?:.|\\r?\\n)*?(?=" (re-quote close) ")'

set-delimiter-tag = tag-open <#'='> <*1 #'\\s+'> new-open-delimiter <*1 #'\\s+'> new-close-delimiter <*1 #'\\s+'> <#'='> tag-close rest
new-open-delimiter = #'[^\\s]+'
new-close-delimiter = #'[^\\s]+?(?=\\s*" (re-quote (str "=" close)) ")'
rest = #'(.|\\r?\\n)*$'"))

(defn gen-parser [delimiters]
  (insta/parser (generate-mustache-spec delimiters)
                :input-format :abnf))

(def default-parser
  (gen-parser default-delimiters))

(defn parse*
  ([mustache]
   (parse* mustache {}))
  ([mustache opts]
   (let [delimiters (:delimiters opts default-delimiters)
         parser (if (= default-delimiters delimiters)
                  default-parser
                  (gen-parser delimiters))]
     (->> (dissoc opts :parser)
          (reduce-kv #(conj %1 %2 %3) [])
          (apply insta/parse parser mustache)))))

(defn- invisible-rightside-children-whitespaces [loc]
  (if (zip/down loc)
    (loop [loc (some-> loc zip/down zip/rightmost)]
      (if (and loc (ast/whitespace? (zip/node loc)))
        (-> (zip/edit loc ast/to-invisible)
            zip/left
            recur)
        (zip/up loc)))
    loc))

(defn- copy-left-whitespaces [loc]
  (loop [loc (zip/left loc)
         whitespaces []]
    (if (ast/whitespace? (zip/node loc))
      (recur (zip/left loc) (conj whitespaces (zip/node loc)))
      whitespaces)))

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
         (let [[_ [_ open] [_ sigil] [_ & path] [_ close]] elm
               delimiters {:open open :close close}]
           (case sigil
             ("#" "^") ;; open section
             (let [standalone? (and (:standalone? state) (= :end-of-line (ffirst parsed)))]
               (recur (-> (cond-> loc standalone? invisible-rightside-children-whitespaces)
                          (zip/append-child (if (= "#" sigil)
                                              (ast/new-standard-section path delimiters)
                                              (ast/new-inverted-section path delimiters)))
                          zip/down
                          zip/rightmost)
                      (cond->> parsed standalone? (drop 2)) ;; `drop 2` means remove EOL&BOL
                      (-> state
                          (update :stack conj path)
                          (assoc :standalone? standalone?))))

             "/" ;; close secion
             (if (= (peek (:stack state)) path)
               (let [standalone? (and (:standalone? state) (= :end-of-line (ffirst parsed)))]
                 (recur (-> (cond-> loc standalone? invisible-rightside-children-whitespaces)
                            (zip/edit ast/set-close-tag-delimiters delimiters)
                            zip/up)
                        (cond->> parsed standalone? (drop 2))
                        (-> state
                            (update :stack pop)
                            (assoc :standalone? standalone?))))
               (throw (ex-info "Unopened section"
                               {:type ::unopend-section
                                :tag path
                                :meta (misc/meta-without-qualifiers elm)})))

             ">" ;; partial
             (let [standalone? (and (:standalone? state) (= :end-of-line (ffirst parsed)))
                   whitespaces (when standalone?
                                 (-> loc
                                     (zip/append-child nil)
                                     zip/down
                                     zip/rightmost
                                     copy-left-whitespaces))
                   children (-> (:partials opts)
                                (walker/traverse path [])
                                (parse opts)
                                (ast/children)
                                (->> (drop 1)))]
               (recur (reduce #(-> %1
                                   (cond-> (ast/beginning-of-line? %2)
                                     (as-> loc' (reduce (fn [l ws] (zip/append-child l ws)) loc' whitespaces)))
                                   (zip/append-child %2))
                              loc
                              children)
                      (cond->> parsed standalone? (drop 2))
                      state))

             (recur (-> loc (zip/append-child (if (= "" sigil)
                                                (ast/new-escaped-variable path delimiters)
                                                (ast/new-unescaped-variable path delimiters))))
                    parsed
                    (assoc state :standalone? false))))

         :alt-unescaped-tag
         (let [[_ [_ open] _ [_ & path] _ [_ close]] elm
               delimiters {:open open :close close}]
           (recur (-> loc (zip/append-child (ast/new-unescaped-variable path delimiters)))
                  parsed
                  (assoc state :standalone? false)))

         :set-delimiter-tag
         (let [[_ _ [_ open] [_ close] _ [_ rest-of-mustache]] elm
               delimiters {:open open :close close}
               parsed (->> (parse* rest-of-mustache {:delimiters delimiters})
                           (drop 1) ;; don't need BOL
                           )
               standalone? (and (:standalone? state)
                                (or (= :end-of-line (ffirst parsed)) (empty? parsed)))]
           (recur (cond-> loc standalone? invisible-rightside-children-whitespaces)
                  (cond->> parsed standalone? (drop 2))
                  (assoc state :standalone? standalone?)))

         :comment-tag
         (let [standalone? (and (:standalone? state) (= :end-of-line (ffirst parsed)))
               [_ _ comment-content _] elm]
           (recur (-> (cond-> loc standalone? invisible-rightside-children-whitespaces)
                      (zip/append-child (ast/new-comment comment-content)))
                  (cond->> parsed standalone? (drop 2))
                  (assoc state :standalone? standalone?)))

         :whitespace
         (recur (-> loc (zip/append-child (ast/new-whitespace (second elm))))
                parsed
                ;; keep current state
                state)

         :beginning-of-line
         (recur (-> loc (zip/append-child (ast/new-beginning-of-line)))
                parsed
                (assoc state :standalone? true))

         (recur (-> loc (zip/append-child (ast/new-text (second elm))))
                parsed
                (assoc state :standalone? false)))))))
