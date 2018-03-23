(ns org.panchromatic.mokuhan.ast
  (:require [fast-zip.core :as zip]
            [clojure.string :as str]
            [org.panchromatic.mokuhan.util.stringbuilder :as sb]))

(defprotocol ASTZipper
  (branch? [this])
  (children [this])
  (make-node [this children]))

(extend-protocol ASTZipper
  #?(:clj Object :cljs default)
  (branch? [this] false)
  (children [this] nil)
  (make-node [this children] nil))

(defn to-visible [x]
  (assoc x :visible true))

(defn to-invisible [x]
  (assoc x :visible false))

(defn visible? [x]
  (:visible x true))

(defn- str-path [path]
  (str/join "." path))

(defn- wrap-delimiters [s {:keys [open close]}]
  (str open s close))

;;; variable
(defprotocol Variable)

(defn variable? [x]
  (satisfies? Variable x))

(defrecord EscapedVariable [path delimiters]
  Variable

  Object
  (toString [_]
    (wrap-delimiters (str-path path) delimiters)))

(defn new-escaped-variable [path delimiters]
  (EscapedVariable. (vec path) delimiters))

(defrecord UnescapedVariable [path delimiters]
  Variable

  Object
  (toString [_]
    (wrap-delimiters (str-path path) delimiters)))

(defn new-unescaped-variable [path delimiters]
  (UnescapedVariable. (vec path) delimiters))

;;; section
(defn get-open-tag-delimiters [x]
  (:open-tag-delimiters x))

(defn set-close-tag-delimiters [x delimiters]
  (assoc x :close-tag-delimiters delimiters))

(defprotocol Section)

(defn section? [x]
  (satisfies? Section x))

(defrecord StandardSection [path contents open-tag-delimiters close-tag-delimiters]
  Section

  ASTZipper
  (branch? [this] true)
  (children [this] contents)
  (make-node [this children]
    (StandardSection. path children open-tag-delimiters close-tag-delimiters))

  Object
  (toString [_]
    (let [path (str-path path)]
      (-> (sb/new-string-builder)
          (sb/append (wrap-delimiters (str "#" path) open-tag-delimiters))
          (as-> sb (reduce #(sb/append %1 (.toString %2)) sb contents))
          (sb/append (wrap-delimiters (str "/" path) close-tag-delimiters))
          (sb/to-string)))))

(defn new-standard-section [path delimiters]
  (StandardSection. path () delimiters delimiters))

(defrecord InvertedSection [path contents open-tag-delimiters close-tag-delimiters]
  Section

  ASTZipper
  (branch? [this] true)
  (children [this] contents)
  (make-node [this children]
    (InvertedSection. path children open-tag-delimiters close-tag-delimiters))

  Object
  (toString [_]
    (let [path (str-path path)]
      (-> (sb/new-string-builder)
          (sb/append (wrap-delimiters (str "#" path) open-tag-delimiters))
          (as-> sb (reduce #(sb/append %1 (.toString %2)) sb contents))
          (sb/append (wrap-delimiters (str "/" path) close-tag-delimiters))
          (sb/to-string)))))

(defn new-inverted-section [path delimiters]
  (InvertedSection. path () delimiters delimiters))

;; other

(defrecord BeginningOfLine [] ;marker
  Object
  (toString [_] ""))

(def beginning-of-line
  (BeginningOfLine.))

(defn new-beginning-of-line []
  beginning-of-line)

(defn beginning-of-line? [x]
  (instance? BeginningOfLine x))

(defrecord Text [content]
  Object
  (toString [this]
    (if (visible? this) content "")))

(defn new-text [content]
  (Text. content))

(defrecord Whitespace [content]
  Object
  (toString [this]
    (if (visible? this) content "")))

(defn new-whitespace [content]
  (Whitespace. content))

(defn whitespace? [x]
  (instance? Whitespace x))

(defrecord Comment [content]
  Object
  (toString [this] ""))

(defn new-comment [content]
  (Comment. content))

(defn comment? [x]
  (instance? Comment x))

(defrecord Mustache [contents]
  ASTZipper
  (branch? [this] true)
  (children [this] contents)
  (make-node [this children]
    (Mustache. children))

  Object
  (toString [_]
    (sb/to-string
     (reduce #(sb/append %1 (.toString %2))
             (sb/new-string-builder)
             contents))))

(defn new-mustache
  ([]
   (new-mustache ()))
  ([contents]
   (Mustache. contents)))

(defn ast-zip
  ([]
   (ast-zip (new-mustache)))
  ([root]
   (zip/zipper branch? children make-node root)))
