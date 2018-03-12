(ns mokuhan.ast
  (:require [fast-zip.core :as zip]))

(defprotocol ASTZipper
  (branch? [this])
  (children [this])
  (make-node [this children]))

(extend-protocol ASTZipper
  Object
  (branch? [this] false)
  (children [this] nil)
  (make-node [this children] nil))

;;; variable
(defprotocol Variable)

(defn variable? [x]
  (satisfies? Variable x))

(defrecord EscapedVariable [path]
  Variable)

(defn new-escaped-variable [path]
  (EscapedVariable. (vec path)))

(defrecord UnescapedVariable [path]
  Variable)

(defn new-unescaped-variable [path]
  (UnescapedVariable. (vec path)))

;;; section
(defprotocol Section)

(defn section? [x]
  (satisfies? Section x))

(defrecord StandardSection [path contents]
  Section
  ASTZipper
  (branch? [this] true)
  (children [this] contents)
  (make-node [this children]
    (StandardSection. path children)))

(defn new-standard-section
  ([path]
   (new-standard-section path []))
  ([path contents]
   (StandardSection. (vec path) contents)))

(defrecord InvertedSection [path contents]
  Section
  ASTZipper
  (branch? [this] true)
  (children [this] contents)
  (make-node [this children]
    (InvertedSection. path children)))

(defn new-inverted-section
  ([path]
   (new-inverted-section path []))
  ([path contents]
   (InvertedSection. (vec path) contents)))

;; other
(defrecord Text [content])

(defn new-text [content]
  (Text. content))

(defrecord Newline [content])

(defn new-newline [content]
  (Newline. content))

(defrecord Comment [content])

(defn new-comment [content]
  (Comment. content))

(defrecord Mustache [contents]
  ASTZipper
  (branch? [this] true)
  (children [this] contents)
  (make-node [this children]
    (Mustache. children)))

(defn new-mustache
  ([]
   (new-mustache []))
  ([contents]
   (Mustache. contents)))

(defn ast-zip
  ([]
   (ast-zip (new-mustache)))
  ([root]
   (zip/zipper branch? children make-node root)))
