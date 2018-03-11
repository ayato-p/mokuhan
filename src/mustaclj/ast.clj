(ns mustaclj.ast
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

(defrecord EscapedVariable [keys]
  Variable)

(defn new-escaped-variable [keys]
  (EscapedVariable. (vec keys)))

(defrecord UnescapedVariable [keys]
  Variable)

(defn new-unescaped-variable [keys]
  (UnescapedVariable. (vec keys)))

;;; section
(defprotocol Section)

(defn section? [x]
  (satisfies? Section x))

(defrecord StandardSection [keys contents]
  Section
  ASTZipper
  (branch? [this] true)
  (children [this] contents)
  (make-node [this children]
    (StandardSection. keys children)))

(defn new-standard-section
  ([keys]
   (new-standard-section keys []))
  ([keys contents]
   (StandardSection. (vec keys) contents)))

(defrecord InvertedSection [keys contents]
  Section
  ASTZipper
  (branch? [this] true)
  (children [this] contents)
  (make-node [this children]
    (InvertedSection. keys children)))

(defn new-inverted-section
  ([keys]
   (new-inverted-section keys []))
  ([keys contents]
   (InvertedSection. (vec keys) contents)))

;; other
(defrecord Text [content])

(defn new-text [content]
  (Text. content))

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
