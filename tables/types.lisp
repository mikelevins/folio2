;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       table types
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.folio.tables)

;;; function alist?
;;;
;;; (alist? p) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if p is a alist

;;; class alist
;;;
;;; ---------------------------------------------------------------------
;;; conses as alists

(defclass alist ()
  ((value :reader value :initarg :value)))

;;; class ordered-map
;;;
;;; ---------------------------------------------------------------------
;;; a species of table that maintains its key/value pairs in the order
;;; they were defined

(defclass ordered-map ()
  ((entries :accessor %table-entries :initform nil :initarg :entries)))

;;; function ordered-map?
;;;
;;; (ordered-map? p) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if p is a ordered-map

(defgeneric ordered-map? (p))

(defmethod ordered-map? (p)
  (declare (ignore p))
  nil)

(defmethod ordered-map? ((p ordered-map))
  (declare (ignore p))
  t)

;;; function plist?
;;;
;;; (plist? p) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if p is a plist

;;; type plist
;;;
;;; ---------------------------------------------------------------------
;;; conses as plists

(defclass plist ()
  ((value :reader value :initarg :value)))

;;; function table?
;;;
;;; (table? p) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if p is a table

(defgeneric table? (p))

(defmethod table? (p)
  (declare (ignore p))
  nil)

(defmethod table? ((p null))
  (declare (ignore p))
  nil)

(defmethod table? ((p cons))
  (declare (ignore p))
  nil)

(defmethod table? ((p alist))
  (declare (ignore p))
  t)

(defmethod table? ((p plist))
  (declare (ignore p))
  t)

(defmethod table? ((p ordered-map))
  (declare (ignore p))
  t)

(defmethod table? ((p fset:map))
  (declare (ignore p))
  t)

;;; type table
;;;
;;; ---------------------------------------------------------------------
;;; the type of tables

(deftype table ()
 '(satisfies table?))
