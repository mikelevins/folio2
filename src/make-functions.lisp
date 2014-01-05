;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       uniform tools for converting values from one type to another
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.make)

;;; function make
;;;
;;; (make type &key &allow-other-keys) => anything
;;; ---------------------------------------------------------------------
;;; returns a new value of TYPE

(defgeneric make (type &key &allow-other-keys))

(defmethod make ((type (eql 'cl:symbol)) &key (name nil) &allow-other-keys)
  (cl:make-symbol (or name (symbol-name (gensym)))))

(defmethod make ((type (eql 'cl:keyword)) &key (name nil) &allow-other-keys)
  (cl:intern (or name (symbol-name (gensym))) :keyword))

(defmethod make ((type (eql 'cl:cons)) &key (car nil)(cdr nil) &allow-other-keys)
  (cl:cons car cdr))

(defmethod make ((type (eql 'cl:list)) &key (elements nil) &allow-other-keys)
  (assert (or (null elements)(cl:typep elements 'cl:sequence))() 
          "Invalid elements argument to make list: ~a"
          elements)
  (coerce elements 'cl:list))

(defmethod make ((type (eql 'cl:array)) 
                 &key
                   (dimensions 0)
                   (element-type t)
                   (initial-element nil initial-element-p)
                   (initial-contents nil initial-contents-p)
                   (adjustable nil)
                   (fill-pointer nil)
                   (displaced-to nil)
                   (displaced-index-offset nil)
                   &allow-other-keys)
  (cond
    (initial-element-p (make-array dimensions 
                                   :element-type element-type
                                   :initial-element initial-element
                                   :adjustable adjustable
                                   :fill-pointer fill-pointer
                                   :displaced-to displaced-to
                                   :displaced-index-offset displaced-index-offset))
    (initial-contents-p (make-array dimensions 
                                    :element-type element-type
                                    :initial-contents initial-contents
                                    :adjustable adjustable
                                    :fill-pointer fill-pointer
                                    :displaced-to displaced-to
                                    :displaced-index-offset displaced-index-offset))
    (t (make-array dimensions 
                   :element-type element-type
                   :initial-element initial-element
                   :adjustable adjustable
                   :fill-pointer fill-pointer
                   :displaced-to displaced-to
                   :displaced-index-offset displaced-index-offset))))

(defmethod make ((type (eql 'cl:string)) &key (length 0)(element-type 'cl:character)(initial-element #\space) &allow-other-keys)
  (cl:make-string length :element-type element-type :initial-element initial-element))

(defmethod make ((type (eql 'cl:hash-table)) 
                 &key
                   (test 'cl:eql)
                   (size 32)
                   (rehash-size 1.5)
                   (rehash-threshold 1.0)
                   &allow-other-keys)
  (cl:make-hash-table :test test :size size :rehash-size rehash-size :rehash-threshold rehash-threshold))



