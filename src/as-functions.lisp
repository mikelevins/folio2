;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       uniform extensible tools for converting values from one type to another
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio2.as)

;;; function as
;;;
;;; (as type val &key &allow-other-keys) => anything
;;; ---------------------------------------------------------------------
;;; returns a value of TYPE that is equivalent in some sense to VAL

(defgeneric as (type val &key &allow-other-keys))

(defmethod as ((type (eql 'cl:symbol)) (val cl:string) &key (package :cl-user) &allow-other-keys)
  (intern val package))

(defmethod as ((type (eql 'cl:string)) (val cl:symbol) &key &allow-other-keys)
  (symbol-name val))
