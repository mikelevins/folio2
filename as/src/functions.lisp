;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       uniform extensible tools for converting values from one type to another
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.as)

;;; function as
;;;
;;; (as type val &key &allow-other-keys) => anything
;;; ---------------------------------------------------------------------
;;; returns a value of TYPE that is equivalent in some sense to VAL

(defgeneric as (type val &key &allow-other-keys))

(defmethod as ((type (eql 'cl:symbol)) (val cl:string) &key &allow-other-keys)
  (intern val))

(defmethod as ((type (eql 'cl:string)) (val cl:symbol) &key &allow-other-keys)
  (symbol-name val))
