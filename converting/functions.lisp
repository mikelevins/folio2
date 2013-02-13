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

(in-package :net.bardcode.folio.converting)

;;; function as
;;;
;;; (as type val) => an instance of type
;;; ---------------------------------------------------------------------
;;; returns a value equivalent to VAL whose type is TYPE

(defgeneric as (type val))

;;; function combined-type
;;;
;;; (combined-type val1 val2) => a type designator
;;; ---------------------------------------------------------------------
;;; returns a type designator suitable for representing a value
;;; contructed by merging VAL1 and VAL2. used, for example, to
;;; determine the output type of an operation that concatenates
;;; sequences of different types. you aren't likely to need to
;;; call combined-type directly, nor to add methods, unless
;;; you're adding support for new composite types to folio

(defgeneric combined-type (val1 val2))
