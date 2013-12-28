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

(in-package :net.bardcode.folio.as)

;;; function as
;;;
;;; (as type val &key &allow-other-keys) => anything
;;; ---------------------------------------------------------------------
;;; returns a value of TYPE that is equivalent in some sense to VAL

(defgeneric as (type val &key &allow-other-keys))

