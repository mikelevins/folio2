;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       the set type
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.sets)

;;; type sequence
;;;
;;; ---------------------------------------------------------------------
;;; the type of sets

(deftype set ()
 '(satisfies set?))

