;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       the pair type
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.folio.pairs)

;;; function pair?
;;;
;;; (pair? p) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if p is a pair

(defgeneric pair? (p))

(defmethod pair? (p)
  (declare (ignore p))
  nil)

(defmethod pair? ((p null))
  (declare (ignore p))
  t)

(defmethod pair? ((p cons))
  (declare (ignore p))
  t)

;;; type pair
;;;
;;; ---------------------------------------------------------------------
;;; the type of pairs

(deftype pair ()
 '(satisfies pair?))
