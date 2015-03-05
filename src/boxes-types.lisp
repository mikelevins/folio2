;;;; ***********************************************************************
;;;;
;;;; Name:          types.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       the box type
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.folio2.boxes)

;;; function box?
;;;
;;; (box? val) => a boolean
;;; ---------------------------------------------------------------------
;;; return true if and only if val is a box

(defmethod box? (x) 
  (declare (ignore x))
  nil)

(defmethod box? ((x cons)) 
  (eq :box (car x)))


(deftype box ()
  `(and cons (satisfies box?)))
