;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       combinators and other conveniences 
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.boxes)

;;; type box
;;;
;;; ---------------------------------------------------------------------
;;; the type of boxes

(deftype box ()
 '(and cons (satisfies box?)))

;;; function box
;;;
;;; (box val) => a box
;;; ---------------------------------------------------------------------
;;; create a box

(defun box (val)(cons :box val))

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

;;; function set-box!
;;;
;;; (set-box! b val) => val
;;; ---------------------------------------------------------------------
;;; replace the contents of the box

(defmethod set-box! ((b cons) val)
    (setf (cdr b) val))

;;; function unbox
;;;
;;; (unbox val) => anything
;;; ---------------------------------------------------------------------
;;; return the contents of the box

(defmethod unbox ((x cons)) 
  (cdr x))

;;; function setf unbox
;;;
;;; (setf (unbox b) val) => box
;;; ---------------------------------------------------------------------
;;; replace the contents of the box

(defsetf unbox set-box!)

