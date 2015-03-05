;;;; ***********************************************************************
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       combinators and other conveniences 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio2.boxes)

;;; function as
;;;
;;; (as type x) => an instance of type
;;; ---------------------------------------------------------------------

(defmethod as ((type (eql 'box)) val &key &allow-other-keys)
  (if (box? val)
      val
      (box val)))

(defmethod as ((type (eql :box)) val &key &allow-other-keys)
  (if (box? val)
      val
      (box val)))

;;; function box
;;;
;;; (box val) => a box
;;; ---------------------------------------------------------------------
;;; create a box

(defun box (val)(cons :box val))

;;; function make
;;;
;;; (make 'box val) => (:box val)
;;; ---------------------------------------------------------------------
;;; create a box

(defmethod make ((type (eql 'box)) &key (value nil) &allow-other-keys)
  (box value))

(defmethod make ((type (eql :box)) &key (value nil) &allow-other-keys)
  (box value))

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

