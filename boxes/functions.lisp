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

;;; function as
;;;
;;; (as type x) => an instance of type
;;; ---------------------------------------------------------------------

(defmethod as ((type (eql 'box)) val &key &allow-other-keys)
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

(defmethod make ((type (eql 'NET.BARDCODE.FOLIO.COMMON:box)) &key (value nil) &allow-other-keys)
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

