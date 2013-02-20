;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       the sequence type
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.sequences)


;;; function sequence?
;;;
;;; (sequence? p) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if p is a sequence

(defgeneric sequence? (p))

(defmethod sequence? (s)
  (declare (ignore s))
  nil)

(defmethod sequence? ((s cl:sequence))
  (declare (ignore s))
  t)

(defmethod sequence? ((s fset:seq))
  (declare (ignore s))
  t)

(defmethod sequence? ((s series::foundation-series))
  (declare (ignore s))
  t)

;;; function series?
;;;
;;; (series? p) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if p is a series

(defgeneric series? (p))

(defmethod series? (s)
  (declare (ignore s))
  nil)

(defmethod series? ((s cl:sequence))
  (declare (ignore s))
  nil)

(defmethod series? ((s fset:seq))
  (declare (ignore s))
  nil)

(defmethod series? ((s series::foundation-series))
  (declare (ignore s))
  t)

;;; type sequence
;;;
;;; ---------------------------------------------------------------------
;;; the type of sequences

(deftype sequence ()
 '(satisfies sequence?))

;;; type series
;;;
;;; ---------------------------------------------------------------------
;;; the type of series

(deftype series ()
 '(satisfies series?))

