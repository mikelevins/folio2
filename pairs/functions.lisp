;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       associating values in pairs
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.pairs)


;;; function left
;;;
;;; (left p) => anything
;;; ---------------------------------------------------------------------
;;; returns the left element (i.e. the CAR) of the pair

(defgeneric left (pair))

(defmethod left ((p cons))
  (car p))

;;; type pair
;;;
;;; ---------------------------------------------------------------------
;;; the type of pairs

(deftype pair ()
 '(satisfies pair?))

;;; function pair
;;;
;;; (pair a b) => Pair
;;; ---------------------------------------------------------------------
;;; returns a pair whose left element is a and whose right element is b

(defgeneric pair (a b))

(defmethod pair (a b)
  (cons a b))

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

;;; function right
;;;
;;; (right p) => anything
;;; ---------------------------------------------------------------------
;;; returns the right element (i.e. the CDR) of the pair

(defgeneric right (pair))

(defmethod right ((p cons))
  (cdr p))







