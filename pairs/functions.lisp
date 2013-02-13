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

;;; function pair
;;;
;;; (pair a b) => Pair
;;; ---------------------------------------------------------------------
;;; returns a pair whose left element is a and whose right element is b

(defgeneric pair (a b))

;;; function pair?
;;;
;;; (pair? p) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if p is a pair

(defgeneric pair? (p))

;;; function right
;;;
;;; (right p) => anything
;;; ---------------------------------------------------------------------
;;; returns the right element (i.e. the CDR) of the pair

(defgeneric right (pair))








