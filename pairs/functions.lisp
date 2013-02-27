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


;;; function as
;;;
;;; (as type x) => an instance of type
;;; ---------------------------------------------------------------------

(defmethod as ((type (eql 'pair))(val null))
  val)

(defmethod as ((type (eql 'pair))(val cons))
  val)

(defmethod as ((type (eql 'pair))(val cl:sequence))
  (coerce val 'cl:list))

(defmethod as ((type (eql 'pair))(val seq))
  (fset:convert 'cl:list val))

(defmethod as ((type (eql 'pair))(val foundation-series))
  (series:collect 'cl:list val))

;;; function left
;;;
;;; (left p) => anything
;;; ---------------------------------------------------------------------
;;; returns the left element (i.e. the CAR) of the pair

(defgeneric left (pair))

(defmethod left ((p cons))
  (car p))

;;; function make
;;;
;;; (make 'pair :left x :right y) => (x . y)
;;; ---------------------------------------------------------------------
;;; create a pair

(defmethod make ((type (eql 'pair)) &key (left nil) (right nil) &allow-other-keys)
  (cons left right))

;;; function pair
;;;
;;; (pair a b) => Pair
;;; ---------------------------------------------------------------------
;;; returns a pair whose left element is a and whose right element is b

(defgeneric pair (a b))

(defmethod pair (a b)
  (cons a b))

;;; function right
;;;
;;; (right p) => anything
;;; ---------------------------------------------------------------------
;;; returns the right element (i.e. the CDR) of the pair

(defgeneric right (pair))

(defmethod right ((p cons))
  (cdr p))







