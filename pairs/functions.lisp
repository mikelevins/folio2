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

(defmethod as ((type (eql 'pair))(val null) &key &allow-other-keys)
  val)

(defmethod as ((type (eql 'pair))(val cons) &key &allow-other-keys)
  val)

(defmethod as ((type (eql 'pair))(val cl:sequence) &key &allow-other-keys)
  (coerce val 'cl:list))

(defmethod as ((type (eql 'pair))(val seq) &key &allow-other-keys)
  (fset:convert 'cl:list val))

(defmethod as ((type (eql 'pair))(val foundation-series) &key &allow-other-keys)
  (series:collect 'cl:list val))

;;; function left
;;;
;;; (left p) => anything
;;; ---------------------------------------------------------------------
;;; returns the left element (i.e. the CAR) of the pair

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

(defmethod pair (a b)
  (cons a b))

;;; function right
;;;
;;; (right p) => anything
;;; ---------------------------------------------------------------------
;;; returns the right element (i.e. the CDR) of the pair

(defmethod right ((p cons))
  (cdr p))







