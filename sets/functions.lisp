;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       functions that accept and produce
;;;;                sequences, series, generators, and streams
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; operations on sequences as sets

(in-package #:net.bardcode.folio.sets)

;;; ---------------------------------------------------------------------
;;; function adjoin 
;;; ---------------------------------------------------------------------
;;;
;;; (adjoin item set1) => set2
;;; ---------------------------------------------------------------------
;;; returns a new setuence that contains X prepended to the elements of
;;; SET

(defgeneric adjoin (item set &key test key))

(defmethod adjoin (item (set null) &key test key)
  (declare (ignore item set))
  (list item))

(defmethod adjoin (item (set cl:cons) &key (test 'equal) key)
  (cl:adjoin item set :test test :key key))

(defmethod adjoin (item (set cl:vector) &key (test 'equal) key)
  (let ((already (cl:find item set :key key :test test)))
    (if already
        set
        (coerce (cons item (coerce set 'cl:list)) 'cl:vector))))

(defmethod adjoin (item (set cl:string) &key (test 'equal) key)
  (error "no applicable method for ADJOIN with arguments: (~S ~S)"
         (class-of item)(class-of set)))

(defmethod adjoin ((item cl:character) (set cl:string) &key (test 'equal) key)
  (let ((already (cl:find item set :key key :test test)))
    (if already
        set
        (coerce (cons item (coerce set 'cl:list)) 'cl:string))))

(defmethod adjoin (item (set fset:set) &key (test 'equal) key)
  (let ((already (fset:find item set :key key :test test)))
    (if already
        set
        (fset:with-first set item))))

(defmethod adjoin (item (set series::foundation-series) &key (test 'equal) key)
  (error "no applicable method for ADJOIN with arguments: (~S ~S)"
         (class-of item)(class-of set)))

;;; ---------------------------------------------------------------------
;;; function difference
;;; ---------------------------------------------------------------------
;;;
;;; (difference set1 set2) => set3
;;; ---------------------------------------------------------------------
;;; returns a new set that contains the elements of SET1 that are
;;; not in SET2

(defgeneric difference (set1 set2)) => set3

(defmethod difference () 
  )

;;; ---------------------------------------------------------------------
;;; function intersection
;;; ---------------------------------------------------------------------
;;;
;;; (intersection ) => 
;;; ---------------------------------------------------------------------
;;; returns a new setuence that contains X prepended to the elements of
;;; SET

(defgeneric intersection ())

(defmethod intersection () )

;;; ---------------------------------------------------------------------
;;; function set?
;;; ---------------------------------------------------------------------
;;;
;;; (set? ) => 
;;; ---------------------------------------------------------------------
;;; returns a new setuence that contains X prepended to the elements of
;;; SET

(defgeneric set? ())

(defmethod set? () )

;;; ---------------------------------------------------------------------
;;; function subset?
;;; ---------------------------------------------------------------------
;;;
;;; (subset? ) => 
;;; ---------------------------------------------------------------------
;;; returns a new setuence that contains X prepended to the elements of
;;; SET

(defgeneric subset? ())

(defmethod subset? () )

;;; ---------------------------------------------------------------------
;;; function union
;;; ---------------------------------------------------------------------
;;;
;;; (union ) => 
;;; ---------------------------------------------------------------------
;;; returns a new setuence that contains X prepended to the elements of
;;; SET

(defgeneric union ())

(defmethod union () )

