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
;;; returns a new set that contains X prepended to the elements of
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

(defgeneric difference (set1 set2 &key key test)) => set3

(defmethod difference ((set1 null) set2 &key key test) 
  (declare (ignore set1))
  nil)

(defmethod difference (set1 (set2 null) &key key test) 
  (declare (ignore set2))
  set1)

(defmethod difference ((set1 cl:sequence) (set2 cl:sequence) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:set-difference (cl:coerce set1 'cl:list)
                     (cl:coerce set2 'cl:list)
                     :key key :test test))

(defmethod difference ((set1 cl:sequence) (set2 fset:seq) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:set-difference (cl:coerce set1 'cl:list)
                     (fset:convert 'cl:list set2)
                     :key key :test test))

(defmethod difference ((set1 cl:sequence) (set2 series::foundation-series) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:set-difference (cl:coerce set1 'cl:list)
                     (series:collect 'cl:list set2)
                     :key key :test test))

(defmethod difference ((set1 fset:seq) (set2 cl:sequence) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:set-difference (fset:convert 'cl:list set1)
                     (cl:coerce set2 'cl:list)
                     :key key :test test))

(defmethod difference ((set1 fset:seq) (set2 fset:seq) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:set-difference (fset:convert 'cl:list set1)
                     (fset:convert 'cl:list set2)
                     :key key :test test))

(defmethod difference ((set1 fset:seq) (set2 series::foundation-series) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:set-difference (fset:convert 'cl:list set1)
                     (series:collect 'cl:list set2)
                     :key key :test test))

(defmethod difference ((set1 series::foundation-series) (set2 cl:sequence) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:set-difference (series:collect 'cl:list set1)
                     (cl:coerce set2 'cl:list)
                     :key key :test test))

(defmethod difference ((set1 series::foundation-series) (set2 fset:seq) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:set-difference (series:collect 'cl:list set1)
                     (fset:convert 'cl:list set2)
                     :key key :test test))

(defmethod difference ((set1 series::foundation-series) (set2 series::foundation-series) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:set-difference (series:collect 'cl:list set1)
                     (series:collect 'cl:list set2)
                     :key key :test test))


;;; ---------------------------------------------------------------------
;;; function intersection
;;; ---------------------------------------------------------------------
;;;
;;; (intersection set1 set2) => set3
;;; ---------------------------------------------------------------------
;;; returns a sequence that contains those elements that appear in both 
;;; SET1 and SET2

(defgeneric intersection (set1 set2 &key key test))

(defmethod intersection ((set1 null) set2 &key key test) 
  (declare (ignore set1))
  nil)

(defmethod intersection (set1 (set2 null) &key key test) 
  (declare (ignore set2))
  set1)

(defmethod intersection ((set1 cl:sequence) (set2 cl:sequence) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:intersection (cl:coerce set1 'cl:list)
                   (cl:coerce set2 'cl:list)
                   :key key :test test))

(defmethod intersection ((set1 cl:sequence) (set2 fset:seq) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:intersection (cl:coerce set1 'cl:list)
                   (fset:convert 'cl:list set2)
                   :key key :test test))

(defmethod intersection ((set1 cl:sequence) (set2 series::foundation-series) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:intersection (cl:coerce set1 'cl:list)
                   (series:collect 'cl:list set2)
                   :key key :test test))

(defmethod intersection ((set1 fset:seq) (set2 cl:sequence) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:intersection (fset:convert 'cl:list set1)
                   (cl:coerce set2 'cl:list)
                   :key key :test test))

(defmethod intersection ((set1 fset:seq) (set2 fset:seq) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:intersection (fset:convert 'cl:list set1)
                   (fset:convert 'cl:list set2)
                   :key key :test test))

(defmethod intersection ((set1 fset:seq) (set2 series::foundation-series) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:intersection (fset:convert 'cl:list set1)
                   (series:collect 'cl:list set2)
                   :key key :test test))

(defmethod intersection ((set1 series::foundation-series) (set2 cl:sequence) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:intersection (series:collect 'cl:list set1)
                   (cl:coerce set2 'cl:list)
                   :key key :test test))

(defmethod intersection ((set1 series::foundation-series) (set2 fset:seq) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:intersection (series:collect 'cl:list set1)
                   (fset:convert 'cl:list set2)
                   :key key :test test))

(defmethod intersection ((set1 series::foundation-series) (set2 series::foundation-series) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:intersection (series:collect 'cl:list set1)
                   (series:collect 'cl:list set2)
                   :key key :test test))

;;; ---------------------------------------------------------------------
;;; function set?
;;; ---------------------------------------------------------------------
;;;
;;; (set? s) => boolean
;;; ---------------------------------------------------------------------

(defgeneric set? ())

;;; ---------------------------------------------------------------------
;;; function subset?
;;; ---------------------------------------------------------------------
;;;
;;; (subset? set1 set2) => boolean
;;; ---------------------------------------------------------------------
;;; returns true if every element of SET1 also appears in SET2

(defgeneric subset? ())

;;; ---------------------------------------------------------------------
;;; function union
;;; ---------------------------------------------------------------------
;;;
;;; (union set1 set2) => set3
;;; ---------------------------------------------------------------------
;;; returns a set that contains all elements that appear either in
;;; SET1 or in SET2

(defgeneric union (set1 set2 &key key test))

(defmethod union ((set1 null) set2 &key key test) 
  (declare (ignore set1))
  nil)

(defmethod union (set1 (set2 null) &key key test) 
  (declare (ignore set2))
  set1)

(defmethod union ((set1 cl:sequence) (set2 cl:sequence) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:union (cl:coerce set1 'cl:list)
                   (cl:coerce set2 'cl:list)
                   :key key :test test))

(defmethod union ((set1 cl:sequence) (set2 fset:seq) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:union (cl:coerce set1 'cl:list)
                   (fset:convert 'cl:list set2)
                   :key key :test test))

(defmethod union ((set1 cl:sequence) (set2 series::foundation-series) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:union (cl:coerce set1 'cl:list)
                   (series:collect 'cl:list set2)
                   :key key :test test))

(defmethod union ((set1 fset:seq) (set2 cl:sequence) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:union (fset:convert 'cl:list set1)
                   (cl:coerce set2 'cl:list)
                   :key key :test test))

(defmethod union ((set1 fset:seq) (set2 fset:seq) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:union (fset:convert 'cl:list set1)
                   (fset:convert 'cl:list set2)
                   :key key :test test))

(defmethod union ((set1 fset:seq) (set2 series::foundation-series) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:union (fset:convert 'cl:list set1)
                   (series:collect 'cl:list set2)
                   :key key :test test))

(defmethod union ((set1 series::foundation-series) (set2 cl:sequence) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:union (series:collect 'cl:list set1)
                   (cl:coerce set2 'cl:list)
                   :key key :test test))

(defmethod union ((set1 series::foundation-series) (set2 fset:seq) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:union (series:collect 'cl:list set1)
                   (fset:convert 'cl:list set2)
                   :key key :test test))

(defmethod union ((set1 series::foundation-series) (set2 series::foundation-series) &key (key 'cl:identity) (test 'cl:equal)) 
  (cl:union (series:collect 'cl:list set1)
                   (series:collect 'cl:list set2)
                   :key key :test test))
