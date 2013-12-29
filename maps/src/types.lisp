;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       map type definitions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.maps)

;;; ---------------------------------------------------------------------
;;; alist
;;; ---------------------------------------------------------------------
;;; for the purposes of folio, an alist is a list all of whose
;;; elements are lists

(defmethod alist? (thing) nil)

(defmethod alist? ((thing cons)) 
  (and thing
       (every 'consp thing)))

(deftype alist ()
  `(and list
        (satisfies alist?)))

;;; ---------------------------------------------------------------------
;;; plist
;;; ---------------------------------------------------------------------
;;; for the purposes of folio, a plist is a list with an even number of
;;; elements whose even-indexed elements are atoms

(defmethod plist? (thing) nil)

(defmethod plist? ((thing cons)) 
  (and thing
       (block testing
         (loop for tail on thing by 'cddr
            do (unless (and (cdr tail)
                            (atom (car tail)))
                 (return-from testing nil)))
         t)))

(deftype plist ()
  `(and list
        (satisfies plist?)))

