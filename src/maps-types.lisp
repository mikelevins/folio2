;;;; ***********************************************************************
;;;;
;;;; Name:          types.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       map type definitions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio2.maps)

;;; ---------------------------------------------------------------------
;;; alist
;;; ---------------------------------------------------------------------
;;; for the purposes of folio2, an alist is a list all of whose
;;; elements are lists

(defmethod alist? (thing)
  (declare (ignore thing))
  nil)

(defmethod alist? ((thing cons)) 
  (and thing
       (every 'consp thing)))

(deftype alist ()
  `(and list
        (satisfies alist?)))

;;; ---------------------------------------------------------------------
;;; plist
;;; ---------------------------------------------------------------------
;;; for the purposes of folio2, a plist is a list with an even number of
;;; elements whose even-indexed elements are atoms

(defmethod plist? (thing)
    (declare (ignore thing))
    nil)

(defmethod plist? ((thing cons)) 
  (and thing
       (block testing
         (loop for tail on thing by #'cddr
            do (unless (and (cdr tail)
                            (atom (car tail)))
                 (return-from testing nil)))
         t)))

(deftype plist ()
  `(and list
        (satisfies plist?)))

