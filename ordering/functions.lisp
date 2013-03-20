;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       sorting values into stable orders
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.ordering)

;;; function >
;;;
;;; (> thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:>, providing an extensible generic version

;;; the default version
(defmethod > (x y &rest more)
  (if (cl:> x y)
      (cl:apply 'cl:> y more)
      nil))

;;; function >=
;;;
;;; (>= thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:>=, providing an extensible generic version

;;; the default version
(defmethod >= (x y &rest more)
  (if (cl:>= x y)
      (cl:apply 'cl:>= y more)
      nil))

;;; function <
;;;
;;; (< thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:<, providing an extensible generic version

;;; the default version
(defmethod < (x y &rest more)
  (if (cl:< x y)
      (cl:apply 'cl:< y more)
      nil))

;;; function <=
;;;
;;; (<= thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:<=, providing an extensible generic version

;;; the default version
(defmethod <= (x y &rest more)
  (if (cl:<= x y)
      (cl:apply 'cl:<= y more)
      nil))

;;; function sort
;;;
;;; (sort sequence &key (test '<)) => sequence'
;;; ---------------------------------------------------------------------
;;; shadows cl:sort, providing a non-destructive extensible generic version

;;; the default version
(defmethod sort (seq pred &key (key nil))
  (cl:sort (cl:copy-seq seq) pred :key key))







