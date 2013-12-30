;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       comparison functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.comparisons)


;;; ---------------------------------------------------------------------
;;; comparisons
;;; ---------------------------------------------------------------------

;;; function > 
;;;
;;; (> thing &rest things) => Boolean
;;; ---------------------------------------------------------------------

(defun > (thing &rest things)
  (if (null things)
      t
      (if (binary-> thing (first things))
          (apply '> (first things)(rest things))
          nil)))

;;; function >=
;;;
;;; (>= thing &rest things) => Boolean
;;; ---------------------------------------------------------------------

(defun >= (thing &rest things)
  (if (null things)
      t
      (if (binary->= thing (first things))
          (apply '>= (first things)(rest things))
          nil)))

;;; function <
;;;
;;; (< thing &rest things) => Boolean
;;; ---------------------------------------------------------------------

(defun < (thing &rest things)
  (if (null things)
      t
      (if (binary-< thing (first things))
          (apply '< (first things)(rest things))
          nil)))

;;; function <=
;;;
;;; (<= thing &rest things) => Boolean
;;; ---------------------------------------------------------------------

(defun <= (thing &rest things)
  (if (null things)
      t
      (if (binary-<= thing (first things))
          (apply '<= (first things)(rest things))
          nil)))

;;; function binary->
;;;
;;; (binary-> x y) => Boolean
;;; ---------------------------------------------------------------------

(defmethod binary-> ((x number)(y number))
  (cl:> x y))

(defmethod binary-> ((x character)(y character))
  (cl:char> x y))

(defmethod binary-> ((x string)(y string))
  (cl:string> x y))

;;; function binary->=
;;;
;;; (binary->= x y) => Boolean
;;; ---------------------------------------------------------------------

(defmethod binary->= ((x number)(y number))
  (cl:>= x y))

(defmethod binary->= ((x character)(y character))
  (cl:char>= x y))

(defmethod binary->= ((x string)(y string))
  (cl:string>= x y))

;;; function binary-<
;;;
;;; (binary-< x y) => Boolean
;;; ---------------------------------------------------------------------

(defmethod binary-< ((x number)(y number))
  (cl:< x y))

(defmethod binary-< ((x character)(y character))
  (cl:char< x y))

(defmethod binary-< ((x string)(y string))
  (cl:string< x y))

;;; function binary-<=
;;;
;;; (binary-<= x y) => Boolean
;;; ---------------------------------------------------------------------

(defmethod binary-<= ((x number)(y number))
  (cl:<= x y))

(defmethod binary-<= ((x character)(y character))
  (cl:char<= x y))

(defmethod binary-<= ((x string)(y string))
  (cl:string<= x y))

;;; function binary-equivalent?
;;;
;;; (binary-equivalent? x y) => Boolean
;;; ---------------------------------------------------------------------

(defmethod binary-equivalent? ((x number)(y number))
  (cl:= x y))

(defmethod binary-equivalent? ((x character)(y character))
  (cl:char= x y))

(defmethod binary-equivalent? ((x string)(y string))
  (cl:string= x y))

;;; function equivalent?
;;;
;;; (equivalent? thing &rest things) => Boolean
;;; ---------------------------------------------------------------------

(defun equivalent? (thing &rest things)
  (if (null things)
      t
      (if (binary-equivalent? thing (first things))
          (apply 'equivalent? (first things)(rest things))
          nil)))
