;;;; ***********************************************************************
;;;;
;;;; Name:          syntax.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       series macros
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio2.series)

;;; macro iterate
;;;
;;; (iterate fn init) => vals
;;; ---------------------------------------------------------------------
;;; Accepts a function FN that accepts a single argument and returns a
;;; single value. Returns a series of values given by
;;;  init
;;;  (fn init)
;;;  (fn (fn init))
;;;  ...

(defmacro iterate (fn init)
  (let ((arg (gensym)))
    `(series:scan-fn t
                     (lambda () ,init)
                     (lambda (,arg) (funcall ,fn ,arg)))))

;;; macro repeat
;;;
;;; (repeat expr) => vals
;;; ---------------------------------------------------------------------
;;; Accepts an expression that yields a value. Returns a series 
;;; constructed by evaluating the expression repeatedly without end.
;;; The series may contain equal values, or, if the expression
;;; refers to some datum that can change, it may contain varying
;;; values. In particular, an expression that updates a variable
;;; in an enclosing scope may contain different values.

(defmacro repeat (expr)
  (let ((ignored (gensym)))
    `(series:map-fn t 
                    (lambda (,ignored)(progn ,expr))
                    (series:scan-range))))

