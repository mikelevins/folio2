;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          syntax.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       syntax extensions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.functions)

;;; macro ^
;;; 
;;; (^ (arg1..argk)  expr1..exprk) => a function
;;; ---------------------------------------------------------------------
;;; A more compact synonym for LAMBDA. This macro is not intended as
;;; a replacement for LAMBDA, but as a convenience for cases in
;;; which the clarity of functional code benefits from compactness.

(defmacro ^ (args &body body)
  `(lambda ,args ,@body))

;;; macro ->
;;;
;;; (-> f1..fk) => fx
;;; ---------------------------------------------------------------------
;;; returns a function FX that accepts k arguments. When applied to k
;;; values, the function yields k values, applying F1 to the first argument,
;;; F2 to the second, and so on. Combines usefully with cascade, e.g:
;;; (cascade (a b c) (-> f1 f2 f3)(-> g1 g2 g3)(-> h1 h2 h3)) => v1 v2 v3
;;; where v1 is (h1 (g1 (f1 a))), and so on for the other values.

(defmacro -> (&rest fns)
  (let ((args (loop for fn in fns collecting (gensym))))
    `(lambda (,@args)
       (apply 'values
              (mapcar (lambda (fn arg)(funcall fn arg))
                      (list ,@fns)
                      (list ,@args))))))

;;; macro fn
;;; 
;;; (fn (arg1..argk) expr1..exprk) => a function
;;; ---------------------------------------------------------------------
;;; A more compact synonym for LAMBDA; a synonym for ^. This macro is
;;; not intended as a replacement for LAMBDA, but as a convenience for
;;; cases in which the clarity of functional code benefits from
;;; compactness.

(defmacro fn (args &body body)
  `(lambda ,args ,@body))

;;; macro cascade
;;;
;;; (cascade (arg1..argk) f1..fn) => val1..valk
;;; ---------------------------------------------------------------------
;;; F1 through FN are all functions that accept K arguments and return
;;; K values. cascade applies F1 to arguments arg1..argk. The K
;;; output values become the inputs to F2. F2's outputs are the inputs
;;; to F3, and so on. The outputs of FN are VAL1..VALK

(defmacro cascade (args &rest fns)
  (if (null fns)
      `(values ,@args)
      (let ((params (loop for arg in args collecting (gensym)))
            (fn (car fns))
            (more-fns (cdr fns)))
        `(multiple-value-bind (,@params)(funcall ,fn ,@args)
           (cascade (,@params) ,@more-fns)))))

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


