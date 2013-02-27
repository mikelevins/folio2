;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       syntactic sugar
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.functions)

;;; macro $
;;;
;;; ($ f &rest args) => vals
;;; ---------------------------------------------------------------------
;;; folio's application operator.
;;; if f is a function or generic function then $ is a synonym for
;;; FUNCALL. If f is a sequence, table, series, or generator, then
;;; it is a synonym for net.bardcode.folio.common:GET-KEY

(defmethod %funcall-applicable (f &rest args)
  (error "Not an applicable object: ~S" f))

(defmethod %funcall-applicable ((f symbol) &rest args)
  (apply f args))

(defmethod %funcall-applicable ((f function) &rest args)
  (apply f args))

(defmethod %funcall-applicable ((f cl:sequence) &rest args)
  (net.bardcode.folio.common:get-key f (car args)))

(defmethod %funcall-applicable ((f seq) &rest args)
  (net.bardcode.folio.common:GET-KEY f (car args)))

(defmethod %funcall-applicable ((f fset:map) &rest args)
  (net.bardcode.folio.common:GET-KEY f (car args)))

(defmethod %funcall-applicable ((f foundation-series) &rest args)
  (net.bardcode.folio.common:GET-KEY f (car args)))

(defmacro $ (f &rest args)
  `(%funcall-applicable ,f ,@args))

;;; macro ^
;;; 
;;; (^ (arg1..argk) expr1..exprk) => a function
;;; ---------------------------------------------------------------------
;;; A more compact synonym for LAMBDA. This macro is not intended as
;;; a replacement for LAMBDA, but as a convenience for cases in
;;; which the clarity of functional code benefits from compactness.

(defmacro ^ (args &body body)
  `(lambda ,args ,@body))

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

(defmacro -> (args &body body)
  (if (null body)
      `(values ,@args)
      (let ((vars (loop for arg in args collect (gensym)))
            (f (car body))
            (more (cdr body)))
        `(multiple-value-bind ,vars (funcall ,f ,@args)
           (-> ,vars ,@more)))))


;;; function ->
;;;
;;; (-> f1..fk) => fx
;;; ---------------------------------------------------------------------
;;; returns a function FX that accepts k arguments. When applied to k
;;; values, the function yields k values, applying F1 to the first argument,
;;; F2 to the second, and so on. Combines usefully with cascade, e.g:
;;; (cascade (a b c) (-> f1 f2 f3)(-> g1 g2 g3)(-> h1 h2 h3)) => v1 v2 v3
;;; where v1 is (h1 (g1 (f1 a))), and so on for the other values.
