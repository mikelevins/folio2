;;;; ***********************************************************************
;;;;
;;;; Name:          syntax.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       reader macro for type conversions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio2.as)

(defun read-type-constraint (stream subchar num)
  (declare (ignore num subchar))
  (let* ((constraint-spec (read-delimited-list #\] stream t))
         (type-constraint (first constraint-spec))
         (expr (read stream nil nil nil)))
    `(as ',type-constraint ,expr)))

(set-dispatch-macro-character #\# #\[ 'read-type-constraint)

(set-macro-character #\] (get-macro-character #\)))
