;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          syntax.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       reader macro for type conversions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.as)

(defun read-type-constraint (stream subchar num)
  (declare (ignore num))
  (let* ((constraint-spec (read-delimited-list #\] stream t))
         (type-constraint (first constraint-spec))
         (expr (read stream nil nil nil)))
    `(as ',type-constraint ,expr)))

(set-dispatch-macro-character #\# #\[ 'read-type-constraint)

(set-macro-character #\] (get-macro-character #\)))
