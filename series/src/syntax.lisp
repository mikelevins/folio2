;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          syntax.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       series macros
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.series)

;;; repeat

(defmacro repeat (expr)
  (let ((ignored (gensym)))
    `(series:map-fn t 
                    (lambda (,ignored)(progn ,expr))
                    (series:scan-range))))
