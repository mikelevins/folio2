;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          syntax.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       syntactic sugar for sequences
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.folio.sequences)


(set-syntax-from-char #\[ #\()
(set-syntax-from-char #\] #\))

(set-macro-character #\[
                (lambda (stream char)
                  (let ((elts (read-delimited-list #\] stream t)))
                    `(cl:list ,@elts))))



