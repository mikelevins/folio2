;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          syntax.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       syntax for sequence literals
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.sequences)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------


(set-macro-character #\[
                     (lambda (stream char)
                       (declare (ignore char))
                       (let ((elts (read-delimited-list #\] stream t)))
                         ` (cl:list ,@elts))))

(set-macro-character #\] (get-macro-character #\)))


