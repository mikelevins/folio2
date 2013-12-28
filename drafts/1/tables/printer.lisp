;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       prettier printer for fset:map
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.folio.tables)

(defmethod print-object ((tbl fset:map) stream)
  (write-char #\{ stream)
  (let ((entries (as 'cl:list tbl)))
    (unless (null entries)
      (format stream "~S ~S" (caar entries)(cdar entries))
      (unless (null (cdr entries))
        (loop for (a . b) in (cdr entries)
             do (format stream " ~S ~S" a b)))))
  (write-char #\} stream))
