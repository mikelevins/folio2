;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       tools for manipulating finite maps
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage :net.bardcode.folio.tables
  (:use :cl)
  (:export
   :alist
   :alist->plist
   :associate
   :contains-key?
   :contains-value?
   :dissociate
   :get-key
   :keys
   :merge
   :ordered-map
   :plist
   :plist->alist
   :put-key
   :table
   :vals
   :zipmap))




