;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - Bard features in Common Lisp
;;;; Purpose:       maps package
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio.maps
  (:use :cl :net.bardcode.folio.as :net.bardcode.folio.make)
  (:shadow :map :merge :values)
  (:export :alist->plist :contains-key? :contains-value? :get-key :keys :map :map? :merge :plist->alist :put-key :values))

