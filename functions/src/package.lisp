;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - Bard features in Common Lisp
;;;; Purpose:       functions package
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio.functions
  (:use :cl :net.bardcode.folio.as :net.bardcode.folio.make)
  (:export :$ :^ :-> :apply :cascade :compose :conjoin :disjoin :flip :fn :function? :functional?
           :generic-function? :iterate :method? :partial :rpartial))

