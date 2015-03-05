;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       functions package
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio2.functions
  (:use :cl :net.bardcode.folio2.as :net.bardcode.folio2.make)
  (:import-from :alexandria :compose :conjoin :disjoin :multiple-value-compose)
  (:export :$ :^ :-> :cascade :compose :conjoin :disjoin :flip :fn :function? :functional?
           :generic-function? :method? :partial :rpartial))

