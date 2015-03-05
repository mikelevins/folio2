;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-functions.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2-functions: combinators and higher-order functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)


(asdf:defsystem :folio2-functions
  :serial t
  :description "tools for working with functions as values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:alexandria)
  :components ((:module "src"
                        :serial t
                        :components ((:file "functions-package")
                                     (:file "functions-functions")))))

;;; (asdf:load-system :folio2-functions)
