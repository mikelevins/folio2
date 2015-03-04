;;;; ***********************************************************************
;;;;
;;;; Name:          folio-functions-syntax.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       optional syntax extensions for folio-functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)


(asdf:defsystem :folio-functions-syntax
  :serial t
  :description "syntax for working with functions as values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:alexandria :folio-functions)
  :components ((:module "src"
                        :serial t
                        :components ((:file "functions-package")
                                     (:file "functions-syntax")))))

;;; (asdf:load-system :folio-functions-syntax)
