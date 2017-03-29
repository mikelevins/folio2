;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-functions-syntax.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       optional syntax extensions for folio2-functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-functions-syntax"
  :serial t
  :description "syntax for working with functions as values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("alexandria" "folio2-functions")
  :components ((:module "src"
                        :serial t
                        :components ((:file "functions-package")
                                     (:file "functions-syntax"))))
  :in-order-to ((test-op (test-op "folio-functions-tests"))))

;;; (asdf:load-system :folio2-functions-syntax)
