;;;; ***********************************************************************
;;;;
;;;; Name:          folio-functions-tests.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio-functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-functions-tests
  :serial t
  :description "tests of the FUNCTIONS subsystem"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-functions :folio-functions-syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "functions")))))

;;; (asdf:load-system :folio-functions-tests)
;;; (net.bardcode.folio.functions.tests::run-function-tests)
;;; (lift:describe-test-result lift:*test-result* t)
