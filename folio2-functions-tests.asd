;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-functions-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio2-functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-functions-tests
  :serial t
  :description "tests of the FUNCTIONS subsystem"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-functions :folio2-functions-syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "functions")))))

;;; (asdf:load-system :folio2-functions-tests)
;;; (net.bardcode.folio2.functions.tests::run-function-tests)
;;; (lift:describe-test-result lift:*test-result* t)
