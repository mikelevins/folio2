;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-functions-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio2-functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-functions-tests"
  :serial t
  :description "tests of the FUNCTIONS subsystem"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2-functions" "folio2-functions-syntax" "lift")
  :components ((:module "tests"
                        :serial t
                        :components ((:file "functions"))))
  :perform (test-op (o c)
             (symbol-call :net.bardcode.folio2.functions.tests :run-function-tests)
             (symbol-call :lift :describe-test-result
                          (symbol-value (find-symbol* :*test-result* :lift)) t)))

;;; (asdf:test-system :folio2-functions-tests)
