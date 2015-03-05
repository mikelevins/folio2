;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-as-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2-as-tests: tests fore the folio2-as system
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-as-tests
  :serial t
  :description "tests of the AS subsystem"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-as :folio2-as-syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "as")))))

;;; (asdf:load-system :folio2-as-tests)
;;; (net.bardcode.folio2.as.tests::run-as-tests)
;;; (lift:describe-test-result lift:*test-result* t)

