;;;; ***********************************************************************
;;;;
;;;; Name:          folio-as-tests.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       folio-as-tests: tests fore the folio-as system
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-as-tests
  :serial t
  :description "tests of the AS subsystem"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-as :folio-as-syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "as")))))

;;; (asdf:load-system :folio-as-tests)
;;; (net.bardcode.folio.as.tests::run-as-tests)
;;; (lift:describe-test-result lift:*test-result* t)

