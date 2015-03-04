;;;; ***********************************************************************
;;;;
;;;; Name:          folio-make-tests.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       folio-make-tests: tests of folio-make
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-make-tests
  :serial t
  :description "tests of MAKE"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-make :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "make")))))

;;; (asdf:load-system :folio-make-tests)
;;; (net.bardcode.folio.make.tests::run-make-tests)
;;; (lift:describe-test-result lift:*test-result* t)
