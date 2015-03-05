;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-make-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2-make-tests: tests of folio2-make
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-make-tests
  :serial t
  :description "tests of MAKE"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-make :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "make")))))

;;; (asdf:load-system :folio2-make-tests)
;;; (net.bardcode.folio2.make.tests::run-make-tests)
;;; (lift:describe-test-result lift:*test-result* t)
