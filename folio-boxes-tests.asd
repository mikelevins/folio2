;;;; ***********************************************************************
;;;;
;;;; Name:          folio-boxes-tests.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       folio-boxes-tests: tests of folio-boxes
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-boxes-tests
  :serial t
  :description "box tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-boxes :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "boxes")))))

;;; (asdf:load-system :folio-boxes-tests)
;;; (net.bardcode.folio.boxes.tests::run-box-tests)
;;; (lift:describe-test-result lift:*test-result* t)
