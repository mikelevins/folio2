;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-boxes-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2-boxes-tests: tests of folio2-boxes
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-boxes-tests
  :serial t
  :description "box tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-boxes :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "boxes")))))

;;; (asdf:load-system :folio2-boxes-tests)
;;; (net.bardcode.folio2.boxes.tests::run-box-tests)
;;; (lift:describe-test-result lift:*test-result* t)
