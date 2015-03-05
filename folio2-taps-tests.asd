;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-taps-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio2-taps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-taps-tests
  :serial t
  :description "sequence and taps tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-taps :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "taps")))))

;;; (asdf:load-system :folio2-taps-tests)
;;; (net.bardcode.folio2.taps.tests::run-tap-tests)
;;; (lift:describe-test-result lift:*test-result* t)
