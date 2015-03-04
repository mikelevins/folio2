;;;; ***********************************************************************
;;;;
;;;; Name:          folio-taps-tests.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio-taps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-taps-tests
  :serial t
  :description "sequence and taps tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-taps :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "taps")))))

;;; (asdf:load-system :folio-taps-tests)
;;; (net.bardcode.folio.taps.tests::run-tap-tests)
;;; (lift:describe-test-result lift:*test-result* t)
