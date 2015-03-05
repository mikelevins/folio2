;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-pairs-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio2-pairs
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-pairs-tests
  :serial t
  :description "pair tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-pairs :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "pairs")))))

;;; (asdf:load-system :folio2-pairs-tests)
;;; (net.bardcode.folio2.pairs.tests::run-pair-tests)
;;; (lift:describe-test-result lift:*test-result* t)
