;;;; ***********************************************************************
;;;;
;;;; Name:          folio-pairs-tests.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio-pairs
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-pairs-tests
  :serial t
  :description "pair tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-pairs :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "pairs")))))

;;; (asdf:load-system :folio-pairs-tests)
;;; (net.bardcode.folio.pairs.tests::run-pair-tests)
;;; (lift:describe-test-result lift:*test-result* t)
