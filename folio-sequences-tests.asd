;;;; ***********************************************************************
;;;;
;;;; Name:          folio-sequences-tests.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio-sequences
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-sequences-tests
  :serial t
  :description "sequence and series tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-sequences :folio-sequences-syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "sequences")))))

;;; (asdf:load-system :folio-sequences-tests)
;;; (net.bardcode.folio.sequences.tests::run-sequence-tests)
;;; (lift:describe-test-result lift:*test-result* t)
