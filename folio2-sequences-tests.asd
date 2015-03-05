;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-sequences-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio2-sequences
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-sequences-tests
  :serial t
  :description "sequence and series tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-sequences :folio2-sequences-syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "sequences")))))

;;; (asdf:load-system :folio2-sequences-tests)
;;; (net.bardcode.folio2.sequences.tests::run-sequence-tests)
;;; (lift:describe-test-result lift:*test-result* t)
