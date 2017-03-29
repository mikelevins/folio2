;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-taps-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio2-taps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-taps-tests"
  :serial t
  :description "sequence and taps tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2-taps" "lift")
  :components ((:module "tests"
                        :serial t
                        :components ((:file "taps"))))
  :perform (test-op (o c)
             (symbol-call :net.bardcode.folio2.taps.tests :run-tap-tests)
             (symbol-call :lift :describe-test-result
                          (symbol-value (find-symbol* :*test-result* :lift)) t)))

;;; (asdf:test-system :folio2-taps-tests)
