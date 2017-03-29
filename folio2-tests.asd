;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of the folio2 umbrella system
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-tests"
  :serial t
  :description "the folio2 umbrella test system"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2")
  :in-order-to
  ((test-op (test-op "folio2-as-tests" "folio2-boxes-tests" "folio2-functions-tests"
                     "folio2-make-tests" "folio2-maps-tests" "folio2-pairs-tests"
                     "folio2-sequences-tests" "folio2-series-tests" "folio2-taps-tests"))))

;;; (asdf:test-system :folio2-tests)
