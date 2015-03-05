;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-series-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio2-series
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-series-tests
  :serial t
  :description "sequence and series tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-series :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "series")))))

;;; (asdf:load-system :folio2-series-tests)
;;; (net.bardcode.folio2.series.tests::run-series-tests)
;;; (lift:describe-test-result lift:*test-result* t)
