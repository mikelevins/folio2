;;;; ***********************************************************************
;;;;
;;;; Name:          folio-series-tests.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio-series
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-series-tests
  :serial t
  :description "sequence and series tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-series :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "series")))))

;;; (asdf:load-system :folio-series-tests)
;;; (net.bardcode.folio.series.tests::run-series-tests)
;;; (lift:describe-test-result lift:*test-result* t)
