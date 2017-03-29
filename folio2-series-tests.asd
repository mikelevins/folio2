;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-series-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of folio2-series
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-series-tests"
  :serial t
  :description "sequence and series tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2-series" "lift")
  :components ((:module "tests"
                        :serial t
                        :components ((:file "series"))))
  :perform (test-op (o c)
             (symbol-call :net.bardcode.folio2.series.tests :run-series-tests)
             (symbol-call :lift :describe-test-result
                          (symbol-value (find-symbol* :*test-result* :lift)) t)))

;;; (asdf:test-system :folio2-series-tests)
