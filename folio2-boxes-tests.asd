;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-boxes-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2-boxes-tests: tests of folio2-boxes
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-boxes-tests"
  :serial t
  :description "box tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-boxes :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "boxes"))))
  :perform (test-op (o c)
             (symbol-call :net.bardcode.folio2.boxes.tests :run-box-tests)
             (symbol-call :lift :describe-test-result
                          (symbol-value (find-symbol* :*test-result* :lift)) t)))

;;; (asdf:test-system :folio2-boxes-tests)
