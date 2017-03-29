;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-make-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2-make-tests: tests of folio2-make
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-make-tests"
  :serial t
  :description "tests of MAKE"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2-make" "lift")
  :components ((:module "tests"
                        :serial t
                        :components ((:file "make"))))
  :perform (test-op (o c)
             (symbol-call :net.bardcode.folio2.make.tests :run-make-tests)
             (symbol-call :lift :describe-test-result
                          (symbol-value (find-symbol* :*test-result* :lift)) t)))

;;; (asdf:test-system :folio2-make-tests)
