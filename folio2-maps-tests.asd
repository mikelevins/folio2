;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-maps-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of finite maps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-maps-tests"
  :serial t
  :description "map tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2-maps" "folio2-maps-syntax" "lift")
  :components ((:module "tests"
                        :serial t
                        :components ((:file "maps"))))
  :perform (test-op (o c)
             (symbol-call :net.bardcode.folio2.maps.tests :run-map-tests)
             (symbol-call :lift :describe-test-result
                          (symbol-value (find-symbol* :*test-result* :lift)) t)))
