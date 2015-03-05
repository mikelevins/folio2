;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-maps-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of finite maps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-maps-tests
  :serial t
  :description "map tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-maps :folio2-maps-syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "maps")))))

;;; (asdf:load-system :folio2-maps-tests)
;;; (net.bardcode.folio2.maps.tests::run-map-tests)
;;; (lift:describe-test-result lift:*test-result* t)
