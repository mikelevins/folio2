;;;; ***********************************************************************
;;;;
;;;; Name:          folio-maps-tests.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       tests of finite maps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-maps-tests
  :serial t
  :description "map tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-maps :folio-maps-syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "maps")))))

;;; (asdf:load-system :folio-maps-tests)
;;; (net.bardcode.folio.maps.tests::run-map-tests)
;;; (lift:describe-test-result lift:*test-result* t)
