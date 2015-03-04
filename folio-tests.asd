;;;; ***********************************************************************
;;;;
;;;; Name:          folio-tests.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       tests of the folio umbrella system
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defparameter *folio-root*
  (make-pathname :directory (pathname-directory *load-truename*)))

;;; (push *folio-root* asdf:*central-registry*)

(asdf:defsystem :folio-tests
  :serial t
  :description "the folio umbrella system"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-as-tests
               :folio-boxes-tests
               :folio-functions-tests
               :folio-make-tests
               :folio-maps-tests
               :folio-pairs-tests
               :folio-sequences-tests
               :folio-series-tests
               :folio-taps-tests)
  :components ((:module "src"
                        :serial t
                        :components ((:file "folio-package")))))

;;; (asdf:load-system :folio-tests)
;;; (lift:describe-test-result lift:*test-result* t)
;;; (net.bardcode.folio.as.tests::run-as-tests)
;;; (net.bardcode.folio.boxes.tests::run-box-tests)
;;; (net.bardcode.folio.functions.tests::run-function-tests)
;;; (net.bardcode.folio.make.tests::run-make-tests)
;;; (net.bardcode.folio.maps.tests::run-map-tests)
;;; (net.bardcode.folio.pairs.tests::run-pair-tests)
;;; (net.bardcode.folio.sequences.tests::run-sequence-tests)
;;; (net.bardcode.folio.series.tests::run-series-tests)
;;; (net.bardcode.folio.taps.tests::run-tap-tests)
