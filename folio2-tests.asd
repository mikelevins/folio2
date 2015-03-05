;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-tests.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tests of the folio2 umbrella system
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defparameter *folio2-root*
  (make-pathname :directory (pathname-directory *load-truename*)))

;;; (push *folio2-root* asdf:*central-registry*)

(asdf:defsystem :folio2-tests
  :serial t
  :description "the folio2 umbrella system"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-as-tests
               :folio2-boxes-tests
               :folio2-functions-tests
               :folio2-make-tests
               :folio2-maps-tests
               :folio2-pairs-tests
               :folio2-sequences-tests
               :folio2-series-tests
               :folio2-taps-tests)
  :components ((:module "src"
                        :serial t
                        :components ((:file "folio2-package")))))

;;; (asdf:load-system :folio2-tests)
;;; (lift:describe-test-result lift:*test-result* t)
;;; (net.bardcode.folio2.as.tests::run-as-tests)
;;; (net.bardcode.folio2.boxes.tests::run-box-tests)
;;; (net.bardcode.folio2.functions.tests::run-function-tests)
;;; (net.bardcode.folio2.make.tests::run-make-tests)
;;; (net.bardcode.folio2.maps.tests::run-map-tests)
;;; (net.bardcode.folio2.pairs.tests::run-pair-tests)
;;; (net.bardcode.folio2.sequences.tests::run-sequence-tests)
;;; (net.bardcode.folio2.series.tests::run-series-tests)
;;; (net.bardcode.folio2.taps.tests::run-tap-tests)
