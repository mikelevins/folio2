;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio.ordering.tests
  (:use :cl :net.bardcode.folio.common :net.bardcode.folio.ordering :lift)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :intersection :last :length
                          :merge :position :position-if :reduce :remove :rest
                          :reverse :second :sequence :sort :union))

(in-package :net.bardcode.folio.ordering.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite ordering-tests () ())

(addtest (ordering-tests)
  test->
  (ensure (> 3 2))
  (ensure (> 3 2 1))
  (ensure (not (> 3 2 2))))

(addtest (ordering-tests)
  test->=
  (ensure (>= 3 2))
  (ensure (>= 3 2 2)))

(addtest (ordering-tests)
  test-<
  (ensure (< 2 3))
  (ensure (< 1 2 3))
  (ensure (not (< 2 2 3))))

(addtest (ordering-tests)
  test-<=
  (ensure (<= 2 3))
  (ensure (<= 2 2 3)))

(addtest (ordering-tests)
  test-sort
  (ensure-same '(1 2 3) (sort '(2 3 1)))
  (ensure-same '("1" "2" "3") (sort '("2" "3" "1") :test 'cl:string-lessp)))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-ordering-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'ordering-tests)))

;;; (net.bardcode.folio.ordering.tests::run-ordering-tests)
