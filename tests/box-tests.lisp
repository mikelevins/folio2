;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          box-tests.lisp
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       testing boxes
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package :cl-user)

(defpackage :net.bardcode.folio.boxes.tests
  (:use :cl :net.bardcode.folio.common :net.bardcode.folio.boxes :lift)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :apply :find :first :intersection :last :length
                          :merge :position :position-if :reduce :remove :rest
                          :reverse :second :sequence :sort :union))

(in-package :net.bardcode.folio.boxes.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite box-tests () ())

;;; ---------------------------------------------------------------------
;;; box type tests
;;; ---------------------------------------------------------------------

(deftestsuite box-type-tests (box-tests) ())

(addtest (box-type-tests)
  test-not-box
  (ensure (not (box? 5))))

(addtest (box-type-tests)
  test-is-box
  (ensure (box? (cons :box 5))))


;;; ---------------------------------------------------------------------
;;; box function tests
;;; ---------------------------------------------------------------------

(deftestsuite box-function-tests (box-tests) ())

(addtest (box-function-tests)
  test-as-box
  (ensure (box? (as 'box 5))))

(addtest (box-function-tests)
  test-box
  (ensure (box? (box 5))))

(addtest (box-function-tests)
  test-make-box
  (ensure (box? (make 'box :value 5))))

(addtest (box-function-tests)
  test-set-box!
  (let ((b (box 1)))
    (ensure (box? b))
    (ensure (= 1 (unbox b)))
    (set-box! b 2)
    (ensure (= 2 (unbox b)))))

(addtest (box-function-tests)
  test-setf-unbox
  (let ((b (box 1)))
    (ensure (box? b))
    (ensure (= 1 (unbox b)))
    (setf (unbox b) 2)
    (ensure (= 2 (unbox b)))))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-box-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'box-tests)))

;;; (net.bardcode.folio.boxes.tests::run-box-tests)
