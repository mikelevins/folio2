;;;; ***********************************************************************
;;;;
;;;; Name:          pairs.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       pair tests
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package :cl-user)

(defpackage :net.bardcode.folio2.pairs.tests
  (:use :cl :net.bardcode.folio2.as :net.bardcode.folio2.pairs :net.bardcode.folio2.make :lift))

(in-package :net.bardcode.folio2.pairs.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite pair-tests () ())

;;; ---------------------------------------------------------------------
;;; pair function tests
;;; ---------------------------------------------------------------------

(addtest (pair-tests)
  test-as-pair
  (ensure-same '(1 2) (as 'pair '(1 2)) :test 'equal)
  (ensure-same '(1 2) (as 'pair (vector 1 2)) :test 'equal))

(addtest (pair-tests)
  test-make
  (ensure-same :a (left (make 'pair :left :a :right :b)))
  (ensure-same :b (right (make 'pair :left :a :right :b))))

(addtest (pair-tests)
  test-pair
  (ensure-same :a (left (pair :a :b)))
  (ensure-same :b (right (pair :a :b))))

(addtest (pair-tests)
  test-pair?
  (ensure (pair? nil))
  (ensure (pair? (pair :a :b)))
  (ensure (not (pair? :a))))

(addtest (pair-tests)
  test-set-pair
  (let ((p (pair :a :b)))
    (ensure-same :a (left p))
    (ensure-same :b (right p))
    (setf (left p) :x)
    (setf (right p) :y)
    (ensure-same :x (left p))
    (ensure-same :y (right p))))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-pair-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'pair-tests)))

;;; (net.bardcode.folio2.pairs.tests::run-pair-tests)
