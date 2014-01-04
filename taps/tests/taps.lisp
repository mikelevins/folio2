;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          taps.lisp
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       tap tests
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package :cl-user)

(defpackage :net.bardcode.folio.taps.tests
  (:use :cl :net.bardcode.folio.as :net.bardcode.folio.taps :net.bardcode.folio.make :lift))

(in-package :net.bardcode.folio.taps.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite tap-tests () ())

;;; ---------------------------------------------------------------------
;;; tap function tests
;;; ---------------------------------------------------------------------

#|
(addtest (tap-tests)
  test-characters
  )
|#

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-tap-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'tap-tests)))

;;; (net.bardcode.folio.taps.tests::run-tap-tests)
