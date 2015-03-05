;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       uniform extensible tools for converting values from one type to another
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio2.as.tests
  (:use :cl :net.bardcode.folio2.as :lift))

(in-package :net.bardcode.folio2.as.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite as-tests () ())

(addtest (as-tests)
         test-as-symbol
         (ensure-same '|Foo| (as 'cl:symbol "Foo" :package :net.bardcode.folio2.as.tests)))

(addtest (as-tests)
         test-as-string
         (ensure-same "Foo" (as 'cl:string '|Foo|)))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-as-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'as-tests)))

;;; (net.bardcode.folio2.as.tests::run-as-tests)
