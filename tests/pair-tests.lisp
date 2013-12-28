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

(defpackage :net.bardcode.folio.pair.tests
  (:use :cl :net.bardcode.folio.common :net.bardcode.folio.pairs :lift)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :intersection :last :length :make
                          :merge :pair :position :position-if :reduce :remove :rest
                          :reverse :search :second :sequence :sort :union))

(in-package :net.bardcode.folio.pair.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite pair-tests () ())

(addtest (pair-tests)
  test-as
  (ensure (null (as 'pair nil)))
  (ensure-same '(1 2 3) (as 'pair '(1 2 3)))
  (ensure-same '(1 2 3) (as 'pair (vector 1 2 3)))
  (ensure-same '(#\1 #\2 #\3) (as 'pair "123"))
  (ensure-same '(1 2 3) (as 'pair (fset:seq 1 2 3)))
  (ensure-same '(1 2 3) (as 'pair (scan '(1 2 3)))))

(addtest (pair-tests)
  test-left
  (ensure-same 'a (left (cons 'a 'b))))

(addtest (pair-tests)
  test-make
  (ensure-same 'a (left (make 'pair :left 'a :right 'b))))

(addtest (pair-tests)
  test-right
  (ensure-same 'b (right (cons 'a 'b)))
  (ensure-same '(2 3) (right '(1 2 3))))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-pair-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'pair-tests)))

;;; (net.bardcode.folio.pair.tests::run-pair-tests)
