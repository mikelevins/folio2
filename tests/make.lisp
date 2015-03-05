;;;; ***********************************************************************
;;;;
;;;; Name:          make.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       make tests
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package :cl-user)

(defpackage :net.bardcode.folio2.make.tests
  (:use :cl :net.bardcode.folio2.make :lift))

(in-package :net.bardcode.folio2.make.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite make-tests () ())

;;; ---------------------------------------------------------------------
;;; make tests
;;; ---------------------------------------------------------------------

(addtest (make-tests)
  test-make-symbol
  (ensure-same "Foo" (symbol-name (make 'cl:symbol :name "Foo"))))

(addtest (make-tests)
  test-make-keyword
  (ensure-same :frob (make 'cl:keyword :name "FROB")))

(addtest (make-tests)
  test-make-cons
  (ensure-same '(:a . :b) (make 'cl:cons :car (make 'cl:keyword :name "A") :cdr (make 'cl:keyword :name "B"))))

(addtest (make-tests)
  test-make-list
  (ensure-same '(#\A #\B #\C) (make 'cl:list :elements "ABC")))

(addtest (make-tests)
  test-make-array
  (ensure-same 'A (aref (make 'cl:array :dimensions '(2 3) :initial-element 'A) 1 2))
  (ensure-same 'H (aref (make 'cl:array :dimensions 8 :initial-contents '(a b c d e f g h)) 7))
  (let ((vec (make 'cl:array :dimensions 4 :fill-pointer 0 :adjustable t)))
    (loop for i from 0 below 10 do (vector-push-extend i vec))
    (ensure-same 9 (aref vec 9))))

(addtest (make-tests)
  test-make-string
  (ensure-same "aaBBB" (concatenate 'cl:string
                                    (make 'cl:string :length 2 :initial-element #\a)
                                    (make 'cl:string :length 3 :initial-element #\B))
               :test 'cl:string=))


(addtest (make-tests)
  test-make-hash-table
  (let ((tbl (make 'cl:hash-table :test 'equal)))
    (setf (gethash "name" tbl) "Fred")
    (setf (gethash "age" tbl) 45)
    (setf (gethash "shape" tbl) 'square)
    (ensure-same '(45 "Fred" square)
                 (list (gethash "age" tbl)
                       (gethash "name" tbl)
                       (gethash "shape" tbl))
                 :test 'cl:equal)))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-make-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'make-tests)))

;;; (net.bardcode.folio2.make.tests::run-make-tests)
;;; (lift:describe-test-result lift:*test-result* t)
