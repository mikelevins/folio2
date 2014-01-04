;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       function tests
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package :cl-user)

(defpackage :net.bardcode.folio.functions.tests
  (:use :cl :net.bardcode.folio.functions :lift))

(in-package :net.bardcode.folio.functions.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite function-tests () ())

;;; ---------------------------------------------------------------------
;;; function syntax tests
;;; ---------------------------------------------------------------------

(deftestsuite function-syntax-tests (function-tests) ())

(addtest (function-syntax-tests)
  test-^
  (ensure-same 5 (funcall (^ () 5)))
  (ensure-same 5 (funcall (^ (x y) (+ x y)) 2 3)))

(addtest (function-syntax-tests)
  test-->
  (ensure-same '(3 4 5) (multiple-value-list (funcall (-> '1+ '1+ '1+) 2 3 4)) :test 'equal))

(addtest (function-syntax-tests)
  test-fn
  (ensure-same 5 (funcall (fn () 5)))
  (ensure-same 5 (funcall (fn (x y) (+ x y)) 2 3)))

(addtest (function-syntax-tests)
  test-cascade
  (ensure-same '(2 4 9) (multiple-value-list 
                         (cascade (1 2 3) 
                                  (-> (^ (x)(* 2 x))
                                      (^ (x)(+ x x))
                                      (^ (x)(* x x))))))
  (ensure-same '(4 5 6) (multiple-value-list 
                         (cascade (1 2 3) 
                                  (-> '1+ '1+ '1+)
                                  (-> '1+ '1+ '1+)
                                  (-> '1+ '1+ '1+)))))

;;; ---------------------------------------------------------------------
;;; function function tests
;;; ---------------------------------------------------------------------

(deftestsuite function-function-tests (function-tests) ())

(addtest (function-function-tests)
  test-flip
  (ensure (funcall (flip '>) 2 3)))

(addtest (function-function-tests)
  test-function?
  (ensure (function? #'cl:values))
  (ensure (function? #'(lambda (x) x)))
  (ensure (not (function? #'shared-initialize)))
  (ensure (not (function? (find-method #'make-instance '() (mapcar 'find-class '(symbol)))))))

(addtest (function-function-tests)
  test-functional?
  (ensure (functional? #'(lambda (x) x)))
  (ensure (functional? #'shared-initialize))
  (ensure (functional? (find-method #'make-instance '() (mapcar 'find-class '(symbol))))))

(addtest (function-function-tests)
  test-generic-function?
  (ensure (not (generic-function? #'(lambda (x) x))))
  (ensure (generic-function? #'shared-initialize))
  (ensure (not (generic-function? (find-method #'make-instance '() (mapcar 'find-class '(symbol)))))))

(addtest (function-function-tests)
  test-method?
  (ensure (not (method? #'(lambda (x) x))))
  (ensure (not (method? #'shared-initialize)))
  (ensure (method? (find-method #'make-instance '() (mapcar 'find-class '(symbol))))))

(addtest (function-function-tests)
  test-partial
  (ensure-same 3 (funcall (partial '1+) 2))
  (ensure-same '(0 1 2 3) (remove-if (partial '< 3) '(0 1 2 3 4 5 6 7)) :test 'equal))

(addtest (function-function-tests)
  test-rpartial
  (ensure-same '(5 6 7) (remove-if (rpartial '< 5) '(0 1 2 3 4 5 6 7)) :test 'equal))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-function-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'function-tests)))

;;; (net.bardcode.folio.functions.tests::run-function-tests)
;;; (lift:describe-test-result lift:*test-result* t)
