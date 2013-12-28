;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          converting-tests.lisp
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       tests of conversion functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio.converting.tests
  (:use :cl :net.bardcode.folio.common :net.bardcode.folio.converting :lift)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :intersection :last :length
                          :merge :position :position-if :reduce :remove :rest
                          :reverse :search :second :sequence :sort :union))

(in-package :net.bardcode.folio.converting.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite converting-tests () ())

(addtest (converting-tests)
  test-array-type-for-copy
  (ensure (subtypep (type-for-copy (make-array 10)) 'array)))

(addtest (converting-tests)
  test-class-type-for-copy
  (ensure (subtypep (type-for-copy (find-class 'cl:vector)) 'cl:built-in-class)))

(addtest (converting-tests)
  test-character-type-for-copy
  (ensure (subtypep (type-for-copy #\A) 'character)))

(addtest (converting-tests)
  test-complex-type-for-copy
  (ensure (subtypep (type-for-copy #C(1 2)) 'complex)))

(addtest (converting-tests)
  test-cons-type-for-copy
  (ensure (subtypep (type-for-copy (list 1 2 3)) 'cons)))

(addtest (converting-tests)
  test-double-float-type-for-copy
  (ensure (subtypep (type-for-copy 1.0d0) 'double-float)))

(addtest (converting-tests)
  test-file-stream-type-for-copy
  (with-open-file (out "/tmp/file-stream-test-file" :direction :output :if-does-not-exist :create :if-exists :append)
    (ensure (subtypep (type-for-copy out) 'file-stream))))

(addtest (converting-tests)
  test-function-type-for-copy
  (ensure (subtypep (type-for-copy (function (lambda (x) x))) 'function)))

(addtest (converting-tests)
  test-hash-table-type-for-copy
  (ensure (subtypep (type-for-copy (make-hash-table)) 'hash-table)))

#+lispworks
(setf (logical-pathname-translations "home")
      `(("*" ,(user-homedir-pathname))))

(addtest (converting-tests)
  test-logical-pathname-type-for-copy
  (ensure (subtypep (type-for-copy (logical-pathname #P"home:")) 'logical-pathname)))

(addtest (converting-tests)
  test-null-type-for-copy
  (ensure (subtypep (type-for-copy nil) 'null)))

(addtest (converting-tests)
  test-package-type-for-copy
  (ensure (subtypep (type-for-copy (find-package :cl)) 'package)))

(addtest (converting-tests)
  test-pathname-type-for-copy
  (ensure (subtypep (type-for-copy #P"/") 'pathname)))

(addtest (converting-tests)
  test-ratio-type-for-copy
  (ensure (subtypep (type-for-copy 2/3) 'ratio)))

(addtest (converting-tests)
  test-random-state-type-for-copy
  (ensure (subtypep (type-for-copy (make-random-state)) 'random-state)))

(addtest (converting-tests)
  test-readtable-type-for-copy
  (ensure (subtypep (type-for-copy cl:*readtable*) 'readtable)))

(addtest (converting-tests)
  test-single-float-type-for-copy
  (ensure (subtypep (type-for-copy 1.0) 'single-float)))

(defclass test-class ()())

(addtest (converting-tests)
  test-standard-class-type-for-copy
  (ensure (subtypep (type-for-copy (class-of (make-instance 'test-class))) 'standard-class)))

(defmethod test-function ((x test-class)) t)

(addtest (converting-tests)
  test-standard-generic-function-type-for-copy
  (ensure (subtypep (type-for-copy #'test-function) 'standard-generic-function)))

(addtest (converting-tests)
  test-standard-method-type-for-copy
  (ensure (subtypep (type-for-copy (first (compute-applicable-methods #'test-function 
                                                                      (list (make-instance 'test-class)))))
                    'standard-method)))

(addtest (converting-tests)
  test-stream-type-for-copy
  (with-input-from-string (in "foo")
    (ensure (subtypep (type-for-copy in) 'stream))))


(addtest (converting-tests)
  test-string-type-for-copy
  (ensure (subtypep (type-for-copy "foo") 'string)))

(addtest (converting-tests)
  test-string-stream-type-for-copy
  (with-input-from-string (in "foo")
    (ensure (subtypep (type-for-copy in) 'string-stream))))

(defstruct test-struct (testfield))

(addtest (converting-tests)
  test-structure-class-type-for-copy
  (ensure (subtypep (type-for-copy (class-of (make-test-struct :testfield 0))) 'structure-class)))

(addtest (converting-tests)
  test-symbol-type-for-copy
  (ensure (subtypep (type-for-copy 'foo) 'symbol)))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-converting-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'converting-tests)))

;;; (net.bardcode.folio.converting.tests::run-converting-tests)
