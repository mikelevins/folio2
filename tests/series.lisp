;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          series.lisp
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       series tests
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package :cl-user)

(defpackage :net.bardcode.folio.series.tests
  (:use :cl :net.bardcode.folio.as :net.bardcode.folio.make :net.bardcode.folio.series :lift)
  (:import-from :net.bardcode.folio.sequences
                :add-first :by :dispose :element :take)
  (:shadowing-import-from :net.bardcode.folio.sequences
                          :first))

(in-package :net.bardcode.folio.series.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite series-tests () ())

;;; ---------------------------------------------------------------------
;;; series tests
;;; ---------------------------------------------------------------------

(addtest (series-tests)
  test-iterate
  (let ((ser1 (iterate '1+ 0))
        (ser2 (iterate 'cdr (list :a :b :c :d :e :f :g :h :i :j :k))))
    (ensure-same 3 (series:collect-nth 3 ser1))
    (ensure-same 9 (series:collect-nth 9 ser1))
    (ensure-same 101 (series:collect-nth 101 ser1))
    (ensure-same :a (first (series:collect-nth 0 ser2)))
    (ensure-same :d (first (series:collect-nth 3 ser2)))
    (ensure-same :j (first (series:collect-nth 9 ser2)))))


(addtest (series-tests)
  test-repeat
  (let* ((counter 0)
         (ser1 (repeat (setf counter (1+ counter))))
         (ser2 (repeat t)))
    (ensure-same '(1 2 3) (as 'list (take 3 ser1)))
    (ensure-same t (element ser2 10))))

(addtest (series-tests)
  test-as
  (ensure (folio::series? (as 'foundation-series '(1 2 3))))
  (ensure (cl:listp (as 'cl:list (series 1 2 3)))))

(addtest (series-tests)
  test-add-first
  (ensure-same 1 (first (add-first 1 (range-from 0)))))

(addtest (series-tests)
  test-by
  (ensure-same '(1 2 3) (as 'cl:list (first (by 3 (range-from 1)))))
  (ensure-same "Foo" (as 'cl:string (first (by 3 (scan "Foobar"))))))

(addtest (series-tests)
  test-dispose
  (let ((ints (range-from 0)))
    (multiple-value-bind (odds evens)(dispose ints 'cl:oddp 'cl:evenp)
      (ensure-same '(1 3 5) (as 'cl:list (take 3 (series:choose odds ints))))
      (ensure-same '(0 2 4) (as 'cl:list (take 3 (series:choose evens ints)))))))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-series-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'series-tests)))

;;; (net.bardcode.folio.series.tests::run-series-tests)
;;; (lift:describe-test-result lift:*test-result* t)

