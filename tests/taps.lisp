;;;; ***********************************************************************
;;;;
;;;; Name:          taps.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       tap tests
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package :cl-user)

(defpackage :net.bardcode.folio2.taps.tests
  (:use :cl :net.bardcode.folio2.as :net.bardcode.folio2.taps :net.bardcode.folio2.make :lift))

(in-package :net.bardcode.folio2.taps.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite tap-tests () ())

;;; ---------------------------------------------------------------------
;;; tap function tests
;;; ---------------------------------------------------------------------

(addtest (tap-tests)
  test-characters
  (ensure-same #\b (series:collect-nth 3 (characters "Foobar"))))

(addtest (tap-tests)
  test-elements
  (ensure-same :c (series:collect-nth 2 (elements (list :a :b :c :d)))))

(addtest (tap-tests)
  test-lines
  (let ((text (format nil "line0~%line1~%line2~%line3~%")))
    (ensure-same "line1" (series:collect-nth 1 (lines text)))))

(addtest (tap-tests)
  test-octets
  (let ((vec (make-array 8 :element-type '(unsigned-byte 8) :initial-contents '(0 1 2 3 4 5 6 7))))
    (ensure-same 5 (series:collect-nth 5 (octets vec)))))

(defclass slots-test-class ()
  ((name :reader slots-test-class-name :initform "Fred")
   (age :reader slots-test-class-age :initform 35)
   (shape :reader slots-test-class-shape :initform :square)))

(addtest (tap-tests)
  test-slots
  (let* ((obj (make-instance 'slots-test-class))
         (slots (as 'list (slots obj))))
    (ensure-same '(name . "Fred") (assoc 'name slots))))

(addtest (tap-tests)
  test-objects
  (let* ((text (format nil ":a :b :c"))
         (objs (as 'list (objects text))))
    (ensure-same :b (elt objs 1))))


;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-tap-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'tap-tests)))

;;; (net.bardcode.folio2.taps.tests::run-tap-tests)
;;; (lift:describe-test-result lift:*test-result* t)

