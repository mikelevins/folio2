;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          maps.lisp
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       map tests
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package :cl-user)

(defpackage :net.bardcode.folio.maps.tests
  (:use :cl :net.bardcode.folio.as :net.bardcode.folio.make :net.bardcode.folio.maps :lift)
  (:shadowing-import-from :net.bardcode.folio.maps :map :merge :values))

(in-package :net.bardcode.folio.maps.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite map-tests () ())

;;; ---------------------------------------------------------------------
;;; map type tests
;;; ---------------------------------------------------------------------

(deftestsuite map-type-tests (map-tests) ())

(addtest (map-type-tests)
  test-alist?
  (ensure (alist? '((a . 1)(b . 2)(c . 3))))
  (ensure (not (alist? '((a . 1)(b . 2) c)))))

(addtest (map-type-tests)
  test-plist?
  (ensure (plist? '(a 1 b 2 c 3)))
  (ensure (not (plist? '((a . 1)(b . 2) c))))
  (ensure (not (plist? '(a b c)))))

;;; ---------------------------------------------------------------------
;;; map function tests
;;; ---------------------------------------------------------------------

(deftestsuite map-function-tests (map-tests) ())

(addtest (map-function-tests)
  test-alist
  (ensure-same '((a . 1)(b . 2)) (alist ('a . 1)('b . 2)) :test 'cl:equal))

(addtest (map-function-tests)
  test-as
  (ensure-same '((a . 1)(b . 2)) (as 'map '((a . 1)(b . 2))) :test 'cl:equal)
  (ensure-same '(a 1 b 2) (as 'map '(a 1 b 2)) :test 'cl:equal)
  (ensure-same '((a . 1)(b . 2)) (as 'alist '(a 1 b 2)) :test 'cl:equal)
  (ensure-same '(a 1 b 2) (as 'plist '((a . 1)(b . 2))) :test 'cl:equal)
  (ensure-same {'a 1 'b 2} (as 'wb-map '(a 1 b 2)) :test 'fset:equal?))

(addtest (map-function-tests)
  test-contains-key?
  (ensure (contains-key? (as 'wb-map '(a 1 b 2)) 'a))
  (ensure (not (contains-key? (as 'wb-map '(a 1 b 2)) 'z))))

(addtest (map-function-tests)
  test-contains-value?
  (ensure (contains-value? (as 'wb-map '(a 1 b 2)) '2))
  (ensure (not (contains-value? (as 'wb-map '(a 1 b 2)) 100))))

(addtest (map-function-tests)
  test-get-key
  (ensure-same 5 (get-key (as 'alist '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6)) :e))
  (ensure-same 5 (get-key (as 'wb-map '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6)) :e)))

(addtest (map-function-tests)
  test-keys
  (ensure-same '(:a :b :c :d :e :f) (keys (as 'alist '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6))) :test 'equal)
  (ensure-same '(:a :b :c :d :e :f) (keys (as 'plist '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6))) :test 'equal)
  (ensure-same '(:a :b :c :d :e :f) (keys (as 'wb-map '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6))) :test 'equal))

(addtest (map-function-tests)
  test-make
  (ensure-same '((a . 1)(b . 2)) (make 'alist :contents '(a 1 b 2)) :test 'cl:equal)
  (ensure-same '(a 1 b 2) (make 'plist :contents '(a 1 b 2)) :test 'cl:equal)
  (ensure-same {'a 1 'b 2} (make 'wb-map :contents '(a 1 b 2)) :test 'fset:equal?))

(addtest (map-function-tests)
  test-map?
  (ensure (map? (make 'alist :contents '(a 1 b 2))))
  (ensure (map? (make 'plist :contents '(a 1 b 2))))
  (ensure (map? (make 'wb-map :contents '(a 1 b 2))))
  (ensure (not (map? '(1 2 3)))))

(addtest (map-function-tests)
  test-merge
  (ensure-same 202
               (get-key (merge (plist :a 1 :b 2 :c 3)(alist (:b . 202) (:d . 404)))
                        :b))
  (ensure-same 202
               (get-key (merge (alist (:a . 1) (:b . 2) (:c . 3))(alist (:b . 202) (:d . 404)))
                        :b))
  (ensure-same 202
               (get-key (merge (as 'wb-map '(:a 1 :b 2))
                               (as 'wb-map '(:b 202 :c 303)))
                        :b))
  (ensure-same 3003
               (get-key (merge (as 'wb-map '(:a 1 :b 2 :c 3))
                               (as 'wb-map '(:c 3003 :d 4004)))
                        :c)))

(addtest (map-function-tests)
  test-put-key
  (ensure-same 505 (get-key (put-key (as 'alist '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6)) :e 505) :e))
  (ensure-same 404 (get-key (put-key (as 'wb-map '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6)) :d 404) :d)))

(addtest (map-function-tests)
  test-values
  (ensure-same '(1 2 3 4 5 6) (values (as 'alist '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6))) :test 'equal)
  (ensure-same '(1 2 3 4 5 6) (values (as 'plist '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6))) :test 'equal)
  (ensure-same '(1 2 3 4 5 6) (values (as 'wb-map '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6))) :test 'equal))

(addtest (map-function-tests)
  test-wb-map?
  (ensure (not (wb-map? (as 'alist '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6)))))
  (ensure (wb-map? (as 'wb-map '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6)))))

;;; ---------------------------------------------------------------------
;;; map syntax tests
;;; ---------------------------------------------------------------------

(deftestsuite map-syntax-tests (map-tests) ())

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-map-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'map-tests)))

;;; (net.bardcode.folio.maps.tests::run-map-tests)
;;; (lift:describe-test-result lift:*test-result* t)
