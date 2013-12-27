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

(defpackage :net.bardcode.folio.function.tests
  (:use :cl :net.bardcode.folio.common :net.bardcode.folio.functions :lift)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :intersection :last :length
                          :merge :position :position-if :reduce :remove :rest
                          :reverse :second :sequence :sort :union))

(in-package :net.bardcode.folio.function.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite function-tests () ())

(addtest (function-tests)
  test-apply
  (ensure (apply 'cl:< (list 0 1 2 3))))

(addtest (function-tests)
  test-compose
  (ensure-same 6 (apply (compose '*) '(2 3)))
  (ensure-same 9 (apply (compose (lambda (x)(* x x))
                                 (lambda (x)(/ x 4))) 
                        '(12)))
  (ensure-same 4 (apply (compose (lambda (ls)(apply '+ ls))
                                 (lambda (ls)(remove-if 'evenp ls))
                                 (lambda (ls)(remove nil ls)))
                        '((0 nil 1 nil 2 nil 3 nil)))))

(addtest (function-tests)
  test-conjoin
  (ensure (apply (conjoin 'numberp 'oddp)
                 '(3)))
  (ensure (not (apply (conjoin 'numberp 'oddp)
                      '(4))))
  (ensure (not (apply (conjoin 'numberp 'oddp)
                      '(foo)))))

(addtest (function-tests)
  test-disjoin
  (ensure (apply (disjoin 'numberp 'stringp)
                 '("foo")))
  (ensure (not (apply (disjoin 'numberp 'stringp)
                      '(:foo)))))

(addtest (function-tests)
  test-flip
  (ensure (apply (flip 'cl:>) '(2 3))))

(defmethod test-function-generic ((thing number))
  thing)

(addtest (function-tests)
  test-function?
  (ensure (not (function? 5)))
  (ensure (function? (function flip)))
  (ensure (function? (lambda (x) x)))
  (ensure (not (function? (function function?))))
  (ensure (not (function? (find-method #'test-function-generic '() (list (find-class 'number)))))))

(addtest (function-tests)
  test-functional?
  (ensure (not (functional? 5)))
  (ensure (functional? (function flip)))
  (ensure (functional? (lambda (x) x)))
  (ensure (functional? (function function?)))
  (ensure (functional? (find-method #'test-function-generic '() (list (find-class 'number))))))

(addtest (function-tests)
  test-generic-function?
  (ensure (not (generic-function? 5)))
  (ensure (not (generic-function? (function flip))))
  (ensure (not (generic-function? (lambda (x) x))))
  (ensure (generic-function? (function function?)))
  (ensure (not (generic-function? (find-method #'test-function-generic '() (list (find-class 'number)))))))

(addtest (function-tests)
  test-method?
  (ensure (not (method? 5)))
  (ensure (not (method? (function flip))))
  (ensure (not (method? (lambda (x) x))))
  (ensure (not (method? (function function?))))
  (ensure (method? (find-method #'test-function-generic '() (list (find-class 'number))))))

(addtest (function-tests)
  test-partial
  (ensure-same '(1 2 3) (mapcar (partial 'cl:+ 1) '(0 1 2)))
  (ensure-same '(1 2 3) (funcall (partial 'cl:map 'list) (partial 'cl:+ 1) '(0 1 2))))

(defun test-insert (ch string index)
  (concatenate 'string
               (subseq string 0 index)
               (string ch)
               (subseq string index)))

(addtest (function-tests)
  test-rotate-left
  (ensure-same "0123456" (funcall (rotate-left 'test-insert) "012356" 4 #\4)))

(addtest (function-tests)
  test-rotate-right
  (ensure-same "0123456" (funcall (rotate-right 'test-insert)  4 #\4 "012356")))

(addtest (function-tests)
  test-rpartial
  (ensure (funcall (rpartial 'cl:< 3) 1))
  (ensure-same '("aardvark" "Apple" "marmot" "Mop") 
               (funcall (rpartial 'cl:sort 'cl:string-lessp) '("marmot" "Apple" "aardvark" "Mop"))))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-function-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'function-tests)))

;;; (net.bardcode.folio.function.tests::run-function-tests)
