;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequences.lisp
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       sequence tests
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package :cl-user)

(defpackage :net.bardcode.folio.sequences.tests
  (:use :cl :net.bardcode.folio.as :net.bardcode.folio.make :net.bardcode.folio.sequences :lift)
  (:shadowing-import-from :net.bardcode.folio.sequences
                          :acons :assoc :assoc-if :assoc-if-not :append
                          :count :count-if :count-if-not
                          :find :find-if :find-if-not :first 
                          :last :length
                          :mismatch
                          :position :position-if :position-if-not 
                          :reduce :remove :remove-duplicates :remove-if :remove-if-not :rest :reverse
                          :sequence :search :second :sort :substitute :substitute-if :substitute-if-not))

(in-package :net.bardcode.folio.sequences.tests)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite sequence-tests () ())

;;; ---------------------------------------------------------------------
;;; sequence tests
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-as
  (ensure-same '(1 2 3) (as 'cl:list (vector 1 2 3)))
  (ensure-same (vector 1 2 3) (as 'cl:vector '(1 2 3)) :test 'cl:equalp)
  (ensure-same "Foo" (as 'cl:string (as 'cl:list (as 'cl:vector "Foo")))))

(addtest (sequence-tests)
  test-acons
  (ensure-same '((:a . 1)(:b . 2)(:c . 3))
               (acons :a 1 '((:b . 2)(:c . 3))))
  (ensure-same (vector '(:a . 1) '(:b . 2) '(:c . 3))
               (acons :a 1 (vector '(:b . 2) '(:c . 3))) :test 'cl:equalp)
  (ensure-same (wb-seq '(:a . 1) '(:b . 2) '(:c . 3))
               (acons :a 1 (wb-seq '(:b . 2) '(:c . 3))) :test 'cl:equalp))

(addtest (sequence-tests)
  test-add-first
  (ensure-same '(:a) (add-first :a nil))
  (ensure-same '(:a :b :c) (add-first :a '(:b :c)))
  (ensure-same (vector 1 2 3 4)(add-first 1 (vector 2 3 4)) :test 'cl:equalp)
  (ensure-same (wb-seq 1 2 3 4)(add-first 1 (wb-seq 2 3 4)) :test 'cl:equalp))

(addtest (sequence-tests)
  test-add-last
  (ensure-same '(:z) (add-last nil :z))
  (ensure-same '(:x :y :z) (add-last '(:x :y) :z))
  (ensure-same (vector 1 2 3 4)(add-last (vector 1 2 3) 4) :test 'cl:equalp)
  (ensure-same (wb-seq 1 2 3 4)(add-last (wb-seq 1 2 3) 4) :test 'cl:equalp))

(addtest (sequence-tests)
  test-append
  (ensure-same '(:a :b :c) (append nil '(:a :b :c)))
  (ensure-same '(:a :b :c) (append '(:a :b :c) nil))
  (ensure-same '(:a :b :c :d :e :f) (append '(:a :b :c) '(:d :e :f)))
  (ensure-same '(:a :b :c :d :e :f) (append '(:a :b) '(:c :d) '(:e :f)))
  (ensure-same (vector :a :b :c :d :e :f) (append (vector :a :b) (vector :c :d) (vector :e :f)) :test 'cl:equalp)
  (ensure-same "abcdef" (append "ab" "cd" "ef")))

(defun %whitespace-char? (ch)
  (member ch (list #\Space #\Newline #\Tab #\Page #\Null #\Newline)))

(addtest (sequence-tests)
  test-apportion
  (multiple-value-bind (odds evens)(apportion '(0 1 2 3 4 5 6 7 8 9) 'oddp 'evenp)
    (ensure-same odds '(1 3 5 7 9))
    (ensure-same evens '(0 2 4 6 8)))
  (multiple-value-bind (digits alphas whites)(apportion " more than 1 or 2 characters" 'digit-char-p 'alpha-char-p '%whitespace-char?)
    (ensure-same "12" digits)
    (ensure-same "morethanorcharacters" alphas)
    (ensure-same "      " whites)))

(addtest (sequence-tests)
  test-assoc
  (ensure-same "baboon" (assoc #\b (vector "aardvark" "baboon" "coatimundi")))
  (ensure-same '(:a . 1) (assoc :a '((:a . 1)(:b . 2)(:c . 3))))
  (ensure-same '(1 2 3) (assoc 1 (wb-seq (vector :a :b :c) (list 1 2 3)(wb-seq 'a 'b 'c)))))

(addtest (sequence-tests)
  test-assoc-if
  (ensure-same "1 baboon" (assoc-if 'digit-char-p (vector "aardvark" "1 baboon" "coatimundi")))
  (ensure-same (vector 1 2 3 4) (assoc-if 'oddp (list (vector 0 1 2 3)(vector 1 2 3 4)(vector 2 3 4 5))) :test 'cl:equalp))

(addtest (sequence-tests)
  test-assoc-if-not
  (ensure-same "aardvark" (assoc-if-not 'digit-char-p (vector "aardvark" "1 baboon" "coatimundi")))
  (ensure-same (vector 0 1 2 3) (assoc-if-not 'oddp (list (vector 0 1 2 3)(vector 1 2 3 4)(vector 2 3 4 5))) :test 'cl:equalp))

(addtest (sequence-tests)
  test-by
  (ensure-same '(0 1) (first (by 2 '(0 1 2 3 4 5 6))))
  (ensure-same "cd" (second (by 2 "abcdefgh")))
  (ensure-same '(g h i) (as 'cl:list (last (by 3 (wb-seq 'a 'b 'c 'd 'e 'f 'g 'h 'i))))))

(addtest (sequence-tests)
  test-count
  (ensure-same 0 (count 'a nil))
  (ensure-same 1 (count 'a '(a b c d)))
  (ensure-same 3 (count #\a "aardvark"))
  (ensure-same 1 (count 'a (wb-seq 'a 'b 'c 'd))))

(addtest (sequence-tests)
  test-count-if
  (ensure-same 0 (count-if 'oddp nil))
  (ensure-same 3 (count-if 'oddp '(0 1 2 3 4 5)))
  (ensure-same 3 (count-if 'digit-char-p "1: money 2: show 3: get ready"))
  (ensure-same 2 (count-if 'symbolp (wb-seq "a" 'b "c" 'd))))

(addtest (sequence-tests)
  test-count-if-not
  (ensure-same 0 (count-if-not 'evenp nil))
  (ensure-same 3 (count-if-not 'evenp '(0 1 2 3 4 5)))
  (ensure-same 26 (count-if-not 'digit-char-p "1: money 2: show 3: get ready"))
  (ensure-same 2 (count-if-not 'symbolp (wb-seq "a" 'b "c" 'd))))

(addtest (sequence-tests)
  test-dispose
  (multiple-value-bind (odds evens)(dispose '(0 1 2 3) 'oddp 'evenp)
    (ensure-same '(nil t nil t) odds)
    (ensure-same '(t nil t nil) evens))
  (multiple-value-bind (odds evens)(dispose (vector 0 1 2 3) 'oddp 'evenp)
    (ensure-same (vector nil t nil t) odds :test 'cl:equalp)
    (ensure-same (vector t nil t nil) evens :test 'cl:equalp))
  (multiple-value-bind (odds evens)(dispose (wb-seq 0 1 2 3) 'oddp 'evenp)
    (ensure-same (wb-seq nil t nil t) odds :test 'cl:equalp)
    (ensure-same (wb-seq t nil t nil) evens :test 'cl:equalp)))

(addtest (sequence-tests)
  test-drop
  (ensure-same nil (drop 0 nil))
  (ensure-error (drop 1 nil))
  (ensure-same '(1 2 3) (drop 1 '(0 1 2 3)))
  (ensure-same "cdef" (drop 2 "abcdef"))
  (ensure-same (wb-seq 'd 'e 'f)(drop 3 (wb-seq 'a 'b 'c 'd 'e 'f)) :test 'cl:equalp))

(addtest (sequence-tests)
  test-drop-while
  (ensure-same nil (drop-while 'oddp nil))
  (ensure-same '(1 2 3) (drop-while 'evenp '(0 1 2 3)))
  (ensure-same "cdef" (drop 2 "abcdef"))
  (ensure-same (wb-seq 'd 'e 'f)(drop-while 'numberp (wb-seq 1 2 3 'd 'e 'f)) :test 'cl:equalp))

(addtest (sequence-tests)
  test-element
  (ensure-error (element nil 1))
  (ensure-same 'd (element '(a b c d e) 3))
  (ensure-same #\d (element "abcd" 3))
  (ensure-same 'd (element (wb-seq 'a 'b 'c 'd 'e) 3)))

(addtest (sequence-tests)
  test-empty?
  (ensure (empty? nil))
  (ensure (not (empty? '(1 2 3))))
  (ensure (empty? ""))
  (ensure (not (empty? "abcd")))
  (ensure (not (empty? (vector 1 2 3))))
  (ensure (not (empty? (wb-seq 1 2 3)))))

(addtest (sequence-tests)
  test-every?
  (ensure (every? 'oddp nil))
  (ensure (every? 'oddp '(1 3 5)))
  (ensure (every? 'digit-char-p ""))
  (ensure (every? 'digit-char-p "123"))
  (ensure (not (every? 'digit-char-p "123abc")))
  (ensure (every? 'oddp (wb-seq 1 3 5))))

(addtest (sequence-tests)
  test-filter
  (ensure-same nil (filter 'oddp nil))
  (ensure-same '(1 3 5) (filter 'oddp '(0 1 2 3 4 5)))
  (ensure-same (vector 1 3 5) (filter 'oddp (vector 0 1 2 3 4 5)) :test 'cl:equalp)
  (ensure-same (wb-seq 1 3 5) (filter 'oddp (wb-seq 0 1 2 3 4 5)) :test 'cl:equalp))

(addtest (sequence-tests)
  test-find
  )

(addtest (sequence-tests)
  test-find-if
  )

(addtest (sequence-tests)
  test-find-if-not
  )

(addtest (sequence-tests)
  test-first
  )

(addtest (sequence-tests)
  test-head
  )

(addtest (sequence-tests)
  test-image
  )

(addtest (sequence-tests)
  test-indexes
  )

(addtest (sequence-tests)
  test-interleave
  )

(addtest (sequence-tests)
  test-interpose
  )

(addtest (sequence-tests)
  test-join
  )

(addtest (sequence-tests)
  test-last
  )

(addtest (sequence-tests)
  test-leave
  )

(addtest (sequence-tests)
  test-length
  )

(addtest (sequence-tests)
  test-mismatch
  )

(addtest (sequence-tests)
  test-partition
  )

(addtest (sequence-tests)
  test-penult
  )

(addtest (sequence-tests)
  test-position
  )

(addtest (sequence-tests)
  test-position-if
  )

(addtest (sequence-tests)
  test-position-if-not
  )

(addtest (sequence-tests)
  test-prefix-match?
  )

(addtest (sequence-tests)
  test-range
  )

(addtest (sequence-tests)
  test-reduce
  )

(addtest (sequence-tests)
  test-remove
  )

(addtest (sequence-tests)
  test-remove-if
  )

(addtest (sequence-tests)
  test-remove-if-not
  )

(addtest (sequence-tests)
  test-remove-duplicates
  )

(addtest (sequence-tests)
  test-rest
  )

(addtest (sequence-tests)
  test-reverse
  )

(addtest (sequence-tests)
  test-search
  )

(addtest (sequence-tests)
  test-second
  )

(addtest (sequence-tests)
  test-select
  )

(addtest (sequence-tests)
  test-shuffle
  )

(addtest (sequence-tests)
  test-some?
  )

(addtest (sequence-tests)
  test-sort
  )

(addtest (sequence-tests)
  test-split
  )

(addtest (sequence-tests)
  test-subsequence
  )

(addtest (sequence-tests)
  test-substitute
  )

(addtest (sequence-tests)
  test-substitute-if
  )

(addtest (sequence-tests)
  test-substitute-if-not
  )

(addtest (sequence-tests)
  test-suffix-match?
  )

(addtest (sequence-tests)
  test-tail
  )

(addtest (sequence-tests)
  test-tails
  )

(addtest (sequence-tests)
  test-take
  )

(addtest (sequence-tests)
  test-take-by
  )

(addtest (sequence-tests)
  test-take-while
  )

(addtest (sequence-tests)
  test-unzip
  )

(addtest (sequence-tests)
  test-zip
  )

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-sequence-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'sequence-tests)))

;;; (net.bardcode.folio.sequences.tests::run-sequence-tests)
;;; (lift:describe-test-result lift:*test-result* t)
