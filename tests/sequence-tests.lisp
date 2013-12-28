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

(defpackage :net.bardcode.folio.sequence.tests
  (:use :cl :net.bardcode.folio.common :net.bardcode.folio.sequences :lift)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :intersection :last :length :make
                          :merge :pair :position :position-if :reduce :remove :rest
                          :reverse :search :second :sequence :sort :union)
  (:import-from :net.bardcode.folio.common
                :seq :series))

(in-package :net.bardcode.folio.sequence.tests)

;;; ---------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------

(defmethod equivalent-series? ((s1 foundation-series)(s2 foundation-series))
  (series:collect-and (series:map-fn t (lambda (x y)(equal x y)) s1 s2)))

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite sequence-tests () ())

;;; add-first
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-add-first
  (ensure-same 'a (first (add-first 'a nil)))
  (ensure-same 'a (first (add-first 'a '(b c))))
  (ensure-same 'a (first (add-first 'a (vector 'b 'c))))
  (ensure-same 'a (first (add-first 'a "bc")))
  (ensure-same #\a (first (add-first #\a "bc")))
  (ensure-same 'a (first (add-first 'a (seq 'b 'c))))
  (ensure-same 'a (first (add-first 'a (scan (list 'b 'c))))))

;;; add-last
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-add-last
  (ensure-same 'z (last (add-last nil 'z)))
  (ensure-same 'z (last (add-last '(x y) 'z)))
  (ensure-same 'z (last (add-last (vector 'b 'c) 'z)))
  (ensure-same 'z (last (add-last "xy" 'z)))
  (ensure-same #\z (last (add-last "xy" #\z)))
  (ensure-same 'z (last (add-last (seq 'x 'y) 'z)))
  (ensure-same 'z (last (add-last (scan (list 'x 'y)) 'z))))

;;; any
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-any
  (ensure (null (any nil)))
  (ensure (member (any '(0 1 2 3 4))
                  '(0 1 2 3 4)))
  (ensure (member (any (seq 0 1 2 3 4))
                  '(0 1 2 3 4)))
  (ensure (member (any (fset:map (:a 0) (:b 1) (:c 2) (:d 3) (:e 4)))
                  '(0 1 2 3 4))))

;;; append
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-append
  ;; first arg null
  (ensure (null (append nil nil)))
  (ensure-same '(1 2 3) (append nil '(1 2 3)))
  (ensure-same (seq 1 2 3) (append nil (seq 1 2 3)) :test 'equalp)
  (ensure-same (scan '(1 2 3)) (append nil (scan '(1 2 3))) :test 'equivalent-series?)
  ;; first arg sequence 
  (ensure-same '(1 2 3) (append '(1 2 3) nil))
  (ensure-same '(1 2 3 4 5 6) (append '(1 2 3) '(4 5 6)))
  (ensure-same '(1 2 3 4 5 6 7 8 9) (append '(1 2 3) '(4 5 6) '(7 8 9)))
  (ensure-same '(1 2 3 4 5 6) (append '(1 2 3) (seq 4 5 6)))
  (ensure-same '(1 2 3 4 5 6) (as 'list (append '(1 2 3) (scan '(4 5 6)))))
  ;; first arg seq 
  (ensure-same (seq 1 2 3) (append (seq 1 2 3) nil) :test 'equalp)
  (ensure-same '(1 2 3 4 5 6) (append (seq 1 2 3) '(4 5 6)))
  (ensure-same '(1 2 3 4 5 6 7 8 9) (append (seq 1 2 3) '(4 5 6) '(7 8 9)))
  (ensure-same (seq 1 2 3 4 5 6) (append (seq 1 2 3) (seq 4 5 6)) :test 'equalp)
  (ensure-same (scan '(1 2 3 4 5 6)) (append (seq 1 2 3) (scan '(4 5 6))) :test 'equivalent-series?)
  ;; first arg series 
  (ensure-same (scan '(1 2 3)) (append (scan '(1 2 3)) nil) :test 'equivalent-series?)
  (ensure-same (scan '(1 2 3 4 5 6)) (append (scan '(1 2 3)) '(4 5 6)) :test 'equivalent-series?)
  (ensure-same (scan '(1 2 3 4 5 6 7 8 9))(append (scan '(1 2 3)) '(4 5 6) '(7 8 9)) :test 'equivalent-series?)
  (ensure-same (scan '(1 2 3 4 5 6)) (append (scan '(1 2 3)) (seq 4 5 6)) :test 'equivalent-series?)
  (ensure-same (scan '(1 2 3 4 5 6)) (append (scan '(1 2 3)) (scan '(4 5 6))) :test 'equivalent-series?))

;;; (net.bardcode.folio.sequence.tests::run-sequence-tests)

;;; as
;;; ---------------------------------------------------------------------
;;; list

(addtest (sequence-tests)
  test-as-list
  (ensure (null (as 'list nil)))
  (ensure-same '(1 2 3) (as 'list (vector 1 2 3)))
  (ensure-same '(1 2 3) (as 'list (seq 1 2 3)))
  (ensure-same '(1 2 3) (as 'list (scan '(1 2 3)))))

;;; vector

(addtest (sequence-tests)
  test-as-vector
  (ensure-same (vector) (as 'vector nil) :test 'equalp)
  (ensure-same (vector 1 2 3) (as 'vector (list 1 2 3)) :test 'equalp)
  (ensure-same (vector 1 2 3) (as 'vector (seq 1 2 3)) :test 'equalp)
  (ensure-same (vector 1 2 3) (as 'vector (scan '(1 2 3))) :test 'equalp))

;;; string

(addtest (sequence-tests)
  test-as-string
  (ensure-same "" (as 'string nil) :test 'equalp)
  (ensure-same "123" (as 'string (list #\1 #\2 #\3)) :test 'equalp)
  (ensure-same "123" (as 'string (seq #\1 #\2 #\3)) :test 'equalp)
  (ensure-same "123" (as 'string (scan '(#\1 #\2 #\3))) :test 'equalp))

;;; seq

(addtest (sequence-tests)
  test-as-seq
  (ensure-same (seq) (as 'seq nil) :test 'equalp)
  (ensure-same (seq 1 2 3) (as 'seq (list 1 2 3)) :test 'equalp)
  (ensure-same (seq 1 2 3) (as 'seq (vector 1 2 3)) :test 'equalp)
  (ensure-same (seq 1 2 3) (as 'seq (seq 1 2 3)) :test 'equalp)
  (ensure-same (seq 1 2 3) (as 'seq (scan '(1 2 3))) :test 'equalp))

;;; series

(addtest (sequence-tests)
  test-as-series
  (ensure-same (scan nil) (as 'series nil) :test 'equivalent-series?)
  (ensure-same (scan '(1 2 3)) (as 'series (list 1 2 3)) :test 'equivalent-series?)
  (ensure-same (scan '(1 2 3)) (as 'series (vector 1 2 3)) :test 'equivalent-series?)
  (ensure-same (scan '(1 2 3)) (as 'series (seq 1 2 3)) :test 'equivalent-series?)
  (ensure-same (scan '(1 2 3)) (as 'series (scan '(1 2 3))) :test 'equivalent-series?))

;;; sequence

(addtest (sequence-tests)
  test-as-sequence
  (ensure (null (as 'sequence nil)))
  (ensure-same '(1 2 3)(as 'sequence (list 1 2 3)))
  (ensure-same '(1 2 3)(as 'sequence (seq 1 2 3)))
  (ensure-same '(1 2 3)(as 'sequence (scan (list 1 2 3)))))

;;; by
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-by
  (ensure (null (by 1 nil)))
  (ensure-same '((1 2 3)(4 5 6)(7 8 9)) (by 3 '(1 2 3 4 5 6 7 8 9)))
  (ensure-same (vector (vector 1 2 3) (vector 4 5 6) (vector 7 8 9)) (by 3 (vector 1 2 3 4 5 6 7 8 9)) :test 'equalp)
  (ensure-same (seq (seq 1 2 3) (seq 4 5 6) (seq 7 8 9)) (by 3 (seq 1 2 3 4 5 6 7 8 9)) :test 'equalp))

;;; coalesce
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-coalesce
  (ensure (empty? (coalesce 'cl:+ nil)))
  (ensure-same (series 3 6 9) (coalesce 'cl:+ '(1 2 3) '(1 2 3) '(1 2 3)) :test 'equivalent-series?)
  (ensure-same (series 3 6 9) (coalesce 'cl:+ '(1 2 3) (vector 1 2 3) (seq 1 2 3)) :test 'equivalent-series?))

;;; drop
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-drop
  (ensure (null (drop 0 nil)))
  (ensure-error (drop 1 nil))
  (ensure-same '(1 2 3) (drop 0 '(1 2 3)))
  (ensure-same '(2 3) (drop 1 '(1 2 3)))
  (ensure-same '(2 3) (drop 2 '(0 1 2 3)))
  (ensure-same (vector 2 3) (drop 2 (vector 0 1 2 3)) :test 'equalp)
  (ensure-same (seq 2 3) (drop 2 (seq 0 1 2 3)) :test 'equalp)
  (ensure-same (scan '(2 3)) (drop 2 (scan '(0 1 2 3))) :test 'equivalent-series?))

;;; drop-while
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-drop-while
  (ensure (null (drop-while 'integerp nil)))
  (ensure-same '(4.1 5.2 6.3) (drop-while 'integerp '(1 2 3 4.1 5.2 6.3)))
  (ensure-same "ABC" (drop-while 'digit-char-p "123ABC"))
  (ensure-same (seq 4.1 5.2 6.3) (drop-while 'integerp (seq 1 2 3 4.1 5.2 6.3)) :test 'equalp)
  (ensure-same (scan '(4.1 5.2 6.3)) (drop-while 'integerp (scan '(1 2 3 4.1 5.2 6.3))) :test 'equivalent-series?))

;;; element
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-element
  (ensure-error (element nil 0))
  (ensure-same 'a (element '(z a b c) 1))
  (ensure-same 'c (element (seq 'a 'b 'c 'd) 2))
  (ensure-same 'c (element (scan '(a b c d)) 2)))

;;; empty?
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-empty?
  (ensure (empty? nil))
  (ensure (not (empty? '(1 2 3))))
  (ensure (empty? (vector)))
  (ensure (not (empty? (vector 1 2 3))))
  (ensure (empty? ""))
  (ensure (not (empty? "123")))
  (ensure (empty? (seq)))
  (ensure (not (empty? (seq 1 2 3))))
  (ensure (empty? (scan nil)))
  (ensure (not (empty? (scan '(1 2 3))))))

;;; every?
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-every?
  (ensure (every? 'oddp nil))
  (ensure (every? 'oddp '(1 3 5)))
  (ensure (not (every? 'oddp '(1 2 3 4 5))))
  (ensure (every? 'oddp (vector)))
  (ensure (every? 'oddp (vector 1 3 5)))
  (ensure (not (every? 'oddp (vector 1 2 3 4 5))))
  (ensure (every? 'oddp (seq)))
  (ensure (every? 'oddp (seq 1 3 5)))
  (ensure (not (every? 'oddp (seq 1 2 3 4 5)))))

;;; filter
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-filter
  (ensure (null (filter 'integerp '(a "b" :c))))
  (ensure-same '(1 2 3) (filter 'integerp '(a 1 "b" 2 :c 3)))
  (ensure-same (vector 1 2 3) (filter 'integerp (vector 'a 1 "b" 2 :c 3)) :test 'equalp)
  (ensure-same (seq 1 2 3) (filter 'integerp (seq 'a 1 "b" 2 :c 3)) :test 'equalp)
  (ensure-same (series 1 2 3) (filter 'integerp (series 'a 1 "b" 2 :c 3)) :test 'equivalent-series?))

;;; find
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-find
  (ensure (null (find 1 nil)))
  (ensure-same 1 (find 1 '(0 1 2 3)))
  (ensure-same 1 (find 1 (vector 0 1 2 3)))
  (ensure (not (find 10 (vector 0 1 2 3))))
  (ensure-same #\1 (find #\1 "01234"))
  (ensure-same 1 (find 1 (seq 0 1 2 3)))
  (ensure-same 1 (find 1 (series 0 1 2 3))))

;;; first
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-first
  (ensure (null (first nil)))
  (ensure-same 1 (first '(1 2 3)))
  (ensure-same 1 (first (vector 1 2 3)))
  (ensure-same #\1 (first "123"))
  (ensure-same 1 (first (seq 1 2 3)))
  (ensure-same 1 (first (series 1 2 3))))

;;; generate
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-generate
  (ensure-same #\a (series:next-in (generate "abcdef")))
  (ensure-same 'a (series:next-in (generate (vector 'a 'b 'c))))
  (ensure-same 'a (series:next-in (generate (seq 'a 'b 'c))))
  (ensure-same 'a (series:next-in (generate (series 'a 'b 'c)))))

;;; image
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-image
  (ensure (null (image 'integerp nil)))
  (ensure-same (list t nil t) (image 'integerp '(1 2.0 3)))
  (ensure-same (vector t nil t) (image 'integerp (vector 1 2.0 3)) :test 'equalp)
  (ensure-same (vector t t t) (image 'alpha-char-p "ABC") :test 'equalp)
  (ensure-same (seq t nil t) (image 'integerp (seq 1 2.0 3)) :test 'equalp)
  (ensure-same (series t nil t) (image 'integerp (series 1 2.0 3)) :test 'equivalent-series?))

;;; indexes
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-indexes
  (ensure (null (indexes nil)))
  (ensure-same (series 0 1 2 3) (indexes '(0 1 2 3)) :test 'equivalent-series?)
  (ensure-same (series 0 1 2 3) (indexes (vector 0 1 2 3)) :test 'equivalent-series?)
  (ensure-same (series 0 1 2 3) (indexes "0123") :test 'equivalent-series?)
  (ensure-same (series 0 1 2 3) (indexes (seq 0 1 2 3)) :test 'equivalent-series?)
  (ensure-same (series 0 1 2 3) (indexes (series 0 1 2 3)) :test 'equivalent-series?))

;;; interleave
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-interleave
  (ensure (null (interleave nil nil)))
  (ensure-same '(4 5 6) (interleave nil '(4 5 6)))
  (ensure-same (seq 4 5 6) (interleave nil (seq 4 5 6)) :test 'equalp)
  (ensure-same (series 4 5 6) (interleave nil (series 4 5 6)) :test 'equivalent-series?)
  (ensure-same '(4 5 6) (interleave '(4 5 6) nil))
  (ensure-same (seq 4 5 6) (interleave (seq 4 5 6) nil) :test 'equalp)
  (ensure-same (series 4 5 6) (interleave (series 4 5 6) nil) :test 'equivalent-series?)
  (ensure-same '(1 4 2 5 3 6) (interleave '(1 2 3) '(4 5 6)))
  (ensure-same (vector 1 4 2 5 3 6) (interleave (vector 1 2 3) (vector 4 5 6)) :test 'equalp)
  (ensure-same (seq 1 4 2 5 3 6) (interleave (seq 1 2 3) (seq 4 5 6)) :test 'equalp)
  (ensure-same (series 1 4 2 5 3 6) (interleave (series 1 2 3) (series 4 5 6)) :test 'equivalent-series?)
  (ensure-same (vector 1 4 2 5 3 6) (interleave (vector 1 2 3) (list 4 5 6)) :test 'equalp)
  (ensure-same (series 1 4 2 5 3 6) (interleave (series 1 2 3) (list 4 5 6)) :test 'equivalent-series?))

;;; interpose
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-interpose
  (ensure (null (interpose 1 nil)))
  (ensure-same '(1 0 2 0 3) (interpose 0 '(1 2 3)))
  (ensure-same (vector 1 0 2 0 3) (interpose 0 (vector 1 2 3)) :test 'equalp)
  (ensure-same "10203" (interpose #\0 "123"))
  (ensure-same (vector #\1 0 #\2 0 #\3) (interpose 0 "123") :test 'equalp)
  (ensure-same (seq 1 0 2 0 3) (interpose 0 (seq 1 2 3)) :test 'equalp)
  (ensure-same (series 1 0 2 0 3) (interpose 0 (series 1 2 3)) :test 'equivalent-series?))

;;; join
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-join
  (ensure (null (join 1 nil)))
  (ensure-same '(a b c - d e f - g h i) (join '- '((a b c)(d e f)(g h i))))
  (ensure-same "Apple,Banana,Cherry" (join #\, '("Apple" "Banana" "Cherry")))
  (ensure-same "Apple, Banana, Cherry" (join ", " '("Apple" "Banana" "Cherry")))
  (ensure-same '(a b c - d e f - g h i) (join '- `((a b c)(d e f) ,(seq 'g 'h 'i))))
  (ensure-same (scan '(a b c - d e f - g h i)) (join '- `((a b c)(d e f) ,(series 'g 'h 'i))) :test 'equivalent-series?))

;;; last
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-last
  (ensure (null (last nil)))
  (ensure-same 3 (last '(1 2 3)))
  (ensure-same 3 (last (vector 1 2 3)))
  (ensure-same #\3 (last "123"))
  (ensure-same 3 (last (seq 1 2 3)))
  (ensure-same 3 (last (series 1 2 3))))

;;; length
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-length
  (ensure-same 0 (length nil))
  (ensure-same 3 (length '(1 2 3)))
  (ensure-same 3 (length (vector 1 2 3)))
  (ensure-same 3 (length "123"))
  (ensure-same 3 (length (seq 1 2 3)))
  (ensure-same 3 (length (series 1 2 3))))

;;; make
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-make
  (ensure-same '(1 2 3) (make 'list :elements '(1 2 3)))
  (ensure-same '(1 2 3) (make 'sequence :elements '(1 2 3)))
  (ensure-same (vector 1 2 3) (make 'vector :elements '(1 2 3)) :test 'equalp)
  (ensure-same "123" (make 'string :elements '(#\1 #\2 #\3)) :test 'equalp)
  (ensure-same (seq 1 2 3) (make 'seq :elements '(1 2 3)) :test 'equalp)
  (ensure-same (series 1 2 3) (make 'series :elements '(1 2 3)) :test 'equivalent-series?))

;;; match-prefix?
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-match-prefix?
  (ensure (match-prefix? nil nil))
  (ensure (match-prefix? nil '(1 2 3 4)))
  (ensure (not (match-prefix? '(1 2 3 4) nil)))
  (ensure (match-prefix? '(1 2) '(1 2 3 4)))
  (ensure (not (match-prefix? '(2) '(1 2 3 4))))
  (ensure (match-prefix? (seq 1 2) (seq 1 2 3 4)))
  (ensure (match-prefix? (series 1 2) (series 1 2 3 4))))

;;; match-suffix?
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-match-suffix?
  (ensure (match-suffix? nil nil))
  (ensure (match-suffix? '(1 2 3 4) nil))
  (ensure (not (match-suffix? nil '(1 2 3 4))))
  (ensure (match-suffix? '(1 2 3 4) '(3 4)))
  (ensure (not (match-suffix? '(1 2 3 4) '(2))))
  (ensure (match-suffix? (seq 1 2 3 4) '(3 4)))
  (ensure (match-suffix? (series 1 2 3 4) '(3 4)))
  (ensure (match-suffix? '(1 2 3 4) (seq 3 4)))
  (ensure (match-suffix? (series 1 2 3 4) (seq 3 4)))
  (ensure (match-suffix? (list 1 2 3 4) (series 3 4)))
  (ensure (match-suffix? (seq 1 2 3 4) (series 3 4)))
  (ensure (match-suffix? (series 1 2 3 4) (series 3 4))))

;;; partition
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-partition
  (multiple-value-bind (odds evens)(partition '(0 1 2 3 4 5) 'oddp 'evenp)
    (ensure-same '(1 3 5) odds)
    (ensure-same '(0 2 4) evens))
  (multiple-value-bind (numbers strings symbols)(partition '(0 goose "apple" 1 "banana" hippo 2 "cherry" 3 4 ibex 5)
                                                           'numberp
                                                           'stringp
                                                           'symbolp)
    (ensure-same '(0 1 2 3 4 5) numbers)
    (ensure-same '("apple" "banana" "cherry") strings)
    (ensure-same '(goose hippo ibex) symbols))
  (multiple-value-bind (nums chars)(partition (seq 0 #\1 2 #\3 4 #\5) 'numberp 'characterp)
    (ensure-same (seq 0 2 4) nums :test 'equalp)
    (ensure-same (seq #\1 #\3 #\5) chars :test 'equalp)))

;;; penult
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-penult
  (ensure (null (penult nil)))
  (ensure-same 'b (penult '(a b c)))
  (ensure-same 'b (penult (vector 'a 'b 'c)))
  (ensure-same #\b (penult "abc"))
  (ensure-same 'b (penult (seq 'a 'b 'c)))
  (ensure-same 'b (penult (series 'a 'b 'c))))

;;; position
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-position
  (ensure (null (position 1 nil)))
  (ensure-same 2 (position 2 '(0 1 2 3 4)))
  (ensure-same 2 (position 2 (vector 0 1 2 3 4)))
  (ensure-same 2 (position #\2 "01234"))
  (ensure-same 2 (position 2 (seq 0 1 2 3 4)))
  (ensure-same 2 (position 2 (series 0 1 2 3 4)))
  (ensure-same 2 (position "Foo" (series "bar" "BAZ" "foo" "grault") :test 'equalp)))

;;; position-if
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-position-if
  (ensure (null (position-if 'stringp nil)))
  (ensure-same 2 (position-if 'stringp '(0 1 "2" 3 4)))
  (ensure-same 2 (position-if 'stringp (vector 0 1 "2" 3 4)))
  (ensure-same 2 (position-if 'stringp (seq 0 1 "2" 3 4)))
  (ensure-same 2 (position-if 'stringp (series 0 1 "2" 3 4))))

;;; range
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-range
  (ensure (null (range 0 0)))
  (ensure-same '(1 2 3) (range 1 4))
  (ensure-same '(2 4 6 8) (range 2 10 :by 2)))

;;; range-from
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-range-from
  (ensure-same (series 1 2 3) (take 3 (range-from 1)) :test 'equivalent-series?)
  (ensure-same (series 5 10 15) (take 3 (range-from 5 :by 5)) :test 'equivalent-series?))

;;; reduce
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-reduce
  (ensure-same 9 (reduce 'cl:+ '(2 3 4)))
  (ensure-same 9 (reduce 'cl:+ (vector 2 3 4)))
  (ensure-same 9 (reduce 'cl:+ (seq 2 3 4)))
  (ensure-same 9 (reduce 'cl:+ (series 2 3 4))))

;;; remove
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-remove
  (ensure (null (remove 1 nil)))
  (ensure-same '("carol" "ted" "alice") (remove "bob" '("bob" "carol" "bob" "ted" "bob" "alice") :test 'string=) :test 'equalp)
  (ensure-same (vector "carol" "ted" "alice")
               (remove "bob" (vector "bob" "carol" "bob" "ted" "bob" "alice") :test 'string=)
               :test 'equalp)
  (ensure-same (seq "carol" "ted" "alice")
               (remove "bob" (seq "bob" "carol" "bob" "ted" "bob" "alice") :test 'string=)
               :test 'equalp)
  (ensure-same (series "carol" "ted" "alice")
               (remove "bob" (series "bob" "carol" "bob" "ted" "bob" "alice") :test 'string=)
               :test 'equivalent-series?))

;;; repeat
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-repeat
  (ensure-same (series 1) (take 1 (repeat 1)) :test 'equivalent-series?)
  (ensure-same (series 1 1 1 1 1 1 1 1 1 1) (take 10 (repeat 1)) :test 'equivalent-series?))

;;; rest
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-rest
  (ensure (null (rest nil)))
  (ensure (null (rest (list 0))))
  (ensure-same 2 (first (rest '(1 2 3 4 5))))
  (ensure-same 2 (first (rest (vector 1 2 3 4 5))))
  (ensure-same #\2 (first (rest "123456")))
  (ensure-same 2 (first (rest (seq 1 2 3 4 5))))
  (ensure-same 2 (first (rest (series 1 2 3 4 5))))
  (ensure-same 2 (first (rest (range-from 1)))))

;;; reverse
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-reverse
  (ensure-same nil (reverse nil))
  (ensure-same '(3 2 1) (reverse '(1 2 3)))
  (ensure-same (vector 3 2 1)(reverse (vector 1 2 3)) :test 'equalp)
  (ensure-same "321" (reverse "123") :test 'equalp)
  (ensure-same (seq 3 2 1)(reverse (seq 1 2 3)) :test 'equalp)
  (ensure-same (series 3 2 1) (reverse (series 1 2 3)) :test 'equivalent-series?))

;;; scan
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-scan
  (ensure (series::foundation-series-p (scan nil)))
  (ensure (series::foundation-series-p (scan '(1 2 3))))
  (ensure (series::foundation-series-p (scan (vector 1 2 3))))
  (ensure (series::foundation-series-p (scan (seq 1 2 3))))
  (ensure (series::foundation-series-p (scan (series 1 2 3)))))

;;; scan-image
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-scan-image
  (ensure-same (series 4 9 16)
               (scan-image (lambda (x)(* x x))
                           '(2 3 4))
               :test 'equivalent-series?)
  (ensure-same (series 1 4 9 16)
               (take 4 (scan-image (lambda (x)(* x x))
                                   (range-from 1)))
               :test 'equivalent-series?))

;;; second
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-second
  (ensure (null (second nil)))
  (ensure-same 2 (second '(1 2 3)))
  (ensure-same 2 (second (vector 1 2 3)))
  (ensure-same #\2 (second "123"))
  (ensure-same 2 (second (seq 1 2 3)))
  (ensure-same 2 (second (series 1 2 3))))

;;; select
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-select
  (ensure-error (select nil (list 1 2 3)))
  (ensure-same '(b d f) (select '(a b c d e f) '(1 3 5)))
  (ensure-same (vector 'b 'd 'f) (select (vector 'a 'b 'c 'd 'e 'f) '(1 3 5)) :test 'equalp)
  (ensure-same (seq 'b 'd 'f) (select (seq 'a 'b 'c 'd 'e 'f) '(1 3 5)) :test 'equalp)
  (ensure-same (seq 'b 'd 'f) (select (seq 'a 'b 'c 'd 'e 'f) (seq 1 3 5)) :test 'equalp)
  (ensure-same (seq 'b 'd 'f) (select (seq 'a 'b 'c 'd 'e 'f) (series 1 3 5)) :test 'equalp)
  (ensure-same (series 'b 'd 'f) (select (series 'a 'b 'c 'd 'e 'f) (series 1 3 5)) :test 'equivalent-series?))

;;; shuffle
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-shuffle
  (let* ((ls (list 1 2 3 4 5 6 7 8))
         (ls* (shuffle ls)))
    (ensure (and (not (equalp ls ls*))
                 (every (lambda (x)(member x ls))
                        ls*))))
  (let* ((vec (vector 1 2 3 4 5 6 7 8))
         (vec* (shuffle vec)))
    (ensure (and (not (equalp vec vec*))
                 (every (lambda (x)(cl:some (lambda (e)(equalp x e)) vec))
                        vec*)))))

;;; some?
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-some?
  (ensure (some? 'integerp '(a b 3 d)))
  (ensure (not (some? 'integerp '(a b c d))))
  (ensure (some? 'integerp (vector :a :b 3 :d)))
  (ensure (some? 'alpha-char-p "123A56"))
  (ensure (not (some? 'alpha-char-p "123456")))
  (ensure (some? 'integerp (seq :a :b 3 :d)))
  (ensure (some? 'integerp (series :a :b 3 :d))))

;;; split
;;; ---------------------------------------------------------------------

(addtest (sequence-tests)
  test-split
  (ensure (null (split nil "foo")))
  (ensure-same '((a b c)(f g h i j)) (split '(a b c d e f g h i j) '(d e)))
  (ensure-same (vector "foo" "bar") (split "fooBAZbar" "BAZ") :test 'equalp)
  )

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-sequence-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'sequence-tests)))

;;; (net.bardcode.folio.sequence.tests::run-sequence-tests)
