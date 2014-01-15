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
  (ensure (not (find 'a nil)))
  (ensure-same 'c (find 'c '(a b c d)))
  (ensure-same 'c (find 'c (vector 'a 'b 'c 'd)))
  (ensure-same #\c (find #\c "abcd"))
  (ensure-same 'c (find 'c (wb-seq 'a 'b 'c 'd))))

(addtest (sequence-tests)
  test-find-if
  (ensure (not (find-if 'oddp nil)))
  (ensure-same 1 (find-if 'oddp '(0 1 2 3)))
  (ensure-same 1 (find-if 'oddp (vector 0 1 2 3)))
  (ensure-same #\1 (find-if 'digit-char-p "ab1cd"))
  (ensure-same 1 (find-if 'oddp (wb-seq 0 1 2 3))))

(addtest (sequence-tests)
  test-find-if-not
  (ensure (not (find-if-not 'oddp nil)))
  (ensure-same 0 (find-if-not 'oddp '(0 1 2 3)))
  (ensure-same 0 (find-if-not 'oddp (vector 0 1 2 3)))
  (ensure-same #\a (find-if-not 'digit-char-p "11ab1cd"))
  (ensure-same 0 (find-if-not 'oddp (wb-seq 0 1 2 3))))

(addtest (sequence-tests)
  test-first
  (ensure (not (first nil)))
  (ensure-same 0 (first '(0 1 2 3)))
  (ensure-same 0 (first (vector 0 1 2 3)))
  (ensure-same #\0 (first "0123"))
  (ensure-same 0 (first (wb-seq 0 1 2 3))))

(addtest (sequence-tests)
  test-head
  (ensure (not (head nil)))
  (ensure-same 0 (head '(0 1 2 3)))
  (ensure-same 0 (head (vector 0 1 2 3)))
  (ensure-same #\0 (head "0123"))
  (ensure-same 0 (head (wb-seq 0 1 2 3))))

(addtest (sequence-tests)
  test-image
  (ensure (not (image '1+ nil)))
  (ensure-same '(1 2 3) (image '1+ '(0 1 2)))
  (ensure-same (vector 1 2 3) (image '1+ (vector 0 1 2)) :test 'equalp)
  (ensure-same (wb-seq 1 2 3) (image '1+ (wb-seq 0 1 2)) :test 'equalp))

(addtest (sequence-tests)
  test-indexes
  (ensure (not (indexes nil)))
  (ensure-same '(0 1 2) (indexes '(a b c)))
  (ensure-same '(0 1 2) (indexes (vector 'a 'b 'c)))
  (ensure-same '(0 1 2) (indexes (wb-seq 'a 'b 'c))))

(addtest (sequence-tests)
  test-interleave
  (ensure-same '(a x b x) (interleave '(a b) '(x x)))
  (ensure-same "a1b2" (interleave "ab" "12"))
  (ensure-same (wb-seq :a 1 :b 2 :c 3) (interleave (wb-seq :a :b :c)(wb-seq 1 2 3)) :test 'equalp))

(addtest (sequence-tests)
  test-interpose
  (ensure (not (interpose :x nil)))
  (ensure-same '(:a :x :b :x :c) (interpose :x '(:a :b :c)))
  (ensure-same (vector :a :x :b :x :c) (interpose :x (vector :a :b :c)) :test 'equalp)
  (ensure-same (wb-seq :a :x :b :x :c) (interpose :x (wb-seq :a :b :c)) :test 'equalp))

(addtest (sequence-tests)
  test-join
  (ensure (not (join nil (list nil nil))))
  (ensure-same '(:a :b :c :x :d :e :f) (join '(:x) '((:a :b :c)(:d :e :f))))
  (ensure-same "the quick brown fox jumped over the lazy dog"
               (join " " '("the quick brown fox" "jumped over" "the lazy dog")))
  (ensure-same (wb-seq :a :b :c :x :d :e :f)
               (join (wb-seq :x) (list (wb-seq :a :b :c)(wb-seq :d :e :f)))
               :test 'equalp))

(addtest (sequence-tests)
  test-last
  (ensure-error (last nil))
  (ensure-same 3 (last '(0 1 2 3)))
  (ensure-same 3 (last (vector 0 1 2 3)))
  (ensure-same #\3 (last "0123"))
  (ensure-same 3 (last (wb-seq 0 1 2 3))))

(addtest (sequence-tests)
  test-leave
  (ensure-error (leave 2 nil))
  (ensure-same '(2 3) (leave 2 '(0 1 2 3)))
  (ensure-same (vector 2 3) (leave 2 (vector 0 1 2 3)) :test 'equalp)
  (ensure-same "23" (leave 2 "0123"))
  (ensure-same (wb-seq 2 3) (leave 2 (wb-seq 0 1 2 3)) :test 'equalp))

(addtest (sequence-tests)
  test-length
  (ensure (zerop (length nil)))
  (ensure-same 3 (length (list 0 1 2)))
  (ensure-same 3 (length "abc"))
  (ensure-same 3 (length (wb-seq 0 1 2))))

(addtest (sequence-tests)
  test-mismatch
  (ensure (not (mismatch nil nil)))
  (ensure-same 0 (mismatch '(a b c) nil))
  (ensure-same 0 (mismatch nil '(a b c)))
  (ensure-same 2 (mismatch '(a b c d) '(a b d e)))
  (ensure-same 2 (mismatch "abcd" "abde"))
  (ensure-same 2 (mismatch (vector 'a 'b 'c 'd) (vector 'a 'b 'd 'e)))
  (ensure-same 2 (mismatch (wb-seq 'a 'b 'c 'd) (wb-seq 'a 'b 'd 'e)))
  (ensure-same 2 (mismatch (vector 'a 'b 'c 'd) (wb-seq 'a 'b 'd 'e))))

(addtest (sequence-tests)
  test-partition
  (multiple-value-bind (odds evens)(partition 'oddp '(0 1 2 3 4 5))
    (ensure-same '(1 3 5) odds)
    (ensure-same '(0 2 4) evens))
  (multiple-value-bind (odds evens)(partition 'oddp (vector 0 1 2 3 4 5))
    (ensure-same (vector 1 3 5) odds :test 'equalp)
    (ensure-same (vector 0 2 4) evens :test 'equalp))
  (multiple-value-bind (odds evens)(partition 'oddp (wb-seq 0 1 2 3 4 5))
    (ensure-same (wb-seq 1 3 5) odds :test 'equalp)
    (ensure-same (wb-seq 0 2 4) evens :test 'equalp)))

(addtest (sequence-tests)
  test-penult
  (ensure-error (penult nil))
  (ensure-same 2 (penult '(0 1 2 3)))
  (ensure-same 2 (penult (vector 0 1 2 3)))
  (ensure-same #\2 (penult "0123"))
  (ensure-same 2 (penult (wb-seq 0 1 2 3))))

(addtest (sequence-tests)
  test-position
  (ensure (not (position 'e nil)))
  (ensure-same 1 (position 'e '(b e e r)))
  (ensure-same 1 (position 'e (vector 'b 'e 'e 'r)))
  (ensure-same 1 (position #\e "beer"))
  (ensure-same 1 (position 'e (wb-seq 'b 'e 'e 'r))))

(addtest (sequence-tests)
  test-position-if
  (ensure (not (position-if 'oddp nil)))
  (ensure-same 1 (position-if 'oddp '(0 1 2 3 4)))
  (ensure-same 1 (position-if 'oddp (vector 0 1 2 3 4)))
  (ensure-same 1 (position-if 'digit-char-p "a1b2c3"))
  (ensure-same 1 (position-if 'oddp (wb-seq 0 1 2 3 4))))

(addtest (sequence-tests)
  test-position-if-not
  (ensure (not (position-if-not 'oddp nil)))
  (ensure-same 1 (position-if-not 'evenp '(0 1 2 3 4)))
  (ensure-same 1 (position-if-not 'evenp (vector 0 1 2 3 4)))
  (ensure-same 1 (position-if-not 'digit-char-p "1a2bcoddp3"))
  (ensure-same 1 (position-if-not 'evenp (wb-seq 0 1 2 3 4))))

(addtest (sequence-tests)
  test-prefix-match?
  (ensure (prefix-match? nil '(a b c)))
  (ensure (not (prefix-match? '(a b c) nil)))
  (ensure (prefix-match? '(a b c) '(a b c d e f g)))
  (ensure (prefix-match? (vector :a :b :c) (vector :a :b :c :d :e :f :g)))
  (ensure (prefix-match? (wb-seq :a :b :c) (wb-seq :a :b :c :d :e :f :g))))

(addtest (sequence-tests)
  test-range
  (ensure-same '(0 1 2 3) (range 0 4))
  (ensure-same '(0 2 4 6 8) (range 0 10 :by 2))
  (ensure-same '(10 8 6 4 2) (range 10 0 :by 2)))

(addtest (sequence-tests)
  test-reduce
  (ensure-same 10 (reduce '+ '(1 2 3 4) :initial-value 0))
  (ensure-same 10 (reduce '+ (vector 1 2 3 4) :initial-value 0))
  (ensure-same 10 (reduce '+ (wb-seq 1 2 3 4) :initial-value 0)))

(addtest (sequence-tests)
  test-remove
  (ensure-same nil (remove 'a nil))
  (ensure-same '(1 2 3 4) (remove 'and '(1 and 2 and 3 and 4)))
  (ensure-same (vector 1 2 3 4) (remove 'and (vector 1 'and 2 'and 3 'and 4)) :test 'equalp)
  (ensure-same (wb-seq 1 2 3 4) (remove 'and (wb-seq 1 'and 2 'and 3 'and 4)) :test 'equalp))

(addtest (sequence-tests)
  test-remove-if
  (ensure-same nil (remove-if 'oddp nil))
  (ensure-same '(0 2 4) (remove-if 'oddp '(0 1 2 3 4 5)))
  (ensure-same (vector 0 2 4) (remove-if 'oddp (vector 0 1 2 3 4 5)) :test 'equalp)
  (ensure-same (wb-seq 0 2 4) (remove-if 'oddp (wb-seq 0 1 2 3 4 5)) :test 'equalp))

(addtest (sequence-tests)
  test-remove-if-not
  (ensure-same nil (remove-if-not 'oddp nil))
  (ensure-same '(1 3 5) (remove-if-not 'oddp '(0 1 2 3 4 5)))
  (ensure-same (vector 1 3 5) (remove-if-not 'oddp (vector 0 1 2 3 4 5)) :test 'equalp)
  (ensure-same (wb-seq 1 3 5) (remove-if-not 'oddp (wb-seq 0 1 2 3 4 5)) :test 'equalp))

(addtest (sequence-tests)
  test-remove-duplicates
  (ensure-same '(a b c) (remove-duplicates '(a a b b c c)))
  (ensure-same "abc" (remove-duplicates "aabbcc"))
  (ensure-same (wb-seq 'a 'b 'c) (remove-duplicates (wb-seq 'a 'a 'b 'b 'c 'c)) :test 'equalp))

(addtest (sequence-tests)
  test-rest
  (ensure-same '(b c) (rest '(a b c)))
  (ensure-same "bc" (rest "abc"))
  (ensure-same (wb-seq 'b 'c) (rest (wb-seq 'a 'b 'c)) :test 'equalp))

(addtest (sequence-tests)
  test-reverse
  (ensure-same '(c b a) (reverse '(a b c)))
  (ensure-same "cba" (reverse "abc"))
  (ensure-same (wb-seq 'c 'b 'a) (reverse (wb-seq 'a 'b 'c)) :test 'equalp))

(addtest (sequence-tests)
  test-search
  (ensure-same 2 (search '(c d) '(a b c d e f)))
  (ensure-same 2 (search (vector 'c 'd) (vector 'a 'b 'c 'd 'e 'f)))
  (ensure-same 2 (search "cd" "abcdefg"))
  (ensure-same 2 (search (wb-seq 'c 'd) (wb-seq 'a 'b 'c 'd 'e 'f))))

(addtest (sequence-tests)
  test-second
  (ensure-same 1 (second '(0 1 2 3)))
  (ensure-same 1 (second (vector 0 1 2 3)))
  (ensure-same #\1 (second "0123"))
  (ensure-same 1 (second (wb-seq 0 1 2 3))))

(addtest (sequence-tests)
  test-select
  (ensure-same '(1 3 5)(select '(0 1 2 3 4 5 6) '(1 3 5)))
  (ensure-same '(1 3 5)(select (vector 0 1 2 3 4 5 6) '(1 3 5)))
  (ensure-same '(1 3 5)(select (vector 0 1 2 3 4 5 6) (vector 1 3 5)))
  (ensure-same '(1 3 5)(select (wb-seq 0 1 2 3 4 5 6) (vector 1 3 5)))
  (ensure-same '(1 3 5)(select '(0 1 2 3 4 5 6) (wb-seq 1 3 5))))

(addtest (sequence-tests)
  test-some?
  (ensure (some? 'oddp '(2 4 6 8 9)))
  (ensure (not (some? 'oddp '(2 4 6 8 10))))
  (ensure (some? 'digit-char-p "abcdef11"))
  (ensure (not (some? 'oddp (wb-seq 2 4 6 8 10)))))

(addtest (sequence-tests)
  test-sort
  (ensure-same '(1 2 3 4 5 6 7 8 9) (sort '(2 1 8 7 4 5 3 6 9) '<))
  (ensure-same (vector 1 2 3 4 5 6 7 8 9) (sort (vector 2 1 8 7 4 5 3 6 9) '<) :test 'equalp)
  (ensure-same "abcdefg" (sort "dbeagcf" 'char<))
  (ensure-same (wb-seq 1 2 3 4 5 6 7 8 9) (sort (wb-seq 2 1 8 7 4 5 3 6 9) '<) :test 'equalp))

(addtest (sequence-tests)
  test-split
  (ensure-same '((0 1)(3 4 5)) (split '(0 1 2 3 4 5) '(2)))
  (ensure-same (vector (vector 0 1)(vector 3 4 5)) (split (vector 0 1 2 3 4 5) (vector 2)) :test 'equalp)
  (ensure-same (wb-seq (wb-seq 0 1)(wb-seq 3 4 5)) (split (wb-seq 0 1 2 3 4 5) (wb-seq 2)) :test 'equalp))

(addtest (sequence-tests)
  test-subsequence
  (ensure-same '(b c) (subsequence '(a b c) 1))
  (ensure-same "bc" (subsequence "abcdefgh" 1 3))
  (ensure-same (wb-seq 'b 'c) (subsequence (wb-seq 'a 'b 'c 'd) 1 3) :test 'equalp))

(addtest (sequence-tests)
  test-substitute
  (ensure-same '(:a :b 0 :d :e 0 :g :h) (substitute 0 :x '(:a :b :x :d :e :x :g :h)))
  (ensure-same (wb-seq :a :b 0 :d :e 0 :g :h) (substitute 0 :x (wb-seq :a :b :x :d :e :x :g :h)) :test 'equalp))

(addtest (sequence-tests)
  test-substitute-if
  (ensure-same '(0 :_ 2 :_ 4) (substitute-if :_ 'oddp '(0 1 2 3 4)))
  (ensure-same (wb-seq 0 :_ 2 :_ 4) (substitute-if :_ 'oddp (wb-seq 0 1 2 3 4)) :test 'equalp))

(addtest (sequence-tests)
  test-substitute-if-not
  (ensure-same '(0 :_ 2 :_ 4) (substitute-if-not :_ 'evenp '(0 1 2 3 4)))
  (ensure-same (wb-seq 0 :_ 2 :_ 4) (substitute-if-not :_ 'evenp (wb-seq 0 1 2 3 4)) :test 'equalp))

(addtest (sequence-tests)
  test-suffix-match?
  (ensure (suffix-match? '(a b c) nil))
  (ensure (not (suffix-match? nil '(a b c))))
  (ensure (suffix-match? '(a b c d e f g) '(e f g)))
  (ensure (suffix-match? (vector :a :b :c :d :e :f :g) (vector :e :f :g)))
  (ensure (suffix-match? (wb-seq :a :b :c :d :e :f :g) (wb-seq :e :f :g))))

(addtest (sequence-tests)
  test-tail
  (ensure-same '(b c) (tail '(a b c)))
  (ensure-same "bc" (tail "abc"))
  (ensure-same (wb-seq 'b 'c) (tail (wb-seq 'a 'b 'c)) :test 'equalp))

(addtest (sequence-tests)
  test-tails
  (ensure-same 'c (first (third (tails '(a b c d e f)))))
  (ensure-same "y" (element (tails "Barney") 5)))

(addtest (sequence-tests)
  test-take
  (ensure-same '(1 2 3) (take 3 '(1 2 3 4 5 6 7 8 9)))
  (ensure-same "Foo" (take 3 "Foobar"))
  (ensure-same (wb-seq :a :b) (take 2 (wb-seq :a :b :c :d :e)) :test 'equalp))

(addtest (sequence-tests)
  test-take-by
  (ensure-same "bc" (second (take-by 2 1 "abcdefghij"))))

(addtest (sequence-tests)
  test-take-while
  (ensure-same '(0 0 0) (take-while 'zerop '(0 0 0 1 2 3)))
  (ensure-same "abcd" (take-while 'alpha-char-p "abcd11111efgh")))

(addtest (sequence-tests)
  test-unzip
  (multiple-value-bind (keys vals)(unzip (zip (vector :a :b :c)(wb-seq 1 2 3)))
    (ensure-same (vector :a :b :c) keys :test 'equalp)
    (ensure-same (vector 1 2 3) vals :test 'equalp)))

(addtest (sequence-tests)
  test-zip
  (ensure-same '((:a . 1)(:b . 2)(:c . 3)) (zip '(:a :b :c) '(1 2 3)))
  (ensure-same (vector '(:a . 1) '(:b . 2) '(:c . 3))(zip (vector :a :b :c)(wb-seq 1 2 3)) :test 'equalp))

;;; ---------------------------------------------------------------------
;;; run tests
;;; ---------------------------------------------------------------------

(defun run-sequence-tests ()
  (let ((*TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t))
    (lift:run-tests :suite 'sequence-tests)))

;;; (net.bardcode.folio.sequences.tests::run-sequence-tests)
;;; (lift:describe-test-result lift:*test-result* t)
