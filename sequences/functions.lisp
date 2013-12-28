;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       functions that accept and produce
;;;;                sequences, series, generators, and streams
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; operations on sequences and sequence-like values. tables are treated
;;; as sequences of pairs. 
;;;
;;; functions that depend finite inputs are not defined on values such
;;; as series, which may be infinite in size. for example, LAST and
;;; LENGTH are not defined on series.
;;;
;;; tables are treated as sequences of pairs, except that functions
;;; that depend on a stable order are not defined on tables that do
;;; not define a stable order. for example, FIRST and ELEMENT are
;;; defined only on implementations of tables whose keys always appear
;;; in a stable order.

(in-package #:net.bardcode.folio.sequences)

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(defmethod combined-type ((s1 null)(s2 null))
  (declare (ignore s1 s2))
  'list)

(defmethod combined-type ((s1 null) s2)
  (declare (ignore s1 s2))
  'list)

(defmethod combined-type ((s1 cons) s2)
  (declare (ignore s1 s2))
  'list)

(defmethod combined-type ((s1 string) (s2 string))
  (declare (ignore s1 s2))
  'string)

(defmethod combined-type ((s1 vector) (s2 vector))
  (declare (ignore s1 s2))
  'vector)

(defmethod combined-type ((s1 vector) (s2 cons))
  (declare (ignore s1 s2))
  'vector)

(defmethod combined-type ((s1 vector) (s2 fset:seq))
  (declare (ignore s1 s2))
  'vector)

(defmethod combined-type ((s1 vector) (s2 foundation-series))
  (declare (ignore s1 s2))
  'vector)

(defmethod combined-type ((s1 string) s2)
  (declare (ignore s1 s2))
  'vector)

(defmethod combined-type ((s1 seq) (s2 seq))
  (declare (ignore s1 s2))
  'seq)

(defmethod combined-type ((s1 seq) s2)
  (declare (ignore s1 s2))
  'list)

(defmethod combined-type ((s1 foundation-series)(s2 foundation-series))
  (declare (ignore s1 s2))
  'foundation-series)

(defmethod combined-type ((s1 foundation-series) s2)
  (declare (ignore s1 s2))
  'list)

;;; ---------------------------------------------------------------------
;;; function add-first
;;; ---------------------------------------------------------------------
;;;
;;; (add-first x seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence that contains X prepended to the elements of
;;; SEQ

(defmethod add-first (x (s null))(cons x s))
(defmethod add-first (x (s list))(cons x s))
(defmethod add-first (x (s vector))(concatenate 'vector (vector x) s))
(defmethod add-first (x (s string))(concatenate 'vector (vector x) s))
(defmethod add-first ((x character) (s string))(concatenate 'string (string x) s))
(defmethod add-first (x (s seq))(fset:with-first s x))
(defmethod add-first (x (s foundation-series))(series:catenate (series x) s))

;;; ---------------------------------------------------------------------
;;; function add-last
;;; ---------------------------------------------------------------------
;;;
;;; (add-last seq x) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence that contains X appended after the elements of
;;; SEQ

(defmethod add-last ((s null) x)(cons x s))
(defmethod add-last ((s list) x)(append s (list x)))
(defmethod add-last ((s vector) x)(concatenate 'vector s (vector x)))
(defmethod add-last ((s string) x)(concatenate 'vector s (vector x)))
(defmethod add-last ((s string) (x character))(concatenate 'string s (string x)))
(defmethod add-last ((s seq) x)(fset:with-last s x))
(defmethod add-last ((s foundation-series) x)(series:catenate s (series x)))

;;; ---------------------------------------------------------------------
;;; function any
;;; ---------------------------------------------------------------------
;;;
;;; (any seq) => anything
;;; ---------------------------------------------------------------------
;;; returns an arbitrary element of seq. any chooses the element
;;; randomly

(defmethod any ((s null)) (declare (ignore s)) nil)
(defmethod any ((s cl:sequence))(elt s (random (length s))))
(defmethod any ((s seq))(fset:@ s (random (fset:size s))))
(defmethod any ((s fset:set))(elt (as 'list s)(random (fset:size s))))
(defmethod any ((s fset:map))(fset:@ s (any (keys s))))

;;; WARNING: nonterminating if s is unbounded
(defmethod any ((s foundation-series))
  (element s (random (length s))))


;;; ---------------------------------------------------------------------
;;; function append
;;; ---------------------------------------------------------------------
;;;
;;; (append &rest seqs) => seq'
;;; ---------------------------------------------------------------------
;;; shadows cl:find, providing an extensible generic version
;;; returns a new sequence. if SEQS is nil, then nil is returned. If
;;; seqs contains a single value then that value is returned. Otherwise,
;;; append returns (reduce 'binary-append seqs)
;;; if you want to extend append with cases for additional sequence types,
;;; add methods to binary-append

(defun append (&rest seqs)
  (if (null seqs)
      nil
      (if (null (cdr seqs))
          (cadr seqs)
          (reduce #'binary-append seqs))))

;;; ---------------------------------------------------------------------
;;; function binary-append
;;; ---------------------------------------------------------------------
;;;
;;; (binary-append seq1 seq2) => seq3
;;; ---------------------------------------------------------------------
;;; returns a sequence containing all the elements of SEQ1 followed by
;;; all the elements of SEQ2.

(defmethod binary-append ((seq1 null)(seq2 null))(declare (ignore seq1 seq2)) nil)
(defmethod binary-append ((seq1 null)(seq2 cl:sequence))(declare (ignore seq1)) seq2)
(defmethod binary-append ((seq1 null)(seq2 seq))(declare (ignore seq1)) seq2)
(defmethod binary-append ((seq1 null)(seq2 foundation-series))(declare (ignore seq1)) seq2)

(defmethod binary-append ((seq1 cl:sequence)(seq2 null))(declare (ignore seq2)) seq1)
(defmethod binary-append ((seq1 cl:sequence)(seq2 cl:sequence)) (concatenate (combined-type seq1 seq2) seq1 seq2))
(defmethod binary-append ((seq1 cl:sequence)(seq2 seq))
  (concatenate (combined-type seq1 seq2) seq1 (fset:convert (combined-type seq1 seq2) seq2)))
(defmethod binary-append ((seq1 cl:sequence)(seq2 foundation-series))
  (append seq1 (series:collect seq2)))

(defmethod binary-append ((seq1 seq)(seq2 null))(declare (ignore seq2)) seq1)
(defmethod binary-append ((seq1 seq)(seq2 cl:sequence)) (as (combined-type seq1 seq2)(fset:concat seq1 seq2)))
(defmethod binary-append ((seq1 seq)(seq2 seq))(fset:concat seq1 seq2))
(defmethod binary-append ((seq1 seq)(seq2 foundation-series))
  (series:catenate (series:scan (as 'list seq1)) seq2))

(defmethod binary-append ((seq1 foundation-series)(seq2 null))(declare (ignore seq2)) seq1)
(defmethod binary-append ((seq1 foundation-series)(seq2 cl:sequence))(series:catenate seq1 (scan seq2)))
(defmethod binary-append ((seq1 foundation-series)(seq2 seq))(as 'series (series:catenate seq1 (scan (as 'list seq2)))))
(defmethod binary-append ((seq1 foundation-series)(seq2 foundation-series))
  (series:catenate seq1 seq2))

;;; function as
;;;
;;; (as type x) => an instance of type
;;; ---------------------------------------------------------------------

(defmethod as ((type (eql 'cl:list)) (val null) &key &allow-other-keys)
  val)

(defmethod as ((type (eql 'cl:list)) (val cl:sequence) &key &allow-other-keys)
  (coerce val 'cl:list))

(defmethod as ((type (eql 'cl:list)) (val seq) &key &allow-other-keys)
  (fset:convert 'cl:list val))

(defmethod as ((type (eql 'cl:list)) (val fset:set) &key &allow-other-keys)
  (fset:convert 'cl:list val))

(defmethod as ((type (eql 'cl:list)) (val foundation-series) &key &allow-other-keys)
  (series:collect 'cl:list val))


(defmethod as ((type (eql 'cl:vector)) (val null) &key &allow-other-keys)
  (vector))

(defmethod as ((type (eql 'cl:vector))(val cl:sequence) &key &allow-other-keys)
  (coerce val 'cl:vector))

(defmethod as ((type (eql 'cl:vector)) (val seq) &key &allow-other-keys)
  (fset:convert 'cl:vector val))

(defmethod as ((type (eql 'cl:vector)) (val foundation-series) &key &allow-other-keys)
  (series:collect 'cl:vector val))


(defmethod as ((type (eql 'cl:string)) (val null) &key &allow-other-keys)
  "")

(defmethod as ((type (eql 'cl:string))(val cl:sequence) &key &allow-other-keys)
  (coerce val 'cl:string))

(defmethod as ((type (eql 'cl:string)) (val seq) &key &allow-other-keys)
  (fset:convert 'cl:string val))

(defmethod as ((type (eql 'cl:string)) (val foundation-series) &key &allow-other-keys)
  (series:collect 'cl:string val))


(defmethod as ((type (eql 'seq)) (val null) &key &allow-other-keys)
  (fset:seq))

(defmethod as ((type (eql 'seq))(val cl:sequence) &key &allow-other-keys)
  (fset:convert 'seq val))

(defmethod as ((type (eql 'seq))(val seq) &key &allow-other-keys)
  val)

(defmethod as ((type (eql 'seq))(val foundation-series) &key &allow-other-keys)
  (fset:convert 'seq (series:collect 'cl:list val)))


(defmethod as ((type (eql 'series::foundation-series)) (val null) &key &allow-other-keys)
  (series:scan nil))

(defmethod as ((type (eql 'series::foundation-series))(val cl:sequence) &key &allow-other-keys)
  (series:scan val))

(defmethod as ((type (eql 'series::foundation-series))(val seq) &key &allow-other-keys)
  (series:scan (fset:convert 'cl:list val)))

(defmethod as ((type (eql 'series::foundation-series))(val foundation-series) &key &allow-other-keys)
  val)


(defmethod as ((type (eql 'sequence)) (val null) &key &allow-other-keys)
  val)

(defmethod as ((type (eql 'sequence))(val cl:sequence) &key &allow-other-keys)
  val)

(defmethod as ((type (eql 'sequence))(val seq) &key &allow-other-keys)
  (fset:convert 'cl:list val))

(defmethod as ((type (eql 'sequence))(val foundation-series) &key &allow-other-keys)
  (series:collect 'list val))


(defmethod as ((type (eql 'series)) (val null) &key &allow-other-keys)
  (series:scan nil))

(defmethod as ((type (eql 'series))(val cl:sequence) &key &allow-other-keys)
  (series:scan val))

(defmethod as ((type (eql 'series))(val seq) &key &allow-other-keys)
  (series:scan (fset:convert 'cl:list val)))

(defmethod as ((type (eql 'series))(val foundation-series) &key &allow-other-keys)
  val)

;;; ---------------------------------------------------------------------
;;; function by
;;; ---------------------------------------------------------------------
;;;
;;; (by n seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence of sequences constructed by taking the elements of
;;; SEQ N at a time

(defmethod by ((n integer)(s list))
  (assert (> n 0)() "count argument to BY must be greater than zero")
  (if (null s)
      nil
      (let ((tl (nthcdr n s)))
        (if tl
            (cons (take n s)
                  (by n tl))
            (list s)))))

(defmethod by ((n integer)(vec vector))
  (assert (> n 0)() "count argument to BY must be greater than zero")
  (if (<= (length vec) n)
      (vector vec)
      (let* ((s (scan vec))
             (starts (series:scan-range :by n :below (length vec)))
             (tails (series:map-fn t (lambda (x)(series:subseries s x)) starts))
             (chunks (series:map-fn t (lambda (y)(series:subseries y 0 n)) tails)))
        (map 'vector 
             (lambda (chunk)(series:collect 'vector chunk))
             (series:collect 'vector chunks)))))

(defmethod by ((n integer)(str string))
  (assert (> n 0)() "count argument to BY must be greater than zero")
  (if (<= (length str) n)
      (vector str)
      (let* ((s (scan str))
             (starts (series:scan-range :by n :below (length str)))
             (tails (series:map-fn t (lambda (x)(series:subseries s x)) starts))
             (chunks (series:map-fn t (lambda (y)(series:subseries y 0 n)) tails)))
        (map 'vector 
             (lambda (chunk)(series:collect 'string chunk))
             (series:collect 'vector chunks)))))

(defmethod by ((n integer)(s seq))
  (assert (> n 0)() "count argument to BY must be greater than zero")
  (if (<= (length s) n)
      (seq s)
      (let* ((s* (scan (fset:convert 'vector s)))
             (starts (series:scan-range :by n :below (length s)))
             (tails (series:map-fn t (lambda (x)(series:subseries s* x)) starts))
             (chunks (series:map-fn t (lambda (y)(series:subseries y 0 n)) tails)))
        (fset:convert 'fset:seq
                      (map 'vector 
                           (lambda (chunk)(fset:convert 'fset:seq (series:collect 'vector chunk)))
                           (series:collect 'vector chunks))))))

(defmethod by ((n integer)(s foundation-series))
  (assert (> n 0)() "count argument to BY must be greater than zero")
  (let* ((starts (series:scan-range :from 0 :by n))
         (ends (series:subseries starts 1))
         (chunks (series:map-fn t (lambda (x y)(series:subseries s x y)) starts ends)))
    chunks))

;;; ---------------------------------------------------------------------
;;; function coalesce
;;; ---------------------------------------------------------------------
;;;
;;; (coalesce fn &rest seqs) => seq'
;;; ---------------------------------------------------------------------
;;; coalesce combines N sequences into one. SEQs is a list of N sequences.
;;; FN is a function that accepts N inputs and returns one output. Applying
;;; coalesce yields a single sequence, series, generator, or stream
;;; that is the sequence of values produced by applying FN to SEQs.
;;; coalesce processes all the sequences in parallel, stopping when the
;;; end of one of them is reached. If the sequences are of different lengths
;;; then the remaining tails of longer sequences are ignored.

(defun coalesce (fn &rest seqs)
  (let ((seqs (mapcar 'scan seqs)))
    (apply 'series:map-fn t fn seqs)))

;;; ---------------------------------------------------------------------
;;; function drop
;;; ---------------------------------------------------------------------
;;;
;;; (drop n seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence containing the elements of SEQ after the first
;;; N elements have been removed

(defmethod drop ((n (eql 0)) (seq null)) seq)
(defmethod drop ((n (eql 0)) (seq cl:sequence)) seq)
(defmethod drop ((n (eql 0)) (seq seq)) seq)
(defmethod drop ((n (eql 0)) (seq foundation-series)) seq)

(defmethod drop ((n integer) (seq null))
  (error "index out of range: ~A" n))

(defmethod drop ((n integer) (seq cl:sequence))
  (subseq seq n))

(defmethod drop ((n integer) (seq seq))
  (fset:subseq seq n))

(defmethod drop ((n integer) (seq foundation-series))
  (series:subseries seq n))

;;; ---------------------------------------------------------------------
;;; function drop-while
;;; ---------------------------------------------------------------------
;;;
;;; (drop-while test seq) => seq'
;;; ---------------------------------------------------------------------

(defmethod drop-while (fn (seq cl:sequence))
  (let ((pos (cl:position-if-not fn seq)))
    (if pos
        (drop pos seq)
        nil)))

(defmethod drop-while (fn (seq seq))
  (let ((pos (fset:position-if-not fn seq)))
    (if pos
        (drop pos seq)
        nil)))

(defmethod drop-while (fn (seq foundation-series))
  (let* ((tests (series:map-fn t (cl:complement fn) seq))
         (flags (series:latch tests :post t)))
    (series:choose flags seq)))

;;; ---------------------------------------------------------------------
;;; function element
;;; ---------------------------------------------------------------------
;;;
;;; (element seq n) => 
;;; ---------------------------------------------------------------------
;;; returns the element of SEQ at index N

(defmethod element ((s null) (n integer))
  (error "index out of range: ~A" n))

(defmethod element ((s cl:sequence) (n integer))
  (cl:elt s n))

(defmethod element ((s seq) (n integer))
  (fset:@ s n))

(defmethod element ((s foundation-series) (n integer))
  (series:collect-nth n s))

;;; ---------------------------------------------------------------------
;;; function empty?
;;; ---------------------------------------------------------------------
;;;
;;; (empty? seq) => a boolean
;;; ---------------------------------------------------------------------
;;; returns true if SEQ contains no elements, and false otherwise

(defmethod empty? ((s null))
  (declare (ignore s))
  t)

(defmethod empty? ((s cons))
  (declare (ignore s))
  nil)

(defmethod empty? ((s cl:sequence))
  (= 0 (cl:length s)))

(defmethod empty? ((s seq))
  (= 0 (fset:size s)))

(defmethod empty? ((s foundation-series))
  (null (first (indexes s))))

;;; ---------------------------------------------------------------------
;;; function every?
;;; ---------------------------------------------------------------------
;;;
;;; (every? test seq) => a boolean
;;; ---------------------------------------------------------------------
;;; returns true if SEQ contains no elements, and false otherwise

(defmethod every? (fn (s null))
  (declare (ignore fn s))
  t)

(defmethod every? (fn (s cl:sequence))
  (cl:every fn s))

(defmethod every? (fn (s seq))
  (fset::every fn s))

;;; WARNING: nonterminating if s is unbounded
(defmethod every? (fn (s foundation-series))
  (series:collect-and (series:map-fn t fn s)))

;;; ---------------------------------------------------------------------
;;; function filter
;;; ---------------------------------------------------------------------
;;;
;;; (filter test seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns those elements of SEQ for which TEST returns true

(defmethod filter (fn (s null))
  (declare (ignore fn s))
  nil)

(defmethod filter (fn (s cl:sequence))
  (cl:remove-if-not fn s))

(defmethod filter (fn (s seq))
  (fset::remove-if-not fn s))

(defmethod filter (fn (s foundation-series))
  (series:choose (series:map-fn t fn s) s))

;;; ---------------------------------------------------------------------
;;; function find
;;; ---------------------------------------------------------------------
;;;
;;; (find item seq &key from-end test test-not start end key) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:find, providing an extensible generic version

(defmethod find (item (s cl:sequence) &key from-end (test 'eql) test-not (start 0) end (key 'identity) &allow-other-keys)
  (cl:find item s :from-end from-end :test test :test-not test-not
           :start start :end end :key key))

(defmethod find (item (s seq) &key (test 'eql) (key 'identity) (start 0) end &allow-other-keys)
  (fset::find item (drop start s) :key key :test test))

(defmethod find (item (s foundation-series) &key (test 'eql) (key 'identity) (start 0) end &allow-other-keys)
  (series:collect-first (series:choose (series:map-fn t (lambda (x) (funcall test x item)) s)
                                       (drop start s))))

;;; ---------------------------------------------------------------------
;;; function first
;;; ---------------------------------------------------------------------
;;;
;;; (first seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the first element of SEQ

(defmethod first ((s null))
  nil)

(defmethod first ((s cl:sequence))
  (cl:elt s 0))

(defmethod first ((s seq))
  (fset::@ s 0))

(defmethod first ((s foundation-series))
  (series:collect-first s))

;;; ---------------------------------------------------------------------
;;; function generate
;;; ---------------------------------------------------------------------
;;;
;;; (generate seq) => generator
;;; ---------------------------------------------------------------------
;;; returns a generator that returns one element at a time from the
;;; input SEQ

(defmethod generate ((s cl:sequence))
  (series:generator (series:scan s)))

(defmethod generate ((s fset:seq))
  (series:generator (scan (fset:convert 'cl:vector s))))

(defmethod generate ((s foundation-series))
  (series:generator s))

;;; ---------------------------------------------------------------------
;;; function image
;;; ---------------------------------------------------------------------
;;;
;;; (image fn seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence containing the values produced by applying
;;; FN to each element of SEQ

(defmethod image  (fn (s null)) (declare (ignore s)) nil)
(defmethod image  (fn (s cl:sequence))(cl:map 'list fn s))
(defmethod image  (fn (s cl:vector))(cl:map 'vector fn s))
(defmethod image  (fn (s seq))(fset:image fn s))
(defmethod image  (fn (s foundation-series))(series:map-fn t fn s))

;;; ---------------------------------------------------------------------
;;; function indexes
;;; ---------------------------------------------------------------------
;;;
;;; (indexes seq1) => seq2
;;; ---------------------------------------------------------------------
;;; returns a sequence of indexes to elements of SEQ1 

(defmethod indexes ((s null))
  (declare (ignore s))
  nil)

(defmethod indexes ((s cl:sequence))
  (indexes (scan s)))

(defmethod indexes ((s seq))
  (indexes (scan (fset:convert 'cl:vector s))))

(defmethod indexes ((s foundation-series))
  (series:choose (series:positions (series:map-fn 'boolean (constantly t) (scan s)))))

;;; ---------------------------------------------------------------------
;;; function interleave
;;; ---------------------------------------------------------------------
;;;
;;; (interleave seq1 seq2) => seq3
;;; ---------------------------------------------------------------------
;;; returns a new sequence SEQ3 that interleaves the elements from
;;; SEQ1 and SEQ2. the first element of SEQ3 is the first element of SEQ1;
;;; the second element of SEQ3 is the first element of SEQ2; and 
;;; the elements of SEQ1 and SEQ2 continue to alternate. If SEQ1 and SEQ2
;;; are of unequal length then the elements of the longer past the 
;;; length of the shorter are appended after the last element of the
;;; shorter.

(defmethod interleave ((s1 null)(s2 null))
  (declare (ignore s1 s2))
  nil)

(defmethod interleave ((s1 null)(s2 cl:sequence))
  (declare (ignore s1 s2))
  s2)

(defmethod interleave ((s1 null)(s2 seq))
  (declare (ignore s1 s2))
  s2)

(defmethod interleave ((s1 null)(s2 foundation-series))
  (declare (ignore s1 s2))
  s2)


(defmethod interleave ((s1 cl:sequence)(s2 null))
  (declare (ignore s1 s2))
  s1)


(defmethod interleave ((s1 cl:sequence)(s2 cl:sequence))
  (let* ((sr1 (series:scan s1))
         (sr2 (series:scan s2))
         (sr3 (interleave sr1 sr2))
         (out-type (combined-type s1 s2)))
    (series:collect out-type sr3)))

(defmethod interleave ((s1 cl:sequence)(s2 seq))
  (interleave s1 (fset:convert 'cl:list s2)))

(defmethod interleave ((s1 cl:sequence)(s2 foundation-series))
  (series:collect (combined-type s1 s2) 
    (interleave (series:scan s1) s2)))


(defmethod interleave ((s1 seq)(s2 null))
  (declare (ignore s1 s2))
  s1)

(defmethod interleave ((s1 seq)(s2 cl:sequence))
  (fset:convert 'seq (interleave (fset:convert 'cl:list s1) s2)))

(defmethod interleave ((s1 seq)(s2 seq))
  (fset:convert 'seq (interleave (fset:convert 'cl:list s1)
                                 (fset:convert 'cl:list s2))))

(defmethod interleave ((s1 seq)(s2 foundation-series))
  (fset:convert 'seq (interleave (fset:convert 'cl:list s1) s2)))

(defmethod interleave ((s1 foundation-series)(s2 null))
  (declare (ignore s1 s2))
  s1)

(defmethod interleave ((s1 foundation-series)(s2 cl:sequence))
  (interleave s1 (series:scan s2)))

(defmethod interleave ((s1 foundation-series)(s2 seq))
  (interleave s1 (fset:convert 'cl:list s2)))

(defmethod interleave ((s1 foundation-series)(s2 foundation-series))
  (let ((toggle t))
    (series:mingle s1 s2 
                   (lambda (x y) 
                     (setf toggle (not toggle))
                     toggle))))

;;; ---------------------------------------------------------------------
;;; function interpose
;;; ---------------------------------------------------------------------
;;;
;;; (interpose cupola seq1) => seq2
;;; ---------------------------------------------------------------------
;;; returns a new sequence SEQ2 that contains the elements of SEQ1, but
;;; with CUPOLA inserted between them

(defmethod interpose (x (s null))
  (declare (ignore x s))
  nil)

(defmethod interpose (x (s cons))
  (if (null (cdr s))
      (list (car s))
      (cons (car s)
            (cons x
                  (interpose x (cdr s))))))

(defmethod interpose (x (s cl:vector))
  (let* ((outlen (1- (* 2 (cl:length s))))
         (outvec (make-array outlen :initial-element x)))
    (loop 
       for i from 0 below outlen
       for j from 0 below outlen by 2
       do (setf (elt outvec j) (elt s i)))
    outvec))

(defmethod interpose ((x cl:character) (s cl:string))
  (coerce (call-next-method) 'string))

(defmethod interpose (x (s seq))
  (interleave s (make-array (1- (fset:size s)) :initial-element x)))

(defmethod interpose (x (s foundation-series))
  (let* ((indexes (scan (indexes s)))
         (xs (series:choose (series:subseries indexes 1)(repeat x))))
    (interleave s xs)))

;;; ---------------------------------------------------------------------
;;; function join
;;; ---------------------------------------------------------------------
;;;
;;; (join cupola seqs) => seq
;;; ---------------------------------------------------------------------
;;; joins SEQS in the manner of binary-join, below. to add support for joining 
;;; new sequence types, add methods to binary-join

(defmethod join (x (s null))
  (declare (ignore x s))
  nil)

(defmethod join (x (s cl:sequence))
  (cl:reduce (lambda (a b)(binary-join x a b)) s))

(defmethod join (x (s seq))
  (fset:reduce (lambda (a b)(binary-join x a b)) s))

(defmethod join (x (s foundation-series))
  (series:collect-fn t
                     (lambda () (series:collect-first s))
                     (lambda (a b) (binary-join x a b))
                     (series:subseries s 1)))

;;; ---------------------------------------------------------------------
;;; function binary-join
;;; ---------------------------------------------------------------------
;;;
;;; (join cupola seq1 seq2) => seq3
;;; ---------------------------------------------------------------------
;;; concatenates SEQ1 and SEQ2 to form the new sequence SEQ3, with CUPOLA
;;; inserted between the elements of SEQ1 and SEQ2

(defmethod binary-join (x (s1 cl:sequence)(s2 cl:sequence))
  (let* ((out-type (combined-type s1 s2))
         (cupola (coerce (list x) out-type)))
    (concatenate out-type s1 cupola s2)))

(defmethod binary-join ((x character) (s1 cl:sequence)(s2 cl:sequence))
  (let* ((out-type (combined-type s1 s2))
         (cupola (string x)))
    (concatenate out-type s1 cupola s2)))

(defmethod binary-join ((cupola string) (s1 cl:string)(s2 cl:string))
  (concatenate 'string s1 cupola s2))

(defmethod binary-join (x (s1 cl:sequence)(s2 seq))
  (let* ((out-type (combined-type s1 s2))
         (cupola (coerce (list x) out-type)))
    (concatenate out-type s1 cupola (fset:convert out-type s2))))

(defmethod binary-join (x (s1 cl:sequence)(s2 foundation-series))
  (binary-join x (series:scan s1) s2))


(defmethod binary-join (x (s1 seq)(s2 cl:sequence))
  (let* ((out-type (combined-type s1 s2))
         (cupola (fset:convert out-type (list x))))
    (fset:concat out-type s1 cupola (fset:convert out-type s2))))

(defmethod binary-join (x (s1 seq)(s2 seq))
  (let ((cupola (fset:convert 'seq (list x))))
    (fset:concat out-type s1 cupola s2)))

(defmethod binary-join (x (s1 seq)(s2 foundation-series))
  (binary-join x (series:scan (fset:convert 'cl:list s1)) s2))


(defmethod binary-join (x (s1 foundation-series)(s2 cl:sequence))
  (binary-join x s1 (series:scan s2)))

(defmethod binary-join (x (s1 foundation-series)(s2 seq))
  (binary-join x s1 (series:scan (fset:convert 'cl:list s2))))

(defmethod binary-join (x (s1 foundation-series)(s2 foundation-series))
  (let ((cupola (series:scan (list x))))
    (series:catenate s1 cupola s2)))

;;; ---------------------------------------------------------------------
;;; function last
;;; ---------------------------------------------------------------------
;;;
;;; (last seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the last element of SEQ

(defmethod last ((s null)) (declare (ignore s)) nil)
(defmethod last ((s cl:cons)) (cl:first (cl:last s)))
(defmethod last ((s cl:sequence))(cl:elt s (1- (cl:length s))))
(defmethod last ((s seq))(fset:last s))
;;; WARNING: nonterminating if s is unbounded
(defmethod last ((s foundation-series))(series:collect-last s))

;;; ---------------------------------------------------------------------
;;; function length
;;; ---------------------------------------------------------------------
;;;
;;; (length seq) => an integer
;;; ---------------------------------------------------------------------
;;; returns a count of the elements in SEQ

(defmethod length ((s null)) (declare (ignore s)) 0)
(defmethod length ((s cl:sequence))(cl:length s))
(defmethod length ((s seq))(fset:size s))
;;; WARNING: nonterminating if s is unbounded
(defmethod length ((s foundation-series))(series:collect-length s))

;;; ---------------------------------------------------------------------
;;; function make
;;; ---------------------------------------------------------------------
;;;
;;; (make 'cl:list &rest args) => args
;;; ---------------------------------------------------------------------
;;; create a list

(defmethod make ((type (eql 'cl:list)) &key elements &allow-other-keys)
  elements)

(defmethod make ((type (eql 'cl:vector)) &key elements &allow-other-keys)
  (coerce elements 'cl:vector))

(defmethod make ((type (eql 'cl:string)) &key elements &allow-other-keys)
  (coerce elements 'cl:string))

(defmethod make ((type (eql 'seq)) &key elements &allow-other-keys)
  (fset:convert 'fset:seq elements))

(defmethod make ((type (eql 'foundation-series)) &key elements &allow-other-keys)
  (series:scan elements))

(defmethod make ((type (eql 'series)) &key elements &allow-other-keys)
  (series:scan elements))

(defmethod make ((type (eql 'sequence)) &key elements &allow-other-keys)
  elements)


;;; ---------------------------------------------------------------------
;;; function match-prefix?
;;; ---------------------------------------------------------------------
;;;
;;; (match-prefix? pref seq &key (test 'equal)) => boolean
;;; ---------------------------------------------------------------------
;;; returns true if the elements of PREF match element-wise the first
;;; several elements of SEQ, using TEST as the matching criterion;
;;; otherwise, returns false. The number of elements tested is equal
;;; to the count of elements in PREF. PREF and SEQ need not be of the
;;; same type.

(defmethod match-prefix? ((pref null)(seq null) &key test start) t)
(defmethod match-prefix? ((pref null) seq &key test start) t)
(defmethod match-prefix? (pref (seq null) &key test start) nil)

(defmethod match-prefix? ((pref cl:list) (sequence cl:list) &key (test 'equal)(start 0))
  (let ((sequence (nthcdr start sequence)))
    (block searching
      (loop for p on pref
         and s on sequence
         do (if (null (cdr s))
                (if (cdr p)
                    ;; ran out of sequence before we ran out of prefix
                    (return-from searching nil)
                    ;; ran out of both, so they must match
                    (return-from searching t))
                (if (null (cdr p))
                    ;; ran out of prefix, so they match so far
                    (return-from searching (funcall test (car p)(car s)))
                    (unless (funcall test (car p)(car s))
                      (return-from searching nil))))))))

(defmethod match-prefix? ((pref cl:sequence) (sequence cl:sequence) &key (test 'equal)(start 0))
  (let ((plen (length pref))
        (slen (length sequence)))
    (if (< slen (+ start plen))
        nil
        (block searching
          (loop for i from 0 below plen 
             do (unless (funcall test (elt pref i)(elt sequence (+ start i)))
                  (return-from searching nil)))
          (return-from searching t)))))

(defmethod match-prefix? ((pref fset:seq) (sequence fset:seq) &key (test 'equal)(start 0))
  (let ((plen (length pref))
        (slen (length sequence)))
    (if (< slen (+ start plen))
        nil
        (block searching
          (loop for i from 0 below plen 
             do (unless (funcall test (fset:@ pref i)(fset:@ sequence (+ start i)))
                  (return-from searching nil)))
          (return-from searching t)))))

(defmethod match-prefix? ((pref foundation-series) (sequence foundation-series) &key (test 'equal)(start 0))
  (let* ((sequence (series:subseries sequence start))
         (len (series:collect-length pref))
         (matches (series:subseries (series:map-fn 'boolean (lambda (p s)(funcall test p s)) pref sequence)
                                    0 len)))
    (series:collect-and matches)))

;;; ---------------------------------------------------------------------
;;; function match-suffix?
;;; ---------------------------------------------------------------------
;;;
;;; (match-suffix? seq suff &key (test 'equal)) => boolean
;;; ---------------------------------------------------------------------
;;; returns true if the elements of SUFF match element-wise the last
;;; several elements of SEQ, using TEST as the matching criterion;
;;; otherwise, returns false. The number of elements tested is equal
;;; to the count of elements in SUFF. SUFF and SEQ need not be of the
;;; same type.

(defmethod match-suffix? ((seq null)(suff null) &key test) t)
(defmethod match-suffix? (seq (suff null) &key test) t)
(defmethod match-suffix? ((seq null) suff &key test) nil)

(defmethod match-suffix? ((seq cl:sequence)(suff cl:sequence) &key (test 'equal))
  (and (<= (cl:length suff)(cl:length seq))
       (equalp (subseq seq (- (length seq)
                              (length suff)))
               suff)))

(defmethod match-suffix? ((seq seq)(suff cl:sequence) &key (test 'equal))
  (match-suffix? (as 'sequence seq) suff :test test))

(defmethod match-suffix? ((seq cl:sequence)(suff seq) &key (test 'equal))
  (match-suffix? seq (as 'list suff) :test test))

(defmethod match-suffix? ((seq seq)(suff seq) &key (test 'equal))
  (and (<= (fset:size suff)(fset:size seq))
       (fset::every test
                    (drop (- (length seq)
                             (length suff))
                          seq)
                    suff)))


;;; WARNING: following are nonterminating if the series are unbounded

(defmethod match-suffix? ((seq cl:sequence)(suff foundation-series) &key (test 'equal))
  (match-suffix? (scan (as 'list seq)) suff :test test))

(defmethod match-suffix? ((seq seq)(suff foundation-series) &key (test 'equal))
  (match-suffix? (scan (as 'list seq)) suff))

(defmethod match-suffix? ((seq foundation-series)(suff foundation-series) &key (test 'equal))
  (match-suffix? (as 'list seq)
                 (as 'list suff)
                 :test test))

(defmethod match-suffix? ((seq foundation-series)(suff cl:sequence) &key (test 'equal))
  (match-suffix? (as 'sequence seq) suff :test test))

(defmethod match-suffix? ((seq foundation-series)(suff seq) &key (test 'equal))
  (match-suffix? seq (scan (as 'list suff)) :test test))


;;; ---------------------------------------------------------------------
;;; function partition
;;; ---------------------------------------------------------------------
;;;
;;; (partition seq &rest fn1 fn2 fn3...) => seq1 seq2 seq3...
;;; ---------------------------------------------------------------------
;;; returns a number of sequences equal to the number of FUNCTIONS.
;;; the elements of SEQ1 are those elements of seq for which FN1
;;; returns true; the elements of SEQ2 are those elements of seq for
;;; which FN2 returns true; and so on

(defmethod partition ((seq null) &rest fns)
  (declare (ignore seq fns))
  nil)

(defmethod partition ((seq cl:sequence) &rest fns)
  (apply #'values (cl:map 'cl:list
                          (lambda (fn)(cl:remove-if-not fn seq))
                          fns)))

(defmethod partition ((seq seq) &rest fns)
  (apply #'values (cl:map 'cl:list
                          (lambda (fn)(fset:remove-if-not fn seq))
                          fns)))

(defmethod partition ((seq foundation-series) &rest fns)
  (apply #'values (cl:map 'cl:list
                          (lambda (fn)(series:choose (series:map-fn t fn seq)
                                                     seq))
                          fns)))

;;; ---------------------------------------------------------------------
;;; function penult
;;; ---------------------------------------------------------------------
;;;
;;; (penult seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the last-but-one element of seq

(defmethod penult ((s null)) (declare (ignore s)) nil)

(defmethod penult ((s cl:cons))
  (if (null (cdr s))
      nil
      (if (null (cddr s))
          (car s)
          (penult (cdr s)))))

(defmethod penult ((s cl:sequence))(cl:elt s (- (cl:length s) 2)))
(defmethod penult ((s seq))(fset:@ s (- (fset:size s) 2)))

;;; WARNING: nonterminating if s is unbounded
(defmethod penult ((s foundation-series))(series:collect-nth (- (series:collect-length s) 2) s))

;;; ---------------------------------------------------------------------
;;; function position
;;; ---------------------------------------------------------------------
;;;
;;; (position item seq &key from-end test test-not start end key) => integer | nil
;;; ---------------------------------------------------------------------
;;; returns the index of ITEM in SEQ, or nil if it's not found

(defmethod position (item (seq null) &key (test 'eql) (start 0) end (key 'cl:identity) &allow-other-keys)
  (declare (ignore item seq test test-not start end key))
  nil)

(defmethod position (item (seq cl:sequence) &key (test 'eql) (start 0) end (key 'cl:identity) &allow-other-keys)
  (cl:position item seq :test test :start start :end end :key key))

(defmethod position (item (seq seq) &key (test 'eql) (start 0) end (key 'cl:identity) &allow-other-keys)
  (fset:position item seq :test test :start start :end end :key key))

;;; WARNING: nonterminating if seq is unbounded
(defmethod position (item (seq foundation-series) &key (test 'eql) (start 0) end (key 'cl:identity) &allow-other-keys)
  (let* ((test (lambda (x)(funcall test (funcall key item)(funcall key x)))))
    (if end
        (let* ((s (series:subseries seq start end))
               (indexes (series:subseries (scan (indexes seq)) start end)))
          (series:collect-first (series:choose (series:map-fn t test s) indexes)))
        (let* ((s (series:subseries seq start))
               (indexes (series:subseries (scan (indexes seq)) start)))
          (series:collect-first (series:choose (series:map-fn t test s) indexes))))))

;;; ---------------------------------------------------------------------
;;; function position-if
;;; ---------------------------------------------------------------------
;;;
;;; (position-if seq &rest fn1 fn2 fn3...) => seq1 seq2 seq3...
;;; ---------------------------------------------------------------------
;;; returns a number of sequences equal to the number of FUNCTIONS.
;;; the elements of SEQ1 are produced by applying FN1 to each element of
;;; SEQ; the elements of SEQ2 are produced by applying FN2 to each 
;;; element of SEQ; and so on

(defmethod position-if (test (seq null) &key (start 0) end (key 'cl:identity) &allow-other-keys)
  (declare (ignore test seq test start end key))
  nil)

(defmethod position-if (test (seq cl:sequence) &key (start 0) end (key 'cl:identity) &allow-other-keys)
  (cl:position-if test seq :start start :end end :key key))

(defmethod position-if (test (seq seq) &key (start 0) end (key 'cl:identity) &allow-other-keys)
  (fset:position-if test seq :start start :end end :key key))

;;; WARNING: nonterminating if seq is unbounded
(defmethod position-if (test (seq foundation-series) &key (start 0) end (key 'cl:identity) &allow-other-keys)
  (let* ((test (lambda (x)(funcall test (funcall key x)))))
    (if end
        (let* ((s (series:subseries seq start end))
               (indexes (series:subseries (scan (indexes seq)) start end)))
          (series:collect-first (series:choose (series:map-fn t test s) indexes)))
        (let* ((s (series:subseries seq start))
               (indexes (series:subseries (scan (indexes seq)) start)))
          (series:collect-first (series:choose (series:map-fn t test s) indexes))))))

;;; function range
;;;
;;; (range start end &key (by 1)) -> integer sequence
;;; ---------------------------------------------------------------------
;;; returns a list of integers starting with START and ending with
;;; (end - 1). Each succeeding integer differs from the previous one by BY.

(defun range (start end &key (by 1))
  (series:collect 'list (series:scan-range :from start :by by :below end)))

;;; function range-from
;;;
;;; (range-from start &key (by 1)) -> integer series
;;; ---------------------------------------------------------------------
;;; returns a series of integers starting with START and continuing
;;; forever. Each succeeding integer differs from the previous one by BY.

(defun range-from (n &key (by 1))
  (series:scan-range :from n :by by))

;;; function reduce
;;;
;;; (reduce fn &rest args) => 
;;; ---------------------------------------------------------------------
;;; applies FN to the first and second elements of ARGS, then applies it
;;; to the result and the third element of ARGS, and so on until the
;;; last element of ARGS is consumed. returns the last value produced.

(defmethod reduce (fn (seq null))
  (declare (ignore fn seq))
  nil)

(defmethod reduce (fn (seq cl:sequence))
  (cl:reduce fn seq))

(defmethod reduce (fn (seq seq))
  (fset:reduce fn seq))

;;; WARNING: nonterminating if s is unbounded
(defmethod reduce (fn (seq foundation-series))
  (reduce fn (series:collect 'list seq)))


;;; function remove
;;;
;;; (remove item seq &key test start end key) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence that contains the elements of SEQ, but with 
;;; ITEM removed

(defmethod remove (item (s null) &key (test 'eql) (start 0) end (key 'cl:identity) &allow-other-keys)
  (declare (ignore item s))
  nil)

(defmethod remove (item (s cl:sequence) &key (test 'eql) (start 0) end (key 'cl:identity)  &allow-other-keys)
  (cl:remove item s :test test :start start :end end :key key))

(defmethod remove (item (s seq) &key (test 'eql) (key 'cl:identity) &allow-other-keys)
  (fset:remove item s :test test :key key))

(defmethod remove (item (seq foundation-series) &key (test 'eql) (start 0) end (key 'cl:identity) &allow-other-keys)
  (let* ((test (lambda (x)(funcall test (funcall key item)(funcall key x)))))
    (if end
        (let* ((pre (series:subseries seq 0 start))
               (body (series:subseries seq start end))
               (post (series:subseries seq end))
               (tests (series:map-fn 'boolean (complement test) body))
               (result (series:choose tests body)))
          (series:catenate pre result post))
        (let* ((pre (series:subseries seq 0 start))
               (body (series:subseries seq start))
               (tests (series:map-fn 'boolean (complement test) body))
               (result (series:choose tests body)))
          (series:catenate pre result)))))

;;; function repeat
;;;
;;; (repeat val) => cycling series
;;; ---------------------------------------------------------------------
;;; returns an infinitely-repeating sequence of VAL.

(defun repeat (s)
  (series:scan-fn 't 
                  (lambda () s)
                  (lambda (i) s)))

;;; function rest
;;;
;;; (rest seq) => anything
;;; ---------------------------------------------------------------------
;;; returns all but the firt element of SEQ

(defmethod rest ((s null))
  (declare (ignore s))
  nil)

(defmethod rest ((s cl:sequence))
  (cl:subseq s 1))

(defmethod rest ((s seq))
  (fset::less-first s))

(defmethod rest ((s foundation-series))
  (series:subseries s 1))

;;; function reverse
;;;
;;; (reverse SEQ) => seq'
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ in reverse order

(defmethod reverse ((s null))
  (declare (ignore s))
  nil)

(defmethod reverse ((s cl:sequence))
  (cl:reverse s))

(defmethod reverse ((s seq))
  (fset::reverse s))

;;; WARNING: nonterminating if s is unbounded
(defmethod reverse ((s foundation-series))
  (series:scan (reverse (series:collect 'list s))))

;;; function scan
;;;
;;; (scan seq) => a series
;;; ---------------------------------------------------------------------
;;; returns a series equivalent to SEQ

(defmethod scan ((s null)) (series:scan s))
(defmethod scan ((s cons)) (series:scan s))
(defmethod scan ((s vector)) (series:scan s))
(defmethod scan ((s seq)) (series:scan (fset:convert 'vector s)))
(defmethod scan ((s foundation-series)) s)

;;; function scan-image
;;;
;;; (scan-image fn seq) => a series
;;; ---------------------------------------------------------------------
;;; returns a series equivalent to (map FN SEQ)

(defmethod scan-image (fn (s null)) (declare (ignore fn s)) nil)
(defmethod scan-image (fn (s cl:sequence)) (scan-image fn (scan s)))
(defmethod scan-image (fn (s seq)) (scan-image fn (scan s)))
(defmethod scan-image (fn (s foundation-series)) (series:map-fn t fn s))

;;; function search
;;;
;;; (search subsequence sequence &key (start 0)(test 'equalp)) => integer
;;; ---------------------------------------------------------------------
;;; returns the position, if any, in SEQUENCE where SUBSEQUENCE appears

(defmethod search ((subsequence null) (sequence cl:sequence) &key (start 0) (test 'equalp))
  start)

(defmethod search ((subsequence null) (sequence seq) &key (start 0) (test 'equalp))
  start)

(defmethod search ((subsequence null) (sequence foundation-series) &key (start 0) (test 'equalp))
  start)

(defmethod search ((subsequence cl:sequence) (sequence cl:sequence) &key (start 0) (test 'equalp))
  (cl:search subsequence sequence :start2 start :test test))

(defmethod search ((subsequence fset:seq) (sequence fset:seq) &key (start 0) (test 'equalp))
  (block searching
    (let ((sublen (length subsequence))
          (seqlen (length sequence))
          (pos start))
      (loop
         (if (< seqlen (+ pos sublen))
             (return-from searching nil)
             (if (match-prefix? subsequence sequence :start pos :test test)
                 (return-from searching pos)
                 (incf pos)))))))

(defmethod search ((subsequence foundation-series) (sequence foundation-series) &key (start 0) (test 'equalp))
  )

;;; function second
;;;
;;; (second seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the second element of SEQ

(defmethod second ((s null))
  nil)

(defmethod second ((s cl:sequence))
  (cl:elt s 1))

(defmethod second ((s seq))
  (fset::@ s 1))

(defmethod second ((s foundation-series))
  (series:collect-nth 1 s))

;;; ---------------------------------------------------------------------
;;; function select
;;; ---------------------------------------------------------------------
;;;
;;; (select seq1 indexes) => seq2
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ1 at the indexes given by the sequence
;;; INDEXES

(defmethod select ((s null) indexes)
  (error "indexes out of range: ~A" indexes))

(defmethod select ((s cl:sequence) (indexes cl:sequence))
  (cl:map (combined-type s s)
          (lambda (i)(cl:elt s i))
          indexes))

(defmethod select ((s cl:sequence) (indexes seq))
  (fset:convert (combined-type s s)
                (fset:image (lambda (i)(cl:elt s i))
                            indexes)))

(defmethod select ((s cl:sequence) (indexes foundation-series))
  (series:collect (combined-type s s)
                  (series:map-fn t (lambda (i)(cl:elt s i))
                                 indexes)))


(defmethod select ((s seq) (indexes cl:sequence))
  (fset:convert 'seq
                (map 'cl:vector
                     (lambda (i)(fset:@ s i))
                     indexes)))

(defmethod select ((s seq) (indexes seq))
  (fset:image (lambda (i)(fset:@ s i))
              indexes))

(defmethod select ((s seq) (indexes foundation-series))
  (fset:convert 'seq
                (series:collect 'cl:vector
                                (series:map-fn t (lambda (i)(fset:@ s i))
                                               indexes))))


(defmethod select ((s foundation-series) (indexes cl:sequence))
  (series:choose (series:mask (series:scan indexes)) s))

(defmethod select ((s foundation-series) (indexes seq))
  (series:choose (series:mask (series:scan (fset:convert 'cl:vector indexes))) s))

(defmethod select ((s foundation-series) (indexes foundation-series))
  (series:choose (series:mask indexes) s))

;;; ---------------------------------------------------------------------
;;; function seq
;;; ---------------------------------------------------------------------
;;;
;;; (seq a b c ...) => seq
;;; ---------------------------------------------------------------------
;;; returns a simple fset:seq containing the elements a b c ...
;;; imported from fset

;;; ---------------------------------------------------------------------
;;; function series
;;; ---------------------------------------------------------------------
;;;
;;; (series a b c ...) => series
;;; ---------------------------------------------------------------------
;;; returns a simple series:series containing the elements a b c ... 
;;; as if from series:scan

(defun series (&rest args)
  (series:scan args))

;;; function shuffle
;;;
;;; (shuffle seq1) => seq2
;;; ---------------------------------------------------------------------
;;; returns a new sequence with the same elements as SEQ1, but
;;; in random order

(defmethod shuffle (s)
  (declare (ignore s))
  nil)

(defmethod shuffle ((s cl:sequence))
  (cl:sort (cl:copy-seq s) (lambda (x y)(zerop (random 2)))))

(defmethod shuffle ((s seq))
  (fset:sort s (lambda (x y)(zerop (random 2)))))

(defmethod shuffle ((s foundation-series))
  (shuffle (series:collect 'cl:list s)))

;;; function some?
;;;
;;; (some? test seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the first element of SEQ for which TEST returns true, or
;;; nil otherwise

(defmethod some? (test (s null)) (declare (ignore fn s)) nil)
(defmethod some? (test (s cl:sequence)) (cl:some test s))

(defmethod some? (test (s seq)) 
  (let ((pos (fset:position-if test s)))
    (if pos
        (fset:@ s pos)
        nil)))

(defmethod some? (test (s foundation-series))
  (series:collect-first
   (series:choose 
    (series:map-fn t test s)
    s)))


;;; function split
;;;
;;; (split seq1 subseq) => seq2
;;; ---------------------------------------------------------------------
;;; returns a sequence of sequences. the output sequences are proper
;;; subsequences of SEQ1, obtained by splitting SEQ1 at occurrences
;;; of SUBSEQ. SUBSEQ does not appear in the output sequences. TEST,
;;; whose default value is equal, is used to match occurrences of
;;; SUBSEQ.
;;; The structure of the output value depends on the types of the
;;; inputs. The container is of the same or compatiable type as
;;; SEQ1. The subesquences within the result sequence are of the same
;;; or compatible types as SUBSEQ. Thus, for example,
;;;  (split "foo" nil)
;;; returns #((#\f)(#\o)(#\o))
;;; the result is a vector because SEQ1 is a string. the individual
;;; elements are not characters, so the result cannot be a string, so
;;; instead it is the next more general type of which string is a 
;;; subtype. The individual elements of the result are lists because
;;; nil is a list.

;;; null, any
(defmethod split ((seq null) subseq &key test)
  (declare (ignore seq subseq test))
  nil)

;;; cons, null
(defmethod split ((seq cl:cons) (subseq null) &key test)
  (declare (ignore subseq test))
  (mapcar 'list seq))

;;; vector, null
(defmethod split ((seq cl:vector) (subseq null) &key test)
  (declare (ignore subseq test))
  (map 'cl:vector 'cl:list seq))

;;; seq, null
(defmethod split ((seq fset:seq) (subseq null) &key test)
  (declare (ignore subseq test))
  (fset:image 'list seq))

;;; series, null
(defmethod split ((seq foundation-series) (subseq null) &key test)
  (declare (ignore subseq test))
  (series:map-fn 'list 'list seq))

;;; cons, cons
(defmethod split ((seq cl:cons) (subseq cl:cons) &key (test 'equal))
  (let ((pos (cl:search subseq seq :test test)))
    (if pos
        (let ((head (subseq seq 0 pos))
              (tail (subseq (subseq seq pos)
                            (length subseq))))
          (cons head
                (split tail subseq :test test)))
        (list seq))))

;;; vector, vector
(defmethod %bite ((seq cl:vector) (subseq cl:vector) &key (test 'equal)(start 0))
  (let ((pos (cl:search subseq seq :test test :start2 start))
        (seqlen (length seq))
        (sublen (length subseq)))
    (if pos
        (values (subseq seq start pos)
                (+ pos sublen))
        (values (subseq seq start seqlen)
                seqlen))))

(defmethod split ((seq cl:vector) (subseq cl:vector) &key (test 'equal))
  (if (empty? subseq)
      (map 'cl:vector 'cl:vector seq)
      (let ((seqlen (length seq))
            (sublen (length subseq))
            (start 0)
            (chunks nil))
        (block searching
          (loop
             (if (>= start seqlen)
                 (return-from searching)
                 (multiple-value-bind (chunk pos) (%bite seq subseq :test test :start start)
                   (push chunk chunks)
                   (setf start pos)))))
        (coerce (reverse chunks)
                'cl:vector))))


;;; ---------------------------------------------------------------------
;;; function subsequence
;;; ---------------------------------------------------------------------
;;;
;;; (subsequence seq start &optional end) => seq2
;;; ---------------------------------------------------------------------
;;; returns a new sequence containing the elements of SEQ starting with
;;; index START. If END is given, the last element of the new sequence is
;;; the element just before index END; otherwise, it is the last element
;;; of SEQ.

(defmethod subsequence ((s null) (start integer) &optional end)
  (error "Index out of range on NIL:" start))

(defmethod subsequence ((s cl:sequence) (start integer) &optional end)
  (cl:subseq s start end))

(defmethod subsequence ((s seq) (start integer) &optional end)
  (fset:subseq s start end))

(defmethod subsequence ((s foundation-series) (start integer) &optional end)
  (series::subseries s start end))

;;; function tails
;;;
;;; (tails seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence of sequences beginning with SEQ, followed by
;;; the tail of SEQ, then the tail of the tail of SEQ, and so on,
;;; ending with the last non-empty tail

(defmethod tails ((seq null))
  (declare (ignore fn s))
  nil)

(defmethod tails ((seq cons))
  (if (null (cdr seq))
      (list seq)
      (cons seq (tails (cdr seq)))))

(defmethod tails ((seq cl:sequence))
  (let ((indexes (range 0 (cl:length seq))))
    (mapcar (lambda (i)(cl:subseq seq i)) indexes)))

(defmethod tails ((seq foundation-series))
  (series:map-fn t
                 #'(lambda (i) (series:subseries seq i))
                 (indexes seq)))

;;; function take
;;;
;;; (take n seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence containing the first N elements of SEQ

(defmethod take ((n (eql 0))(s null))
  (declare (ignore n))
  nil)

(defmethod take ((n integer)(s null))
  (error "index out of range: ~s" n))

(defmethod take ((n integer)(s cl:sequence))
  (cl:subseq s 0 n))

(defmethod take ((n integer)(s seq))
  (fset::subseq s 0 n))

(defmethod take ((n integer)(s foundation-series))
  (series:subseries s 0 n))

;;; function take-by
;;;
;;; (take-by n advance seq) => a sequence of sequences
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ N at a time. each chunk beings ADVANCE
;;; places after the start of the previous chunk

(defmethod take-by ((n (eql 0))(advance (eql 0))(s null))
  (declare (ignore n))
  nil)

(defmethod take-by ((n integer)(advance integer)(s null))
  (error "index and advance out of range: ~s, ~s" n advance))

(defmethod take-by ((n integer)(advance integer)(s cl:sequence))
  (let ((out-type (combined-type s s)))
    (cl:mapcar (lambda (p)(coerce (series:collect 'list p) out-type)) 
               (series:collect 'list (take-by n advance (series:scan s))))))

(defmethod take-by ((n integer)(advance integer)(s seq))
  (let ((out-type (combined-type s s)))
    (cl:mapcar (lambda (p)(coerce (series:collect 'list p) out-type)) 
               (series:collect 'cl:list
                 (take-by n advance
                          (series:scan (fset:convert 'vector s)))))))

(defmethod take-by ((n integer)(advance integer)(s foundation-series))
  (let ((indexes (indexes s)))
    (multiple-value-bind (is ps)
        (series:until-if #'null
                         indexes
                         (series:map-fn t
                                        (lambda (i)(series:subseries s i (+ i n)))
                                        indexes))
      ps)))


;;; function take-while
;;;
;;; (take-while test seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns elements of SEQ one after the other until TEST returns true

(defmethod take-while (test (s cl:sequence))
  (cl:subseq s 0 (cl:position-if test s)))

(defmethod take-while (test (s seq))
  (fset:subseq s 0 (fset:position-if test s)))

(defmethod take-while (test (s foundation-series))
  (multiple-value-bind (is ps)
      (series:until-if (lambda (x)(not (funcall test x)))
                       s
                       s)
    ps))


;;; function unique
;;;
;;; (unique seq &key (test 'equal)) => seq'
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ with duplicates removed. TEST is used
;;; to test whether two elements SEQ are the same.

(defmethod unique ((s cl:sequence) &key (test #'equal))
  (cl:remove-duplicates s :test test))

(defmethod unique ((s seq) &key (test #'equal))
  (fset:convert 'seq (cl:remove-duplicates (fset:convert 'list s) :test test)))

(defmethod unique ((seq foundation-series) &key (test #'equal))
  (series:choose
   (series:map-fn t
                  (lambda (e i)
                    (not
                     (some? (lambda (x)(funcall test e x)) 
                            (series:subseries seq 0 i))))
                  seq
                  (indexes seq))
   seq))


;;; function unzip
;;;
;;; (unzip seq) => seq1 seq2
;;; ---------------------------------------------------------------------
;;; SEQ must be a sequence of pairs. returns two sequences; the first
;;; contains the heads of the pairs in SEQ, and the second contains
;;; the tails

(defmethod unzip ((seq null))
  (declare (ignore seq))
  (values nil nil))

(defmethod unzip ((seq cons))
  (values (mapcar #'car seq)
          (mapcar #'cdr seq)))

(defmethod unzip ((seq seq))
  (values (fset:image #'car seq)
          (fset:image #'cdr seq)))

(defmethod unzip ((seq foundation-series))
  (values (series:map-fn t #'car seq)
          (series:map-fn t #'cdr seq)))

;;; function zip
;;;
;;; (unzip seq1 seq2) => seq3
;;; ---------------------------------------------------------------------
;;; returns a sequence of pairs in which each left element is from SEQ1
;;; and each right element is the corresponding one from SEQ2.

(defmethod zip ((seq1 null) seq2)
  (declare (ignore seq1 seq2))
  nil)

(defmethod zip (seq1 (seq2 null))
  (declare (ignore seq1 seq2))
  nil)


(defmethod zip ((seq1 cons)(seq2 cons))
  (mapcar #'cons seq1 seq2))

(defmethod zip ((seq1 cons)(seq2 cl:sequence))
  (zip seq1 (coerce seq2 'list)))

(defmethod zip ((seq1 cons)(seq2 seq))
  (zip (coerce seq1 'list) (fset:convert 'cl:list seq2)))

(defmethod zip ((seq1 cons)(seq2 foundation-series))
  (series:map-fn t #'cons (series:scan seq1) seq2))


(defmethod zip ((seq1 cl:sequence)(seq2 cons))
  (zip (coerce seq1 'list) seq2))

(defmethod zip ((seq1 cl:sequence)(seq2 cl:sequence))
  (zip (coerce seq1 'list) (coerce seq2 'list)))

(defmethod zip ((seq1 cl:sequence)(seq2 seq))
  (zip (coerce seq1 'list) (fset:convert 'cl:list seq2)))

(defmethod zip ((seq1 cl:sequence)(seq2 foundation-series))
  (series:map-fn t #'cons (series:scan seq1) seq2))


(defmethod zip ((seq1 seq)(seq2 cons))
  (zip (fset:convert 'cl:list seq1) seq2))

(defmethod zip ((seq1 seq)(seq2 cl:sequence))
  (zip (fset:convert 'cl:list seq1) (fset:convert 'cl:list seq2)))

(defmethod zip ((seq1 seq)(seq2 seq))
  (zip (fset:convert 'cl:list seq1) (fset:convert 'cl:list seq2)))

(defmethod zip ((seq1 seq)(seq2 foundation-series))
  (series:map-fn t #'cons (series:scan (fset:convert 'list seq1)) seq2))


(defmethod zip ((seq1 foundation-series)(seq2 cl:sequence))
  (series:map-fn t #'cons seq1 (series:scan seq2)))

(defmethod zip ((seq1 foundation-series)(seq2 seq))
  (series:map-fn t #'cons seq1 (series:scan (fset:convert 'list seq2))))

(defmethod zip ((seq1 foundation-series)(seq2 foundation-series))
  (series:map-fn t #'cons seq1 seq2))



