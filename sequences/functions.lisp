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

(defmethod combined-type ((s1 string) s2)
  (declare (ignore s1 s2))
  'vector)

(defmethod combined-type ((s1 fset:seq) (s2 fset:seq))
  (declare (ignore s1 s2))
  'fset:seq)

(defmethod combined-type ((s1 fset:seq) s2)
  (declare (ignore s1 s2))
  'list)

(defmethod combined-type ((s1 series::foundation-series)(s2 series::foundation-series))
  (declare (ignore s1 s2))
  'series::foundation-series)

(defmethod combined-type ((s1 series::foundation-series) s2)
  (declare (ignore s1 s2))
  'list)

(defmethod %split-at ((n integer)(ls null))
  (declare (ignore n ls))
  (values nil nil))

(defmethod %split-at ((n integer)(ls cons))
  (do ((i n)
       (collected nil)
       (remaining ls))
      ((or (null remaining)
           (<= i 0))
       (values (cl:reverse collected)
               remaining))
    (setf i (1- i)
          collected (cons (car remaining)
                          collected)
          remaining (cdr remaining))))


;;; ---------------------------------------------------------------------
;;; function add-first
;;; ---------------------------------------------------------------------
;;;
;;; (add-first x seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence that contains X prepended to the elements of
;;; SEQ

(defgeneric add-first (x seq))

(defmethod add-first (x (s null))(cons x s))
(defmethod add-first (x (s list))(cons x s))
(defmethod add-first (x (s vector))(concatenate 'vector (vector x) s))
(defmethod add-first (x (s string))(concatenate 'vector (vector x) s))
(defmethod add-first ((x character) (s string))(concatenate 'string (string x) s))
(defmethod add-first (x (s fset:seq))(fset:with-first s x))
(defmethod add-first (x (s series::foundation-series))(series:catenate (series:scan (list x)) s))

;;; ---------------------------------------------------------------------
;;; function add-last
;;; ---------------------------------------------------------------------
;;;
;;; (add-last seq x) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence that contains X appended after the elements of
;;; SEQ

(defgeneric add-last (seq x))

(defmethod add-last ((s null) x)(cons x s))
(defmethod add-last ((s list) x)(append s (list x)))
(defmethod add-last ((s vector) x)(concatenate 'vector s (vector x)))
(defmethod add-last ((s string) x)(concatenate 'vector s (vector x)))
(defmethod add-last ((s string) (x character))(concatenate 'string s (string x)))
(defmethod add-last ((s fset:seq) x)(fset:with-last s x))
(defmethod add-last ((s series::foundation-series) x)(series:catenate s (series:scan (list x))))

;;; ---------------------------------------------------------------------
;;; function any
;;; ---------------------------------------------------------------------
;;;
;;; (any seq) => anything
;;; ---------------------------------------------------------------------
;;; returns an arbitrary element of seq. any chooses the element
;;; randomly

(defgeneric any (seq))

(defmethod any ((s null)) (declare (ignore s)) nil)
(defmethod any ((s sequence))(elt s (random (length s))))
(defmethod any ((s fset:seq))(fset:@ s (random (fset:size s))))
(defmethod any ((s fset:map))(fset:@ s ()))

(defmethod any ((s series::foundation-series))
  (block searching
    (series:iterate ((x s))
        (when (zerop (random 2))
          (return-from searching x)))))

;;; ---------------------------------------------------------------------
;;; function append
;;; ---------------------------------------------------------------------
;;;
;;; (append &rest seqs) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence. if SEQS is nil, then nil is returned. If
;;; seqs contains a single value then that value is returned. Otherwise,
;;; append returns (reduce 'append2 seqs)
;;; if you want to extend append with cases for additional sequence types,
;;; add methods to append2

(defun append (&rest seqs)
  (if (null seqs)
      nil
      (if (null (cdr seqs))
          (cadr seqs)
          (reduce #'append2 seqs))))

;;; ---------------------------------------------------------------------
;;; function append2
;;; ---------------------------------------------------------------------
;;;
;;; (append2 seq1 seq2) => seq3
;;; ---------------------------------------------------------------------
;;; returns a sequence containing all the elements of SEQ1 followed by
;;; all the elements of SEQ2.

(defgeneric append2 (seq1 seq2))

(defmethod append2 ((seq1 null)(seq2 null))(declare (ignore seq1 seq2)) nil)
(defmethod append2 ((seq1 null)(seq2 sequence))(declare (ignore seq1)) seq2)
(defmethod append2 ((seq1 null)(seq2 fset:seq))(declare (ignore seq1)) seq2)
(defmethod append2 ((seq1 null)(seq2 series::foundation-series))(declare (ignore seq1)) seq2)

(defmethod append2 ((seq1 sequence)(seq2 null))(declare (ignore seq2)) seq1)
(defmethod append2 ((seq1 sequence)(seq2 sequence)) (concatenate (type-for-copy seq1) seq1 seq2))
(defmethod append2 ((seq1 sequence)(seq2 fset:seq))
  (concatenate (combined-type seq1 seq2) seq1 (fset:convert (combined-type seq1 seq2) seq2)))
(defmethod append2 ((seq1 sequence)(seq2 series::foundation-series))
  (series:catenate (series:scan seq1) seq2))

;;; ---------------------------------------------------------------------
;;; function by
;;; ---------------------------------------------------------------------
;;;
;;; (by n seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence of sequences constructed by taking the elements of
;;; SEQ N at a time

(defgeneric by (n seq))

(defmethod by (n (s null))
  (declare (ignore n))
  nil)

(defmethod by ((n integer)(s list))
  (assert (> n 0)() "count argument to BY must be greater than zero")
  (if (null s)
      nil
      (multiple-value-bind (collected remaining)(%split-at n s)
        (cons collected
              (by n remaining)))))

(defmethod by ((n integer)(s vector))
  (assert (> n 0)() "count argument to BY must be greater than zero")
  (multiple-value-bind (vcount leftover)(truncate (cl:length s) n)
    (let* ((last-seg (subseq s (* vcount n)))
           (tail (if (> (cl:length last-seg) 0)
                     (list last-seg)
                     nil))
           (segs (loop for i from vcount above 0 
                    do (push (subseq s (* (1- i) n)(* i n))
                             tail))))
      tail)))

(defmethod by ((n integer)(s fset:seq))
  (assert (> n 0)() "count argument to BY must be greater than zero")
  (multiple-value-bind (vcount leftover)(truncate (fset:size s) n)
    (let* ((last-seg (fset:subseq s (* vcount n)))
           (tail (if (> (fset:size last-seg) 0)
                     (list last-seg)
                     nil))
           (segs (loop for i from vcount above 0 
                    do (push (fset:subseq s (* (1- i) n)(* i n))
                             tail))))
      tail)))

(defmethod by ((n integer)(s series::foundation-series))
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
  (let ((SERIES:*SUPPRESS-SERIES-WARNINGS* t)
        (seqs (mapcar 'scan seqs)))
    (series:collect (apply 'series:map-fn t fn seqs))))

;;; ---------------------------------------------------------------------
;;; function drop
;;; ---------------------------------------------------------------------
;;;
;;; (drop n seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence containing the elements of SEQ after the first
;;; N elements have been removed

(defgeneric drop (n seq))

(defmethod drop ((n integer) (seq null))
  (error "index out of range: ~A" n))

(defmethod drop ((n integer) (seq sequence))
  (subseq seq n))

(defmethod drop ((n integer) (seq fset:seq))
  (fset:subseq seq n))

(defmethod drop ((n integer) (seq series::foundation-series))
  (series:subseries seq n))

;;; ---------------------------------------------------------------------
;;; function drop-while
;;; ---------------------------------------------------------------------
;;;
;;; (drop-while test seq) => seq'
;;; ---------------------------------------------------------------------

(defgeneric drop-while (test seq))

(defmethod drop-while (fn (seq null))
  (error "index out of range: ~A" n))

(defmethod drop-while (fn (seq sequence))
  (let ((pos (cl:position-if-not fn seq)))
    (if pos
        (drop pos seq)
        nil)))

(defmethod drop-while (fn (seq fset:seq))
  (let ((pos (fset:position-if-not fn seq)))
    (if pos
        (drop pos seq)
        nil)))

(defmethod drop-while (fn (seq series::foundation-series))
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

(defgeneric element (seq n))

(defmethod element ((s null) (n integer))
  (error "index out of range: ~A" n))

(defmethod element ((s sequence) (n integer))
  (cl:elt s n))

(defmethod element ((s fset:seq) (n integer))
  (fset:@ s n))

(defmethod element ((s series::foundation-series) (n integer))
  (series:collect-nth n s))

;;; ---------------------------------------------------------------------
;;; function empty?
;;; ---------------------------------------------------------------------
;;;
;;; (empty? seq) => a boolean
;;; ---------------------------------------------------------------------
;;; returns true if SEQ contains no elements, and false otherwise

(defgeneric empty? (seq))

(defmethod empty? ((s null))
  (declare (ignore s))
  t)

(defmethod empty? ((s cons))
  (declare (ignore s))
  nil)

(defmethod empty? ((s sequence))
  (= 0 (cl:length s)))

(defmethod empty? ((s fset:seq))
  (= 0 (fset:size s)))

(defmethod empty? ((s series::foundation-series))
  (let ((SERIES:*SUPPRESS-SERIES-WARNINGS* t))
    (= 0 (series:collect-length s))))

;;; ---------------------------------------------------------------------
;;; function every?
;;; ---------------------------------------------------------------------
;;;
;;; (every? test seq) => a boolean
;;; ---------------------------------------------------------------------
;;; returns true if SEQ contains no elements, and false otherwise

(defgeneric every? (test seq))

(defmethod every? (fn (s null))
  (declare (ignore fn s))
  t)

(defmethod every? (fn (s sequence))
  (cl:every fn s))

(defmethod every? (fn (s fset:seq))
  (fset::every fn s))

(defmethod every? (fn (s series::foundation-series))
  (series:collect-and (series:map-fn t fn s)))

;;; ---------------------------------------------------------------------
;;; function filter
;;; ---------------------------------------------------------------------
;;;
;;; (filter test seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns those elements of SEQ for which TEST returns true

(defgeneric filter (test seq))

(defmethod filter (fn (s null))
  (declare (ignore fn s))
  nil)

(defmethod filter (fn (s sequence))
  (cl:remove-if-not fn s))

(defmethod filter (fn (s fset:seq))
  (fset::remove-if-not fn s))

(defmethod filter (fn (s series::foundation-series))
  (series:choose (series:map-fn t fn s) s))

;;; ---------------------------------------------------------------------
;;; function first
;;; ---------------------------------------------------------------------
;;;
;;; (first seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the first element of SEQ

(defgeneric first (seq))

(defmethod first ((s null))
  nil)

(defmethod first ((s sequence))
  (cl:elt s 0))

(defmethod first ((s fset:seq))
  (fset::@ s 0))

(defmethod first ((s series::foundation-series))
  (series:collect-first s))

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

(defgeneric interleave (seq1 seq2))

(defmethod interleave ((s1 null)(s2 null))
  (declare (ignore s1 s2))
  nil)

(defmethod interleave ((s1 null)(s2 cl:sequence))
  (declare (ignore s1 s2))
  s2)

(defmethod interleave ((s1 null)(s2 fset:seq))
  (declare (ignore s1 s2))
  s2)

(defmethod interleave ((s1 null)(s2 series::foundation-series))
  (declare (ignore s1 s2))
  s2)



(defmethod interleave ((s1 cl:sequence)(s2 null))
  (declare (ignore s1 s2))
  s1)


(defmethod interleave ((s1 cl:sequence)(s2 cl:sequence))
  (let* ((SERIES:*SUPPRESS-SERIES-WARNINGS* t)
         (sr1 (series:scan s1))
         (sr2 (series:scan s2))
         (sr3 (interleave sr1 sr2))
         (out-type (combined-type s1 s2)))
    (series:collect out-type sr3)))

(defmethod interleave ((s1 cl:sequence)(s2 fset:seq))
  (interleave s1 (fset:convert 'cl:list s2)))

(defmethod interleave ((s1 cl:sequence)(s2 series::foundation-series))
  (let ((SERIES:*SUPPRESS-SERIES-WARNINGS* t))
    (series:collect (combined-type s1 s2) 
                    (interleave (series:scan s1) s2))))



(defmethod interleave ((s1 fset:seq)(s2 null))
  (declare (ignore s1 s2))
  s1)

(defmethod interleave ((s1 fset:seq)(s2 cl:sequence))
  (fset:convert 'fset:seq (interleave (fset:convert 'cl:list s1) s2)))

(defmethod interleave ((s1 fset:seq)(s2 fset:seq))
  (fset:convert 'fset:seq (interleave (fset:convert 'cl:list s1)
                                      (fset:convert 'cl:list s2))))

(defmethod interleave ((s1 fset:seq)(s2 series::foundation-series))
  (fset:convert 'fset:seq (interleave (fset:convert 'cl:list s1) s2)))



(defmethod interleave ((s1 series::foundation-series)(s2 null))
  (declare (ignore s1 s2))
  s1)

(defmethod interleave ((s1 series::foundation-series)(s2 cl:sequence))
  (let ((SERIES:*SUPPRESS-SERIES-WARNINGS* t))
    (interleave s1 (series:scan s2))))

(defmethod interleave ((s1 series::foundation-series)(s2 fset:seq))
  (interleave s1 (fset:convert 'cl:list s2)))

(defmethod interleave ((s1 series::foundation-series)(s2 series::foundation-series))
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

(defgeneric interpose (cupola seq))

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

(defmethod interpose (x (s fset:seq))
  (interleave s (make-array (1- (fset:size s)) :initial-element x)))

(defmethod interpose (x (s series::foundation-series))
  (let* ((indexes (series:positions (series:map-fn 'boolean (constantly t) s)))
         (xs (series:choose (series:subseries indexes 1)(repeat x))))
    (interleave s xs)))

;;; ---------------------------------------------------------------------
;;; function join
;;; ---------------------------------------------------------------------
;;;
;;; (join cupola seqs) => seq
;;; ---------------------------------------------------------------------
;;; joins SEQS in the manner of join2, below. to add support for joining 
;;; new sequence types, add methods to join2

(defgeneric join (cupola seq))

(defmethod join (x (s null))
  (declare (ignore x s))
  nil)

(defmethod join (x (s sequence))
  (cl:reduce (lambda (a b)(join2 x a b)) s))

(defmethod join (x (s fset:seq))
  (fset:reduce (lambda (a b)(join2 x a b)) s))

(defmethod join (x (s series::foundation-series))
  (let ((SERIES:*SUPPRESS-SERIES-WARNINGS* t))
    (series:collect-fn t
                       (lambda () (series:collect-first s))
                       (lambda (a b) (join2 x a b))
                       (series:subseries s 1))))

;;; ---------------------------------------------------------------------
;;; function join2
;;; ---------------------------------------------------------------------
;;;
;;; (join cupola seq1 seq2) => seq3
;;; ---------------------------------------------------------------------
;;; concatenates SEQ1 and SEQ2 to form the new sequence SEQ3, with CUPOLA
;;; inserted between the elements of SEQ1 and SEQ2

(defgeneric join2 (cupola seq1 seq2))

(defmethod join2 (x (s1 sequence)(s2 sequence))
  (let* ((out-type (combined-type s1 s2))
         (cupola (coerce (list x) out-type)))
    (concatenate out-type s1 cupola s2)))

(defmethod join2 ((x character) (s1 sequence)(s2 sequence))
  (let* ((out-type (combined-type s1 s2))
         (cupola (string x)))
    (concatenate out-type s1 cupola s2)))

(defmethod join2 (x (s1 sequence)(s2 fset:seq))
  (let* ((out-type (combined-type s1 s2))
         (cupola (coerce (list x) out-type)))
    (concatenate out-type s1 cupola (fset:convert out-type s2))))

(defmethod join2 (x (s1 cl:sequence)(s2 series::foundation-series))
  (join2 x (series:scan s1) s2))


(defmethod join2 (x (s1 fset:seq)(s2 cl:sequence))
  (let* ((out-type (combined-type s1 s2))
         (cupola (fset:convert out-type (list x))))
    (fset:concat out-type s1 cupola (fset:convert out-type s2))))

(defmethod join2 (x (s1 fset:seq)(s2 fset:seq))
  (let ((cupola (fset:convert 'fset:seq (list x))))
    (fset:concat out-type s1 cupola s2)))

(defmethod join2 (x (s1 fset:seq)(s2 series::foundation-series))
  (join2 x (series:scan (fset:convert 'cl:list s1)) s2))


(defmethod join2 (x (s1 series::foundation-series)(s2 cl:sequence))
  (join2 x s1 (series:scan s2)))

(defmethod join2 (x (s1 series::foundation-series)(s2 fset:seq))
  (join2 x s1 (series:scan (fset:convert 'cl:list s2))))

(defmethod join2 (x (s1 series::foundation-series)(s2 series::foundation-series))
  (let ((cupola (series:scan (list x))))
    (series:catenate s1 cupola s2)))

;;; ---------------------------------------------------------------------
;;; function last
;;; ---------------------------------------------------------------------
;;;
;;; (last seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the last element of SEQ

(defgeneric last (seq))

(defmethod last ((s null)) (declare (ignore s)) nil)
(defmethod last ((s cl:cons)) (cl:first (cl:last s)))
(defmethod last ((s cl:sequence))(cl:elt s (1- (cl:length s))))
(defmethod last ((s fset:seq))(fset:last s))
(defmethod last ((s series::foundation-series))(series:collect-last s))

;;; ---------------------------------------------------------------------
;;; function length
;;; ---------------------------------------------------------------------
;;;
;;; (length seq) => an integer
;;; ---------------------------------------------------------------------
;;; returns a count of the elements in SEQ

(defgeneric length (seq))


(defmethod length ((s null)) (declare (ignore s)) 0)
(defmethod length ((s cl:sequence))(cl:length s))
(defmethod length ((s fset:seq))(fset:size s))
(defmethod length ((s series::foundation-series))(series:collect-length s))

;;; ---------------------------------------------------------------------
;;; function map
;;; ---------------------------------------------------------------------
;;;
;;; (map fn seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence containing the values produced by applying
;;; FN to each element of SEQ

(defgeneric map (fn seq))

(defmethod map (fn (s null)) (declare (ignore s)) nil)
(defmethod map (fn (s cl:sequence))(cl:map 'list fn s))
(defmethod map (fn (s fset:seq))(fset:image fn s))
(defmethod map (fn (s series::foundation-series))(series:map-fn t fn s))

;;; ---------------------------------------------------------------------
;;; function next-last
;;; ---------------------------------------------------------------------
;;;
;;; (next-last seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the last-but-one element of seq

(defgeneric next-last (seq))

(defmethod next-last ((s null)) (declare (ignore s)) nil)

(defmethod next-last ((s cl:cons))
  (if (null (cdr s))
      nil
      (if (null (cddr s))
          (car s)
          (next-last (cdr s)))))

(defmethod last ((s cl:sequence))(cl:elt s (- (cl:length s) 2)))
(defmethod last ((s fset:seq))(fset:@ s (- (fset:size s) 2)))
(defmethod last ((s series::foundation-series))(series:collect-nth (- (series:collect-length s) 2) s))

;;; ---------------------------------------------------------------------
;;; function partition
;;; ---------------------------------------------------------------------
;;;
;;; (partition seq &rest fn1 fn2 fn3...) => seq1 seq2 seq3...
;;; ---------------------------------------------------------------------
;;; returns a number of sequences equal to the number of FUNCTIONS.
;;; the elements of SEQ1 are produced by applying FN1 to each element of
;;; SEQ; the elements of SEQ2 are produced by applying FN2 to each 
;;; element of SEQ; and so on

(defgeneric partition (seq &rest fns))


;;; function range-from
;;;
;;; (range-from start &key (by 1)) -> integer series
;;; ---------------------------------------------------------------------
;;; returns a series of integers starting with START and continuing
;;; forever. Each succeeding integer differes from the previous one by BY.

(defun range-from (n &key (by 1))
  (series:scan-range :from n :by by))

;;; function reduce
;;;
;;; (reduce fn &rest args) => 
;;; ---------------------------------------------------------------------
;;; applies FN to the first and second elements of ARGS, then applies it
;;; the the result and the third element of ARGS, and so on until the
;;; last element of ARGS is consumed. returns the last value produced.

(defgeneric reduce (fn &rest args))

;;; function repeat
;;;
;;; (repeat val) => cycling series
;;; ---------------------------------------------------------------------
;;; returns an infinitely-repeating sequence of VAL. If VAL is a
;;; sequence, the elements of VAL appear in order repeatedly.

(defmethod repeat (s)
  (series:scan-fn 't 
                  (lambda () s)
                  (lambda (i) s)))

(defmethod repeat ((s cl:sequence))
  )

(defmethod repeat ((s fset:seq))
  )

(defmethod repeat ((s series::foundation-series))
  )

;;; function rest
;;;
;;; (rest seq) => anything
;;; ---------------------------------------------------------------------
;;; returns all but the firt element of SEQ

(defgeneric rest (seq))

;;; function reverse
;;;
;;; (reverse SEQ) => seq'
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ in reverse order

(defgeneric reverse (seq))

;;; function scan
;;;
;;; (scan seq) => a series
;;; ---------------------------------------------------------------------
;;; returns a series equivalent to SEQ

(defgeneric scan (seq))

(defmethod scan ((s null)) (series:scan s))
(defmethod scan ((s cons)) (series:scan s))
(defmethod scan ((s vector)) (series:scan s))
(defmethod scan ((s fset:seq)) (series:scan (fset:convert 'vector s)))
(defmethod scan ((s series::foundation-series)) s)

;;; function scan-map
;;;
;;; (scan-map fn seq) => a series
;;; ---------------------------------------------------------------------
;;; returns a series equivalent to (map FN SEQ)

(defgeneric scan-map (seq))

;;; function second
;;;
;;; (second seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the second element of SEQ

(defgeneric second (seq))

;;; function some?
;;;
;;; (some? test seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the first element of SEQ for which TEST returns true, or
;;; nil otherwise

(defgeneric some? (test seq))

;;; function take
;;;
;;; (take n seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence containing the first N elements of SEQ

(defgeneric take (n seq))

;;; function take-by
;;;
;;; (take-by n advance seq) => a sequence of sequences
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ N at a time. each chunk beings ADVANCE
;;; places after the start of the previous chunk

(defgeneric take-by (n advance seq))

;;; function take-while
;;;
;;; (take-while test seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns elements of SEQ one after the other until TEST returns true

(defgeneric take-while (test seq))

;;; function unique
;;;
;;; (unique seq &key (test 'equal)) => seq'
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ with duplicates removed. TEST is used
;;; to test whether two elements SEQ are the same.

(defgeneric unique (seq &key test))

;;; function unzip
;;;
;;; (unzip seq) => seq1 seq2
;;; ---------------------------------------------------------------------
;;; SEQ must be a sequence of pairs. returns two sequences; the first
;;; contains the heads of the pairs in SEQ, and the second contains
;;; the tails

(defgeneric unzip (seq))


;;; function zip
;;;
;;; (unzip seq1 seq2) => seq3
;;; ---------------------------------------------------------------------
;;; returns a sequence of pairs in which each left element is from SEQ1
;;; and each right element is the corresponding one from SEQ2.

(defgeneric zip (seq1 seq2))



