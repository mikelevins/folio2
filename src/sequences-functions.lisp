;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       sequence functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.sequences)

;;; ---------------------------------------------------------------------
;;; conversions: as
;;; ---------------------------------------------------------------------

(defmethod as ((type (eql 'cl:list))(sequence cl:cons) &key &allow-other-keys)
  sequence)

(defmethod as ((type (eql 'cl:list))(sequence cl:vector) &key &allow-other-keys)
  (cl:coerce sequence 'cl:list))

(defmethod as ((type (eql 'cl:list))(sequence wb-seq) &key &allow-other-keys)
  (fset:convert 'cl:list sequence))

(defmethod as ((type (eql 'cl:vector))(sequence cl:cons) &key &allow-other-keys)
  (cl:coerce sequence 'cl:vector))

(defmethod as ((type (eql 'cl:string))(sequence cl:cons) &key &allow-other-keys)
  (if (cl:every 'cl:characterp sequence)
      (cl:coerce sequence 'cl:string)
      (cl:coerce sequence 'cl:vector)))

(defmethod as ((type (eql 'cl:vector))(sequence cl:vector) &key &allow-other-keys)
  sequence)

(defmethod as ((type (eql 'cl:vector))(sequence wb-seq) &key &allow-other-keys)
  (fset:convert 'cl:vector sequence))

(defmethod as ((type (eql 'wb-seq))(sequence cl:cons) &key &allow-other-keys)
  (fset:convert 'fset:seq sequence))

(defmethod as ((type (eql 'wb-seq))(sequence cl:vector) &key &allow-other-keys)
  (fset:convert 'fset:seq sequence))

(defmethod as ((type (eql 'wb-seq))(sequence wb-seq) &key &allow-other-keys)
  sequence)

;;; ---------------------------------------------------------------------
;;; constructions: make
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; sequence functions
;;; ---------------------------------------------------------------------

;;; function acons [bounded]
;;;
;;; (acons key value sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod acons (key value (sequence cl:null)) 
  (cl:cons (cl:cons key value) sequence))

(defmethod acons (key value (sequence cl:cons)) 
  (cl:cons (cl:cons key value) sequence))

(defmethod acons (key value (sequence cl:vector)) 
  (cl:concatenate 'cl:vector (cl:vector (cl:cons key value)) sequence))

(defmethod acons (key value (sequence wb-seq)) 
  (fset:concat (wb-seq (cl:cons key value)) sequence))

;;; function add-first
;;;
;;; (add-first x sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod add-first (x (sequence cl:null))
  (cl:list x))

(defmethod add-first (x (sequence cl:cons))
  (cl:cons x sequence))

(defmethod add-first (x (sequence cl:vector))
  (cl:concatenate 'cl:vector (cl:list x) sequence))

(defmethod add-first ((x cl:character) (sequence cl:string))
  (cl:concatenate 'cl:string (cl:list x) sequence))

(defmethod add-first (x (sequence wb-seq))
  (fset:with-first sequence x))

;;; function add-last [bounded]
;;;
;;; (add-last sequence x) => sequence'
;;; ---------------------------------------------------------------------

(defmethod add-last ((sequence cl:null) x)
  (cl:list x))

(defmethod add-last ((sequence cl:cons) x)
  (cl:append sequence (cl:list x)))

(defmethod add-last ((sequence cl:vector) x)
  (cl:concatenate 'cl:vector sequence (list x)))

(defmethod add-last ((sequence cl:string) (x cl:character))
  (cl:concatenate 'cl:string sequence (list x)))

(defmethod add-last ((sequence wb-seq) x)
  (fset:with-last sequence x))

;;; function any
;;;
;;; (any sequence) => anything
;;; ---------------------------------------------------------------------

(defparameter *any-random-state* (make-random-state))

(defmethod any ((sequence cl:null))
  nil)

(defmethod any ((sequence cl:sequence))
  (elt sequence (cl:random (cl:length sequence) *any-random-state*)))

(defmethod any ((sequence wb-seq))
  (fset:@ sequence (cl:random (fset:size sequence) *any-random-state*)))

;;; function append [bounded]
;;;
;;; (append &rest sequences) => sequence
;;; ---------------------------------------------------------------------

(defun append (&rest sequences)
  (cl:reduce 'binary-append sequences))

;;; function apportion [bounded]
;;;
;;; (apportion seq &rest fn1 fn2 fn3...) => seq1 seq2 seq3...
;;; ---------------------------------------------------------------------
;;; returns a number of sequences equal to the number of FUNCTIONS.
;;; the elements of SEQ1 are those elements of seq for which FN1
;;; returns true; the elements of SEQ2 are those elements of seq for
;;; which FN2 returns true; and so on

(defmethod apportion ((seq null) &rest fns)
  (declare (ignore fns))
  nil)

(defmethod apportion ((seq cl:sequence) &rest fns)
  (apply #'values (cl:map 'cl:list
                          (lambda (fn)(cl:remove-if-not fn seq))
                          fns)))

(defmethod apportion ((seq wb-seq) &rest fns)
  (apply #'values (cl:map 'cl:list
                          (lambda (fn)(fset:remove-if-not fn seq))
                          fns)))

;;; function assoc [bounded]
;;;
;;; (assoc item sequence &key key test) => pair
;;; ---------------------------------------------------------------------

(defmethod assoc (item (sequence cl:null) &key (key 'cl:identity) (test 'cl:eql))
  (declare (ignore item sequence key test))
  nil)

(defmethod assoc (item (sequence cl:cons) &key (key 'cl:identity) (test 'cl:eql))
  (block searching
    (progn
      (loop for x in sequence
         do (when (funcall test (funcall key item) (funcall key (head x)))
              (return-from searching x)))
      nil)))

(defmethod assoc (item (sequence cl:sequence) &key (key 'cl:identity) (test 'cl:eql))
  (block searching
    (progn
      (loop for i from 0 below (cl:length sequence)
         do (let ((x (cl:elt sequence i)))
              (when (funcall test (funcall key item) (funcall key (head x)))
                (return-from searching x))))
      nil)))

(defmethod assoc (item (sequence wb-seq) &key (key 'cl:identity) (test 'cl:eql))
  (block searching
    (progn
      (loop for i from 0 below (fset:size sequence)
         do (let ((x (fset:@ sequence i)))
              (when (funcall test (funcall key item) (funcall key (head x)))
                (return-from searching x))))
      nil)))

;;; function assoc-if [bounded]
;;;
;;; (assoc predicate sequence &key key) => pair
;;; ---------------------------------------------------------------------

(defmethod assoc-if (predicate (sequence cl:null) &key (key 'cl:identity))
  (declare (ignore predicate sequence key))
  nil)

(defmethod assoc-if (predicate (sequence cl:cons) &key (key 'cl:identity) (test 'cl:eql))
  (block searching
    (progn
      (loop for x in sequence
         do (when (funcall predicate (funcall key (head x)))
              (return-from searching x)))
      nil)))

(defmethod assoc-if (predicate (sequence cl:sequence) &key (key 'cl:identity) (test 'cl:eql))
  (block searching
    (progn
      (loop for i from 0 below (cl:length sequence)
         do (let ((x (cl:elt sequence i)))
              (when (funcall predicate (funcall key (head x)))
                (return-from searching x))))
      nil)))

(defmethod assoc-if (predicate (sequence wb-seq) &key (key 'cl:identity))
  (block searching
    (progn
      (loop for i from 0 below (fset:size sequence)
         do (let ((x (fset:@ sequence i)))
              (when (funcall predicate (funcall key (head x)))
                (return-from searching x))))
      nil)))

;;; function assoc-if-not [bounded]
;;;
;;; (assoc predicate sequence &key key) => pair
;;; ---------------------------------------------------------------------

(defun assoc-if-not (predicate sequence &key (key 'cl:identity))
  (assoc-if (complement predicate) sequence :key key))

;;; function binary-append [bounded]
;;;
;;; (binary-append sequence1 sequence2) => sequence3
;;; ---------------------------------------------------------------------

(defmethod binary-append ((sequence1 cl:null) (sequence2 cl:null)) nil)
(defmethod binary-append ((sequence1 cl:null) (sequence2 cl:cons)) sequence2)
(defmethod binary-append ((sequence1 cl:null) (sequence2 cl:vector)) (coerce sequence2 'cl:list))
(defmethod binary-append ((sequence1 cl:null) (sequence2 wb-seq)) (as 'cl:list sequence2))

(defmethod binary-append ((sequence1 cl:cons) (sequence2 cl:null)) sequence1)
(defmethod binary-append ((sequence1 cl:cons) (sequence2 cl:cons)) (cl:append sequence1 sequence2))
(defmethod binary-append ((sequence1 cl:cons) (sequence2 cl:vector)) (cl:concatenate 'cl:list sequence1 sequence2))
(defmethod binary-append ((sequence1 cl:cons) (sequence2 wb-seq)) (cl:append sequence1 (as 'cl:list sequence2)))

(defmethod binary-append ((sequence1 cl:vector) (sequence2 cl:null)) sequence1)
(defmethod binary-append ((sequence1 cl:vector) (sequence2 cl:cons))(cl:concatenate 'cl:vector sequence1 sequence2))
(defmethod binary-append ((sequence1 cl:vector) (sequence2 cl:vector))(cl:concatenate 'cl:vector sequence1 sequence2))
(defmethod binary-append ((sequence1 cl:vector) (sequence2 wb-seq))
    (cl:concatenate 'cl:vector sequence1 (as 'cl:vector sequence2)))

(defmethod binary-append ((sequence1 cl:string) (sequence2 cl:null)) sequence1)
(defmethod binary-append ((sequence1 cl:string) (sequence2 cl:sequence))
  (if (cl:every 'cl:characterp sequence2)
      (cl:concatenate 'cl:string sequence1 sequence2)
      (cl:concatenate 'cl:vector sequence1 sequence2)))
(defmethod binary-append ((sequence1 cl:string) (sequence2 wb-seq))
  (binary-append sequence1 (as 'cl:vector sequence2)))

(defmethod binary-append ((sequence1 wb-seq) (sequence2 cl:null)) sequence1)
(defmethod binary-append ((sequence1 wb-seq) (sequence2 cl:cons)) (fset:concat sequence1 sequence2))
(defmethod binary-append ((sequence1 wb-seq) (sequence2 cl:vector)) (fset:concat sequence1 sequence2))
(defmethod binary-append ((sequence1 wb-seq) (sequence2 wb-seq)) (fset:concat sequence1 sequence2))

;;; function by
;;;
;;; (by n sequence) => sequences
;;; ---------------------------------------------------------------------

(defmethod by ((n integer)(sequence cl:null)) nil)

(defmethod by ((n integer)(sequence cl:cons)) 
  (if (nthcdr (1- n) sequence)
      (cons (take n sequence)
            (by n (drop n sequence)))
      (list sequence)))

(defmethod by ((n integer)(sequence cl:vector)) 
  (if (< n (cl:length sequence))
      (concatenate 'cl:vector
                   (vector (take n sequence))
                   (by n (drop n sequence)))
      (vector sequence)))

(defmethod by ((n integer)(sequence cl:string)) 
  (if (< n (cl:length sequence))
      (concatenate 'cl:vector
                   (vector (take n sequence))
                   (by n (drop n sequence)))
      (vector sequence)))

(defmethod by ((n integer)(sequence wb-seq)) 
  (if (< n (fset:size sequence))
      (fset:concat (fset:seq (take n sequence))
                   (by n (drop n sequence)))
      (fset:seq sequence)))

;;; function count [bounded]
;;;
;;; (count item sequence &key start end key test) => integer
;;; ---------------------------------------------------------------------

(defmethod count (item (sequence cl:null) &key start end key test) 
  (declare (ignore item sequence start end key test))
  0)

(defmethod count (item (sequence cl:sequence) &key (start 0) end (key 'cl:identity) (test 'cl:eql)) 
  (cl:count item sequence :start start :end end :key key :test test))

(defmethod count (item (sequence wb-seq) &key (start 0) end (key 'cl:identity) (test 'cl:eql)) 
  (let ((seq (fset:subseq sequence start end)))
    (fset:count item seq :test test :key key)))

;;; function count-if [bounded]
;;;
;;; (count-if predicate sequence &key start end key) => integer
;;; ---------------------------------------------------------------------

(defmethod count-if (predicate (sequence cl:null) &key start end key) 
  (declare (ignore predicate sequence start end key))
  0)

(defmethod count-if (predicate (sequence cl:sequence) &key (start 0) end (key 'cl:identity)) 
  (cl:count-if predicate sequence :start start :end end :key key))

(defmethod count-if (predicate (sequence wb-seq) &key (start 0) end (key 'cl:identity)) 
  (let ((end (or end (fset:size sequence)))
        (count 0))
    (loop for i from start below end
       do (when (funcall predicate (funcall key (fset:@ sequence i)))
            (incf count)))
    count))

;;; function count-if-not [bounded]
;;;
;;; (count-if-not predicate sequence &key start end key) => integer
;;; ---------------------------------------------------------------------

(defun count-if-not (predicate sequence &key (start 0) end (key 'cl:identity)) 
  (count-if (cl:complement predicate) sequence :start start :end end :key key))

;;; function dispose
;;;
;;; (dispose seq &rest fn1 fn2 fn3...) => seq1 seq2 seq3...
;;; ---------------------------------------------------------------------
;;; returns a number of sequences equal to the number of FUNCTIONS.
;;; the elements of SEQ1 are the results of (fn1 x), where x is an element
;;; of SEQ; the elements of SEQ2 are the results of (fn2 x); and so on

(defmethod dispose ((seq null) &rest fns)
  (declare (ignore seq fns))
  nil)

(defmethod dispose ((seq cl:sequence) &rest fns)
  (apply #'values (cl:map 'cl:list
                          (lambda (fn)(cl:map (cl:type-of seq) fn seq))
                          fns)))

(defmethod dispose ((seq wb-seq) &rest fns)
  (apply #'values (cl:map 'cl:list
                          (lambda (fn)(fset:image fn seq))
                          fns)))

;;; function drop
;;;
;;; (drop n sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod drop ((n (eql 0))(sequence cl:null)) nil)
(defmethod drop ((n integer)(sequence cl:null)) (error "Can't drop ~s items from NIL" n))
(defmethod drop ((n integer)(sequence cl:sequence))(cl:subseq sequence n))
(defmethod drop ((n integer)(sequence wb-seq))(fset:subseq sequence n))

;;; function drop-while
;;;
;;; (drop-while predicate sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod drop-while (predicate (sequence cl:null))(declare (ignore predicate sequence)) nil)
(defmethod drop-while (predicate (sequence cl:sequence))
  (let ((pos (position-if-not predicate sequence)))
    (if pos
        (drop pos sequence)
        (drop (cl:length sequence) sequence))))

(defmethod drop-while (predicate (sequence wb-seq))
  (let ((pos (position-if-not predicate sequence)))
    (if pos
        (drop pos sequence)
        (drop (fset:size sequence) sequence))))

;;; function element
;;;
;;; (element sequence n) => anything
;;; ---------------------------------------------------------------------

(defmethod element ((sequence cl:null)(n integer)) (error "Can't take an element from NIL"))

(defmethod element ((sequence cl:sequence)(n integer))
  (elt sequence n))

(defmethod element ((sequence wb-seq)(n integer)) 
  (multiple-value-bind (val found?)(fset:@ sequence n)
    (if found? val (error "Index ~a out of range" n))))

;;; function empty?
;;;
;;; (empty? sequence) => Boolean
;;; ---------------------------------------------------------------------

(defmethod empty? ((sequence cl:null)) t)
(defmethod empty? ((sequence cl:sequence))(cl:zerop (cl:length sequence)))
(defmethod empty? ((sequence wb-seq))(zerop (fset:size sequence)))

;;; function every? [bounded]
;;;
;;; (every? predicate sequence &rest sequences) => Generalized Boolean
;;; ---------------------------------------------------------------------

(defun every? (predicate sequence &rest sequences)
  (cl:apply 'fset::every predicate sequence sequences))

;;; function filter
;;;
;;; (filter x sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod filter (predicate (sequence cl:null)) 
  (declare (ignore predicate))
  nil)

(defmethod filter (predicate (sequence cl:sequence))
  (cl:remove-if-not predicate sequence))

(defmethod filter (predicate (sequence wb-seq))
  (fset:remove-if-not predicate sequence))

;;; function find [bounded]
;;;
;;; (find item sequence &key test start end key) => anything
;;; ---------------------------------------------------------------------

(defmethod find (item (sequence cl:null) &key (test 'eql) (start 0) end (key 'cl:identity)) 
  (declare (ignore item sequence test start end key))
  nil)

(defmethod find (item (sequence cl:sequence) &key (test 'eql) (start 0) end (key 'cl:identity))
  (cl:find item sequence :test test :start start :end end :key key))

(defmethod find (item (sequence wb-seq) &key (test 'eql) (start 0) end (key 'cl:identity))
  (let ((sequence (fset:subseq sequence start end)))
    (fset:find item sequence :key key :test test)))

;;; function find-if [bounded]
;;;
;;; (find-if predicate sequence &key start end key) => anything
;;; ---------------------------------------------------------------------

(defmethod find-if (predicate (sequence cl:null) &key (start 0) end (key 'cl:identity)) 
  (declare (ignore predicate start end key))
  nil)

(defmethod find-if (predicate (sequence cl:sequence) &key (start 0) end (key 'cl:identity))
  (cl:find-if predicate sequence :start start :end end :key key))

(defmethod find-if (predicate (sequence wb-seq) &key (start 0) end (key 'cl:identity))
  (let ((sequence (fset:subseq sequence start end)))
    (fset:find-if predicate sequence :key key)))

;;; function find-if-not [bounded]
;;;
;;; (find-if-not predicate sequence &key start end key) => anything
;;; ---------------------------------------------------------------------

(defun find-if-not (predicate sequence &key (start 0) end (key 'cl:identity)) 
  (find-if (cl:complement predicate) sequence :start start :end end :key key))

;;; function first
;;;
;;; (first sequence) => anything
;;; ---------------------------------------------------------------------

(defmethod first ((sequence cl:null))
  nil)

(defmethod first ((sequence cl:cons))
  (cl:first sequence))

(defmethod first ((sequence cl:sequence))
  (cl:elt sequence 0))

(defmethod first ((sequence wb-seq))
  (fset:@ sequence 0))

;;; function head
;;;
;;; (head sequence) => anything
;;; ---------------------------------------------------------------------

(defmethod head ((sequence cl:null))
  nil)

(defmethod head ((sequence cl:cons))
  (cl:first sequence))

(defmethod head ((sequence cl:sequence))
  (cl:elt sequence 0))

(defmethod head ((sequence wb-seq))
  (fset:@ sequence 0))

;;; function image
;;;
;;; (image function sequence) => sequence
;;; ---------------------------------------------------------------------

(defmethod image (function (sequence cl:null))
  (declare (ignore function sequence))
  nil)

(defmethod image (function (sequence cl:cons))
  (mapcar function sequence))

(defmethod image (function (sequence cl:vector))
  (map 'cl:vector function sequence))

(defmethod image (function (sequence cl:string))
  (map 'cl:vector function sequence))

(defmethod image (function (sequence wb-seq))
  (fset:image function sequence))

;;; function indexes
;;;
;;; (indexes function sequence) => sequence
;;; ---------------------------------------------------------------------

(defmethod indexes ((sequence cl:null))
  nil)

(defmethod indexes ((sequence cl:cons))
  (loop for i from 0 below (cl:length sequence) collect i))

(defmethod indexes ((sequence cl:vector))
  (loop for i from 0 below (cl:length sequence) collect i))

(defmethod indexes ((sequence wb-seq))
  (loop for i from 0 below (fset:size sequence) collect i))

;;; function interleave
;;;
;;; (interleave sequence1 sequence2) => sequence3
;;; ---------------------------------------------------------------------

;;; null
(defmethod interleave ((sequence1 cl:null)(sequence2 cl:null)) nil)
(defmethod interleave ((sequence1 cl:null)(sequence2 cl:cons)) nil)
(defmethod interleave ((sequence1 cl:null)(sequence2 cl:vector)) nil)
(defmethod interleave ((sequence1 cl:null)(sequence2 wb-seq)) nil)

;;; cons
(defmethod interleave ((sequence1 cl:cons)(sequence2 cl:null)) nil)

(defmethod interleave ((sequence1 cl:cons)(sequence2 cl:cons)) 
  (loop for x in sequence1 for y in sequence2 append (list x y)))

(defmethod interleave ((sequence1 cl:cons)(sequence2 cl:vector)) 
  (loop for x in sequence1 for y across sequence2 append (list x y)))

(defmethod interleave ((sequence1 cl:cons)(sequence2 wb-seq)) 
  (interleave sequence1 (as 'cl:list sequence2)))

;;; vector
(defmethod interleave ((sequence1 cl:vector)(sequence2 cl:null)) (cl:vector))

(defmethod interleave ((sequence1 cl:vector)(sequence2 cl:cons)) 
  (interleave sequence1 (as 'cl:vector sequence2)))

(defmethod interleave ((sequence1 cl:vector)(sequence2 cl:vector)) 
  (cl:coerce (loop for x across sequence1 for y across sequence2 append (list x y))
             'cl:vector))

(defmethod interleave ((sequence1 cl:string)(sequence2 cl:string)) 
  (cl:coerce (loop for x across sequence1 for y across sequence2 append (list x y))
             'cl:string))

(defmethod interleave ((sequence1 cl:vector)(sequence2 wb-seq)) 
  (interleave sequence1 (as 'cl:vector sequence2)))

;;; wb-seq
(defmethod interleave ((sequence1 wb-seq)(sequence2 cl:null))(fset:seq))

(defmethod interleave ((sequence1 wb-seq)(sequence2 cl:cons))
  (interleave sequence1 (as 'wb-seq sequence2)))

(defmethod interleave ((sequence1 wb-seq)(sequence2 cl:vector))
  (interleave sequence1 (as 'wb-seq sequence2)))

(defmethod interleave ((sequence1 wb-seq)(sequence2 wb-seq)) 
  (as 'wb-seq
      (interleave (as 'cl:vector sequence1)
                  (as 'cl:vector sequence2))))

;;; function interpose
;;;
;;; (interpose item sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod interpose (item (sequence cl:null))
  (declare (ignore item sequence))
  nil)

(defmethod interpose (item (sequence cl:cons)) 
  (if (null (cdr sequence))
      sequence
      (cons (car sequence)
        (cons item
              (interpose item (cdr sequence))))))

(defmethod interpose (item (sequence cl:vector)) 
  (let ((len (cl:length sequence)))
    (case len
      ((0 1) sequence)
      (t (let ((result (make-array (1- (* 2 len)) :initial-element item)))
           (loop for i from 0 below len 
              do (setf (elt result (* i 2))
                       (elt sequence i)))
           result)))))

(defmethod interpose (item (sequence wb-seq)) 
  (let ((len (fset:size sequence)))
    (case len
      ((0 1) sequence)
      (t (let ((result-len (1- (* 2 len))))
           (fset:convert 'fset:seq
                         (loop for i from 0 below result-len 
                            collect (if (zerop (mod i 2))
                                        (fset:@ sequence (/ i 2))
                                        item))))))))


;;; function join [bounded]
;;;
;;; (join cupola sequences) => sequence
;;; ---------------------------------------------------------------------

(defun join (cupola sequences)
  (apply 'append (interpose cupola sequences)))

;;; function last  [bounded]
;;;
;;; (last sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod last ((sequence cl:null)) 
  (error "NIL has no elements"))

(defmethod last ((sequence cl:cons)) 
  (cl:car (cl:last sequence)))

(defmethod last ((sequence cl:vector)) 
  (let ((len (cl:length sequence)))
    (if (> len 0)
        (elt sequence (1- len))
        (error "~S has no elements" sequence))))

(defmethod last ((sequence wb-seq)) 
  (multiple-value-bind (val found?)(fset:@ sequence (1- (fset:size sequence)))
    (if found? val (error "~S has no elements" sequence))))

;;; function leave  [bounded]
;;;
;;; (leave n sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod leave ((n integer)(sequence cl:null)) 
  (cond
    ((< n 0)(error "Can't leave fewer than 0 elements"))
    ((= n 0) nil)
    (t (error "NIL has fewer than ~s elements" n))))

(defmethod leave ((n integer)(sequence cl:sequence)) 
  (let ((len (cl:length sequence)))
    (cond
      ((< n 0)(error "Can't leave fewer than 0 elements"))
      ((<= n len)(cl:subseq sequence (- len n) len))
      (t (error "Sequence has fewer than ~s elements" n)))))

(defmethod leave ((n integer)(sequence wb-seq)) 
  (let ((len (fset:size sequence)))
    (cond
      ((< n 0)(error "Can't leave fewer than 0 elements"))
      ((<= n len)(fset:subseq sequence (- len n) len))
      (t (error "Sequence has fewer than ~s elements" n)))))

;;; function length  [bounded]
;;;
;;; (length sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod length ((sequence cl:null)) 0)

(defmethod length ((sequence cl:sequence)) 
  (cl:length sequence))

(defmethod length ((sequence wb-seq)) 
  (fset:size sequence))

;;; function mismatch  [bounded]
;;;
;;; (mismatch sequence1 sequence2 &key test key start1 start2 end1 end2) => position
;;; ---------------------------------------------------------------------

(defmethod mismatch ((sequence1 cl:null)(sequence2 cl:null) &key test key start1 start2 end1 end2) 
  (declare (ignore sequence1 sequence2 test key start1 start2 end1 end2))
  nil)

(defmethod mismatch ((sequence1 cl:sequence)(sequence2 cl:sequence) &key (test 'cl:eql) (key 'cl:identity) (start1 0) (start2 0) end1 end2) 
  (cl:mismatch sequence1 sequence2 :test test :key key :start1 start1 :start2 start2 :end1 end1 :end2 end2))

(defmethod mismatch ((sequence1 cl:sequence)(sequence2 wb-seq) &key (test 'cl:eql) (key 'cl:identity) (start1 0) (start2 0) end1 end2) 
  (mismatch sequence1 (as 'cl:vector sequence2) :test test :key key :start1 start1 :start2 start2 :end1 end1 :end2 end2))

(defmethod mismatch ((sequence1 wb-seq)(sequence2 cl:sequence) &key (test 'cl:eql) (key 'cl:identity) (start1 0) (start2 0) end1 end2) 
  (mismatch (as 'cl:vector sequence1) sequence2 :test test :key key :start1 start1 :start2 start2 :end1 end1 :end2 end2))

(defmethod mismatch ((sequence1 wb-seq)(sequence2 wb-seq) &key (test 'cl:eql) (key 'cl:identity) (start1 0) (start2 0) end1 end2) 
  (mismatch (as 'cl:vector sequence1)(as 'cl:vector sequence2) :test test :key key :start1 start1 :start2 start2 :end1 end1 :end2 end2))

;;; function partition 
;;;
;;; (partition predicate sequence1) => sequence2, sequence3
;;; ---------------------------------------------------------------------

(defmethod partition (predicate (sequence cl:null)) 
  (declare (ignore predicate sequence))
  (values nil nil))

(defmethod partition (predicate (sequence cl:sequence))
  (values (cl:remove-if-not predicate sequence)
          (cl:remove-if predicate sequence)))

(defmethod partition (predicate (sequence wb-seq)) 
  (fset:partition predicate sequence))

;;; function penult  [bounded]
;;;
;;; (penult sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod penult ((sequence cl:null)) 
  (error "NIL has no elements"))

(defmethod penult ((sequence cl:sequence)) 
  (let ((len (cl:length sequence)))
    (if (> len 1)
        (elt sequence (1- (1- len)))
        (error "~S has fewer than 2 elements" sequence))))

(defmethod penult ((sequence wb-seq)) 
  (let ((len (fset:size sequence)))
    (if (> len 1)
        (fset:@ sequence (1- (1- len)))
        (error "~S has fewer than 2 elements" sequence))))

;;; function position [bounded]
;;;
;;; (position item sequence &key test start end key) => anything
;;; ---------------------------------------------------------------------

(defmethod position (item (sequence cl:null) &key (test 'eql) (start 0) end (key 'cl:identity)) 
  (declare (ignore item sequence test start end key))
  nil)

(defmethod position (item (sequence cl:sequence) &key (test 'eql) (start 0) end (key 'cl:identity))
  (cl:position item sequence :test test :start start :end end :key key))

(defmethod position (item (sequence wb-seq) &key (test 'eql) (start 0) end (key 'cl:identity))
  (let ((sequence (fset:subseq sequence start end)))
    (fset:position item sequence :key key :test test)))

;;; function position-if [bounded]
;;;
;;; (position-if predicate sequence &key start end key) => anything
;;; ---------------------------------------------------------------------

(defmethod position-if (predicate (sequence cl:null) &key (start 0) end (key 'cl:identity)) 
  (declare (ignore predicate sequence start end key))
  nil)

(defmethod position-if (predicate (sequence cl:sequence) &key (start 0) end (key 'cl:identity))
  (cl:position-if predicate sequence :start start :end end :key key))

(defmethod position-if (predicate (sequence wb-seq) &key (start 0) end (key 'cl:identity))
  (let ((sequence (fset:subseq sequence start end)))
    (fset:position-if predicate sequence :key key)))

;;; function position-if-not [bounded]
;;;
;;; (position-if-not predicate sequence &key start end key) => anything
;;; ---------------------------------------------------------------------

(defun position-if-not (predicate sequence &key (start 0) end (key 'cl:identity)) 
  (position-if (cl:complement predicate) sequence :start start :end end :key key))

;;; function prefix-match?
;;;
;;; (prefix-match? prefix sequence &key test key) => Generalized Boolean
;;; ---------------------------------------------------------------------

(defun %general-prefix-match (prefix sequence &key (test 'cl:equal) (key 'cl:identity))
  (or (empty? prefix)
      (and (<= (length prefix)
               (length sequence))
           (block searching
             (progn
               (loop for i from 0 below (length prefix)
                  do (unless (funcall test
                                      (funcall key (element prefix i))
                                      (funcall key (element sequence i)))
                       (return-from searching nil)))
               t)))))

(defmethod prefix-match? (prefix sequence &key test key) 
  (declare (ignore prefix sequence test key))
  nil)

(defmethod prefix-match? ((prefix cl:null)(sequence cl:null) &key test key)(declare (ignore prefix sequence test key)) t)
(defmethod prefix-match? ((prefix cl:null)(sequence cl:sequence) &key test key)(declare (ignore prefix sequence test key)) t)
(defmethod prefix-match? ((prefix cl:null)(sequence wb-seq) &key test key)(declare (ignore prefix sequence test key)) t)

(defmethod prefix-match? ((prefix cl:sequence)(sequence cl:null) &key test key)
  (declare (ignore test key))
  (empty? prefix))

(defmethod prefix-match? ((prefix cl:sequence)(sequence cl:sequence) &key (test 'cl:equal) (key 'cl:identity)) 
  (%general-prefix-match prefix sequence :test test :key key))

(defmethod prefix-match? ((prefix cl:sequence)(sequence wb-seq) &key (test 'cl:equal) (key 'cl:identity)) 
  (%general-prefix-match prefix sequence :test test :key key))


(defmethod prefix-match? ((prefix wb-seq)(sequence cl:null) &key test key)
  (declare (ignore test key))
  (empty? prefix))

(defmethod prefix-match? ((prefix wb-seq)(sequence cl:sequence) &key (test 'cl:equal) (key 'cl:identity)) 
  (%general-prefix-match prefix sequence :test test :key key))

(defmethod prefix-match? ((prefix wb-seq)(sequence wb-seq) &key (test 'cl:equal) (key 'cl:identity)) 
  (%general-prefix-match prefix sequence :test test :key key))

;;; function range [bounded]
;;;
;;; (range start end &key by) => sequence
;;; ---------------------------------------------------------------------

(defmethod range ((start integer) (end integer) &key (by 1))
  (let ((step by))
    (if (plusp (- end start))
        (loop for i from start below end by step collect i)
        (loop for i downfrom start above end by step collect i))))

;;; function reduce [bounded]
;;;
;;; (reduce fn sequence &key key initial-value) => sequence'
;;; ---------------------------------------------------------------------

(defmethod reduce (fn (sequence cl:sequence) &key (key 'cl:identity) (initial-value nil))
  (cl:reduce fn sequence :key key :initial-value initial-value))

(defmethod reduce (fn (sequence wb-seq) &key (key 'cl:identity) (initial-value nil))
  (fset:reduce fn sequence :key key :initial-value initial-value))

;;; function remove
;;;
;;; (remove item sequence &key test start end key) => sequence'
;;; ---------------------------------------------------------------------

(defmethod remove (item (sequence cl:null) &key (test 'eql) (start 0) end (key 'cl:identity)) 
  (declare (ignore item sequence test start end key))
  nil)

(defmethod remove (item (sequence cl:sequence) &key (test 'eql) (start 0) end (key 'cl:identity))
  (cl:remove item sequence :test test :start start :end end :key key))

(defmethod remove (item (sequence wb-seq) &key (test 'eql) (start 0) end (key 'cl:identity))
  (let ((sequence (fset:subseq sequence start end)))
    (fset:remove item sequence :key key :test test)))

;;; function remove-if
;;;
;;; (remove-if predicate sequence &key start end key) => sequence'
;;; ---------------------------------------------------------------------

(defmethod remove-if (predicate (sequence cl:null) &key (start 0) end (key 'cl:identity)) 
  (declare (ignore predicate start end key))
  nil)

(defmethod remove-if (predicate (sequence cl:sequence) &key (start 0) end (key 'cl:identity))
  (cl:remove-if predicate sequence :start start :end end :key key))

(defmethod remove-if (predicate (sequence wb-seq) &key (start 0) end (key 'cl:identity))
  (let ((sequence (fset:subseq sequence start end)))
    (fset:remove-if predicate sequence :start start :end end :key key)))

;;; function remove-if-not
;;;
;;; (remove-if-not predicate sequence &key start end key) => anything
;;; ---------------------------------------------------------------------

(defmethod remove-if-not (predicate sequence &key (start 0) end (key 'cl:identity)) 
  (remove-if (cl:complement predicate) sequence :start start :end end :key key))

;;; function remove-duplicates [bounded]
;;;
;;; (remove-duplicates sequence &key test start end key) => sequence'
;;; ---------------------------------------------------------------------

(defmethod remove-duplicates ((sequence cl:null) &key (test 'eql) (start 0) end (key 'cl:identity)) 
  (declare (ignore test start end key))
  nil)

(defmethod remove-duplicates ((sequence cl:sequence) &key (test 'eql) (start 0) end (key 'cl:identity)) 
  (cl:remove-duplicates sequence :test test :start start :end end :key key))

(defmethod remove-duplicates ((sequence wb-seq) &key (test 'eql) (start 0) end (key 'cl:identity)) 
  (as 'wb-seq (cl:remove-duplicates (as 'cl:vector sequence) :test test :start start :end end :key key)))

;;; function rest
;;;
;;; (rest sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod rest ((sequence cl:null))
  nil)

(defmethod rest ((sequence cl:sequence))
  (cl:subseq sequence 1))

(defmethod rest ((sequence wb-seq))
  (fset:subseq sequence 1))

;;; reverse [bounded]
;;;
;;; (reverse sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod reverse ((sequence cl:null))
  nil)

(defmethod reverse ((sequence cl:sequence))
  (cl:reverse sequence))

(defmethod reverse ((sequence wb-seq))
  (fset:reverse sequence))

;;; search  [bounded]
;;;
;;; (search sequence1 sequence2 &key start1 end1 start2 end2 test key) => Generalized Boolean
;;; ---------------------------------------------------------------------

(defmethod search ((sequence1 cl:null)(sequence2 cl:null) &key (start1 0) end1 (start2 0) end2 (test 'cl:equal) (key 'cl:identity))
  (declare (ignore sequence1 sequence2 start1 end1 start2 end2 test key))
  0)

(defmethod search ((sequence1 cl:null)(sequence2 cl:sequence) &key (start1 0) end1 (start2 0) end2 (test 'cl:equal) (key 'cl:identity)) 
  (declare (ignore sequence1 sequence2 start1 end1 start2 end2 test key))
  0)

(defmethod search ((sequence1 cl:null)(sequence2 wb-seq) &key (start1 0) end1 (start2 0) end2 (test 'cl:equal) (key 'cl:identity)) 
  (declare (ignore sequence1 sequence2 start1 end1 start2 end2 test key))
  0)

(defmethod search ((sequence1 cl:sequence)(sequence2 cl:null) &key (start1 0) end1 (start2 0) end2 (test 'cl:equal) (key 'cl:identity)) 
  (declare (ignore sequence1 sequence2 start1 end1 start2 end2 test key))
  nil)

(defmethod search ((sequence1 cl:sequence)(sequence2 cl:sequence) &key (start1 0) end1 (start2 0) end2 (test 'cl:equal) (key 'cl:identity)) 
  (cl:search sequence1 sequence2 :start1 start1 :start2 start2 :end1 end1 :end2 end2 :test test :key key))

(defmethod search ((sequence1 cl:sequence)(sequence2 wb-seq) &key (start1 0) end1 (start2 0) end2 (test 'cl:equal) (key 'cl:identity)) 
  (cl:search sequence1 (as 'cl:vector sequence2) :start1 start1 :start2 start2 :end1 end1 :end2 end2 :test test :key key))

(defmethod search ((sequence1 wb-seq)(sequence2 cl:null) &key (start1 0) end1 (start2 0) end2 (test 'cl:equal) (key 'cl:identity))
  (declare (ignore start1 start2 end1 end2 test key))
  nil)

(defmethod search ((sequence1 wb-seq)(sequence2 cl:sequence) &key (start1 0) end1 (start2 0) end2 (test 'cl:equal) (key 'cl:identity)) 
  (cl:search (as 'cl:vector sequence1) sequence2 :start1 start1 :start2 start2 :end1 end1 :end2 end2 :test test :key key))

(defmethod search ((sequence1 wb-seq)(sequence2 wb-seq) &key (start1 0) end1 (start2 0) end2 (test 'cl:equal) (key 'cl:identity)) 
  (cl:search (as 'cl:vector sequence1) (as 'cl:vector sequence2) :start1 start1 :start2 start2 :end1 end1 :end2 end2 :test test :key key))

;;; function second 
;;;
;;; (second sequence) => anything
;;; ---------------------------------------------------------------------

(defmethod second ((sequence cl:null))
  (declare (ignore sequence))
  nil)

(defmethod second ((sequence cl:cons))
  (cl:second sequence))

(defmethod second ((sequence cl:sequence))
  (cl:elt sequence 1))

(defmethod second ((sequence wb-seq))
  (fset:@ sequence 1))

;;; function select
;;;
;;; (select sequence indexes) => sequence2
;;; ---------------------------------------------------------------------

;;; null
(defmethod select ((sequence1 cl:null)(sequence2 cl:null)) nil)
(defmethod select ((sequence1 cl:null)(sequence2 cl:cons)) nil)
(defmethod select ((sequence1 cl:null)(sequence2 cl:vector)) (vector))
(defmethod select ((sequence1 cl:null)(sequence2 wb-seq)) (wb-seq))

;;; cons
(defmethod select ((sequence1 cl:cons)(sequence2 cl:null)) nil)

(defmethod select ((sequence1 cl:sequence)(sequence2 cl:cons)) 
  (loop for i in sequence2 collect (cl:elt sequence1 i)))

(defmethod select ((sequence1 wb-seq)(sequence2 cl:cons)) 
  (loop for i in sequence2 collect (fset:@ sequence1 i)))

;;; vector
(defmethod select ((sequence1 cl:null)(sequence2 cl:vector)) nil)

(defmethod select ((sequence1 cl:sequence)(sequence2 cl:vector)) 
  (loop for i across sequence2 collect (cl:elt sequence1 i)))

(defmethod select ((sequence1 wb-seq)(sequence2 cl:vector)) 
  (loop for i across sequence2 collect (fset:@ sequence1 i)))

;;; wb-seq
(defmethod select ((sequence1 cl:null)(sequence2 wb-seq)) nil)

(defmethod select ((sequence1 cl:sequence)(sequence2 wb-seq)) 
  (fset:convert 'cl:list (fset:image (lambda (i)(fset:@ sequence1 i)) sequence2)))

(defmethod select ((sequence1 wb-seq)(sequence2 wb-seq)) 
  (fset:convert 'cl:list (fset:image (lambda (i)(fset:@ sequence1 i)) sequence2)))

;;;sequence 

;;; function sequence
;;;
;;; (sequence &rest elements) => sequence
;;; ---------------------------------------------------------------------

(defun sequence (&rest elements)
  (as 'cl:vector elements))

;;; function sequence?
;;;
;;; (sequence? thing) => Boolean
;;; ---------------------------------------------------------------------

(defmethod sequence? (thing)(declare (ignore thing)) nil)
(defmethod sequence? ((thing cl:null))(declare (ignore thing)) t)
(defmethod sequence? ((thing cl:cons))(declare (ignore thing)) t)
(defmethod sequence? ((thing wb-seq))(declare (ignore thing)) t)

;;; function shuffle  [bounded]
;;;
;;; (shuffle sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod shuffle ((sequence cl:null))(declare (ignore sequence)) nil)
(defmethod shuffle ((sequence cl:cons)) (cl:sort (copy-tree sequence) (lambda (x y)(declare (ignore x y))(cl:elt '(nil t)(random 2)))))
(defmethod shuffle ((sequence cl:vector)) 
  (let ((sequence* (cl:map (type-of sequence) 'cl:identity sequence)))
    (cl:sort sequence*
             (lambda (x y)(declare (ignore x y))(cl:elt '(nil t)(random 2))))))
(defmethod shuffle ((sequence wb-seq)) (fset:sort sequence (lambda (x y)(declare (ignore x y))(cl:elt '(nil t)(random 2)))))

;;;some?  [bounded]

;;; function some?  [bounded]
;;;
;;; (some? predicate sequence &rest sequences) => anything
;;; ---------------------------------------------------------------------

(defun some? (predicate sequence &rest sequences) 
  (let* ((sequences (cons sequence sequences))
         (lens (mapcar 'length sequences))
         (len (apply 'cl:min lens)))
    (block searching
      (loop for i from 0 below len
         do (let* ((args (mapcar (lambda (s)(element s i)) sequences))
                   (result (apply predicate args)))
              (when result (return-from searching result))
              nil)))))

;;; function sort  [bounded]
;;;
;;; (sort sequence predicate &key key) => sequence'
;;; ---------------------------------------------------------------------

(defmethod sort ((sequence cl:null) predicate &key (key 'cl:identity)) 
  (declare (ignore sequence predicate key))
  nil)

(defmethod sort ((sequence cl:sequence) predicate &key (key 'cl:identity)) 
  (let ((sequence* (cl:map (type-of sequence) 'cl:identity sequence)))
    (cl:sort sequence* predicate :key key)))


(defmethod sort ((sequence wb-seq) predicate &key (key 'cl:identity))
  (fset:sort sequence predicate :key key))

;;; function split  [bounded]
;;;
;;; (split sequence sentinel &key key test) => sequence'
;;; ---------------------------------------------------------------------

;;; null
(defmethod split ((sequence cl:null)(sentinel cl:null) &key (key 'cl:identity)(test 'cl:eql))
  (declare (ignore key test))
  nil)

(defmethod split ((sequence cl:null)(sentinel cl:cons) &key (key 'cl:identity)(test 'cl:eql)) 
  (declare (ignore key test))
  nil)

(defmethod split ((sequence cl:null)(sentinel cl:vector) &key (key 'cl:identity)(test 'cl:eql)) 
  (declare (ignore key test))
  nil)

(defmethod split ((sequence cl:null)(sentinel wb-seq) &key (key 'cl:identity)(test 'cl:eql)) 
  (declare (ignore key test))
  nil)

;;; cons
(defmethod split ((sequence cl:cons)(sentinel cl:null) &key (key 'cl:identity)(test 'cl:eql)) 
  (declare (ignore key test))
  (by 1 sequence))

(defmethod split ((sequence cl:cons)(sentinel cl:cons) &key (key 'cl:identity)(test 'cl:eql)) 
  (if (empty? sentinel)
      (by 1 sequence)
      (if (< (length sequence)
             (length sentinel))
          (list sequence)
          (let ((pos (search sentinel sequence :key key :test test)))
            (if pos
                (cons (cl:subseq sequence 0 pos)
                      (split (cl:subseq sequence (+ pos (length sentinel)))
                             sentinel :key key :test test))
                (list sequence))))))

(defmethod split ((sequence cl:cons)(sentinel cl:vector) &key (key 'cl:identity)(test 'cl:eql)) 
  (if (empty? sentinel)
      (by 1 sequence)
      (split sequence (as 'cl:list sentinel) :key key :test test)))

(defmethod split ((sequence cl:cons)(sentinel wb-seq) &key (key 'cl:identity)(test 'cl:eql)) 
  (if (empty? sentinel)
      (by 1 sequence)
      (split sequence (as 'cl:list sentinel) :key key :test test)))

;;; vector
(defmethod split ((sequence cl:vector)(sentinel cl:null) &key (key 'cl:identity)(test 'cl:eql)) 
  (declare (ignore sentinel key test))
  (by 1 sequence))

(defmethod split ((sequence cl:vector)(sentinel cl:cons) &key (key 'cl:identity)(test 'cl:eql)) 
  (split sequence (as 'cl:vector sentinel) :key key :test test))

(defmethod split ((sequence cl:vector)(sentinel cl:vector) &key (key 'cl:identity)(test 'cl:eql)) 
  (if (empty? sentinel)
      (by 1 sequence)
      (if (< (length sequence)
             (length sentinel))
          (if (empty? sequence)
              (cl:vector)
              (cl:vector sequence))
          (let ((pos (search sentinel sequence :key key :test test)))
            (if pos
                (concatenate 'cl:vector
                             (cl:vector (cl:subseq sequence 0 pos))
                             (split (cl:subseq sequence (+ pos (length sentinel)))
                                    sentinel :key key :test test))
                (if (empty? sequence)
                    (cl:vector)
                    (cl:vector sequence)))))))

(defmethod split ((sequence cl:vector)(sentinel wb-seq) &key (key 'cl:identity)(test 'cl:eql)) 
  (split sequence (as 'cl:vector sentinel) :key key :test test))

;;; wb-seq
(defmethod split ((sequence wb-seq)(sentinel cl:null) &key (key 'cl:identity)(test 'cl:eql)) 
  (declare (ignore sentinel key test))
  (by 1 sequence))

(defmethod split ((sequence wb-seq)(sentinel cl:cons) &key (key 'cl:identity)(test 'cl:eql)) 
  (split sequence (as 'wb-seq sentinel) :key key :test test))

(defmethod split ((sequence wb-seq)(sentinel cl:vector) &key (key 'cl:identity)(test 'cl:eql)) 
  (split sequence (as 'wb-seq sentinel) :key key :test test))

(defmethod split ((sequence wb-seq)(sentinel wb-seq) &key (key 'cl:identity)(test 'cl:eql)) 
  (if (empty? sentinel)
      (by 1 sequence)
      (if (< (length sequence)
             (length sentinel))
          (if (empty? sequence)
              (wb-seq)
              (wb-seq sequence))
          (let ((pos (search sentinel sequence :key key :test test)))
            (if pos
                (fset:concat
                 (wb-seq (fset:subseq sequence 0 pos))
                 (split (fset::subseq sequence (+ pos (length sentinel)))
                        sentinel :key key :test test))
                (if (empty? sequence)
                    (wb-seq)
                    (wb-seq sequence)))))))

;;; function subsequence 
;;;
;;; (subsequence sequence start &optional end) => sequence'
;;; ---------------------------------------------------------------------

(defmethod subsequence ((sequence cl:sequence) (start integer) &optional (end nil))
  (let ((end (or end (length sequence))))
    (cl:subseq sequence start end)))

(defmethod subsequence ((sequence wb-seq) (start integer) &optional (end nil))
  (let ((end (or end (length sequence))))
    (fset:subseq sequence start end)))

;;; function substitute
;;;
;;; (substitute new-item old-item sequence &key key test) => sequence'
;;; ---------------------------------------------------------------------

(defmethod substitute (new-item old-item (sequence cl:sequence) &key (test 'cl:eql) (key 'cl:identity))
  (cl:substitute new-item old-item sequence :test test :key key))

(defmethod substitute (new-item old-item (sequence wb-seq) &key (key 'cl:identity))
  (fset:substitute new-item old-item sequence :key key))

;;; function substitute-if
;;;
;;; (substitute-if new-item predicate sequence &key key) => sequence'
;;; ---------------------------------------------------------------------

(defmethod substitute-if (new-item predicate (sequence cl:sequence) &key (key 'cl:identity))
  (cl:substitute-if new-item predicate sequence :key key))

(defmethod substitute-if (new-item predicate (sequence wb-seq) &key (key 'cl:identity))
  (fset:substitute-if new-item predicate sequence :key key))

;;; function substitute-if-not
;;;
;;; (substitute-if-not new-item predicate sequence &key key) => sequence'
;;; ---------------------------------------------------------------------

(defmethod substitute-if-not (new-item predicate (sequence cl:sequence) &key (key 'cl:identity))
  (cl:substitute-if new-item (cl:complement predicate) sequence :key key))

(defmethod substitute-if-not (new-item predicate (sequence wb-seq) &key (key 'cl:identity))
  (fset:substitute-if new-item (cl:complement predicate) sequence :key key))

;;; function suffix-match? [bounded]  
;;;
;;; (suffix-match? sequence suffix &key test key) => Generalized Boolean
;;; ---------------------------------------------------------------------

(defun %general-suffix-match (sequence suffix &key (test 'cl:equal) (key 'cl:identity))
  (or (empty? suffix)
      (let* ((sufflen (length suffix))
             (seqlen (length sequence))
             (suffstart (- seqlen sufflen)))
        (and (<= sufflen seqlen)
             (block searching
               (progn
                 (loop for i from 0 below (length suffix)
                    do (unless (funcall test
                                        (funcall key (element suffix i))
                                        (funcall key (element sequence (+ suffstart i))))
                         (return-from searching nil)))
                 t))))))

(defmethod suffix-match? (sequence suffix &key test key) 
  (declare (ignore sequence suffix test key))
  nil)

(defmethod suffix-match? ((sequence cl:null)(suffix cl:null) &key test key)
  (declare (ignore sequence suffix test key))
  t)

(defmethod suffix-match? ((sequence cl:sequence)(suffix cl:null) &key test key)
  (declare (ignore sequence suffix test key))
  t)

(defmethod suffix-match? ((sequence wb-seq)(suffix cl:null) &key test key)
  (declare (ignore sequence suffix test key))
  t)

(defmethod suffix-match? ((sequence cl:null)(suffix cl:sequence) &key test key)
  (declare (ignore test key))
  (empty? suffix))

(defmethod suffix-match? ((sequence cl:sequence)(suffix cl:sequence) &key (test 'cl:equal) (key 'cl:identity)) 
  (%general-suffix-match sequence suffix :test test :key key))

(defmethod suffix-match? ((sequence wb-seq)(suffix cl:sequence) &key (test 'cl:equal) (key 'cl:identity)) 
  (%general-suffix-match sequence suffix :test test :key key))


(defmethod suffix-match? ((sequence cl:null)(suffix wb-seq) &key test key)
  (declare (ignore test key))
  (empty? suffix))

(defmethod suffix-match? ((sequence cl:sequence)(suffix wb-seq) &key (test 'cl:equal) (key 'cl:identity)) 
  (%general-suffix-match sequence suffix :test test :key key))

(defmethod suffix-match? ((sequence wb-seq)(suffix wb-seq) &key (test 'cl:equal) (key 'cl:identity)) 
  (%general-suffix-match sequence suffix :test test :key key))

;;; function tail
;;;
;;; (tail sequence) => anything
;;; ---------------------------------------------------------------------

(defmethod tail ((sequence cl:null))
  nil)

(defmethod tail ((sequence cl:sequence))
  (cl:subseq sequence 1))

(defmethod tail ((sequence wb-seq))
  (fset:subseq sequence 1))

;;; function tails
;;;
;;; (tails sequence &key by) => list
;;; ---------------------------------------------------------------------

(defun %general-tails-by (sequence step)
  (let ((indexes (range 0 (length sequence) :by step)))
    (loop for i in indexes collect (subsequence sequence i))))

(defmethod tails ((sequence cl:null) &key by) 
  (declare (ignore sequence by))
  nil)

(defmethod tails ((sequence cl:cons) &key (by 1)) 
  (%general-tails-by sequence by))

(defmethod tails ((sequence cl:vector) &key (by 1)) 
  (%general-tails-by sequence by))

(defmethod tails ((sequence wb-seq) &key (by 1)) 
  (%general-tails-by sequence by))

;;; function take
;;;
;;; (take n sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod take ((n (eql 0))(sequence cl:null)) (declare (ignore n sequence)) nil)
(defmethod take ((n integer)(sequence cl:null)) (error "Can't take ~s items from NIL" n))
(defmethod take ((n integer)(sequence cl:sequence))(cl:subseq sequence 0 (cl:min n (length sequence))))
(defmethod take ((n integer)(sequence wb-seq))(fset:subseq sequence 0 (cl:min n (length sequence))))

;;; function take-by
;;;
;;; (take-by m n sequence) => sequences
;;; ---------------------------------------------------------------------

(defmethod take-by ((m (eql 0))(n (eql 0))(sequence cl:null)) nil)
(defmethod take-by ((m integer)(n integer)(sequence cl:null)) (error "Can't take-by ~s ~s items from NIL" m n))

(defmethod take-by ((m integer)(n integer)(sequence cl:sequence))
  (mapcar (lambda (s)(take m s))
          (tails sequence :by n)))

(defmethod take-by ((m integer)(n integer)(sequence wb-seq))
  (mapcar (lambda (s)(take m s))
          (tails sequence :by n)))

;;; function take-while
;;;
;;; (take-while predicate sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod take-while (predicate (sequence cl:null))(declare (ignore predicate)) nil)

(defmethod take-while (predicate (sequence cl:sequence)) 
  (let ((pos (position-if-not predicate sequence)))
    (if pos
        (subsequence sequence 0 pos)
        sequence)))

(defmethod take-while (predicate (sequence wb-seq)) 
  (let ((pos (position-if-not predicate sequence)))
    (if pos
        (subsequence sequence 0 pos)
        sequence)))

;;; function unzip
;;;
;;; (unzip sequence1) => sequence2, sequence3
;;; ---------------------------------------------------------------------

(defmethod unzip ((sequence cl:null))(values nil nil))

(defmethod unzip ((sequence cl:sequence))
  (dispose sequence 'left 'right))

(defmethod unzip ((sequence wb-seq))
  (dispose sequence 'left 'right))

;;; function wb-seq?
;;;
;;; (wb-seq? thing) => Boolean
;;; ---------------------------------------------------------------------

(defmethod wb-seq? (x)(declare (ignore x)) nil)
(defmethod wb-seq? ((x fset:seq)) t)

;;; function zip
;;;
;;; (zip sequence1 sequence2) => sequence3
;;; ---------------------------------------------------------------------

(defmethod zip ((sequence1 cl:null) (sequence2 cl:null)) nil)
(defmethod zip ((sequence1 cl:null) (sequence2 cl:cons)) nil)
(defmethod zip ((sequence1 cl:null) (sequence2 cl:vector)) nil)
(defmethod zip ((sequence1 cl:null) (sequence2 wb-seq)) nil)

(defmethod zip ((sequence1 cl:cons) (sequence2 cl:null)) nil)
(defmethod zip ((sequence1 cl:cons) (sequence2 cl:cons)) (mapcar 'cl:cons sequence1 sequence2))

(defmethod zip ((sequence1 cl:cons) (sequence2 cl:vector)) 
  (loop for x in sequence1 for y across sequence2 collect (cons x y)))

(defmethod zip ((sequence1 cl:cons) (sequence2 wb-seq)) 
  (loop for x in sequence1 for y from 0 below (fset:size sequence2) 
     collect (cons x (fset:@ sequence2 y))))

(defmethod zip ((sequence1 cl:vector) (sequence2 cl:null)) (vector))

(defmethod zip ((sequence1 cl:vector) (sequence2 cl:cons))
  (as 'cl:vector 
      (loop for x across sequence1 for y in sequence2 collect (cons x y))))

(defmethod zip ((sequence1 cl:vector) (sequence2 cl:vector))
  (as 'cl:vector 
      (loop for x across sequence1 for y across sequence2 collect (cons x y))))

(defmethod zip ((sequence1 cl:vector) (sequence2 wb-seq))
  (as 'cl:vector
      (loop for x across sequence1 for y from 0 below (fset:size sequence2) 
         collect (cons x (fset:@ sequence2 y)))))

(defmethod zip ((sequence1 cl:string) (sequence2 cl:null)) "")

(defmethod zip ((sequence1 wb-seq) (sequence2 cl:null)) (wb-seq))

(defmethod zip ((sequence1 wb-seq) (sequence2 cl:cons)) 
  (as 'wb-seq
   (loop for x from 0 below (fset:size sequence1) for y in sequence2
      collect (cons (fset:@ sequence1 x) y))))

(defmethod zip ((sequence1 wb-seq) (sequence2 cl:vector)) 
  (as 'wb-seq
      (loop for x from 0 below (fset:size sequence1) for y across sequence2
         collect (cons (fset:@ sequence1 x) y))))

(defmethod zip ((sequence1 wb-seq) (sequence2 wb-seq)) 
  (as 'wb-seq
      (loop for x from 0 below (fset:size sequence1) for y from 0 below (fset:size sequence2)
         collect (cons (fset:@ sequence1 x)
                       (fset:@ sequence2 y)))))

