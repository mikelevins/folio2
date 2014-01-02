;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       series functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.series)

;;; ---------------------------------------------------------------------
;;; conversions: as
;;; ---------------------------------------------------------------------

(defmethod as ((type (eql 'foundation-series)) (value cl:sequence) &key &allow-other-keys)
  (series:scan value))

(defmethod as ((type (eql 'series)) (value cl:sequence) &key &allow-other-keys)
  (series:scan value))

(defmethod as ((type (eql 'cl:sequence)) (value foundation-series) &key &allow-other-keys)
  (series:collect value))

(defmethod as ((type (eql 'cl:list)) (value foundation-series) &key &allow-other-keys)
  (cl:coerce (series:collect value) 'cl:list))

;;; ---------------------------------------------------------------------
;;; copying
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; constructions: make
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; sequence functions
;;; ---------------------------------------------------------------------
;;; series extends most sequence functions with versions that operate
;;; on series. The exception is that sequence functions that are not
;;; defined on unbounded sequences are not implemented for series,
;;; because series are allowed to be unbounded. As a simple example,
;;; the sequence function LAST is not implemented for series because
;;; it makes sense only when applied to a bounded sequence (i.e. a
;;; sequence that has a last item). 
;;; 
;;; One other case where sequence functions are not extended to work with
;;; series occurs when any implementation would necessarily be
;;; prohibitively inefficient. An example of this case is the sequence
;;; function REMOVE-DUPLICATES. It is possible to process an unbounded
;;; series in such a way that distinguished values are removed from
;;; it as they are generated, but implementing REMOVE-DUPLICATES would
;;; require checking each value against a collection of previously-seen
;;; values--a collection that grows without bound. Since there is no
;;; reasonable way to do this efficiently, there is no series version of
;;; REMOVE-DUPLICATES.

;;; function add-first
;;;
;;; (add-first x series) => series'
;;; ---------------------------------------------------------------------

(defmethod add-first (x (series foundation-series))
  (series:catenate (series x) series))

;;; function by
;;;
;;; (by n series) => series'
;;; ---------------------------------------------------------------------

(defmethod by ((n integer)(series foundation-series))
  (scan (multiple-value-list (series:chunk n n series))))

;;; function dispose
;;;
;;; (dispose series &rest fn1 fn2 fn3...) => ser1 ser2 ser3...
;;; ---------------------------------------------------------------------
;;; returns a number of series equal to the number of FUNCTIONS.
;;; the elements of SER1 are the results of (fn1 x), where x is an element
;;; of SERIES; the elements of SER2 are the results of (fn2 x); and so on

(defmethod dispose ((series foundation-series) &rest fns)
  (apply #'values (cl:map 'cl:list
                          (lambda (fn)(series:map-fn t fn series))
                          fns)))

;;; function drop
;;;
;;; (drop n series) => series'
;;; ---------------------------------------------------------------------

(defmethod drop ((n integer)(series foundation-series))
    (series:subseries series n))

;;; function drop-while
;;;
;;; (drop-while predicate series) => series'
;;; ---------------------------------------------------------------------

(defmethod drop-while (predicate (series foundation-series))
  (series:subseries series
                    (1+ (series:collect-last (series:choose (series:until-if 'not (series:map-fn t predicate series))
                                                            (range-from 0))))))

;;; function element
;;;
;;; (element series n) => anything
;;; ---------------------------------------------------------------------

(defmethod element ((series foundation-series)(n integer)) 
  (series:collect-nth n series))

;;; function filter
;;;
;;; (filter predicate series) => series'
;;; ---------------------------------------------------------------------

(defmethod filter (predicate (series foundation-series)) 
  (series:choose (series:map-fn t predicate series)
                 series))

;;; function first
;;;
;;; (first sequence) => anything
;;; ---------------------------------------------------------------------

(defmethod first ((series foundation-series))
  (series:collect-nth 0 series))

;;; function head
;;;
;;; (head sequence) => anything
;;; ---------------------------------------------------------------------

(defmethod head ((series foundation-series))
  (series:collect-nth 0 series))

;;; function image
;;;
;;; (image function series) => series'
;;; ---------------------------------------------------------------------

(defmethod image (function (series foundation-series))
  (series:map-fn t function series))

;;; function indexes
;;;
;;; (indexes function sequence) => sequence
;;; ---------------------------------------------------------------------

(defmethod indexes ((s foundation-series))
  (series:choose (series:positions (series:map-fn 'boolean (constantly t) s))))

;;; interleave
;;; function interleave
;;;
;;; (interleave series1 series2) => series3
;;; ---------------------------------------------------------------------

(defun %make-toggle (&rest args)
  (let ((state t))
    (lambda (&rest args)
      (declare (ignore args))
      (setf state (not state)))))

(defmethod interleave ((sequence1 foundation-series)(sequence2 cl:null))(series))

(defmethod interleave ((sequence1 foundation-series)(sequence2 cl:cons))
  (interleave sequence1 (scan sequence2)))

(defmethod interleave ((sequence1 foundation-series)(sequence2 cl:vector))
  (interleave sequence1 (scan sequence2)))

(defmethod interleave ((sequence1 foundation-series)(sequence2 wb-seq)) 
  (interleave sequence1 (scan (fset:convert 'cl:vector sequence2))))

(defmethod interleave ((sequence1 cl:null)(sequence2 foundation-series))(series))

(defmethod interleave ((sequence1 cl:cons)(sequence2 foundation-series))
  (interleave (scan sequence1) sequence2))

(defmethod interleave ((sequence1 cl:vector)(sequence2 foundation-series))
  (interleave (scan sequence1) sequence2))

(defmethod interleave ((sequence1 wb-seq)(sequence2 foundation-series)) 
  (interleave (scan (fset:convert 'cl:vector sequence1)) sequence2))

(defmethod interleave ((sequence1 foundation-series)(sequence2 foundation-series)) 
  (series:mingle sequence1 sequence2 (%make-toggle)))

;;; function interpose
;;;
;;; (interpose item sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod interpose (item (sequence foundation-series)) 
  (let ((items (repeat item)))
    (interleave sequence items)))

;;; function partition 
;;;
;;; (partition predicate sequence1) => sequence2, sequence3
;;; ---------------------------------------------------------------------

(defmethod partition (predicate (series foundation-series))
  (series:split-if series predicate))

;;; function prefix-match?
;;;
;;; (prefix-match? prefix sequence &key test key) => Generalized Boolean
;;; ---------------------------------------------------------------------
;;; NOTE: the case where prefix is a series is undefined because
;;; there is no practical way to determine whether an unbounded
;;; prefix matches an unbounded series

(defmethod prefix-match? ((prefix cl:null)(sequence foundation-series) &key test key)
  (declare (ignore test key))
  t)

(defmethod prefix-match? ((prefix cl:sequence)(sequence foundation-series) &key (test 'cl:eql) (key 'cl:identity)) 
  (series:collect-and (series:map-fn t
                                     (lambda (x y)(funcall test (funcall key x)(funcall key y)))
                                     (scan prefix) sequence)))

(defmethod prefix-match? ((prefix wb-seq)(sequence foundation-series) &key (test 'cl:eql) (key 'cl:identity)) 
  (series:collect-and (series:map-fn t
                                     (lambda (x y)(funcall test (funcall key x)(funcall key y)))
                                     (scan (fset:convert 'cl:vector prefix)) sequence)))

;;; function remove
;;;
;;; (remove item sequence &key test start end key) => anything
;;; ---------------------------------------------------------------------

(defmethod remove (item (sequence foundation-series) &key (test 'eql) (start 0) (end nil endp) (key 'cl:identity))
  (let* ((segment (if endp
                      (series:subseries sequence start end)
                      (series:subseries sequence start)))
         (filtered-segment (series:choose (series:map-fn t 
                                                         (lambda (s)
                                                           (funcall (complement test)
                                                                    (funcall key item)
                                                                    (funcall key s))) 
                                                         segment)
                                          segment))
         (pre (series:subseries sequence 0 start)))
    (if endp
        (series:catenate pre filtered-segment (series:subseries sequence end))
        (series:catenate pre filtered-segment))))

;;; function remove-if
;;;
;;; (remove-if predicate sequence &key start end key) => anything
;;; ---------------------------------------------------------------------

(defmethod remove-if (predicate (sequence foundation-series) &key  (start 0) (end nil endp) (key 'cl:identity))
  (let* ((segment (if endp
                      (series:subseries sequence start end)
                      (series:subseries sequence start)))
         (filtered-segment (series:choose (series:map-fn t 
                                                         (lambda (s)
                                                           (funcall (complement predicate)
                                                                    (funcall key s))) 
                                                         segment)
                                          segment))
         (pre (series:subseries sequence 0 start)))
    (if endp
        (series:catenate pre filtered-segment (series:subseries sequence end))
        (series:catenate pre filtered-segment))))

;;; function remove-if-not
;;;
;;; (remove-if-not predicate sequence &key start end key) => anything
;;; ---------------------------------------------------------------------

(defmethod remove-if-not (predicate (sequence foundation-series) &key  (start 0) (end nil endp) (key 'cl:identity))
  (let* ((segment (if endp
                      (series:subseries sequence start end)
                      (series:subseries sequence start)))
         (filtered-segment (series:choose (series:map-fn t 
                                                         (lambda (s)
                                                           (funcall predicate (funcall key s))) 
                                                         segment)
                                          segment))
         (pre (series:subseries sequence 0 start)))
    (if endp
        (series:catenate pre filtered-segment (series:subseries sequence end))
        (series:catenate pre filtered-segment))))

;;; function rest 
;;;
;;; (rest sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod rest ((sequence foundation-series))
  (series:subseries sequence 1))

;;; function second 
;;;
;;; (second sequence) => anything
;;; ---------------------------------------------------------------------

(defmethod second ((sequence foundation-series))
  (series:collect-nth 1 sequence))

;;; function select
;;;
;;; (select sequence1 sequence2) => sequence3
;;; ---------------------------------------------------------------------

(defmethod select ((sequence1 cl:null)(sequence2 foundation-series))(series))

(defmethod select ((sequence1 cl:sequence)(sequence2 foundation-series))
  (series:choose (series:mask (scan sequence1))
                 sequence2))


;;; function subsequence
;;;
;;; (subsequence sequence start &optional end) => sequence'
;;; ---------------------------------------------------------------------

(defmethod subsequence ((sequence foundation-series) (start integer) &optional (end nil))
  (series:subseries sequence start end))

;;; function substitute
;;;
;;; (substitute new-item old-item sequence &key key) => sequence'
;;; ---------------------------------------------------------------------

(defmethod substitute (new-item old-item (sequence foundation-series) &key (test 'cl:eql)(key 'cl:identity))
  (series:map-fn t
                 (lambda (s)(if (funcall test (funcall key old-item)(funcall key s))
                                new-item
                                s)) 
                 sequence))

;;; function substitute-if
;;;
;;; (substitute-if new-item predicate sequence &key key) => sequence'
;;; ---------------------------------------------------------------------

(defmethod substitute-if (new-item predicate (sequence foundation-series) &key (key 'cl:identity))
  (series:map-fn t
                 (lambda (s)(if (funcall predicate (funcall key s))
                                new-item
                                s)) 
                 sequence))

;;; function substitute-if-not
;;;
;;; (substitute-if-not new-item predicate sequence &key key) => sequence'
;;; ---------------------------------------------------------------------

(defmethod substitute-if-not (new-item predicate (sequence foundation-series) &key (key 'cl:identity))
  (series:map-fn t
                 (lambda (s)(if (funcall predicate (funcall key s))
                                s
                                new-item)) 
                 sequence))

;;; function tail 
;;;
;;; (tail sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod tail ((sequence foundation-series))
  (series:subseries sequence 1))

;;; function tails
;;;
;;; (tails sequence &key by) => list
;;; ---------------------------------------------------------------------

(defmethod tails ((sequence foundation-series) &key (by 1)) 
  (series:mapping ((index (series:scan-range :by by)))
                  (series:subseries sequence index)))

;;; function take
;;;
;;; (take n series) => sequence
;;; ---------------------------------------------------------------------

(defmethod take ((n integer)(series foundation-series))
  (series:subseries series 0 n))

;;; function take-by
;;;
;;; (take-by m n sequence) => sequences
;;; ---------------------------------------------------------------------

(defmethod take-by ((m integer)(n integer)(sequence foundation-series))
  (series:mapping ((index (series:scan-range :by n)))
                  (series:subseries sequence index (+ index m))))

;;; function take-while
;;;
;;; (take-while predicate series) => series'
;;; ---------------------------------------------------------------------

(defmethod take-while (predicate (series foundation-series))
  (series:until-if (lambda (x)(not (funcall predicate x)))
                   series))

;;; function unzip
;;;
;;; (unzip sequence1) => sequence2, sequence3
;;; ---------------------------------------------------------------------

(defmethod unzip ((sequence foundation-series))
  (dispose sequence 'left 'right))

;;; function zip
;;;
;;; (zip sequence1 sequence2) => sequence3
;;; ---------------------------------------------------------------------

(defmethod zip ((sequence1 cl:null) (sequence2 foundation-series)) nil)

(defmethod zip ((sequence1 cl:sequence) (sequence2 foundation-series)) 
  (zip sequence1
       (series:collect (take (length sequence1) sequence2))))

(defmethod zip ((sequence1 wb-seq) (sequence2 foundation-series)) 
  (zip sequence1
       (series:collect (take (length sequence1) sequence2))))

(defmethod zip ((sequence1 foundation-series) (sequence2 cl:sequence)) 
  (series:map-fn t 'pair
                 sequence1 (scan sequence2)))

(defmethod zip ((sequence1 foundation-series) (sequence2 wb-seq)) 
  (series:map-fn t 'pair
                 sequence1 (scan (fset:convert 'cl:vector sequence2))))

(defmethod zip ((sequence1 foundation-series) (sequence2 foundation-series)) 
  (series:map-fn t 'pair
                 sequence1 sequence2))

;;; ---------------------------------------------------------------------
;;; series functions
;;; ---------------------------------------------------------------------

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

;;; function range-from
;;;
;;; (range start end &key by) => sequence
;;; ---------------------------------------------------------------------

(defmethod range-from ((start integer) &key (by 1))
  (series:scan-range :from start :by by))

;;; scan

(defmethod scan ((seq cl:sequence))
  (series:scan seq))

(defmethod scan ((seq wb-seq))
  (series:scan (as 'cl:vector seq)))

(defmethod scan ((seq foundation-series)) seq)

;;; function series
;;;
;;; (series &rest elements) => series
;;; ---------------------------------------------------------------------

(defun series (&rest elements)
  (series:scan elements))


;;; function series?
;;;
;;; (series? thing) => Boolean
;;; ---------------------------------------------------------------------

(defmethod series? (thing)(declare (ignore thing)) nil)
(defmethod series? ((thing foundation-series)) t)

