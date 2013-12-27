;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          generic-functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       common generic functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.common)


;;; >
;;;
;;; (> thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:>, providing an extensible generic version

(defgeneric > (thing1 thing2 &rest more-things))


;;; >=
;;;
;;; (>= thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:>=, providing an extensible generic version

(defgeneric >= (thing1 thing2 &rest more-things))


;;; <
;;;
;;; (< thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:<, providing an extensible generic version

(defgeneric < (thing1 thing2 &rest more-things))


;;; <=
;;;
;;; (<= thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:<=, providing an extensible generic version

(defgeneric <= (thing1 thing2 &rest more-things))


;;; add-first
;;;
;;; (add-first x seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence that contains X prepended to the elements of
;;; SEQ

(defgeneric add-first (x seq))


;;; add-last
;;;
;;; (add-last seq x) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence that contains X appended after the elements of
;;; SEQ

(defgeneric add-last (seq x))


;;; adjoin 
;;;
;;; (adjoin item set1) => set2
;;; ---------------------------------------------------------------------
;;; returns a new set that contains X prepended to the elements of
;;; SET

(defgeneric adjoin (item set &key test key))


;;; alist->plist
;;;
;;; (alist->plist alist) => plist
;;; ---------------------------------------------------------------------
;;; returns a plist--a list of alternating key/value elements
;;; constructed from the keys and values of the alist

(defgeneric alist->plist (thing))


;;; any
;;;
;;; (any seq) => anything
;;; ---------------------------------------------------------------------
;;; returns an arbitrary element of seq. any chooses the element
;;; randomly

(defgeneric any (seq))


;;; apply
;;;
;;; (apply fn seq &rest more-seqs) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:apply, providing an extensible generic version.

(defgeneric apply (fn seq))


;;; as
;;;
;;; (as type val) => an instance of type
;;; ---------------------------------------------------------------------
;;; returns a value equivalent to VAL whose type is TYPE

(defgeneric as (type val &key &allow-other-keys))


;;; associate
;;;
;;; (associate table1 key val &key (test 'equal) &allow-other-keys) => table2
;;; ---------------------------------------------------------------------
;;; returns a table TABLE2 that contains the elements of TABLE1, plus the
;;; key/value pair of KEY and VAL. If TABLE1 contains KEY then its value is
;;; replaced by VAL in TABLE2.

(defgeneric associate (table key val &key test &allow-other-keys))


;;; by
;;;
;;; (by n seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence of sequences constructed by taking the elements of
;;; SEQ N at a time

(defgeneric by (n seq))


;;; characters 
;;;
;;; (characters input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the characters provided by INPUT-STREAM

(defgeneric characters (input-stream &key &allow-other-keys))



;;; combined-type
;;;
;;; (combined-type val1 val2) => a type designator
;;; ---------------------------------------------------------------------
;;; returns a type designator suitable for representing a value
;;; contructed by merging VAL1 and VAL2. used, for example, to
;;; determine the output type of an operation that concatenates
;;; sequences of different types. you aren't likely to need to
;;; call combined-type directly, nor to add methods, unless
;;; you're adding support for new composite types to folio

(defgeneric combined-type (val1 val2))


;;; concat2
;;;
;;; (concat2 seq1 seq2) => seq3
;;; ---------------------------------------------------------------------
;;; returns a sequence containing all the elements of SEQ1 followed by
;;; all the elements of SEQ2.

(defgeneric concat2 (seq1 seq2))


;;; contains-key?
;;;
;;; (contains-key? map key &key (test 'equal)) => a generalized boolean
;;; ---------------------------------------------------------------------
;;; returns a generalized boolean that is true if MAP contains a key
;;; that is equivalent to KEY in the sense of TEST, and returns a
;;; false value otherwise. support for the TEST keyword depends on the
;;; representation of MAP.

(defgeneric contains-key? (map key &key &allow-other-keys))


;;; contains-value?
;;;
;;; (contains-value? map val &key (test 'equal)) => a generalized boolean
;;; ---------------------------------------------------------------------
;;; returns a generalized boolean that is true if MAP contains a value
;;; that is equivalent to VAL in the sense of TEST, and returns a
;;; false value otherwise

(defgeneric contains-value? (map key &key &allow-other-keys))


;;; difference
;;;
;;; (difference set1 set2) => set3
;;; ---------------------------------------------------------------------
;;; returns a new set that contains the elements of SET1 that are
;;; not in SET2

(defgeneric difference (set1 set2 &key key test)) 


;;; drop
;;;
;;; (drop n seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence containing the elements of SEQ after the first
;;; N elements have been removed

(defgeneric drop (n seq))


;;; drop-while
;;;
;;; (drop-while test seq) => seq'
;;; ---------------------------------------------------------------------

(defgeneric drop-while (test seq))


;;; element
;;;
;;; (element seq n) => 
;;; ---------------------------------------------------------------------
;;; returns the element of SEQ at index N

(defgeneric element (seq n))


;;; empty?
;;;
;;; (empty? seq) => a boolean
;;; ---------------------------------------------------------------------
;;; returns true if SEQ contains no elements, and false otherwise

(defgeneric empty? (seq))


;;; every?
;;;
;;; (every? test seq) => a boolean
;;; ---------------------------------------------------------------------
;;; returns true if SEQ contains no elements, and false otherwise

(defgeneric every? (test seq))


;;; filter
;;;
;;; (filter test seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns those elements of SEQ for which TEST returns true

(defgeneric filter (test seq))


;;; find
;;;
;;; (find item seq &key from-end test test-not start end key) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:find, providing an extensible generic version

(defgeneric find (item seq &key from-end test test-not start end key &allow-other-keys))


;;; first
;;;
;;; (first seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the first element of SEQ

(defgeneric first (seq))


;;; function?
;;;
;;; (function? thing) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if THING is a function (that is, it's a function,
;;; but not a generic function or method)

(defgeneric function? (thing))


;;; functional?
;;;
;;; (functional? thing) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if THING is a function, a generic function,
;;; or a method

(defgeneric functional? (thing))


;;; generate
;;;
;;; (generate seq) => generator
;;; ---------------------------------------------------------------------
;;; returns a generator that returns one element at a time from the
;;; input SEQ

(defgeneric generate (seq))


;;; generic-function?
;;;
;;; (generic-function? thing) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if THING is a generic function

(defgeneric generic-function? (thing))


;;; get-key
;;;
;;; (get-key map key &key (test 'equal)(default nil)) => anything
;;; ---------------------------------------------------------------------
;;; returns the value stored on KEY in MAP if KEY is present; returns
;;; DEFAULT otherwise. Matches KEY using TEST

(defgeneric get-key (map key &key &allow-other-keys))


;;; image
;;;
;;; (image fn seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence containing the values produced by applying
;;; FN to each element of SEQ

(defgeneric image (fn seq))


;;; input-stream? 
;;;
;;; (input-stream? thing) => boolean
;;; ---------------------------------------------------------------------
;;; returns true if THING is an input-stream

(defgeneric input-stream? (thing))


;;; interleave
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


;;; interpose
;;;
;;; (interpose cupola seq1) => seq2
;;; ---------------------------------------------------------------------
;;; returns a new sequence SEQ2 that contains the elements of SEQ1, but
;;; with CUPOLA inserted between them

(defgeneric interpose (cupola seq))


;;; intersection
;;;
;;; (intersection set1 set2) => set3
;;; ---------------------------------------------------------------------
;;; returns a sequence that contains those elements that appear in both 
;;; SET1 and SET2

(defgeneric intersection (set1 set2 &key key test))


;;; join
;;;
;;; (join cupola seqs) => seq
;;; ---------------------------------------------------------------------
;;; joins SEQS in the manner of binary-join, below. to add support for joining 
;;; new sequence types, add methods to binary-join

(defgeneric join (cupola seq))


;;; binary-join
;;;
;;; (join cupola seq1 seq2) => seq3
;;; ---------------------------------------------------------------------
;;; concatenates SEQ1 and SEQ2 to form the new sequence SEQ3, with CUPOLA
;;; inserted between the elements of SEQ1 and SEQ2

(defgeneric binary-join (cupola seq1 seq2))


;;; keys
;;;
;;; (keys map) => a list of keys
;;; ---------------------------------------------------------------------
;;; returns a sequence of all the keys appearing in MAP. If MAP is a
;;; sequence then KEYS returns a sequence of indexes. If MAP is a
;;; series then KEYS returns a series of indexes.

(defgeneric keys (map))


;;; last
;;;
;;; (last seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the last element of SEQ

(defgeneric last (seq))


;;; left
;;;
;;; (left p) => anything
;;; ---------------------------------------------------------------------
;;; returns the left element (i.e. the CAR) of the pair

(defgeneric left (pair))


;;; length
;;;
;;; (length seq) => an integer
;;; ---------------------------------------------------------------------
;;; returns a count of the elements in SEQ

(defgeneric length (seq))


;;; lines 
;;;
;;; (lines input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the lines provided by INPUT-STREAM

(defgeneric lines (input-stream))


;;; make
;;;
;;; (make type &rest initargs &key &allow-other-keys) => a value
;;; ---------------------------------------------------------------------
;;; returns a new value of the indicated type, initialized with
;;; parameters supplied by initargs

(defgeneric make (type &rest initargs &key &allow-other-keys))


;;; match-prefix?
;;;
;;; (match-prefix? pref seq &key (test 'equal)) => boolean
;;; ---------------------------------------------------------------------
;;; returns true if the elements of PREF match element-wise the first
;;; several elements of SEQ, using TEST as the matching criterion;
;;; otherwise, returns false. The number of elements tested is equal
;;; to the count of elements in PREF. PREF and SEQ need not be of the
;;; same type.

(defgeneric match-prefix? (pref seq &key test))


;;; match-suffix?
;;;
;;; (match-suffix? seq suff &key (test 'equal)) => boolean
;;; ---------------------------------------------------------------------
;;; returns true if the elements of SUFF match element-wise the last
;;; several elements of SEQ, using TEST as the matching criterion;
;;; otherwise, returns false. The number of elements tested is equal
;;; to the count of elements in SUFF. SUFF and SEQ need not be of the
;;; same type.

(defgeneric match-suffix? (seq suff &key test))


;;; method?
;;;
;;; (method? thing) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if THING is a method

(defgeneric method? (thing))



;;; objects 
;;;
;;; (objects input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the Lisp objects provided by INPUT-STREAM

(defgeneric objects (input-stream))


;;; octets 
;;;
;;; (octets input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the octets provided by INPUT-STREAM

(defgeneric octets (input-stream))


;;; output-stream? 
;;;
;;; (output-stream? thing) => boolean
;;; ---------------------------------------------------------------------
;;; returns true if THING is an output-stream

(defgeneric output-stream? (thing))


;;; (pair a b) => Pair
;;; ---------------------------------------------------------------------
;;; returns a pair whose left element is a and whose right element is b

(defgeneric pair (a b))


;;; partition
;;;
;;; (partition seq &rest fn1 fn2 fn3...) => seq1 seq2 seq3...
;;; ---------------------------------------------------------------------
;;; returns a number of sequences equal to the number of FUNCTIONS.
;;; the elements of SEQ1 are produced by applying FN1 to each element of
;;; SEQ; the elements of SEQ2 are produced by applying FN2 to each 
;;; element of SEQ; and so on

(defgeneric partition (seq &rest fns))

;;; penult
;;;
;;; (penult seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the last-but-one element of seq

(defgeneric penult (seq))


;;; position
;;;
;;; (position item seq &key from-end test test-not start end key) => integer | nil
;;; ---------------------------------------------------------------------
;;; returns the index of ITEM in SEQ, or nil if it's not found

(defgeneric position (item sequence &key from-end test test-not start end key &allow-other-keys))


;;; position-if
;;;
;;; (position-if seq &rest fn1 fn2 fn3...) => seq1 seq2 seq3...
;;; ---------------------------------------------------------------------
;;; returns a number of sequences equal to the number of FUNCTIONS.
;;; the elements of SEQ1 are produced by applying FN1 to each element of
;;; SEQ; the elements of SEQ2 are produced by applying FN2 to each 
;;; element of SEQ; and so on

(defgeneric position-if (test sequence &key from-end start end key &allow-other-keys))


;;; put-key
;;;
;;; (put-key map1 key val (test 'equal)) => map2
;;; ---------------------------------------------------------------------
;;; returns a new map that contains the key/value pairs from MAP1, but
;;; with the pair (KEY . VAL) added. If KEY appears in MAP1 then
;;; its associated value is replaced by VAL in MAP2. Keys are compared
;;; using TEST.

(defgeneric put-key (map key val &key &allow-other-keys))


;;; remove
;;;
;;; (remove item seq ) => cycling series
;;; ---------------------------------------------------------------------
;;; returns an infinitely-repeating sequence of VAL.

(defgeneric remove (test seq &key from-end test test-not start end count key &allow-other-keys))


;;; rest
;;;
;;; (rest seq) => anything
;;; ---------------------------------------------------------------------
;;; returns all but the firt element of SEQ

(defgeneric rest (seq))


;;; reverse
;;;
;;; (reverse SEQ) => seq'
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ in reverse order

(defgeneric reverse (seq))


;;; right
;;;
;;; (right p) => anything
;;; ---------------------------------------------------------------------
;;; returns the right element (i.e. the CDR) of the pair

(defgeneric right (pair))


;;; scan
;;;
;;; (scan seq) => a series
;;; ---------------------------------------------------------------------
;;; returns a series equivalent to SEQ

(defgeneric scan (seq))


;;; scan-image
;;;
;;; (scan-image fn seq) => a series
;;; ---------------------------------------------------------------------
;;; returns a series equivalent to (map FN SEQ)

(defgeneric scan-image (fn seq))


;;; second
;;;
;;; (second seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the second element of SEQ

(defgeneric second (seq))


;;; select
;;;
;;; (select seq1 indexes) => seq2
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ1 at the indexes given by the sequence
;;; INDEXES

(defgeneric select (seq indexes))


;;; set?
;;;
;;; (set? s) => boolean
;;; ---------------------------------------------------------------------

(defgeneric set? (s))


;;; shuffle
;;;
;;; (shuffle seq1) => seq2
;;; ---------------------------------------------------------------------
;;; returns a new sequence with the same elements as SEQ1, but
;;; in random order

(defgeneric shuffle (p))


;;; some?
;;;
;;; (some? test seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the first element of SEQ for which TEST returns true, or
;;; nil otherwise

(defgeneric some? (test seq))


;;; sort
;;;
;;; (sort sequence &key (test '<)) => sequence'
;;; ---------------------------------------------------------------------
;;; shadows cl:sort, providing a non-destructive extensible generic version
;;; pair
;;;

(defgeneric sort (sequence &key test))


;;; split
;;;
;;; (split seq1 subseq) => seq2
;;; ---------------------------------------------------------------------
;;; returns a sequence of sequences. the output sequences are proper
;;; subsequences of SEQ1, obtained by splitting SEQ1 at occurrences
;;; of SUBSEQ. SUBSEQ does not appear in the output sequences. TEST,
;;; whose default value is equal, is used to match occurrences of
;;; SUBSEQ.

(defgeneric split (seq subseq &key test))


;;; subsequence
;;;
;;; (subsequence seq start &optional end) => seq2
;;; ---------------------------------------------------------------------
;;; returns a new sequence containing the elements of SEQ starting with
;;; index START. If END is given, the last element of the new sequence is
;;; the element just before index END; otherwise, it is the last element
;;; of SEQ.

(defgeneric subsequence (seq start &optional end))


;;; subset?
;;;
;;; (subset? set1 set2) => boolean
;;; ---------------------------------------------------------------------
;;; returns true if every element of SET1 also appears in SET2

(defgeneric subset? (set1 set2))


;;; tails
;;;
;;; (tails seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence of sequences beginning with SEQ, followed by
;;; the tail of SEQ, then the tail of the tail of SEQ, and so on,
;;; ending with the last non-empty tail

(defgeneric tails (seq))


;;; take
;;;
;;; (take n seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence containing the first N elements of SEQ

(defgeneric take (n seq))


;;; take-by
;;;
;;; (take-by n advance seq) => a sequence of sequences
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ N at a time. each chunk beings ADVANCE
;;; places after the start of the previous chunk

(defgeneric take-by (n advance seq))


;;; take-while
;;;
;;; (take-while test seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns elements of SEQ one after the other until TEST returns true

(defgeneric take-while (test seq))


;;; type-for-copy
;;;
;;; (type-for-copy val) => a type deisgnator
;;; ---------------------------------------------------------------------
;;; returns a type designator suitable as the type of a copy of VAL
;;; used by functions that copy and convert values for determining
;;; what output types to use

(defgeneric type-for-copy (thing))


;;; union
;;;
;;; (union set1 set2) => set3
;;; ---------------------------------------------------------------------
;;; returns a set that contains all elements that appear either in
;;; SET1 or in SET2

(defgeneric union (set1 set2 &key key test))


;;; unique
;;;
;;; (unique seq &key (test 'equal)) => seq'
;;; ---------------------------------------------------------------------
;;; returns the elements of SEQ with duplicates removed. TEST is used
;;; to test whether two elements SEQ are the same.

(defgeneric unique (seq &key test))


;;; unzip
;;;
;;; (unzip seq) => seq1 seq2
;;; ---------------------------------------------------------------------
;;; SEQ must be a sequence of pairs. returns two sequences; the first
;;; contains the heads of the pairs in SEQ, and the second contains
;;; the tails

(defgeneric unzip (seq))


;;; vals
;;;
;;; (vals map) => a list of values
;;; ---------------------------------------------------------------------
;;; returns a sequence of all the values appearing in MAP. If MAP is a
;;; sequence then the returned value is MAP itself.

(defgeneric vals (map))


;;; zip
;;;
;;; (unzip seq1 seq2) => seq3
;;; ---------------------------------------------------------------------
;;; returns a sequence of pairs in which each left element is from SEQ1
;;; and each right element is the corresponding one from SEQ2.

(defgeneric zip (seq1 seq2))
