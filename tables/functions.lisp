;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       table functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.folio.tables)

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(defmethod %find-entry ((om ordered-map) key &key (test 'equal))
  (assoc key (%table-entries om) :test test))

;;; ---------------------------------------------------------------------
;;; API
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; as
;;; ---------------------------------------------------------------------

(defmethod as ((type (eql 'cl:list))(tbl fset:map))
  (fset:convert 'cl:list tbl))

;;; generic function alist->plist
;;;
;;; (alist->plist alist) => plist
;;; ---------------------------------------------------------------------
;;; returns a plist--a list of alternating key/value elements
;;; constructed from the keys and values of the alist

(defgeneric alist->plist (thing))

(defmethod alist->plist (x)
  (error "Not an alist:" thing))

(defmethod alist->plist ((x null))
  (declare (ignore x))
  nil)

(defmethod alist->plist ((x cons))
  (loop for (a . b) in x appending (cl:list a b)))

;;; generic function associate
;;;
;;; (associate table1 key val &key (test 'equal) &allow-other-keys) => table2
;;; ---------------------------------------------------------------------
;;; returns a table TABLE2 that contains the elements of TABLE1, plus the
;;; key/value pair of KEY and VAL. If TABLE1 contains KEY then its value is
;;; replaced by VAL in TABLE2.

(defgeneric associate (table key val &key test &allow-other-keys))

(defmethod associate ((m ordered-map) key val &key (test 'equal) &allow-other-keys)
  (if (%find-entry m key :test test)
      m
      (let* ((outm (make-instance 'ordered-map :entries (cl:copy-seq (%table-entries m))))
             (already (assoc key (%table-entries outm) :test test)))
        (if already
            (setf (cdr already) val)
            (setf (%table-entries outm)
                  (append (%table-entries outm)
                          (cl:list (cons key val)))))
        outm)))

(defmethod associate ((table alist) key val &key (test 'equal) &allow-other-keys)
  (let ((old-value (value table)))
    (make-instance 'alist
                   :value (cl:acons key val
                                    (cl:remove key old-value :test test :key 'car)))))

(defmethod associate ((table plist) key val &key (test 'equal) &allow-other-keys)
  (let* ((old-value (value table))
         (new-table (make-instance 'alist
                   :value (cl:copy-seq old-value)))
         (new-already (cl:member key (value new-table) :test test)))
    (if new-already
        (progn
          (setf (cdr new-already)
                (cons val (cddr new-already)))
          new-table)
        (progn
          (setf (value new-table)
                (append (value new-table)
                        (cl:list key val)))
          new-table))))

(defmethod associate ((table fset:map) key val &key &allow-other-keys)
  (fset:with table key val))


;;; generic function contains-key?
;;;
;;; (contains-key? map key &key (test 'equal)) => a generalized boolean
;;; ---------------------------------------------------------------------
;;; returns a generalized boolean that is true if MAP contains a key
;;; that is equivalent to KEY in the sense of TEST, and returns a
;;; false value otherwise. support for the TEST keyword depends on the
;;; representation of MAP.

(defgeneric contains-key? (map key &key &allow-other-keys))

(defmethod contains-key? ((map cl:sequence) key &key &allow-other-keys)
  (declare (ignore key))
  nil)

(defmethod contains-key? ((map cl:sequence) (key integer) &key &allow-other-keys)
  (< -1 key (length map)))

(defmethod contains-key? ((map alist) key &key (test 'equal) &allow-other-keys)
  (and (cl:assoc key map :test test) t))

(defmethod contains-key? ((map plist) key &key (test 'equal) &allow-other-keys)
  (block searching
    (loop (for x on (value map) by 'cddr
               do (when (funcall test val (car x))
                    (return-from searching t))))
    nil))

(defmethod contains-key? ((map seq) key &key &allow-other-keys)
  (fset:domain-contains? map key))

(defmethod contains-key? ((map fset:map) key &key &allow-other-keys)
  (fset:domain-contains? map key))

(defmethod contains-key? ((map foundation-series) (key integer) &key &allow-other-keys)
  (let ((absent (gensym)))
    (and (not (eq absent (series:collect-nth key map absent)))
         t)))

;;; generic function contains-value?
;;;
;;; (contains-value? map val &key (test 'equal)) => a generalized boolean
;;; ---------------------------------------------------------------------
;;; returns a generalized boolean that is true if MAP contains a value
;;; that is equivalent to VAL in the sense of TEST, and returns a
;;; false value otherwise

(defgeneric contains-value? (map key &key &allow-other-keys))

(defmethod contains-value? ((map cl:sequence) val &key (test #'equal) &allow-other-keys)
  (cl:some (lambda (i)(funcall test val i)) map))

(defmethod contains-value? ((map alist) val &key (test #'equal) &allow-other-keys)
  (cl:some (lambda (i)(funcall test val (cdr i))) (value map)))

(defmethod contains-value? ((map plist) val &key (test #'equal) &allow-other-keys)
  (block searching
    (loop (for x on (value map) by 'cddr
               do (when (funcall test val (cadr x))
                    (return-from searching t))))
    nil))

(defmethod contains-value? ((map seq) val &key &allow-other-keys)
  (fset:range-contains? map val))

(defmethod contains-value? ((map fset:map) key &key &allow-other-keys)
  (fset:range-contains? map key))

(defmethod contains-value? ((map foundation-series) val &key (test #'equal) &allow-other-keys)
  (series:collect-first (series:choose-if (lambda (x)(funcall test val x)) map)))

;;; generic function get-key
;;; (get-key map key &key (test 'equal)(default nil)) => anything
;;; ---------------------------------------------------------------------
;;; returns the value stored on KEY in MAP if KEY is present; returns
;;; DEFAULT otherwise. Matches KEY using TEST

(defgeneric get-key (map key &key &allow-other-keys))

(defmethod get-key ((map cl:sequence)(key integer) &key (default nil) &allow-other-keys)
  (if (< -1 key (length map))
      (elt map key)
      default))

(defmethod get-key ((map alist) key &key (test 'equal) (default nil) &allow-other-keys)
  (let ((entry (cl:assoc key (value map) :test test)))
    (if entry
        (cdr entry)
        default)))

(defmethod get-key ((map plist) val &key (test #'equal) (default nil)&allow-other-keys)
  (block searching
    (loop (for x on (value map) by 'cddr
               do (when (funcall test key (car x))
                    (return-from searching (cadr x)))))
    default))

(defmethod get-key ((map seq)(key integer) &key (default nil) &allow-other-keys)
  (multiple-value-bind (result found?)(fset:lookup map key)
    (if found? result default)))

(defmethod get-key ((map fset:map) key &key (default nil) &allow-other-keys)
  (multiple-value-bind (result found?)(fset:lookup map key)
    (if found? result default)))

(defmethod get-key ((map foundation-series) (key integer) &key (default nil) &allow-other-keys)
  (series:collect-nth key map default))

;;; generic function keys
;;; (keys map) => a list of keys
;;; ---------------------------------------------------------------------
;;; returns a sequence of all the keys appearing in MAP. If MAP is a
;;; sequence then KEYS returns a sequence of indexes. If MAP is a
;;; series then KEYS returns a series of indexes.

(defgeneric keys (map))

(defmethod keys ((map cl:sequence))
  (loop for i from 0 below (length map) collect i))

(defmethod keys ((map alist))
  (mapcar 'car (value map)))

(defmethod keys ((map plist))
  (loop (for x on (value map) by 'cddr
             collecting (car x))))

(defmethod keys ((map seq))
  (fset:domain map))

(defmethod keys ((map fset:map))
  (fset:domain map))

(defmethod keys ((map foundation-series))
  (series:positions (series:map-fn 'boolean (constantly t) map)))

;;; ---------------------------------------------------------------------
;;; function make
;;; ---------------------------------------------------------------------
;;;
;;; (make 'table &key ) => a table
;;; ---------------------------------------------------------------------
;;; create a table

(defmethod make ((type (eql 'alist))  &key &allow-other-keys)
  )

(defmethod make ((type (eql 'ordered-map))  &key &allow-other-keys)
  )

(defmethod make ((type (eql 'plist))  &key &allow-other-keys)
  )

(defmethod make ((type (eql 'table))  &key &allow-other-keys)
  )


;;; generic function merge
;;; (merge table1 table2 &key (test 'equal)) => table3
;;; ---------------------------------------------------------------------
;;; returns a new table constructed by combining the key/value pairs
;;; from both TABLE1 and TABLE2. If a key appears in both tables then
;;; the value from TABLE2 is used. Keys are compared using
;;; TEST. sequences and sequence-like objects are treated as maps from
;;; indexes to elements.

(defmethod %convert-to-merge ((table null))
  nil)

(defmethod %convert-to-merge ((table cl:sequence))
  (loop 
     for i from 0 below (length table)
     collecting (cons i (elt table i))))

(defmethod %convert-to-merge ((table seq))
  (loop 
     for i from 0 below (fset:size table)
     collecting (cons i (elt table i))))

(defmethod %merge-converted-tables ((table1 null) (table2 null))
  nil)

(defmethod %convert-from-merge (out-type (table null))
  nil)

(defun merge (table1 table2 &key &allow-other-keys)
  (let ((out-type (type-for-copy table1)))
    (%convert-from-merge out-type
                         (%merge-converted-tables (%convert-to-merge table1)
                                                  (%convert-to-merge table2)))))


;;; generic function put-key
;;; (put-key map1 key val (test 'equal)) => map2
;;; ---------------------------------------------------------------------
;;; returns a new map that contains the key/value pairs from MAP1, but
;;; with the pair (KEY . VAL) added. If KEY appears in MAP1 then
;;; its associated value is replaced by VAL in MAP2. Keys are compared
;;; using TEST.

(defgeneric put-key (map key val &key &allow-other-keys))

(defmethod put-key ((map cl:sequence) (key integer) val &key &allow-other-keys)
  (if (< -1 key (length map))
      (let ((outmap (copy-seq map)))
        (setf (elt outmap key) val)
        outmap)
      (error "key ~A is out of range" key)))

(defmethod put-key ((map string) (key integer) (val character) &key &allow-other-keys)
  (if (< -1 key (length map))
      (let ((outmap (copy-seq map)))
        (setf (elt outmap key) val)
        outmap)
      (error "key ~A is out of range" key)))


(defmethod put-key ((map seq) key val &key &allow-other-keys)
  (if (< -1 key (fset:size map))
      (fset:with map key val)
      (error "key ~A is out of range" key)))

(defmethod put-key ((map fset:map) key val &key &allow-other-keys)
  (fset:with map key val))

(defmethod put-key ((map foundation-series) (key integer) val &key &allow-other-keys)
  (series:map-fn 't
                 (lambda (i v)(if (= i key) val v))
                 (keys map)
                 map))

;;; function table
;;; (table &rest key-value-plist) => a map
;;; ---------------------------------------------------------------------
;;; creates a new map whose keys and values are given by alternating
;;; keys and values in KEY-VALUE-PLIST. If a key appears more than once
;;; in KEY-VALUE-PLIST, an error is signaled. Keys are compared using
;;; the function bound to *table-key-test*, which, by default, is EQUAL.

(defun table (&rest key-value-plist)
  (let ((alist (loop for x on key-value-plist by #'cddr collect (cons (car x)(cadr x)))))
    (fset:convert 'fset:map alist)))

;;; generic function vals
;;; (vals map) => a list of values
;;; ---------------------------------------------------------------------
;;; returns a sequence of all the values appearing in MAP. If MAP is a
;;; sequence then the returned value is MAP itself.

(defgeneric vals (map))

(defmethod vals ((map cl:sequence)) map)
(defmethod vals ((map seq)) map)
(defmethod vals ((map fset:map)) (fset:range map))
(defmethod vals ((map foundation-series)) map)
