;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       tables functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.folio.tables)

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

(defmethod contains-key? ((map fset:seq) key &key &allow-other-keys)
  (fset:domain-contains? map key))

(defmethod contains-key? ((map fset:map) key &key &allow-other-keys)
  (fset:domain-contains? map key))

(defmethod contains-key? ((map series::foundation-series) (key integer) &key &allow-other-keys)
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
  (some (lambda (i)(funcall test val i)) map))

(defmethod contains-value? ((map fset:seq) val &key &allow-other-keys)
  (fset:range-contains? map val))

(defmethod contains-value? ((map fset:map) key &key &allow-other-keys)
  (fset:range-contains? map key))

(defmethod contains-value? ((map series::foundation-series) val &key (test #'equal) &allow-other-keys)
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

(defmethod get-key ((map fset:seq)(key integer) &key (default nil) &allow-other-keys)
  (multiple-value-bind (result found?)(fset:lookup map key)
    (if found? result default)))

(defmethod get-key ((map fset:map) key &key (default nil) &allow-other-keys)
  (multiple-value-bind (result found?)(fset:lookup map key)
    (if found? result default)))

(defmethod get-key ((map series::foundation-series) (key integer) &key (default nil) &allow-other-keys)
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

(defmethod keys ((map fset:seq))
  (fset:domain map))

(defmethod keys ((map fset:map))
  (fset:domain map))

(defmethod keys ((map series::foundation-series))
  (series:positions (series:map-fn 'boolean (constantly t) map)))

;;; generic function merge-tables
;;; (merge-tables map1 map2 &key (test 'equal)) => map3
;;; ---------------------------------------------------------------------
;;; returns a new map constructed by combining the key/value pairs
;;; from both MAP1 and MAP2. If a key appears in both maps then the
;;; value from MAP2 is used. Keys are compared using TEST. sequences
;;; and sequence-like objects are treated as maps from indexes to
;;; elements.

(defgeneric merge-tables (map1 map2 &key &allow-other-keys))

(defmethod merge-tables ((map1 cl:sequence)(map2 cl:sequence) &key &allow-other-keys)
  (if (<= (length map1)(length map2))
      map2
      (concatenate 'list map2 (subseq map1 (length map2)))))

(defmethod merge-tables ((map1 string)(map2 string) &key &allow-other-keys)
  (if (<= (length map1)(length map2))
      map2
      (concatenate 'string map2 (subseq map1 (length map2)))))

(defmethod merge-tables ((map1 vector)(map2 vector) &key &allow-other-keys)
  (if (<= (length map1)(length map2))
      map2
      (concatenate 'vector map2 (subseq map1 (length map2)))))

(defmethod merge-tables ((map1 cl:sequence)(map2 fset:seq) &key &allow-other-keys)
  (merge-tables map1 (fset:convert 'list map2)))

(defmethod merge-tables ((map1 cl:sequence)(map2 fset:map) &key &allow-other-keys)
  (merge-tables map1 (fset:convert 'list map2)))

(defmethod merge-tables ((map1 cl:sequence)(map2 series::foundation-series) &key &allow-other-keys)
  (merge-tables (series:scan map1) map2))



(defmethod merge-tables ((map1 fset:seq)(map2 cl:sequence) &key &allow-other-keys)
  (merge-tables map1 (fset:convert 'fset:seq map2)))

(defmethod merge-tables ((map1 fset:seq)(map2 fset:seq) &key &allow-other-keys)
  (if (<= (fset:size map1)(fset:size map2))
      map2
      (fset:concat map2 (fset:subseq map1 (fset:size map2)))))

(defmethod merge-tables ((map1 fset:seq)(map2 fset:map) &key &allow-other-keys)
  (merge-tables map1 (fset:convert 'fset:seq map2)))

(defmethod merge-tables ((map1 fset:seq)(map2 series::foundation-series) &key &allow-other-keys)
  (merge-tables (series:scan (fset:convert 'list map1)) map2))



(defmethod merge-tables ((map1 fset:map)(map2 cl:sequence) &key &allow-other-keys)
  (merge-tables map1 (fset:convert 'fset:seq map2)))

(defmethod merge-tables ((map1 fset:map)(map2 fset:seq) &key &allow-other-keys)
  (merge-tables map1
                (fset:convert 'fset:map
                              (fset:convert 'list
                                            (fset:image (lambda (i)(cons i (fset:@ map2 i)))
                                                        (fset:domain map2))))))

(defmethod merge-tables ((map1 fset:map)(map2 fset:map) &key &allow-other-keys)
  (fset:map-union map1 map2))

(defmethod merge-tables ((map1 fset:map)(map2 series::foundation-series) &key &allow-other-keys)
  (merge-tables (series:scan (fset:convert 'list map1)) 
                (series:map-fn 't
                               (lambda (k i)(cons k i))
                               (keys map2)
                               map2)))


(defmethod merge-tables ((map1 series::foundation-series)(map2 cl:sequence) &key &allow-other-keys)
  (merge-tables map1 (series:scan map2)))

(defmethod merge-tables ((map1 series::foundation-series)(map2 fset:seq) &key &allow-other-keys)
  (merge-tables map1 (series:scan (fset:convert 'list map2))))

(defmethod merge-tables ((map1 series::foundation-series)(map2 fset:map) &key &allow-other-keys)
  (merge-tables map1 (series:scan (fset:convert 'list map2))))

(defmethod merge-tables ((map1 series::foundation-series)(map2 series::foundation-series) &key &allow-other-keys)
  (labels ((nils () 
             (series:scan-fn 't 
                             (lambda () nil)
                             (lambda (i)(declare (ignore i)) nil))))
    (let* ((pos1 (series:mask (keys map1)))
           (pos2 (series:mask (keys map2)))
           (map1 (series:catenate map1 (nils)))
           (map2 (series:catenate map2 (nils))))
      (series:until-if #'null
                       (series:map-fn 't (lambda (m2 p2 m1 p1)
                                           (cond 
                                             (p2 m2)
                                             (p1 m1)
                                             (t nil)))
                                      map2 pos2 map1 pos1)))))


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


(defmethod put-key ((map fset:seq) key val &key &allow-other-keys)
  (if (< -1 key (fset:size map))
      (fset:with map key val)
      (error "key ~A is out of range" key)))

(defmethod put-key ((map fset:map) key val &key &allow-other-keys)
  (fset:with map key val))

(defmethod put-key ((map series::foundation-series) (key integer) val &key &allow-other-keys)
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
(defmethod vals ((map fset:seq)) map)
(defmethod vals ((map fset:map)) (fset:range map))
(defmethod vals ((map series::foundation-series)) map)
