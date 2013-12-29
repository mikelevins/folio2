;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       map functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.maps)

;;; ---------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------

(defun %expand-alist-pairs (pairs)
  (if (null pairs)
      nil
      (cons (let ((first (first pairs)))
              `(cl:cons ,(car first) ,(cdr first)))
            (%expand-alist-pairs (cdr pairs)))))

(defmethod %merge-alists ((alist1 cl:null)(alist2 cl:null) &key test) nil)
(defmethod %merge-alists ((alist1 cl:cons)(alist2 cl:null) &key test) alist1)
(defmethod %merge-alists ((alist1 cl:null)(alist2 cl:cons) &key test) alist2)

(defmethod %merge-alists ((alist1 cl:cons)(alist2 cl:cons) &key (test 'eql))
  (append alist2 (set-difference alist1 alist2 :key 'car :test test)))

(defun %put-plist-key (x key value &key (test 'eql))
  (if (null x)
      (list key value)
      (if (null (cdr x))
          (error "Malformed plist: ~s" x)
          (if (funcall test key (car x))
              (cons key 
                    (cons value
                          (cddr x)))
              (cons (car x) 
                    (cons (cadr x)
                          (%put-plist-key (cddr x) key value :test test)))))))

(defun %put-alist-key (x key value &key (test 'eql))
  (if (null x)
      (list (cons key value))
      (if (funcall test key (car (car x)))
          (cons (cons key value)
                (cdr x))
          (cons (car x)
                (%put-alist-key (cdr x) key value :test test)))))

;;; macro 
;;;
;;; (alist pair ...) => alist
;;; ---------------------------------------------------------------------

(defmacro alist (&rest pairs)
  (cons 'cl:list 
        (%expand-alist-pairs pairs)))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod as ((type (eql 'map)) value &key &allow-other-keys)
  (make 'map :contents value))

(defmethod as ((type (eql 'map)) (value cl:null) &key &allow-other-keys)
  nil)

(defmethod as ((type (eql 'map)) (value cl:cons) &key &allow-other-keys)
  (cond
    ((plist? value) value)
    ((alist? value) value)
    (t (error "Not a valid map: ~s" value))))

(defmethod as ((type (eql 'alist)) value &key &allow-other-keys)
  (make 'alist :contents value))

(defmethod as ((type (eql 'alist)) (value cl:cons) &key &allow-other-keys)
  (cond
    ((plist? value)(make 'alist :contents value))
    ((alist? value) value)
    (t (error "Not a valid map: ~s" value))))

(defmethod as ((type (eql 'alist)) (value wb-map) &key &allow-other-keys)
  (fset:convert 'cl:list value))

(defmethod as ((type (eql 'plist)) value &key &allow-other-keys)
  (make 'plist :contents value))

(defmethod as ((type (eql 'plist)) (value cl:cons) &key &allow-other-keys)
  (cond
    ((plist? value) value)
    ((alist? value)(loop for pair in value append (list (car pair)(cdr pair))))
    (t (error "Not a valid map: ~s" value))))

(defmethod as ((type (eql 'wb-map)) value &key &allow-other-keys)
  (make 'wb-map :contents value))

(defmethod as ((type (eql 'wb-map)) (value cl:cons) &key &allow-other-keys)
  (cond
    ((plist? value)(make 'wb-map :contents value))
    ((alist? value)(fset:convert 'wb-map value))
    (t (error "Not a valid map: ~s" value))))


;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod contains-key? ((x cl:cons) key &key (test 'eql))
  (member key (keys x) :test test))

(defmethod contains-key? ((x wb-map) key &key &allow-other-keys)
  (fset:domain-contains? x key))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod contains-value? ((x cl:cons) value &key (test 'eql))
  (member value (values x) :test test))

(defmethod contains-value? ((x wb-map) value &key &allow-other-keys)
  (fset:range-contains? x value))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod get-key ((x cl:cons) key &key (test 'eql)(default nil))
  (cond
    ((plist? x)(getf x key default))
    ((alist? x)(let ((entry (assoc key x :test test)))
                 (if entry (cdr entry) default)))
    (t (error "Not a valid map: ~s" x))))

(defmethod get-key ((x wb-map) key &key (default nil) &allow-other-keys)
  (let ((result (fset:@ x key)))
    (if (fset:equal? result (fset:map-default x))
        default
        result)))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod keys ((x cl:cons))
  (cond
    ((plist? x)(loop for tail on x by 'cddr collect (car tail)))
    ((alist? x)(mapcar 'car x))
    (t (error "Not a valid map: ~s" x))))

(defmethod keys ((x wb-map))
  (fset:convert 'cl:list (fset:domain x)))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod make ((type (eql 'map)) &key (contents nil) &allow-other-keys)
  (apply 'plist contents))

(defmethod make ((type (eql 'plist)) &key (contents nil) &allow-other-keys)
  (apply 'plist contents))

(defmethod make ((type (eql 'alist)) &key (contents nil) &allow-other-keys)
  (let ((plist (apply 'plist contents)))
    (loop for tail on plist by 'cddr collect (cons (car tail)(cadr tail)))))

(defmethod make ((type (eql 'wb-map)) &key (contents nil) &allow-other-keys)
  (fset:convert 'wb-map (make 'alist :contents contents)))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod map? (x) nil)
(defmethod map? ((x wb-map)) t)

(defmethod map? ((x cons)) 
  (or (alist? x)
      (plist? x)))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod merge ((map1 cl:null) (map2 cl:null) &key test &allow-other-keys)
  nil)

(defmethod merge ((map1 cl:null) (map2 cl:cons) &key test &allow-other-keys)
  map2)

(defmethod merge ((map1 cl:null) (map2 wb-map) &key test &allow-other-keys)
  (as 'plist map2))

(defmethod merge ((map1 cl:cons) (map2 cl:null) &key test &allow-other-keys)
  map1)

(defmethod merge ((map1 cl:cons) (map2 cl:cons) &key (test 'eql) &allow-other-keys)
  (cond
    ((plist? map1)(as 'plist (%merge-alists (as 'alist map1)(as 'alist map2) :test test)))
    ((alist? map1)(as 'alist (%merge-alists (as 'alist map1)(as 'alist map2) :test test)))
    (t (error "Not a valid map: ~s" map1))))

(defmethod merge ((map1 cl:cons) (map2 wb-map) &key test &allow-other-keys)
  (merge map1 (as 'alist map2)))

(defmethod merge ((map1 wb-map)(map2 cl:null)  &key &allow-other-keys)
  map1)

(defmethod merge ((map1 wb-map) (map2 cl:cons) &key &allow-other-keys)
  (merge map1 (as 'wb-map map2)))

(defmethod merge ((map1 wb-map)(map2 wb-map)  &key &allow-other-keys)
  (fset:map-union map1 map2))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defun plist (&rest keys-and-values)
  (if (plist? keys-and-values)
      keys-and-values
      (error "Malformed plist: ~s"
             keys-and-values)))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod put-key ((x null) key value &key &allow-other-keys)
  (list (cons key value)))

(defmethod put-key ((x cl:cons) key value &key (test 'eql) &allow-other-keys)
  (cond
    ((plist? x)(%put-plist-key x key value :test test))
    ((alist? x)(%put-alist-key x key value :test test))
    (t (error "Not a valid map: ~s" x))))

(defmethod put-key ((x wb-map) key value &key &allow-other-keys)
  (fset:with x key value))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod values ((x cl:cons))
  (cond
    ((plist? x)(loop for tail on x by 'cddr collect (cadr tail)))
    ((alist? x)(mapcar 'cdr x))
    (t (error "Not a valid map: ~s" x))))

(defmethod values ((x wb-map))
  (fset:convert 'cl:list (fset:range x)))

;;; function 
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defmethod wb-map? (thing)
  (typep thing 'wb-map))



