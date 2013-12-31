;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       uniform tools for converting values from one type to another
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.copy)

(defgeneric copy (val &key &allow-other-keys))
(defgeneric deep-copy (val &key &allow-other-keys))
(defgeneric shallow-copy (val &key &allow-other-keys))

(defmethod copy (x &key &allow-other-keys)(shallow-copy x))

(defmethod deep-copy ((x cl:symbol) &key &allow-other-keys) x)
(defmethod deep-copy ((x cl:number) &key &allow-other-keys) x)
(defmethod deep-copy ((x cl:character) &key &allow-other-keys) x)
(defmethod deep-copy ((x cl:null) &key &allow-other-keys) x)
(defmethod deep-copy ((x cl:cons) &key &allow-other-keys) 
  (cl:cons (deep-copy (cl:car x))
           (deep-copy (cl:cdr x))))
(defmethod deep-copy ((x cl:string) &key &allow-other-keys) 
  (coerce (cl:coerce x 'cl:vector) 'cl:string))
(defmethod deep-copy ((x cl:vector) &key &allow-other-keys) 
  (coerce (cl:map 'cl:list 'deep-copy x) 'cl:vector))

(defmethod deep-copy ((x cl:hash-table) &key &allow-other-keys) 
  (let ((table (make-hash-table :test (hash-table-test x)
                                :size (hash-table-size x)
                                :rehash-size (hash-table-rehash-size x)
                                :rehash-threshold (hash-table-rehash-threshold x))))
    (cl:maphash (lambda (k v)(cl:setf (cl:gethash k table) v))
                x)
    table))

(defmethod deep-copy ((x cl:pathname) &key &allow-other-keys) 
  (cl:make-pathname :host (cl:pathname-host x)
                    :device (cl:pathname-device x)
                    :directory (cl:pathname-directory x)
                    :name (cl:pathname-name x)
                    :type (cl:pathname-type x)
                    :version (cl:pathname-version x)))

(defmethod shallow-copy (x &key &allow-other-keys) x)
