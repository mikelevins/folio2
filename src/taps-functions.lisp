;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       tap functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; a tap is a series that supplies a specified type of values
;;; obtained from a specified type of source--usually a string,
;;; stream, sequence, or map.

(in-package :net.bardcode.folio.taps)

;;; function characters
;;;
;;; (characters string-or-stream) => a series of characters
;;; ---------------------------------------------------------------------

(defmethod characters ((in cl:pathname)) (series:scan-file in 'cl:read-char))
(defmethod characters ((in cl:stream)) (series:scan-stream in 'cl:read-char))
(defmethod characters ((str cl:string)) 
  (with-input-from-string (in str)
    (characters in)))

;;; function elements
;;;
;;; (elements sequence) => a series of values found as elements of a sequence
;;; ---------------------------------------------------------------------

(defmethod elements ((in cl:null)) (series:scan nil))
(defmethod elements ((in cl:sequence))(series:scan in))
(defmethod elements ((in wb-seq))(series:scan (fset:convert 'cl:vector in)))

;;; function lines
;;;
;;; (lines string-or-stream) => a series of lines (strings)
;;; ---------------------------------------------------------------------

(defmethod lines ((in cl:pathname)) (series:scan-file in 'cl:read-line))
(defmethod lines ((in cl:stream))(series:scan-stream in 'cl:read-line))

(defmethod lines ((str cl:string)) 
  (with-input-from-string (in str)
    (lines in)))

;;; function octets
;;;
;;; (octets sequence-or-stream) => a series of octets
;;; ---------------------------------------------------------------------

(defmethod octets ((path cl:pathname)) 
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (series:scan-stream in 'cl:read-byte)))

(defmethod octets ((in cl:vector)) 
  (if (cl:subtypep (cl:array-element-type in) '(unsigned-byte 8))
      (series:scan in)
      (error "Can't return a series of octets from a value of type ~S"
             (type-of in))))

(defmethod octets ((in cl:stream)) 
  (if (cl:subtypep (cl:stream-element-type in) '(unsigned-byte 8))
      (series:scan-stream in 'read-byte)
      (error "Can't return a series of octets from a stream with element type ~S"
             (cl:stream-element-type in))))

;;; function slots
;;;
;;; (slots map-or-instance) => a series of pairs representing
;;; associations between slot names and values in a map or
;;; an instance of a class
;;; ---------------------------------------------------------------------

(defmethod slots ((in cl:standard-object)) 
  (let* ((class (cl:class-of in))
         (slot-definitions (closer-mop:compute-slots class)))
    (series:map-fn t
                   (lambda (sdesc)(cons (closer-mop:slot-definition-name sdesc)
                                        (cl:slot-value in (closer-mop:slot-definition-name sdesc))))
                   (series:scan slot-definitions))))

(defmethod slots ((in cl:cons)) 
  (cond
    ((alist? in)(series:scan-alist in))
    ((plist? in)(series:scan-plist in))
    (t (error "The list ~S is not recognized as a map" in))))

(defmethod slots ((in cl:hash-table)) 
  (multiple-value-bind (keys vals)(series:scan-hash in)
    (series:map-fn t
                   (lambda (k v)(cons k v))
                   keys
                   vals)))

(defmethod slots ((in wb-map)) 
  (series:map-fn t
                 (lambda (s)(cl:cons s (fset:@ in s)))
                 (series:scan (fset:convert 'cl:vector (fset:domain in)))))

;;; function objects
;;;
;;; (objects string-or-stream) => a series of Lisp objects produced by READ
;;; ---------------------------------------------------------------------

(defmethod objects ((in cl:pathname)) (series:scan-file in 'cl:read))
(defmethod objects ((in cl:stream)) (series:scan-stream in 'cl:read))

(defmethod objects ((str cl:string)) 
  (with-input-from-string (in str)
    (objects in)))







