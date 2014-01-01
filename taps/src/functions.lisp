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
;;;(defmethod elements ((in wb-seq)) )

;;; function lines
;;;
;;; (lines string-or-stream) => a series of lines (strings)
;;; ---------------------------------------------------------------------

(defmethod lines ((in cl:pathname)) (series:scan-file in 'cl:read-line))
(defmethod lines ((in cl:stream))(series:scan-stream in 'cl:read-line))
(defmethod lines ((in cl:string)) 
  (with-input-from-string (in str)
    (lines in)))

;;; function octets
;;;
;;; (octets sequence-or-stream) => a series of octets
;;; ---------------------------------------------------------------------

(defmethod octets ((path cl:pathname)) 
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (series:scan-stream in 'cl:read-byte)))

(defmethod octets ((in cl:sequence)) )
(defmethod octets ((in cl:stream)) )

;;; function slots
;;;
;;; (slots map-or-instance) => a series of pairs representing
;;; associations between slot names and values in a map or
;;; an instance of a class
;;; ---------------------------------------------------------------------

(defmethod slots ((in cl:standard-object)) )
(defmethod slots ((in cl:cons)) )
;;;(defmethod slots ((in wb-map)) )

;;; function objects
;;;
;;; (objects string-or-stream) => a series of Lisp objects produced by READ
;;; ---------------------------------------------------------------------

(defmethod objects ((in cl:pathname)) (series:scan-file in 'cl:read))
(defmethod objects ((in cl:stream)) (series:scan-stream in 'cl:read))
(defmethod objects ((str cl:string)) 
  (with-input-from-string (in str)
    (objects in)))







