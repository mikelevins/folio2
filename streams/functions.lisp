;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       scanning streams
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.streams)

;;; ---------------------------------------------------------------------
;;; function characters 
;;; ---------------------------------------------------------------------
;;;
;;; (characters input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the characters provided by INPUT-STREAM

(defgeneric characters (input-stream))

(defmethod characters (thing)
  (error "Don't know how to collect characters from ~S" thing))

(defmethod characters ((in stream))
  (assert (cl:input-stream-p in)() "Can't collect characters from ~S; it's not an input stream" in)
  (assert (cl:subtypep (stream-element-type in) 'character)() "Can't collect characters from ~S; it's not a character stream" in)
  (series:scan-stream in #'read-char))

;;; ---------------------------------------------------------------------
;;; function lines 
;;; ---------------------------------------------------------------------
;;;
;;; (lines input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the lines provided by INPUT-STREAM

(defgeneric lines (input-stream))

(defmethod lines (thing)
  (error "Don't know how to collect lines from ~S" thing))

(defmethod lines ((in stream))
  (assert (cl:input-stream-p in)() "Can't collect lines from ~S; it's not an input stream" in)
  (assert (cl:subtypep (stream-element-type in) 'character)() "Can't collect lines from ~S; it's not a character stream" in)
  (series:scan-stream in #'read-line))

;;; ---------------------------------------------------------------------
;;; function make
;;; ---------------------------------------------------------------------
;;;
;;; (make 'cl:stream &key ) => a stream
;;; ---------------------------------------------------------------------
;;; create a set

(defmethod make ((type (eql 'cl:stream)) &key &allow-other-keys)
  )

;;; ---------------------------------------------------------------------
;;; function objects 
;;; ---------------------------------------------------------------------
;;;
;;; (objects input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the Lisp objects provided by INPUT-STREAM

(defgeneric objects (input-stream))

(defmethod objects (thing)
  (error "Don't know how to collect objects from ~S" thing))

(defmethod objects ((in stream))
  (assert (cl:input-stream-p in)() "Can't collect objects from ~S; it's not an input stream" in)
  (assert (cl:subtypep (stream-element-type in) 'character)() "Can't collect objects from ~S; it's not a character stream" in)
  (series:scan-stream in #'read))


;;; ---------------------------------------------------------------------
;;; function octets 
;;; ---------------------------------------------------------------------
;;;
;;; (octets input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the octets provided by INPUT-STREAM

(defgeneric octets (input-stream))

(defmethod octets (thing)
  (error "Don't know how to collect octets from ~S" thing))

(defmethod octets ((in stream))
  (assert (cl:input-stream-p in)() "Can't collect octets from ~S; it's not an input stream" in)
  (assert (cl:subtypep (stream-element-type in) 'unsigned-byte)() "Can't collect octets from ~S; it's not a byte stream" in)
  (series:scan-stream in #'read-byte))

