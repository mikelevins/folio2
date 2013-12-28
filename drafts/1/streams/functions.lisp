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


;;; function as
;;;
;;; (as 'input-stream x) => an input-stream
;;; (as 'output-stream x) => an output-stream
;;; ---------------------------------------------------------------------

(defmethod as ((type (eql 'input-stream)) (val pathname) 
               &key direction element-type external-format 
                 if-exists if-does-not-exist
                 &allow-other-keys)
  (cl:open val :direction direction :element-type element-type 
           :external-format external-format :if-exists if-exists 
           :if-does-not-exist if-does-not-exist))

(defmethod as ((type (eql 'input-stream)) (val string) &key start end &allow-other-keys)
  (make-string-input-stream val start end))

(defmethod as ((type (eql 'input-stream)) (val cl:sequence) &key &allow-other-keys)
  (make-sequence-input-stream val start end))

(defmethod as ((type (eql 'input-stream)) (val seq) &key &allow-other-keys)
  (make-seq-input-stream val start end))

(defmethod as ((type (eql 'input-stream)) (val foundation-series) &key &allow-other-keys)
  (make-series-input-stream val start end))

;;; ---------------------------------------------------------------------
;;; function characters 
;;; ---------------------------------------------------------------------
;;;
;;; (characters input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the characters provided by INPUT-STREAM

(defmethod characters (thing &key &allow-other-keys)
  (error "Don't know how to collect characters from ~S" thing))

(defmethod characters ((in stream) &key &allow-other-keys)
  (assert (cl:input-stream-p in)() "Can't collect characters from ~S; it's not an input stream" in)
  (assert (cl:subtypep (stream-element-type in) 'character)() "Can't collect characters from ~S; it's not a character stream" in)
  (series:scan-stream in #'read-char))

(defmethod characters ((path pathname) &key &allow-other-keys)
  (with-open-file (in path :direction :input :element-type 'cl:character)
    (characters in)))

(defmethod characters ((input cl:string) &key index (start 0) end &allow-other-keys)
  (with-input-from-string (in input :index index :start start :end end)
    (characters in)))

;;; ---------------------------------------------------------------------
;;; function input-stream? 
;;; ---------------------------------------------------------------------
;;;
;;; (input-stream? thing) => boolean
;;; ---------------------------------------------------------------------
;;; returns true if THING is an input-stream

(defmethod input-stream? (thing)
  nil)

(defmethod input-stream? ((thing cl:stream))
  (cl:input-stream-p thing))

;;; ---------------------------------------------------------------------
;;; function lines 
;;; ---------------------------------------------------------------------
;;;
;;; (lines input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the lines provided by INPUT-STREAM

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
;;; (make 'output-stream &key element-type) => output-stream
;;; ---------------------------------------------------------------------

(defmethod make ((type (eql 'output-stream)) &key (element-type 'cl:character) &allow-other-keys)
  (make-output-stream element-type))

;;; ---------------------------------------------------------------------
;;; function make-output-stream 
;;; ---------------------------------------------------------------------
;;;
;;; (make 'output-stream &key element-type) => output-stream
;;; ---------------------------------------------------------------------

(defmethod make-output-stream ((element-type (eql 'cl:character)) &key &allow-other-keys)
    (make-string-output-stream :element-type element-type))

;;; ---------------------------------------------------------------------
;;; function objects 
;;; ---------------------------------------------------------------------
;;;
;;; (objects input-stream) => series
;;; ---------------------------------------------------------------------
;;; returns a series of the Lisp objects provided by INPUT-STREAM

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

(defmethod octets (thing)
  (error "Don't know how to collect octets from ~S" thing))

(defmethod octets ((in stream))
  (assert (cl:input-stream-p in)() "Can't collect octets from ~S; it's not an input stream" in)
  (assert (cl:subtypep (stream-element-type in) 'unsigned-byte)() "Can't collect octets from ~S; it's not a byte stream" in)
  (series:scan-stream in #'read-byte))

;;; ---------------------------------------------------------------------
;;; function output-stream? 
;;; ---------------------------------------------------------------------
;;;
;;; (output-stream? thing) => boolean
;;; ---------------------------------------------------------------------
;;; returns true if THING is an output-stream

(defmethod output-stream? (thing)
  nil)

(defmethod output-stream? ((thing cl:stream))
  (cl:output-stream-p thing))
