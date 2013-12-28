;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       the streamable types
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.sets)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the STREAMABLE types identify values that can be conveniently
;;; treated by foloio as streams 

;;; type readable
;;;
;;; ---------------------------------------------------------------------
;;; the type of values that can be treated as input streams

(defgeneric readable? (thing))

(defmethod readable? (thing) nil)

(defmethod readable? ((thing cl:stream)) 
  (cl:input-stream-p thing))

(defmethod readable? ((thing cl:pathname)) t)
(defmethod readable? ((thing cl:string)) t)

(deftype readable ()
 '(satisfies readable?))

;;; type writable
;;;
;;; ---------------------------------------------------------------------
;;; the type of values that can be treated as output streams

(defgeneric writable? (thing))

(defmethod writable? (thing) nil)

(defmethod writable? ((thing cl:stream)) 
  (cl:output-stream-p thing))

(defmethod writable? ((thing cl:pathname)) t)
(defmethod writable? ((thing cl:string)) t)

(deftype writable ()
 '(satisfies writable?))

;;; type streamable
;;;
;;; ---------------------------------------------------------------------
;;; the type of values that can be treated as input or output streams

(defun streamable? (thing)
  (or (readable? thing)
      (writable? thing)))

(deftype streamable ()
 '(satisfies streamable?))

