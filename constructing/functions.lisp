;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       uniform tools for constructing values
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.constructing)

;;; function make
;;;
;;; (make type &key initargs) => a value
;;; ---------------------------------------------------------------------
;;; returns a new value of the indicated type, initialized with
;;; parameters supplied by initargs

(defgeneric make (type &rest initargs))

;;; function type-for-copy
;;;
;;; (type-for-copy val) => a type deisgnator
;;; ---------------------------------------------------------------------
;;; returns a type designator suitable as the type of a copy of VAL
;;; used by functions that copy and convert values for determining
;;; what output types to use

(defgeneric type-for-copy (thing))

(defmethod type-for-copy ((val array)) (type-of val))
(defmethod type-for-copy ((val built-in-class)) 'cl:built-in-class)
(defmethod type-for-copy ((val character)) 'cl:character)
(defmethod type-for-copy ((val complex)) 'cl:complex)
(defmethod type-for-copy ((val cons)) 'cl:cons)
(defmethod type-for-copy ((val double-float)) 'cl:double-float)
(defmethod type-for-copy ((val file-stream)) 'cl:file-stream)
(defmethod type-for-copy ((val function)) 'cl:function)
(defmethod type-for-copy ((val hash-table)) 'cl:hash-table)
(defmethod type-for-copy ((val logical-pathname)) 'cl:logical-pathname)
(defmethod type-for-copy ((val null)) 'cl:null)
(defmethod type-for-copy ((val package)) 'cl:package)
(defmethod type-for-copy ((val pathname)) 'cl:pathname)
(defmethod type-for-copy ((val ratio)) 'cl:ratio)
(defmethod type-for-copy ((val random-state)) 'cl:random-state)
(defmethod type-for-copy ((val readtable)) 'cl:readtable)
(defmethod type-for-copy ((val restart)) 'cl:restart)
(defmethod type-for-copy ((val single-float)) 'cl:single-float)
(defmethod type-for-copy ((val standard-class)) 'cl:standard-class)
(defmethod type-for-copy ((val standard-generic-function)) 'cl:standard-generic-function)
(defmethod type-for-copy ((val standard-method)) 'cl:standard-method)
(defmethod type-for-copy ((val stream)) 'cl:stream)
(defmethod type-for-copy ((val string)) 'cl:string)
(defmethod type-for-copy ((val string-stream)) 'cl:string-stream)
(defmethod type-for-copy ((val structure-class)) 'cl:structure-class)
(defmethod type-for-copy ((val symbol)) 'cl:symbol)


