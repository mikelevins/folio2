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

(defgeneric make (type &rest initargs &key &allow-other-keys))


