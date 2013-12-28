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

(in-package :net.bardcode.folio.make)

;;; function make
;;;
;;; (make type &key &allow-other-keys) => anything
;;; ---------------------------------------------------------------------
;;; returns a new value of TYPE

(defgeneric make (type &key &allow-other-keys))

