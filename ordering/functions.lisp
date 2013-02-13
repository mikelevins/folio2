;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       sorting values into stable orders
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.ordering)


;;; function >
;;;
;;; (> thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:>, providing an extensible generic version

(defgeneric > (thing1 thing2 &rest more-things))

;;; function >=
;;;
;;; (>= thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:>=, providing an extensible generic version

(defgeneric >= (thing1 thing2 &rest more-things))

;;; function <
;;;
;;; (< thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:<, providing an extensible generic version

(defgeneric < (thing1 thing2 &rest more-things))

;;; function <=
;;;
;;; (<= thing1 thing2 &rest more-things) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:<=, providing an extensible generic version

(defgeneric <= (thing1 thing2 &rest more-things))

;;; function sort
;;;
;;; (sort sequence &key (test '<)) => sequence'
;;; ---------------------------------------------------------------------
;;; shadows cl:sort, providing an extensible generic version

(defgeneric sort (sequence &key test))








