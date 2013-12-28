;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       folio meta-system
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio
  :serial t
  :description "the folio umbrella system"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make))


(defun load-folio ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio))

;;; (load-folio)
