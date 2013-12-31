;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.copy.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       extensible conversion utility
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.copy
  :serial t
  :description "uniform tools for converting values from one type to another"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.copy.tests
  :serial t
  :description "converting tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.copy :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "copy")))))


(defun load-copy ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.copy))

(defun load-folio-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.copy.tests))

;;; (load-copy)
;;; (load-copy-tests)
