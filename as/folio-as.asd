;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.as.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       extensible conversion utility
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.as
  :serial t
  :description "uniform tools for converting values from one type to another"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.as.tests
  :serial t
  :description "converting tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "as")))))


(defun load-as ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.as))

(defun load-folio-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.as.tests))

;;; (load-as)
;;; (load-as-tests)
