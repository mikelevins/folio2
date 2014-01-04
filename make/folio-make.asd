;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.make.maked
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       extensible conversion utility
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.make
  :serial t
  :description "uniform tools for converting values from one type to another"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.make.tests
  :serial t
  :description "converting tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.make :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "make")))))

(asdf:defsystem :net.bardcode.folio.make.tests
  :serial t
  :description "make tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.make :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "make")))))


(defun load-make ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.make))

(defun load-make-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.make.tests))

;;; (load-make)
;;; (load-make-tests)
