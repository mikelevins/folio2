;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.boxes.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio.boxes
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.boxes
  :serial t
  :description "wrapping values in mutable containers"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")
                                     (:file "types")))))

(asdf:defsystem :net.bardcode.folio.boxes.tests
  :serial t
  :description "box tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.boxes :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "boxes")))))

(defun load-boxes ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.boxes))

(defun load-box-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.boxes.tests))

;;; (load-boxes)
;;; (load-box-tests)
