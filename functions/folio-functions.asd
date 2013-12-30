;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio-functions.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio-functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.functions
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:alexandria)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")
                                     (:file "syntax")))))

(asdf:defsystem :net.bardcode.folio.functions.tests
  :serial t
  :description "function tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.functions :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "functions")))))

(defun load-functions ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.functions))

(defun load-function-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.functions.tests))

;;; (load-functions)
;;; (load-function-tests)
