;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.generators.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio.generators
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.generators
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.generators.tests
  :serial t
  :description "generator tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.generators :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "generators")))))

(defun load-generators ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.generators))

(defun load-generator-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.generators.tests))

;;; (load-generators)
;;; (load-generator-tests)
