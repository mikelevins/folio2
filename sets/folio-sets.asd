;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.sets.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio.sets
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.sets
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.sets.tests
  :serial t
  :description "set tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.sets :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "sets")))))

(defun load-sets ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.sets))

(defun load-set-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.sets.tests))

;;; (load-sets)
;;; (load-set-tests)
