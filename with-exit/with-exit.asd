;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.with-exit.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio.with-exit
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.with-exit
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.with-exit.tests
  :serial t
  :description "with-exit tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.with-exit :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "with-exit")))))

(defun load-with-exit ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.with-exit))

(defun load-with-exit-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.with-exit.tests))

;;; (load-with-exit)
;;; (load-with-exit-tests)
