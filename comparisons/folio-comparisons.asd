;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.comparisons.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio.comparisons
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.comparisons
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.comparisons.tests
  :serial t
  :description "comparison tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.comparisons :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "comparisons")))))

(defun load-comparisons ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.comparisons))

(defun load-comparison-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.comparisons.tests))

;;; (load-comparisons)
;;; (load-comparison-tests)
