;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.pairs.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio.pairs
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.pairs
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.pairs.tests
  :serial t
  :description "pair tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.pairs :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "pairs")))))

(defun load-pairs ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.pairs))

(defun load-pair-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.pairs.tests))

;;; (load-pairs)
;;; (load-pair-tests)
