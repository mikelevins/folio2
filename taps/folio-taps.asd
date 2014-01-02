;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio-taps.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio.taps
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.taps
  :serial t
  :description "operations on taps of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :closer-mop
               :net.bardcode.folio.as
               :net.bardcode.folio.make
               :net.bardcode.folio.maps
               :net.bardcode.folio.sequences
               :net.bardcode.folio.series
               :net.bardcode.folio.pairs)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.taps.tests
  :serial t
  :description "sequence and taps tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.taps :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(defun load-taps ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.taps))

(defun load-sequence-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.taps.tests))

;;; (load-taps)
;;; (load-sequence-tests)
