;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.series.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio.series
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.series
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.series.tests
  :serial t
  :description "series tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.series :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "series")))))

(defun load-series ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.series))

(defun load-series-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.series.tests))

;;; (load-series)
;;; (load-series-tests)
