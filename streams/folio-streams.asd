;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.streams.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio.streams
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.streams
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.streams.tests
  :serial t
  :description "stream tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.streams :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "streams")))))

(defun load-streams ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.streams))

(defun load-stream-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.streams.tests))

;;; (load-streams)
;;; (load-stream-tests)
