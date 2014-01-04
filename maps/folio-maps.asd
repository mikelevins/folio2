;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.maps.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio.maps
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.maps
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "types")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.maps-syntax
  :serial t
  :description "reader syntax for maps"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.maps)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "syntax")))))

(asdf:defsystem :net.bardcode.folio.maps.tests
  :serial t
  :description "map tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.maps :net.bardcode.folio.maps-syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "maps")))))

(defun load-maps (&key (load-syntax t))
  (asdf:oos 'asdf:load-op :net.bardcode.folio.maps)
  (when load-syntax
    (asdf:oos 'asdf:load-op :net.bardcode.folio.maps-syntax)))

(defun load-map-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.maps.tests))

;;; (load-maps)
;;; (load-map-tests)
