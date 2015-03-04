;;;; ***********************************************************************
;;;;
;;;; Name:          folio-make.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       folio-make: an extensible constructor
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-make
  :serial t
  :description "uniform tools for constructing arbitrary values"
  :author "mikel evins <mevins@me.com>"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :components ((:module "src"
                        :serial t
                        :components ((:file "make-package")
                                     (:file "make-functions")))))
;;; (asdf:load-system :folio-make)
