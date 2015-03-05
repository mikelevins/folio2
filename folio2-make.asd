;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-make.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2-make: an extensible constructor
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-make
  :serial t
  :description "uniform tools for constructing arbitrary values"
  :author "mikel evins <mevins@me.com>"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :components ((:module "src"
                        :serial t
                        :components ((:file "make-package")
                                     (:file "make-functions")))))
;;; (asdf:load-system :folio2-make)
