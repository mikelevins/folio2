;;;; ***********************************************************************
;;;;
;;;; Name:          folio-boxes.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       folio-boxes: mutable containers
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-boxes
  :serial t
  :description "wrapping values in mutable containers"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-as :folio-make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "boxes-package")
                                     (:file "boxes-types")
                                     (:file "boxes-functions")))))

;;; (asdf:load-system :folio-boxes)
