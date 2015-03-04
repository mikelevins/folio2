;;;; ***********************************************************************
;;;;
;;;; Name:          folio-as-syntax.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       optional syntax extensions for folio-as
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-as-syntax
  :serial t
  :description "reader syntax for type conversions"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-as)
  :components ((:module "src"
                        :serial t
                        :components ((:file "as-package")
                                     (:file "as-syntax")))))

;;; (asdf:load-system :folio-as-syntax)
