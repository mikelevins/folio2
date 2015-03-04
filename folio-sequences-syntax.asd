;;;; ***********************************************************************
;;;;
;;;; Name:          folio-sequences-syntax.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       optional syntax extensions for sequences
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-sequences-syntax
  :serial t
  :description "reader syntax for sequences"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-sequences)
  :components ((:module "src"
                        :serial t
                        :components ((:file "sequences-package")
                                     (:file "sequences-syntax")))))

;;; (asdf:load-system :folio-sequences-syntax)
