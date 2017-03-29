;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-as-syntax.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       optional syntax extensions for folio2-as
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-as-syntax"
  :serial t
  :description "reader syntax for type conversions"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2-as")
  :components ((:module "src"
                        :serial t
                        :components ((:file "as-package")
                                     (:file "as-syntax"))))
  :in-order-to ((test-op (test-op "folio-as-tests"))))

;;; (asdf:load-system :folio2-as-syntax)
