;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-sequences-syntax.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       optional syntax extensions for sequences
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-sequences-syntax"
  :serial t
  :description "reader syntax for sequences"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2-sequences")
  :components ((:module "src"
                        :serial t
                        :components ((:file "sequences-package")
                                     (:file "sequences-syntax"))))
  :in-order-to ((test-op (test-op "folio-sequences-tests"))))

;;; (asdf:load-system :folio2-sequences-syntax)
