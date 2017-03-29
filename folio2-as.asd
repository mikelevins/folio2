;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-as.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2-as: an extensible type-conversion utility
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; system
(defsystem "folio2-as"
  :serial t
  :description "uniform tools for converting values from one type to another"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :components ((:module "src"
                        :serial t
                        :components ((:file "as-package")
                                     (:file "as-functions"))))
  :in-order-to ((test-op (test-op "folio-as-tests"))))

;;; (asdf:load-system :folio2-as)
