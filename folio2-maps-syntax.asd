;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-maps-syntax.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       optional syntax extensions for maps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-maps-syntax"
  :serial t
  :description "reader syntax for maps"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2-maps")
  :components ((:module "src"
                        :serial t
                        :components ((:file "maps-package")
                                     (:file "maps-syntax"))))
  :in-order-to ((test-op (test-op "folio-maps-tests"))))

;;; (asdf:load-system :folio2-maps-syntax)
