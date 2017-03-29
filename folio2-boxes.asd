;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-boxes.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2-boxes: mutable containers
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-boxes"
  :serial t
  :description "wrapping values in mutable containers"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2-as" "folio2-make")
  :components ((:module "src"
                        :serial t
                        :components ((:file "boxes-package")
                                     (:file "boxes-types")
                                     (:file "boxes-functions"))))
  :in-order-to ((test-op (test-op "folio-boxes-tests"))))

;;; (asdf:load-system :folio2-boxes)
