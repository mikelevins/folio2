;;;; ***********************************************************************
;;;;
;;;; Name:          folio2.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2 umbrella system
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2"
  :serial t
  :description "the folio2 umbrella system"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("fset" "series" "alexandria"
               "folio2-as"
               "folio2-as-syntax"
               "folio2-boxes"
               "folio2-functions"
               "folio2-functions-syntax"
               "folio2-make"
               "folio2-maps"
               "folio2-maps-syntax"
               "folio2-pairs"
               "folio2-sequences"
               "folio2-sequences-syntax"
               "folio2-series"
               "folio2-taps")
  :components ((:module "src"
                        :serial t
                        :components ((:file "folio2-package")
                                     (:file "version"))))
  :in-order-to ((test-op (test-op "folio2-tests"))))

;;; (asdf:load-system :folio2)
