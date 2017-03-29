;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-taps.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       values as series
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-taps"
  :serial t
  :description "tools for treating objects as streams of values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("fset" "closer-mop"
               "folio2-as" "folio2-make" "folio2-maps"
               "folio2-sequences" "folio2-series" "folio2-pairs")
  :components ((:module "src"
                        :serial t
                        :components ((:file "taps-package")
                                     (:file "taps-functions"))))
  :in-order-to ((test-op (test-op "folio-taps-tests"))))

;;; (asdf:load-system :folio2-taps)
