;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-pairs.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       extensible abstract pairs
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-pairs"
  :serial t
  :description "tools for working with pairs of values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("folio2-as" "folio2-make")
  :components ((:module "src"
                        :serial t
                        :components ((:file "pairs-package")
                                     (:file "pairs-functions"))))
  :in-order-to ((test-op (test-op "folio-pairs-tests"))))

;;; (asdf:load-system :folio2-pairs)
