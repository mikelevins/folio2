;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-maps.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       functional finite maps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(defsystem "folio2-maps"
  :serial t
  :description "tools for working with finite maps"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on ("fset" "folio2-as" "folio2-make")
  :components ((:module "src"
                        :serial t
                        :components ((:file "maps-package")
                                     (:file "maps-types")
                                     (:file "maps-functions"))))
  :in-order-to ((test-op (test-op "folio-maps-tests"))))

;;; (asdf:load-system :folio2-maps)
