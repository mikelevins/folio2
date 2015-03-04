;;;; ***********************************************************************
;;;;
;;;; Name:          folio-maps.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       functional finite maps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-maps
  :serial t
  :description "tools for working with finite maps"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :folio-as :folio-make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "maps-package")
                                     (:file "maps-types")
                                     (:file "maps-functions")))))

;;; (asdf:load-system :folio-maps)
