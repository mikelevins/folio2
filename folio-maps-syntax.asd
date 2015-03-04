;;;; ***********************************************************************
;;;;
;;;; Name:          folio-maps-syntax.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       optional syntax extensions for maps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-maps-syntax
  :serial t
  :description "reader syntax for maps"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-maps)
  :components ((:module "src"
                        :serial t
                        :components ((:file "maps-package")
                                     (:file "maps-syntax")))))

;;; (asdf:load-system :folio-maps-syntax)

