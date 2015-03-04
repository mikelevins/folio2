;;;; ***********************************************************************
;;;;
;;;; Name:          folio-pairs.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       extensible abstract pairs
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-pairs
  :serial t
  :description "tools for working with pairs of values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-as :folio-make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "pairs-package")
                                     (:file "pairs-functions")))))

;;; (asdf:load-system :folio-pairs)
