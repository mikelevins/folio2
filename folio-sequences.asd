;;;; ***********************************************************************
;;;;
;;;; Name:          folio-sequences.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       common operations on sequences
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-sequences
  :serial t
  :description "operations on sequences of values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-as
               :folio-make
               :folio-pairs
               :fset :series)
  :components ((:module "src"
                        :serial t
                        :components ((:file "sequences-package")
                                     (:file "sequences-functions")))))

;;; (asdf:load-system :folio-sequences)
