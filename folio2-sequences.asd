;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-sequences.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       common operations on sequences
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-sequences
  :serial t
  :description "operations on sequences of values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-as
               :folio2-make
               :folio2-pairs
               :fset :series)
  :components ((:module "src"
                        :serial t
                        :components ((:file "sequences-package")
                                     (:file "sequences-functions")))))

;;; (asdf:load-system :folio2-sequences)
