;;;; ***********************************************************************
;;;;
;;;; Name:          folio-series.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       common operations on series
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-series
  :serial t
  :description "operations on (possibly unbounded) series of values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio-as
               :folio-make
               :folio-sequences
               :folio-pairs
               :fset :series)
  :components ((:module "src"
                        :serial t
                        :components ((:file "series-package")
                                     (:file "series-syntax")
                                     (:file "series-functions")))))

;;; (asdf:load-system :folio-series)
