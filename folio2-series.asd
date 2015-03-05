;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-series.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       common operations on series
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio2-series
  :serial t
  :description "operations on (possibly unbounded) series of values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:folio2-as
               :folio2-make
               :folio2-sequences
               :folio2-pairs
               :fset :series)
  :components ((:module "src"
                        :serial t
                        :components (#+sbcl(:file "suppress-series-warnings")
                                     (:file "series-package")
                                     (:file "series-syntax")
                                     (:file "series-functions")))))

;;; (asdf:load-system :folio2-series)
