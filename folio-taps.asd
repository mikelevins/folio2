;;;; ***********************************************************************
;;;;
;;;; Name:          folio-taps.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       values as series
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem :folio-taps
  :serial t
  :description "tools for treating objects as streams of values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :closer-mop
                     :folio-as
                     :folio-make
                     :folio-maps
                     :folio-sequences
                     :folio-series
                     :folio-pairs)
  :components ((:module "src"
                        :serial t
                        :components ((:file "taps-package")
                                     (:file "taps-functions")))))

;;; (asdf:load-system :folio-taps)
