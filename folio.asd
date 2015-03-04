;;;; ***********************************************************************
;;;;
;;;; Name:          folio.asd
;;;; Project:       folio - Functional idioms for Common Lisp
;;;; Purpose:       folio umbrella system
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defparameter *folio-root*
  (make-pathname :directory (pathname-directory *load-truename*)))

;;; (push *folio-root* asdf:*central-registry*)

(asdf:defsystem :folio
  :serial t
  :description "the folio functional-idioms system"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :alexandria 
                     :folio-as
                     :folio-as-syntax
                     :folio-boxes
                     :folio-functions
                     :folio-functions-syntax
                     :folio-make
                     :folio-maps
                     :folio-maps-syntax
                     :folio-pairs
                     :folio-sequences
                     :folio-sequences-syntax
                     :folio-series
                     :folio-taps)
  :components ((:module "src"
                        :serial t
                        :components ((:file "folio-package")
                                     (:file "version")))))

;;; (asdf:load-system :folio)
