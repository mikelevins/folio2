;;;; ***********************************************************************
;;;;
;;;; Name:          folio2-functions.asd
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2-functions: combinators and higher-order functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)


(defparameter *folio2-root*
  (make-pathname :directory (pathname-directory *load-truename*)))

;;; (push *folio2-root* asdf:*central-registry*)

(asdf:defsystem :folio2-functions
  :serial t
  :description "tools for working with functions as values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:alexandria :folio2-as :folio2-make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "functions-package")
                                     (:file "functions-functions")))))

;;; (asdf:load-system :folio2-functions)
