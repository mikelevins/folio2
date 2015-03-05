;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       taps package
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio2.taps
  (:use :cl :net.bardcode.folio2.as :net.bardcode.folio2.make)
  (:import-from :fset :wb-map :wb-seq)
  (:import-from :net.bardcode.folio2.maps :alist :alist? :plist :plist?)
  (:export
   :characters
   :elements
   :lines
   :objects
   :octets
   :slots
   :tokens))



