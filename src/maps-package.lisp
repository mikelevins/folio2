;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       maps package
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio2.maps
  (:use :cl :net.bardcode.folio2.as :net.bardcode.folio2.make)
  (:shadow :map :merge :values)
  (:import-from :fset :wb-map)
  (:export 
   :alist :alist?
   :contains-key? :contains-value?
   :get-key
   :keys
   :map :map? :merge
   :plist :plist? :put-key
   :values
   :wb-map :wb-map?))




