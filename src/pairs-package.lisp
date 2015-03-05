;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       pairs package
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio2.pairs
  (:use :cl :net.bardcode.folio2.as :net.bardcode.folio2.make)
  (:export :left :pair :pair? :right :set-left! :set-right!))
