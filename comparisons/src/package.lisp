;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - Bard features in Common Lisp
;;;; Purpose:       comparisons package
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio.comparisons
  (:use :cl :net.bardcode.folio.as :net.bardcode.folio.comparisons)
  (:shadow :> :>= :< :<=)
  (:export :> :>= :< :<= :equivalent?))

