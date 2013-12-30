;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - Bard features in Common Lisp
;;;; Purpose:       series package
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio.series
  (:use :cl :net.bardcode.folio.as :net.bardcode.folio.make
         :net.bardcode.folio.sequences)
  (:shadow :append :find :last :length :search :sequence)
  (:import-from :series :foundation-series)
  (:export
   :foundation-series
   :range-from :repeat
   :scan :series :series?))



