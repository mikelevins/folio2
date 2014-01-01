;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - Bard features in Common Lisp
;;;; Purpose:       folio umbrella package
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio
  (:nicknames :folio)
  (:shadow :< :<= :> :>= 
           :acons :append :assoc 
           :count :count-if :count-if-not 
           :find :find-if :find-if-not :first 
           :last :length 
           :map :merge :mismatch 
           :position :position-if :position-if-not 
           :reduce :remove :remove-duplicates :remove-if :remove-if-not :rest :reverse 
           :search :second :sequence :sort :substitute :substitute-if :substitute-if-not :values )
  (:use :cl
        :net.bardcode.folio.as
        :net.bardcode.folio.boxes
        :net.bardcode.folio.comparisons
        :net.bardcode.folio.copy
        :net.bardcode.folio.functions
        :net.bardcode.folio.make
        :net.bardcode.folio.maps
        :net.bardcode.folio.pairs
        :net.bardcode.folio.sequences
        :net.bardcode.folio.series
        ;;:net.bardcode.folio.sets
        ;;:net.bardcode.folio.taps
        ))


