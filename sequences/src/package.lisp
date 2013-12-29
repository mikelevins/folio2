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
  (:use :cl :net.bardcode.folio.as :net.bardcode.folio.make)
  (:shadow :append :find :last :length :search :sequence)
  (:export
   ;; (unbounded) series
   :add-first
   :by
   :coalesce
   :drop :drop-while
   :element :empty?
   :filter :first
   :generate :generator
   :head
   :image :indexes :interleave :interpose
   :partition :position :prefix-match?
   :range-from :reduce :remove :repeat :repeatedly :rest
   :scan :scan-image :second :select :series :series? :some? :subseries
   :tail :tails :take :take-by :take-while
   ;; (bounded) sequences
   :add-last :any :append
   :binary-append :binary-join
   :every?
   :find
   :join
   :last :length
   :penult
   :range :reverse
   :search :sequence :sequence? :shuffle :sort :split :subsequence :suffix-match?
   :unique :unzip
   :fset-sequence :fset-sequence?
   :zip))


