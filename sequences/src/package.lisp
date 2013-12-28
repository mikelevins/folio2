;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - Bard features in Common Lisp
;;;; Purpose:       sequences package
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio.sequences
  (:use :cl :net.bardcode.folio.as :net.bardcode.folio.make)
  (:shadow :append :find :last :length :search :sequence)
  (:export :add-first :add-last :any :append
           :binary-append :by
           :coalesce
           :drop :drop-while
           :element :empty? :every?
           :filter :find :first
           :head
           :image :indexes :interleave :interpose
           :join :binary-join
           :last :length
           :partition :penult :position :prefix-match?
           :range :range-from :reduce :remove :repeat :repeatedly :rest :reverse
           :scan :scan-image :search :second :select :sequence :sequence? :series :series? 
           :shuffle :some? :sort :split :subsequence :suffix-match?
           :tail :tails :take :take-by :take-while
           :unique :unzip
           :zip))

