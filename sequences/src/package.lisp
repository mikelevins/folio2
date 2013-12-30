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
  (:import-from :fset :wb-seq)
  (:export
   :add-first :add-last :any :append
   :binary-append :binary-join :by
   :coalesce
   :drop :drop-while
   :element :empty? :every?
   :filter :find :first
   :head
   :image :indexes :interleave :interpose
   :join
   :last :leave :length 
   :partition :penult :position :prefix-match?
   :range :reduce :remove :rest :reverse
   :search :second :select :sequence :sequence?
   :shuffle :some? :sort :split :subsequence :suffix-match?
   :tail :tails :take :take-by :take-while
   :unique :unzip 
   :wb-seq :wb-seq?
   :zip))



