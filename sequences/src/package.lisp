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
  (:shadow :append :count :count-if :count-if-not :find :find-if :find-if-not :first :last :length 
           :mismatch :position :position-if :position-if-not 
           :remove :remove-duplicates :remove-if :remove-if-not
           :search :sequence :sort :stable-sort  :substitute :substitute-if :substitute-if-not)
  (:import-from :fset :wb-seq)
  (:export
   :add-first :add-last :any :append
   :binary-append :binary-join :by
   :count :count-if :count-if-not
   :drop :drop-while
   :element :empty? :every?
   :filter :find :find-if :find-if-not :first
   :head
   :image :indexes :interleave :interpose
   :join
   :last :leave :length 
   :make :mismatch
   :partition :penult :position :position-if :position-if-not :prefix-match?
   :range :reduce :remove :remove-duplicates  :remove-if :remove-if-not :rest :reverse
   :search :second :select :sequence :sequence? :shuffle :some? :sort :stable-sort
   :split :subsequence :substitute :substitute-if :substitute-if-not :suffix-match?
   :tail :tails :take :take-by :take-while
   :unzip 
   :wb-seq :wb-seq?
   :zip))





