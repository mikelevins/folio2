;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       tools for manipulating sequences, series, and generators
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage :net.bardcode.folio.sequences
  (:use :cl)
  (:shadowing-import-from :net.bardcode.folio.common
                          :append :first :last :length :map :position :reduce :remove :rest :reverse :second :sort)
  (:import-from :net.bardcode.folio.common 
                :add-first :add-last :any :append2
                :by
                :coalesce :combined-type
                :drop :dropn :drop-while
                :element :empty? :every?
                :filter
                :generate 
                :interleave :interpose
                :join :join2
                :next-last
                :partition
                :range :range-from :repeat
                :scan :scan-map :select :sequence? :shuffle :slice :some? 
                :split :subsequence 
                :tails :take :take-by :take-while :type-for-copy
                :unique :unzip
                :zip)
  (:export :add-first :add-last :any :append2 :append 
           :by
           :coalesce
           :drop :dropn :drop-while
           :element :empty? :every?
           :filter :first
           :generate
           :interleave :interpose
           :join
           :last :length 
           :map
           :next-last
           :partition :position
           :range :range-from :reduce :remove :repeat :rest :reverse
           :scan :scan-map :second :select :sequence? :shuffle :slice :some? :sort :split :subsequence
           :tails :take :take-by :take-while 
           :unique :unzip
           :zip))



