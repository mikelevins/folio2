;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       uniform tools for converting values from one type to another
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage :net.bardcode.folio.converting
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
                :join
                :next-last
                :partition
                :range :range-from :repeat
                :scan :scan-map :select :sequence? :shuffle :slice :some? 
                :split :subsequence 
                :tails :take :take-by :take-while :type-for-copy
                :unique :unzip
                :zip))




