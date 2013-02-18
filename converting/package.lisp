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
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :get :last :length :map :merge :position :position-if :put 
                          :reduce :remove :rest :reverse :second :sequence :sort :union)
  (:import-from :net.bardcode.folio.common 
                :add-first :add-last :any :append2
                :by
                :coalesce :combined-type
                :drop :drop-while
                :element :empty? :every?
                :filter
                :generate 
                :interleave :interpose
                :join
                :next-last
                :partition
                :range :range-from :repeat
                :scan :scan-map :select :sequence? :shuffle :some? 
                :split :subsequence 
                :tails :take :take-by :take-while :type-for-copy
                :unique :unzip
                :zip))




