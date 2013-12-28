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
                          :> :>= :< :<=
                          :adjoin :append :binary-append :find :first :get :last :length :merge :position :position-if :put 
                          :reduce :remove :rest :reverse :search :second :sequence :sort :union)
  (:import-from :net.bardcode.folio.common 
                :add-first :add-last :any :as
                :by
                :coalesce :combined-type
                :drop :drop-while
                :element :empty? :every?
                :filter :foundation-series
                :generate 
                :image :indexes :interleave :interpose
                :join :binary-join
                :keys
                :make :match-prefix? :match-suffix?
                :partition :penult
                :range :range-from :repeat
                :scan :scan-image :seq :select :sequence? :series :series? :shuffle :some? 
                :split :subsequence 
                :tails :take :take-by :take-while :type-for-copy
                :unique :unzip
                :zip))



