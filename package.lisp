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
  #|(:shadow :< :<= :> :>= 
           :acons :append :assoc 
           :count :count-if :count-if-not 
           :find :find-if :find-if-not :first 
           :last :length 
           :map :merge :mismatch 
           :position :position-if :position-if-not 
           :reduce :remove :remove-duplicates :remove-if :remove-if-not :rest :reverse 
           :search :second :sequence :sort :substitute :substitute-if :substitute-if-not :values )|#
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
        :net.bardcode.folio.taps)
  (:shadowing-import-from :net.bardcode.folio.comparisons :< :<= :> :>=)
  (:shadowing-import-from :net.bardcode.folio.maps :values :merge :map)
  (:shadowing-import-from :net.bardcode.folio.sequences
                          :acons :append :assoc :count :count-if :count-if-not :find :find-if :find-if-not :first 
                          :last :length :mismatch :position :position-if :position-if-not :reduce :remove :remove-duplicates
                          :remove-if :remove-if-not :rest :reverse :search :second :sequence :sort :substitute :substitute-if
                          :substitute-if-not)

  #|(:export
   :-> :> :>= :< :<= :^ 
   :acons :add-first :add-last :alist :alist? :any :append :apportion :as :assoc
   :binary-< :binary-<= :binary-> :binary->=  :binary-append :binary-equivalent? :binary-join :box :box? :by
   :cascade :characters :coalesce :compose :conjoin :contains-key? :contains-value? :copy :count :count-if :count-if-not
   :deep-copy :disjoin :dispose :drop :drop-while
   :element :elements :empty? :equivalent? :every?
   :filter :find :find-if :find-if-not :first :flip :fn :foundation-series :function? :functional?
   :generic-function? :get-key 
   :head
   :image :indexes :interleave :interpose :iterate 
   :join 
   :keys
   :last :leave :left :length :lines
   :make :map :map? :merge :method? :mismatch :multiple-value-compose
   :octets
   :pair :pair? :partial :partition :penult :plist :plist? :position :position-if :position-if-not :prefix-match? :put-key
   :range :range-from :reduce :remove :remove-duplicates :remove-if :remove-if-not :repeat :rest :reverse :right :rpartial
   :scan :search :second :select :sequence :sequence? :series :series? :set-box! :set-left! :set-right! :shallow-copy
   :shuffle :slots :some? :sort :split :subsequence :substitute :substitute-if :substitute-if-not :suffix-match? 
   :tail :tails :take :take-by :take-while :tokens 
   :unbox :unzip 
   :values 
   :wb-map :wb-map? :wb-seq :wb-seq?
   :zip)|#
  )


