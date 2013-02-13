;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       combinators and other conveniences 
;;;;                for working with functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio
  (:nicknames :folio)
  (:shadowing-import-from :net.bardcode.folio.common
                          :adjoin :append :apply :find :first :last :length
                          :map :merge :position :reduce :remove :rest
                          :reverse :second :sequence :sort :union) 
  (:use :cl :net.bardcode.folio.common)
  (:export
   ;; boxes
   :box :box? :set-box! :unbox
   ;; constructing
   :make
   ;; converting
   :as :combined-type :type-for-copy
   ;; functions
   :$ :^ :-> :apply :cascade :compose :conjoin :disjoin :flip :fn :partial :rotate-left :rotate-right :rpartial
   ;; ordering
   :> :>= :< :<= :sort
   ;; pairs
   :left :pair :pair? :right
   ;; sequences
   :add-first :add-last :any :append :append2
   :by
   :coalesce :concat :cycle
   :drop :drop-while :dropn
   :element :empty? :every?
   :filter :find :first
   :generate
   :interleave :interpose
   :join :join2
   :last :length
   :map
   :next-last
   :partition :position :position-if
   :range :range-from :reduce :remove :repeat :rest :reverse 
   :scan :scan-map :second :select :sequence :sequence? :shuffle :slice :some? :sort :split :subsequence
   :tails :take :take-by :take-while :taken
   :unique :unzip
   :zip
   ;; sets
   :adjoin :difference :intersect :subset? :union
   ;; streams
   :characters :lines :objects :octets
   ;; tables
   :alist :alist->plist :associate :contains-key? :contains-value? :dissociate :get-key :keys :merge
   :ordered-map :plist :plist->alist :put-key :table :vals :zipmap
   ;; text
   :join-text :split-text))




