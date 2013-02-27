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

;;; folio1 APIs not covered:
;;; functions: choose-any contains? get make-as put slice

(defpackage :net.bardcode.folio
  (:nicknames :folio)
  (:shadowing-import-from :net.bardcode.folio.common
                          :adjoin :apply :find :first :indexes :intersection :last :length
                          :merge :position :position-if :reduce :remove :rest
                          :reverse :second :sequence :sort :union) 
  (:import-from :net.bardcode.folio.common :foundation-series :seq) 
  (:use :cl :net.bardcode.folio.common)
  (:export
   ;; boxes
   :box :box? :set-box! :unbox
   ;; constructing
   :make
   ;; converting
   :as :combined-type :type-for-copy
   ;; functions
   :$ :^ :-> :apply :cascade :compose :conjoin :disjoin :flip :fn :function? :functional? :generic-function? :method?
   :partial :rotate-left :rotate-right :rpartial
   ;; ordering
   :> :>= :< :<= :sort
   ;; pairs
   :left :pair :pair? :right
   ;; sequences
   :add-first :add-last :any
   :by
   :coalesce :concat
   :drop :drop-while
   :element :empty? :every?
   :filter :find :first
   :generate
   :head
   :image :indexes :interleave :interpose
   :join :join2
   :last :length
   :next-last
   :ordered-map
   :partition :position :position-if
   :range :range-from :reduce :remove :repeat :rest :reverse 
   :scan :scan-map :second :select :sequence :sequence? :series? :shuffle :some? :sort :split :subsequence
   :tail :tails :take :take-by :take-while
   :unique :unzip
   :zip
   ;; sets
   :adjoin :difference :intersection :set? :subset? :union
   ;; streams
   :characters :lines :objects :octets
   ;; tables
   :alist :alist? :alist->plist :associate :contains-key? :contains-value? :dissociate :get-key :keys :merge
   :ordered-map :ordered-map? :plist :plist? :plist->alist :put-key :table :table? :vals :zipmap
   ;; text
   :join-text :split-text :text?))



