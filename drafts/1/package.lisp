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
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :indexes :intersection :last :length
                          :merge :position :position-if :reduce :remove :repeatedly :rest
                          :reverse :search :second :sequence :sort :union) 
  (:import-from :net.bardcode.folio.common :binary-append :foundation-series :make-output-stream :seq) 
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
   ;; comparing
   :> :>= :< :<= :equivalent?
   ;; pairs
   :left :pair :pair? :right
   ;; sequences
   :add-first :add-last :any
   :binary-append
   :every?
   :find
   :join :binary-join
   :last :length
   :match-suffix?
   :position
   :range 
   :reduce :reverse
   :search :select
   :seq :sequence :sequence? :shuffle
   :some? :sort :split :subsequence
   :unique 
   ;; series
   :by
   :coalesce
   :drop :drop-while
   :element :empty?
   :filter :first
   :generate
   :head
   :image :indexes :interleave :interpose
   :match-prefix?
   :partition :penult
   :range-from :remove :repeat :repeatedly :rest 
   :scan :scan-image :second :series :series?
   :tail :tails :take :take-by :take-while
   :unzip
   :zip
   ;; sets
   :adjoin :difference :intersection :set? :subset? :union
   ;; streams
   :characters :input-stream :input-stream? :lines :objects :octets :output-stream :output-stream? 
   :readable :readable? :streamable :streamable? :writable :writable?
   ;; mappings
   :alist :alist? :alist->plist :associate :contains-key? :contains-value? :dissociate :get-key :keys 
   :mapping :mapping? :merge
   :ordered-map :ordered-map? :plist :plist? :plist->alist :put-key :vals :zipmap
   ;; text
   :join-text :split-text :text?))



