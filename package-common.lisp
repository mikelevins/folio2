;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package-common.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       all external names used by folio packages 
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio.common
  (:shadow :> :>= :< :<= :adjoin :append :apply :find :first :get :intersection :last :length :merge :position :position-if :put 
           :reduce :remove :rest :reverse :second :sequence :set :sort :union)
  (:use :cl)
  (:import-from :fset :seq)
  (:import-from :series series::foundation-series)
  (:export :$ :^ :-> :> :>= :< :<=
           :adjoin :add-first :add-last :alist :alist? :alist->plist :any :append :apply :as :associate
           :binary-append :box :box? :by
           :cascade :characters :coalesce :combined-type :compose :concat :conjoin :contains-key? :contains-value?
           :difference :disjoin :dissociate :drop :drop-while 
           :element :empty? :every? 
           :filter :find :first :flip :fn :foundation-series :function? :functional?
           :generate :generic-function? :get-key
           :head
           :image :indexes :input-stream :input-stream? :interleave :interpose :intersection
           :join :binary-join :join-text
           :keys
           :last :left :length :lines
           :make :make-output-stream :match-prefix? :match-suffix? :merge :method?
           :objects :octets :ordered-map :ordered-map? :output-stream :output-stream?
           :pair :pair? :partial :partition :penult :plist :plist? :plist->alist :position :position-if :put-key
           :range :range-from :readable :readable? :reduce :remove :repeat :repeatedly :rest :reverse :right 
           :rotate-left :rotate-right :rpartial
           :scan :scan-image :second :select :seq :series :series? :sequence :sequence? :set-box! :set? :shuffle :some?
           :sort :split :split-text :stream? :streamable :streamable? :subsequence :subset?
           :table :table? :tail :tails :take :take-by :take-while :text? :type-for-copy
           :unbox :union :unique :unzip
           :vals
           :writable :writable?
           :zip :zipmap))
