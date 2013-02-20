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
  (:shadow :adjoin :append :apply :find :first :get :intersection :last :length :map :merge :position :position-if :put 
           :reduce :remove :rest :reverse :second :sequence :set :sort :union)
  (:use :cl)
  (:export :$ :^ :-> :> :>= :< :<=
           :adjoin :add-first :add-last :alist :alist? :alist->plist :any :append :append2 :apply :as :associate
           :box :box? :by
           :cascade :characters :coalesce :combined-type :compose :concat :conjoin :contains-key? :contains-value?
           :difference :disjoin :dissociate :drop :drop-while 
           :element :empty? :every? 
           :filter :find :first :flip :fn :function? :functional?
           :generate :generic-function? :get-key
           :indexes :interleave :interpose  :intersection
           :join :join2 :join-text
           :keys
           :last :left :length :lines
           :make :map :merge :method?
           :next-last
           :objects :octets :ordered-map :ordered-map?
           :pair :pair? :partial :partition :plist :plist? :plist->alist :position :position-if :put-key
           :range :range-from :reduce :remove :repeat :rest :reverse :right :rotate-left :rotate-right :rpartial
           :scan :scan-map :second :select :series? :sequence :sequence? :set-box! :set? :shuffle :some?
           :sort :split :split-text :stream? :subsequence :subset?
           :table :table? :tails :take :take-by :take-while :text? :type-for-copy
           :unbox :union :unique :unzip
           :vals
           :zip :zipmap))
