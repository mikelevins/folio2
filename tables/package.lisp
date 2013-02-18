;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       tools for manipulating finite maps
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage :net.bardcode.folio.tables
  (:use :cl)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :get :last :length :map :merge :position :position-if :put 
                          :reduce :remove :rest :reverse :second :sequence :sort :union)
  (:export
   :alist
   :alist->plist
   :associate
   :contains-key?
   :contains-value?
   :dissociate
   :get-key
   :keys
   :merge
   :ordered-map
   :plist
   :plist->alist
   :put-key
   :table
   :vals
   :zipmap))




