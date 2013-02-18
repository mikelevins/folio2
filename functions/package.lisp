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

(defpackage :net.bardcode.folio.functions
  (:use :cl)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :get :last :length :map :merge :position :position-if :put 
                          :reduce :remove :rest :reverse :second :sequence :sort :union)
  (:import-from :net.bardcode.folio.common 
                :$ :^ :-> :cascade :compose :conjoin :disjoin :flip :fn :function? :functional? :generic-function? :method?
                :partial :rotate-left :rotate-right :rpartial) 
  (:export
   :$ :^ :-> :apply :cascade :compose :conjoin :disjoin :flip :fn :function? :functional? :generic-function? :method?
   :partial :rotate-left :rotate-right :rpartial))


