;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       functional operations on text strings
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:net.bardcode.folio.text
  (:use #:cl)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :get :last :length :map :merge :position :position-if :put 
                          :reduce :remove :rest :reverse :second :sequence :sort :union)
  (:export
   #:join-text
   #:split-text))





