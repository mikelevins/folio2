;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       scanning streams
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:net.bardcode.folio.streams
  (:use #:cl)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :append :apply :find :first :get :last :length :map :merge :position :position-if :put 
                          :reduce :remove :rest :reverse :second :sequence :sort :union)
  (:export
   #:characters
   #:lines
   #:objects
   #:octets))




