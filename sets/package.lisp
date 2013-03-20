;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       treating sequences as sets
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage :net.bardcode.folio.sets
  (:use :cl :net.bardcode.folio.common)
  (:shadowing-import-from :net.bardcode.folio.common
                          :> :>= :< :<=
                          :adjoin :apply :find :first :get :intersection :last :length :merge :position :position-if :put 
                          :reduce :remove :rest :reverse :second :sequence :set :sort :union)
  (:import-from :net.bardcode.folio.common 
                :as :difference :set? :subset?))





