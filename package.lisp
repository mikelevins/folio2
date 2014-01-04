;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - Bard features in Common Lisp
;;;; Purpose:       folio umbrella package
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio
  (:nicknames :folio)
  (:use :cl)
  (:use :net.bardcode.folio.as)
  (:use :net.bardcode.folio.boxes)
  (:use :net.bardcode.folio.functions)
  (:use :net.bardcode.folio.make)
  (:use :net.bardcode.folio.pairs)
  (:use :net.bardcode.folio.taps)
  (:import-from :net.bardcode.folio.maps :alist :alist? :contains-key? :contains-value?
                :get-key :keys :map? :plist :plist? :put-key :wb-map :wb-map?)
  (:shadowing-import-from :net.bardcode.folio.maps :values :merge :map)
  (:import-from :net.bardcode.folio.sequences
                :add-first :add-last :any :apportion
                :binary-append :binary-join :by
                :dispose :drop :drop-while
                :element :empty? :every?
                :filter
                :head
                :image :indexes :interleave :interpose
                :join
                :leave
                :make
                :partition :penult :prefix-match?
                :range
                :select :sequence? :shuffle :some?
                :split :subsequence :suffix-match?
                :tail :tails :take :take-by :take-while
                :unzip 
                :wb-seq :wb-seq?
                :zip)
  (:shadowing-import-from :net.bardcode.folio.sequences
                          :acons :assoc :assoc-if :assoc-if-not :append
                          :count :count-if :count-if-not
                          :find :find-if :find-if-not :first
                          :last :length 
                          :mismatch
                          :position :position-if :position-if-not 
                          :reduce :remove :remove-duplicates :remove-if :remove-if-not :rest :reverse
                          :search :sequence :second :sort :substitute :substitute-if :substitute-if-not)
  (:use :net.bardcode.folio.series))


