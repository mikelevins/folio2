;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       folio2 umbrella package
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio2
  (:nicknames :folio2)
  (:use :cl)
  (:use :net.bardcode.folio2.as)
  (:use :net.bardcode.folio2.boxes)
  (:use :net.bardcode.folio2.functions)
  (:use :net.bardcode.folio2.make)
  (:use :net.bardcode.folio2.pairs)
  (:use :net.bardcode.folio2.taps)
  (:import-from :net.bardcode.folio2.maps :alist :alist? :contains-key? :contains-value?
                :get-key :keys :map? :plist :plist? :put-key :wb-map :wb-map?)
  (:shadowing-import-from :net.bardcode.folio2.maps :values :merge :map)
  (:import-from :net.bardcode.folio2.sequences
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
  (:shadowing-import-from :net.bardcode.folio2.sequences
                          :acons :assoc :assoc-if :assoc-if-not :append
                          :count :count-if :count-if-not
                          :find :find-if :find-if-not :first
                          :last :length 
                          :mismatch
                          :position :position-if :position-if-not 
                          :reduce :remove :remove-duplicates :remove-if :remove-if-not :rest :reverse
                          :search :sequence :second :sort :substitute :substitute-if :substitute-if-not)
  (:use :net.bardcode.folio2.series)
  (:export  :*folio2-version*
            :alist :alist? :contains-key? :contains-value?
            :get-key :keys :map? :plist :plist? :put-key :wb-map :wb-map? :values :merge :map
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
            :zip
            :acons :assoc :assoc-if :assoc-if-not :append
            :count :count-if :count-if-not
            :find :find-if :find-if-not :first
            :last :length 
            :mismatch
            :position :position-if :position-if-not 
            :reduce :remove :remove-duplicates :remove-if :remove-if-not :rest :reverse
            :search :sequence :second :sort :substitute :substitute-if :substitute-if-not))


