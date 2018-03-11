;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       series package
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio2.series
  (:use :cl :net.bardcode.folio2.as :net.bardcode.folio2.make)
  (:import-from :fset :wb-seq)
  (:import-from :series :foundation-series :cotruncate :subseries)
  (:import-from :net.bardcode.folio2.pairs :left :pair :right)
  (:shadowing-import-from :net.bardcode.folio2.sequences
                          :add-first :add-last :any :append :apportion :assoc
                          :binary-append :binary-join :by
                          :count :count-if :count-if-not
                          :deep-copy :dispose :drop :drop-while
                          :element :empty? :every?
                          :filter :find :find-if :find-if-not :first
                          :head
                          :image :indexes :interleave :interpose
                          :join
                          :last :leave :length 
                          :make :mismatch
                          :partition :penult :position :position-if :position-if-not :prefix-match?
                          :range :reduce :remove :remove-duplicates :remove-if :remove-if-not :rest :reverse
                          :search :second :select :sequence :sequence? :shallow-copy :shuffle :some? :sort
                          :split :subsequence :substitute :substitute-if :substitute-if-not :suffix-match?
                          :tail :tails :take :take-by :take-while
                          :unzip 
                          :wb-seq?
                          :zip)
  (:export
   :coalesce
   :foundation-series
   :iterate
   :range-from
   :repeat
   :scan
   :series
   :series?))



