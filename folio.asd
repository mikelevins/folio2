;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       folio meta-system
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio
  :serial t
  :description "the folio umbrella system"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :alexandria 
                     :net.bardcode.folio.as
                     :net.bardcode.folio.boxes
                     :net.bardcode.folio.functions
                     :net.bardcode.folio.make
                     :net.bardcode.folio.maps
                     :net.bardcode.folio.pairs
                     :net.bardcode.folio.sequences
                     :net.bardcode.folio.series
                     :net.bardcode.folio.taps)
  :components ((:file "package")))


(defun load-folio ()
  (let* ((asdf:*compile-file-warnings-behaviour* #+sbcl :ignore #-sbcl :warn)
         (asdf:*compile-file-failure-behaviour* #+sbcl :ignore #-sbcl :warn)
         (project-root (slot-value (asdf:find-system :net.bardcode.folio) 'asdf::absolute-pathname))
         (sysdefs (list (merge-pathnames "as/folio-as.asd" project-root)
                        (merge-pathnames "boxes/folio-boxes.asd" project-root)
                        (merge-pathnames "functions/folio-functions.asd" project-root)
                        (merge-pathnames "make/folio-make.asd" project-root)
                        (merge-pathnames "maps/folio-maps.asd" project-root)
                        (merge-pathnames "pairs/folio-pairs.asd" project-root)
                        (merge-pathnames "sequences/folio-sequences.asd" project-root)
                        (merge-pathnames "series/folio-series.asd" project-root)
                        (merge-pathnames "taps/folio-taps.asd" project-root))))
    (dolist (def sysdefs)(load def))
    (asdf:oos 'asdf:load-op :net.bardcode.folio)))

;;; (load-folio)
