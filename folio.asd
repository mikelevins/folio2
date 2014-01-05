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

;;; ---------------------------------------------------------------------
;;; AS
;;; ---------------------------------------------------------------------

;;; system
(asdf:defsystem :net.bardcode.folio.as
  :serial t
  :description "uniform tools for converting values from one type to another"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :components ((:module "src"
                        :serial t
                        :components ((:file "as-package")
                                     (:file "as-functions")))))

;;; syntax extensions
(asdf:defsystem :net.bardcode.folio.as.syntax
  :serial t
  :description "reader syntax for type conversions"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as)
  :components ((:module "src"
                        :serial t
                        :components ((:file "as-package")
                                     (:file "as-syntax")))))

;;; tests
(asdf:defsystem :net.bardcode.folio.as.tests
  :serial t
  :description "converting tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.as.syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "as")))))

;;; loaders
;;; ---------------------------------------------------------------------

(defun load-as (&key (load-syntax t))
  (asdf:oos 'asdf:load-op :net.bardcode.folio.as)
  (when load-syntax
    (asdf:oos 'asdf:load-op :net.bardcode.folio.as.syntax)))

(defun load-as-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.as.tests))

;;; (load-as)
;;; (load-as-tests)

;;; ---------------------------------------------------------------------
;;; BOXES
;;; ---------------------------------------------------------------------

;;; system
(asdf:defsystem :net.bardcode.folio.boxes
  :serial t
  :description "wrapping values in mutable containers"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "boxes-package")
                                     (:file "boxes-types")
                                     (:file "boxes-functions")))))

;;; tests
(asdf:defsystem :net.bardcode.folio.boxes.tests
  :serial t
  :description "box tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.boxes :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "boxes")))))

;;; loaders
;;; ---------------------------------------------------------------------

(defun load-boxes ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.boxes))

(defun load-box-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.boxes.tests))

;;; (load-boxes)
;;; (load-box-tests)

;;; ---------------------------------------------------------------------
;;; FUNCTIONS
;;; ---------------------------------------------------------------------

;;; system
(asdf:defsystem :net.bardcode.folio.functions
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:alexandria)
  :components ((:module "src"
                        :serial t
                        :components ((:file "functions-package")
                                     (:file "functions-functions")))))

;;; syntax
(asdf:defsystem :net.bardcode.folio.functions.syntax
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:alexandria :net.bardcode.folio.functions)
  :components ((:module "src"
                        :serial t
                        :components ((:file "functions-package")
                                     (:file "functions-syntax")))))

;;; tests
(asdf:defsystem :net.bardcode.folio.functions.tests
  :serial t
  :description "function tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.functions :net.bardcode.folio.functions.syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "functions")))))

;;; loaders
;;; ---------------------------------------------------------------------
(defun load-functions (&key (load-syntax t))
  (asdf:oos 'asdf:load-op :net.bardcode.folio.functions)
  (when load-syntax
    (asdf:oos 'asdf:load-op :net.bardcode.folio.functions.syntax)))

(defun load-function-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.functions.tests))

;;; (load-functions)
;;; (load-function-tests)

;;; ---------------------------------------------------------------------
;;; MAKE
;;; ---------------------------------------------------------------------

;;; system
(asdf:defsystem :net.bardcode.folio.make
  :serial t
  :description "uniform tools for converting values from one type to another"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :components ((:module "src"
                        :serial t
                        :components ((:file "make-package")
                                     (:file "make-functions")))))

;;; tests
(asdf:defsystem :net.bardcode.folio.make.tests
  :serial t
  :description "converting tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.make :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "make")))))

;;; loaders
;;; ---------------------------------------------------------------------
(defun load-make ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.make))

(defun load-make-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.make.tests))

;;; (load-make)
;;; (load-make-tests)

;;; ---------------------------------------------------------------------
;;; MAPS
;;; ---------------------------------------------------------------------

;;; system
(asdf:defsystem :net.bardcode.folio.maps
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "maps-package")
                                     (:file "maps-types")
                                     (:file "maps-functions")))))

;;; syntax
(asdf:defsystem :net.bardcode.folio.maps.syntax
  :serial t
  :description "reader syntax for maps"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.maps)
  :components ((:module "src"
                        :serial t
                        :components ((:file "maps-package")
                                     (:file "maps-syntax")))))

;;; tests
(asdf:defsystem :net.bardcode.folio.maps.tests
  :serial t
  :description "map tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.maps :net.bardcode.folio.maps.syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "maps")))))


;;; loaders
;;; ---------------------------------------------------------------------

(defun load-maps (&key (load-syntax t))
  (asdf:oos 'asdf:load-op :net.bardcode.folio.maps)
  (when load-syntax
    (asdf:oos 'asdf:load-op :net.bardcode.folio.maps-syntax)))

(defun load-map-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.maps.tests))

;;; (load-maps)
;;; (load-map-tests)

;;; ---------------------------------------------------------------------
;;; PAIRS
;;; ---------------------------------------------------------------------

;;; system
(asdf:defsystem :net.bardcode.folio.pairs
  :serial t
  :description "associating paris of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as :net.bardcode.folio.make)
  :components ((:module "src"
                        :serial t
                        :components ((:file "pairs-package")
                                     (:file "pairs-functions")))))

;;; tests
(asdf:defsystem :net.bardcode.folio.pairs.tests
  :serial t
  :description "pair tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.pairs :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "pairs")))))

;;; loaders
;;; ---------------------------------------------------------------------

(defun load-pairs ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.pairs))

(defun load-pair-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.pairs.tests))

;;; (load-pairs)
;;; (load-pair-tests)

;;; ---------------------------------------------------------------------
;;; SEQUENCES
;;; ---------------------------------------------------------------------

;;; system
(asdf:defsystem :net.bardcode.folio.sequences
  :serial t
  :description "operations on sequences of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as
               :net.bardcode.folio.make
               :net.bardcode.folio.pairs
               :fset :series)
  :components ((:module "src"
                        :serial t
                        :components ((:file "sequences-package")
                                     (:file "sequences-functions")))))

;;; syntax
(asdf:defsystem :net.bardcode.folio.sequences.syntax
  :serial t
  :description "reader syntax for sequences"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.sequences)
  :components ((:module "src"
                        :serial t
                        :components ((:file "sequences-package")
                                     (:file "sequences-syntax")))))

;;; tests
(asdf:defsystem :net.bardcode.folio.sequences.tests
  :serial t
  :description "sequence and series tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.sequences :net.bardcode.folio.sequences.syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "sequences")))))

;;; loaders
;;; ---------------------------------------------------------------------

(defun load-sequences (&key (load-syntax t))
  (asdf:oos 'asdf:load-op :net.bardcode.folio.sequences)
  (when load-syntax
    (asdf:oos 'asdf:load-op :net.bardcode.folio.sequences.syntax)))

(defun load-sequence-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.sequences.tests))

;;; (load-sequences)
;;; (load-sequence-tests)

;;; ---------------------------------------------------------------------
;;; SERIES
;;; ---------------------------------------------------------------------

;;; system
(asdf:defsystem :net.bardcode.folio.series
  :serial t
  :description "operations on series of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as
               :net.bardcode.folio.make
               :net.bardcode.folio.sequences
               :net.bardcode.folio.pairs
               :fset :series)
  :components ((:module "src"
                        :serial t
                        :components ((:file "series-package")
                                     (:file "series-functions")))))

;;; syntax
(asdf:defsystem :net.bardcode.folio.series.syntax
  :serial t
  :description "reader syntax for sequences"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.series)
  :components ((:module "src"
                        :serial t
                        :components ((:file "series-package")
                                     (:file "series-syntax")))))

;;; tests
(asdf:defsystem :net.bardcode.folio.series.tests
  :serial t
  :description "sequence and series tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.series :net.bardcode.folio.series.syntax :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "series")))))

;;; loaders
;;; ---------------------------------------------------------------------

(defun load-series (&key (load-syntax t))
  (asdf:oos 'asdf:load-op :net.bardcode.folio.series)
  (when load-syntax
    (asdf:oos 'asdf:load-op :net.bardcode.folio.series.syntax)))

(defun load-sequence-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.series.tests))

;;; (load-series)
;;; (load-sequence-tests)

;;; ---------------------------------------------------------------------
;;; TAPS
;;; ---------------------------------------------------------------------

;;; system
(asdf:defsystem :net.bardcode.folio.taps
  :serial t
  :description "operations on taps of values"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :closer-mop
               :net.bardcode.folio.as
               :net.bardcode.folio.make
               :net.bardcode.folio.maps
               :net.bardcode.folio.sequences
               :net.bardcode.folio.series
               :net.bardcode.folio.pairs)
  :components ((:module "src"
                        :serial t
                        :components ((:file "taps-package")
                                     (:file "taps-functions")))))

;;; tests
(asdf:defsystem :net.bardcode.folio.taps.tests
  :serial t
  :description "sequence and taps tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.taps :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "taps")))))

;;; loaders
;;; ---------------------------------------------------------------------

(defun load-taps ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.taps))

(defun load-tap-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.taps.tests))

;;; (load-taps)
;;; (load-tap-tests)

;;; ---------------------------------------------------------------------
;;; FOLIO umbrella system
;;; ---------------------------------------------------------------------

;;; system
(asdf:defsystem :net.bardcode.folio
  :serial t
  :description "the folio umbrella system"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :alexandria 
                     :net.bardcode.folio.as
                     :net.bardcode.folio.as.syntax
                     :net.bardcode.folio.boxes
                     :net.bardcode.folio.functions
                     :net.bardcode.folio.functions.syntax
                     :net.bardcode.folio.make
                     :net.bardcode.folio.maps
                     :net.bardcode.folio.maps.syntax
                     :net.bardcode.folio.pairs
                     :net.bardcode.folio.sequences
                     :net.bardcode.folio.sequences.syntax
                     :net.bardcode.folio.series
                     :net.bardcode.folio.series.syntax
                     :net.bardcode.folio.taps)
  :components ((:module "src"
                        :serial t
                        :components ((:file "folio-package")))))

(asdf:defsystem :net.bardcode.folio.tests
  :serial t
  :description "the folio umbrella system"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.as.tests
               :net.bardcode.folio.boxes.tests
               :net.bardcode.folio.functions.tests
               :net.bardcode.folio.make.tests
               :net.bardcode.folio.maps.tests
               :net.bardcode.folio.pairs.tests
               ;;:net.bardcode.folio.sequences.tests
               ;;:net.bardcode.folio.series.tests
               :net.bardcode.folio.taps.tests)
  :components ((:module "src"
                        :serial t
                        :components ((:file "folio-package")))))

;;; loaders
;;; ---------------------------------------------------------------------

(defun load-folio ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio))

(defun load-folio-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.tests))

;;; loaders
;;; (load-folio)
;;; (load-folio-tests)
