;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.packages
  :serial t
  :description "all folio packages"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:file "package-common")
               (:module "boxes" :components ((:file "package")))
               (:module "converting" :components ((:file "package")))
               (:module "functions" :components ((:file "package")))
               (:module "ordering" :components ((:file "package")))
               (:module "pairs" :components ((:file "package")))
               (:module "sequences" :components ((:file "package")))
               (:module "sets" :components ((:file "package")))
               (:module "streams" :components ((:file "package")))
               (:module "tables" :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.generic-functions
  :serial t
  :description "all folio generic functions"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.packages)
  :components ((:file "generic-functions")))

(asdf:defsystem :net.bardcode.folio.boxes
  :serial t
  :description "wrapping values in mutable containers"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.packages :net.bardcode.folio.generic-functions)
  :components ((:module "boxes"
                        :serial t
                        :components ((:file "functions")
                                     (:file "types")))))

(asdf:defsystem :net.bardcode.folio.boxes.tests
  :serial t
  :description "box tests"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.packages :net.bardcode.folio.generic-functions
               :net.bardcode.folio.boxes :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "box-tests")))))

(asdf:defsystem :net.bardcode.folio.converting
  :serial t
  :description "uniform tools for converting values from one type to another"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.packages :net.bardcode.folio.generic-functions)
  :components ((:module "converting"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.converting.tests
  :serial t
  :description "converting tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.packages :net.bardcode.folio.generic-functions 
                     :net.bardcode.folio.converting :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "converting-tests")))))

(asdf:defsystem :net.bardcode.folio.functions
  :serial t
  :description "combinators and other conveniences for working with functions"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.packages :net.bardcode.folio.generic-functions)
  :components ((:module "functions"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")
                                     (:file "syntax")))))

(asdf:defsystem :net.bardcode.folio.function.tests
  :serial t
  :description "function tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.packages :net.bardcode.folio.generic-functions 
                                            :net.bardcode.folio.functions :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "function-tests")))))

(asdf:defsystem :net.bardcode.folio.ordering
  :serial t
  :description "sorting values into stable orders"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.packages :net.bardcode.folio.generic-functions)
  :components ((:module "ordering"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.ordering.tests
  :serial t
  :description "ordereing test"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.packages :net.bardcode.folio.generic-functions 
                     :net.bardcode.folio.ordering :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "ordering-tests")))))

(asdf:defsystem :net.bardcode.folio.pairs
  :serial t
  :description "associating one value with another"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.packages :net.bardcode.folio.generic-functions)
  :components ((:module "pairs"
                        :serial t
                        :components ((:file "package")
                                     (:file "types")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.pair.tests
  :serial t
  :description "pair tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.packages :net.bardcode.folio.generic-functions
                                            :net.bardcode.folio.pairs :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "pair-tests")))))

(asdf:defsystem :net.bardcode.folio.sequences
  :serial t
  :description "tools for manipulating sequences, series, and generators"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.packages :net.bardcode.folio.generic-functions)
  :components ((:module "sequences"
                        :serial t
                        :components ((:file "package")
                                     (:file "types")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.sequence.tests
  :serial t
  :description "sequence tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.packages :net.bardcode.folio.generic-functions
                     :net.bardcode.folio.sequences :lift)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "sequence-tests")))))

(asdf:defsystem :net.bardcode.folio.sequences-syntax
  :serial t
  :description "syntactic sugar for sequences"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.sequences :net.bardcode.folio.packages
               :net.bardcode.folio.generic-functions)
  :components ((:module "sequences"
                        :serial t
                        :components ((:file "package")
                                     (:file "syntax")))))

(asdf:defsystem :net.bardcode.folio.sets
  :serial t
  :description "treating sequences as sets"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.packages :net.bardcode.folio.generic-functions)
  :components ((:module "sets"
                        :serial t
                        :components ((:file "package")
                                     (:file "types")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.set-syntax
  :serial t
  :description "syntactic sugar for sets"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.sets :net.bardcode.folio.packages
               :net.bardcode.folio.generic-functions)
  :components ((:module "sets"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.streams
  :serial t
  :description "input streams as series"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:series :net.bardcode.folio.packages
               :net.bardcode.folio.generic-functions)
  :components ((:module "streams"
                        :serial t
                        :components ((:file "package")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.tables
  :serial t
  :description "tools for manipulating finite maps"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.packages
               :net.bardcode.folio.generic-functions)
  :components ((:module "tables"
                        :serial t
                        :components ((:file "package")
                                     (:file "types")
                                     (:file "functions")))))

(asdf:defsystem :net.bardcode.folio.table-syntax
  :serial t
  :description "syntactic sugar for finite maps"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.tables :net.bardcode.folio.packages
               :net.bardcode.folio.generic-functions)
  :components ((:module "tables"
                        :serial t
                        :components ((:file "package")
                                     (:file "syntax")))))

(asdf:defsystem :net.bardcode.folio.table-printer
  :serial t
  :description "prettier printing for fset-based tables"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.tables :net.bardcode.folio.packages
               :net.bardcode.folio.generic-functions)
  :components ((:module "tables"
                        :serial t
                        :components ((:file "package")
                                     (:file "printer")))))

(asdf:defsystem :net.bardcode.folio
  :serial t
  :description "umbrella system for loading all folio systems"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:net.bardcode.folio.packages
               :net.bardcode.folio.generic-functions
               :net.bardcode.folio.boxes 
               :net.bardcode.folio.converting
               :net.bardcode.folio.functions
               :net.bardcode.folio.ordering
               :net.bardcode.folio.pairs
               :net.bardcode.folio.sequences-syntax
               :net.bardcode.folio.sequences
               :net.bardcode.folio.set-syntax
               :net.bardcode.folio.sets
               :net.bardcode.folio.streams
               :net.bardcode.folio.table-syntax
               :net.bardcode.folio.tables
               :net.bardcode.folio.table-printer)
  :components ((:file "package")))


(asdf:defsystem :net.bardcode.folio.tests
  :serial t
  :description "folio system tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:lift :net.bardcode.folio)
  :components ((:file "package")
               (:module "tests"
                        :serial t
                        :components
                        ((:file "box-tests")
                         (:file "converting-tests")
                         (:file "function-tests")
                         (:file "ordering-tests")
                         (:file "pair-tests")
                         (:file "sequence-tests")))))


(defun load-folio ()
  (let ((asdf:*compile-file-warnings-behaviour* #+sbcl :ignore #-sbcl :warn)
        (asdf:*compile-file-failure-behaviour* #+sbcl :ignore #-sbcl :warn))
    (asdf:oos 'asdf:load-op :net.bardcode.folio)))

(defun load-folio-tests ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio.tests))

;;; (load-folio)
;;; (load-folio-tests)
