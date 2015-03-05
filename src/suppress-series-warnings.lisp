;;;; ***********************************************************************
;;;;
;;;; Name:          suppress-series-warnings.lisp
;;;; Project:       folio2 - Functional idioms for Common Lisp
;;;; Purpose:       series functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :series)

;;; suppress series warnings so that SBCL can load the folio series functions
#+sbcl(setf *suppress-series-warnings* t)
