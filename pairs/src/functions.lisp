;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       pair functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.pairs)

;;; function as
;;;
;;; (as pair x) => an instance of type
;;; ---------------------------------------------------------------------

(defmethod as ((type (eql 'pair)) (val cl:cons) &key &allow-other-keys)
  val)

(defmethod as ((type (eql 'pair)) (val cl:vector) &key &allow-other-keys)
  (coerce val 'cl:list))

;;; function left
;;;
;;; (left pair) => anything
;;; ---------------------------------------------------------------------
;;; return the left element of the pair

(defmethod left ((val null)) nil)

(defmethod left ((val cons)) 
  (car val))

;;; function make
;;;
;;; (make 'pair :left val1 :right val2) => pair
;;; ---------------------------------------------------------------------
;;; create a pair

(defmethod make ((type (eql 'pair)) &key (left nil)(right nil) &allow-other-keys)
  (cons left right))

(defmethod make ((type (eql :pair)) &key (left nil)(right nil) &allow-other-keys)
  (cons left right))

;;; function pair
;;;
;;; (pair l r) => pair
;;; ---------------------------------------------------------------------
;;; return a new pair

(defun pair (l r) (cons l r))

;;; function pair?
;;;
;;; (pair? val) => boolean
;;; ---------------------------------------------------------------------
;;; return a true value if VAL is a pair

(defmethod pair? (val)(declare (ignore val)) nil)
(defmethod pair? ((val null))(declare (ignore val)) t)
(defmethod pair? ((val cons))(declare (ignore val)) t)


;;; function right
;;;
;;; (right pair) => anything
;;; ---------------------------------------------------------------------
;;; return the right element of the pair

(defmethod right ((val null)) nil)

(defmethod right ((val cons)) 
  (cdr val))

;;; function set-left!
;;;
;;; (set-left! pair val) => val
;;; ---------------------------------------------------------------------
;;; destructively updates the left element of PAIR

(defmethod set-left! ((p cons) val) 
  (setf (car p) val))

(defsetf left set-left!)

;;; function set-right!
;;;
;;; (set-right! pair val) => val
;;; ---------------------------------------------------------------------
;;; destructively updates the right element of PAIR

(defmethod set-right! ((p cons) val) 
  (setf (cdr p) val))

(defsetf right set-right!)


