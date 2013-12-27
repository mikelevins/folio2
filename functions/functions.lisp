;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       combinators and other conveniences 
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.functions)

;;; function apply
;;;
;;; (apply fn seq &rest more-seqs) => anything
;;; ---------------------------------------------------------------------
;;; shadows cl:apply, providing an extensible generic version.

;;; the default version
(defmethod apply (applicable (seq cl:cons))
  (cl:apply applicable seq))

;;; function compose
;;;
;;; (compose f1 f2 ... fk) => fn
;;; ---------------------------------------------------------------------
;;; returns a function whose effect is the same as that of applying
;;; (f1 (f2 (...(fk args)))). All arguments f1..fk must accept and return 
;;; the same number of values

(defun compose (f &rest fs)
  (if (null fs)
      f
      (if (null (cdr fs))
          (lambda (x) (funcall f (funcall (car fs) x)))
          (lambda (x) (funcall f (funcall (apply 'compose fs) x))))))

;;; function conjoin
;;;
;;; (conjoin p1 p2 ... pk) => fn
;;; ---------------------------------------------------------------------
;;; returns a function whose effect is the same as that of applying
;;; (and (p1 arg)(p2 arg)...(pk arg))

(defun conjoin (&rest preds)
  (lambda (&rest args)
    (let ((val nil))
      (some (lambda (p) 
              (setf val (apply p args))
              (not val))
            preds)
      val)))

;;; function disjoin
;;;
;;; (disjoin p1 p2 ... pk) => fn
;;; ---------------------------------------------------------------------
;;; returns a function whose effect is the same as that of applying
;;; (or (p1 arg)(p2 arg)...(pk arg))

(defun disjoin (&rest preds)
  (lambda (&rest args)
    (let ((val nil))
      (some (lambda (p) 
              (setf val (apply p args))
              val)
            preds)
      val)))

;;; function flip
;;;
;;; (flip f1) => f2
;;; ---------------------------------------------------------------------
;;; given an argument f1 of the form (lambda (a b)...), returns a
;;; function of the form (lambda (b a) ...). Except for the order of the
;;; arguments a and b, the new function is identical to the old.

(defun flip (f) (lambda (x y) (funcall f y x)))

;;; function function?
;;;
;;; (function? thing) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if THING is a function (that is, it's a function,
;;; but not s generic function or method)

(defmethod function? (thing)
  (declare (ignore thing))
  nil)

(defmethod function? ((thing function))
  (declare (ignore thing))
  t)

(defmethod function? ((thing generic-function))
  (declare (ignore thing))
  nil)

(defmethod function? ((thing method))
  (declare (ignore thing))
  nil)

;;; function functional?
;;;
;;; (functional? thing) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if THING is a function, a generic function,
;;; or a method

(defmethod functional? (thing)
  (declare (ignore thing))
  nil)

(defmethod functional? ((thing function))
  (declare (ignore thing))
  t)

(defmethod functional? ((thing generic-function))
  (declare (ignore thing))
  t)

(defmethod functional? ((thing method))
  (declare (ignore thing))
  t)

;;; function generic-function?
;;;
;;; (generuc-function? thing) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if THING is a generic function

(defmethod generic-function? (thing)
  (declare (ignore thing))
  nil)

(defmethod generic-function? ((thing generic-function))
  (declare (ignore thing))
  t)

;;; function method?
;;;
;;; (method? thing) => Boolean
;;; ---------------------------------------------------------------------
;;; returns a true value if THING is a method

(defmethod method? (thing)
  (declare (ignore thing))
  nil)

(defmethod method? ((thing function))
  (declare (ignore thing))
  nil)

(defmethod method? ((thing method))
  (declare (ignore thing))
  t)


;;; function partial
;;;
;;; (partial f1 arg1..argk) => f2
;;; ---------------------------------------------------------------------
;;; partially applies the function f1 to the arguments arg1..argk,
;;; returning a left section of f1. In other words, if f1 accepts
;;; arguments a, b, c, and d, then (partial f1 0 1) returns an f2 in
;;; which a is bound to 0 and b is bound to 1. f2 then requires
;;; two arguments. Evaluating (f2 2 3) binds c and d to 2 and 3,
;;; respectively, then computes the same result as if we had
;;; originally called (f1 0 1 2 3).

(defun partial (f &rest args)
  (lambda (&rest more-args)
    (apply f `(,@args ,@more-args))))


;;; function rotate-left
;;;
;;; (rotate-left f) => f2
;;; ---------------------------------------------------------------------
;;; returns a function f2 that behaves exactly like f, except that
;;; the order of arguments it accepts is rotated left by one place.
;;; In other words, supposing that f accepts arguments a b c, then
;;; f2 accepts the same arguments as b c a

(defun rotate-left (f)
  (lambda (&rest args)
    (apply f `(,@(cl:last args) ,@(subseq args 0 (1- (length args)))))))

;;; function rotate-right
;;;
;;; (rotate-right f) => f2
;;; ---------------------------------------------------------------------
;;; returns a function f2 that behaves exactly like f, except that
;;; the order of arguments it accepts is rotated left by one place.
;;; In other words, supposing that f accepts arguments a b c, then
;;; f2 accepts the same arguments as c a b

(defun rotate-right (f)
  (lambda (&rest args)
    (apply f `(,@(cdr args) ,(car args)))))

;;; function rpartial
;;;
;;; (rpartial f1 arg1..argk) => f2
;;; ---------------------------------------------------------------------
;;; partially applies the function f1 to the arguments arg1..argk,
;;; returning a right section of f1. In other words, if f1 accepts
;;; arguments a, b, c, and d, then (rpartial f1 2 3) returns an f2 in
;;; which c is bound to 2 and d is bound to 3. f2 then requires
;;; two arguments. Evaluating (f2 0 1) binds a and b to 0 and 1,
;;; respectively, then computes the same result as if we had
;;; originally called (f1 0 1 2 3).


(defun rpartial (f &rest args)
  (lambda (&rest more-args)
    (apply f `(,@more-args ,@args))))


