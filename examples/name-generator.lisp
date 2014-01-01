;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          name-generator.lisp
;;;; Project:       folio - Bard features in Common Lisp
;;;; Purpose:       the name-generator example program
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :net.bardcode.folio.examples
  (:use :net.bardcode.folio.as :net.bardcode.folio.bind
        :net.bardcode.folio.make :net.bardcode.folio.with-exit))

(in-package :net.bardcode.folio.examples)

(defmethod triples ((s sequence)) 
    (by 3 s))

(defmethod valid-lines ((path string))
  (filter (complement 'empty?)
          (image 'trim
                 (lines (pathname path)))))

(defmethod long-enough? ((s sequence))
  (>= (length s) 3))

(defmethod rules ((path string))
  (let* ((lines (vaid-lines lines))
         (triples (image 'triples lines))
         (starts (image 'first triples))
         (parts (reduce 'append (image 'rest triples))))
    (values (filter 'long-enough? starts)
            (filter 'long-enough? parts))))

(defun make-name-extender (parts)
  (lambda (start)
    (let ((segment (any (filter (partial 'prefix-match? (leave 2 start)))
                        parts)))
      (if segment
          (append start (drop 2 segment))
          nil))))

(defparameter extend-name (make-name-extender parts))

(defmethod build-name ((starts sequence)(parts sequence))
  (take-while 'something (iterate extend-name (any starts))))

(defmethod names ((path string) &optional (number 10))
  (multiple-value-bind (starts parts)(rules path)
    (take number (repeat (build-name starts parts)))))
