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

(in-package :folio)

(defmethod read-samples ((path pathname))
  (as 'cl:list
      (filter (complement #'empty?)
              (lines path))))

(defmethod read-samples ((path string))
  (read-samples (pathname path)))

(defun triples (s)
  (take-by 3 1 s))

(defmethod parse-samples ((path pathname))
  (image #'triples
         (read-samples path)))

(defmethod parse-samples ((path string))
  (parse-samples (pathname path)))

(defun joinable? (left right)
  (equal (drop 1 left)
         (take 2 right)))

(defun join-chunks (left right)
  (append (take 2 left)
          (drop 1 right)))

(defun find-extension (stem blocks)
  (any (filter (^ (b) (joinable? stem b))
               blocks)))

(defun collect-extensions (stem extensions)
  (as 'cl:list
      (take-while (complement #'empty?)
                  (iterate (^ (x)(when x (find-extension x extensions)))
                      stem))))

(defun assemble-name (chunks)
  (reduce #'join-chunks (rest chunks)
          :initial-value (first chunks)))

(defun build-name (building-blocks)
  (let* ((extensions (reduce #'append (image #'rest building-blocks)))
         (start ($ (compose #'first #'any) building-blocks))
         (chunks (filter (^ (chunk)(> (length chunk) 2))
                         (collect-extensions start extensions))))
    (when chunks
      (assemble-name chunks))))

(defmethod gen-names ((path pathname)(count integer))
  (let ((building-blocks (image #'triples (read-samples path))))
    (take count
          (repeat (build-name building-blocks)))))

(defmethod gen-names ((path string)(count integer))
  (gen-names (pathname path) count))

;;; (defparameter $dickens "/Users/mikel/Workshop/programming/folio2/examples/namefiles/dickens.names")
;;; (defparameter $sindarin "/Users/mikel/Workshop/programming/folio2/examples/namefiles/sindarin.names")
;;; (defparameter $us "/Users/mikel/Workshop/programming/folio2/examples/namefiles/us.names")
;;; (gen-names $dickens 10)
;;; (gen-names $sindarin 10)
;;; (gen-names $us 10)
