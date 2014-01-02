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

(defparameter +maximum-name-length+ 12)

(defun pending? (nm)(right nm))
(defun name-part (nm)(left nm))
(defmethod triples ((s cl:sequence)) (take-by 3 1 s))

(defmethod read-names ((path pathname)) (lines path))

(defmethod read-names ((path string)) 
  (filter (compose (partial 'cl:<= 3) 'length)
          (as 'cl:sequence (read-names (pathname path)))))

(defmethod rules ((names cl:sequence))
  (multiple-value-bind (starts parts)(dispose (image 'triples names) 'left 'right)
    (cl:values starts (reduce 'append parts))))

(defun find-extension (nm parts)
  (any (filter (partial 'prefix-match? (leave 2 nm))
               parts)))

(defun merge-name (nm segment)
  (if  (< (length segment) 3)
       (append nm segment)
       (append nm (drop 2 segment))))

(defun make-name-builder (parts)
  (lambda (start)
    (let* ((nm (left start))
           (pending (right start))
           (extension (find-extension nm parts)))
      (let ((next-nm (merge-name nm extension)))
        (if (or (< (length extension) 3)
                (> (length next-nm) +maximum-name-length+))
            (pair next-nm nil)
            (pair next-nm t))))))

(defmethod build-name ((starts cl:sequence)(parts cl:sequence))
  (name-part (last (as 'cl:list
                       (take-while 'pending?
                                   (iterate (make-name-builder parts)
                                       (pair (any starts) t)))))))


(defmethod names ((path string) &optional (number 10))
  (multiple-value-bind (starts parts)(rules (read-names path))
    (as 'cl:list (take number (repeat (build-name starts parts))))))


;;; (multiple-value-setq ($starts $parts) (rules (read-names "/Users/mikel/Workshop/folio2/examples/namefiles/dickens.names")))
;;; (build-name $starts $parts)
;;; (names "/Users/mikel/Workshop/folio2/examples/namefiles/dickens.names" 10)
;;; (dolist (nm (names "/Users/mikel/Workshop/folio2/examples/namefiles/dickens.names" 30) (terpri))(terpri)(princ nm))
;;; (dolist (nm (names "/Users/mikel/Workshop/folio2/examples/namefiles/gnome-female.names" 30) (terpri))(terpri)(princ nm))
;;; (dolist (nm (names "/Users/mikel/Workshop/folio2/examples/namefiles/gnome-male.names" 30) (terpri))(terpri)(princ nm))
;;; (dolist (nm (names "/Users/mikel/Workshop/folio2/examples/namefiles/us.names" 30) (terpri))(terpri)(princ nm))
