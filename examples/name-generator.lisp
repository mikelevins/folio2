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


(defmethod parse-names ((lines sequence))
  (scan-image (partition 'first (^ l -> (reduce 'append (rest l))))
              (scan-image 'triples
                          (filter (complement 'empty?) 
                                  (image 'trim lines)))))

(defmethod read-names ((path string))
  (parse-names (with-open-file (in path)
                 (loop for line = (read-line in nil :eof) 
                    until (eq line :eof)
                    collect line))))


(defmethod build-name ((start string)(parts sequence))
  (iterate build ((name start))
           (let ((chunk (any (filter (partial 'prefix-match? [(penult name)(last name)])
                                     parts))))
             (if chunk
                 (build (append name (drop 2 chunk)))
                 name))))

(defmethod build-names ((starts sequence)(parts sequence))
  (repeat (build-name (any starts) parts)))

(defmethod names ((path string) &optional (number 10))
  (bind ((starts parts (read-names path))
         (names (build-names starts parts)))
    (take number names)))




