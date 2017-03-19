#|
  This file is a part of simpath project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage simpath
  (:use :cl :trivia :cl-cudd :alexandria :iterate))
(in-package :simpath)

;; blah blah blah.


;; input: explicit graph representation

(deftype edge ()
  `(cons integer integer))

(defstruct input
  (s 0 :type non-negative-integer)
  (t 0 :type non-negative-integer)
  (edges #() :type (array edge)))

(defun grid-node (size x y)
  ;; breadth-first from top-left to bottom-right
  ;;   |  0  1  2  3  4
  ;;---+---------------
  ;; 0 |  0  1  3  6 10 ** **
  ;; 1 |  2  4  7 11 15 **
  ;; 2 |  5  8 12 16 19
  ;; 3 |  9 13 17 20 22
  ;; 4 | 14 18 21 23 24
  ;;     **
  (let ((n (+ x y)))
    (if (< n size)
        (+ (* 1/2 n (1+ n))
           y)
        (+ (* 1/2 n (1+ n))
           y
           ;; adjust the missing cells (**)
           (let ((m (- n size -1)))
             (* -1 m m))))))

(let ((size 5))
  (print
   (iter (with a = (make-array (list size size)))
         (for x below size)
         (iter (for y below size)
               (setf (aref a y x) (grid-node size x y)))
         (finally (return a)))))

(defun grid (size)
  (flet ((node (x y)
           ;; breadth-first from top-left to bottom-right
           ;;   |  0  1  2  3  4
           ;;---+---------------
           ;; 0 |  0  1  3  6 10 ** **
           ;; 1 |  2  4  7 11 15 **
           ;; 2 |  5  8 12 16 19
           ;; 3 |  9 13 17 20 22
           ;; 4 | 14 18 21 23 24
           ;;
           (let ((n (+ x y)))
             (if (< n size)
                 (+ (* 1/2 n (1+ n))
                    y)
                 (+ (* 1/2 n (1+ n))
                    y
                    ;; adjust the missing cells (**)
                    (let ((m (- n size -1)))
                      (* -1 m m)))))))
    (make-input
     :s 0
     :t (1- (* size size))
     :edges
     (coerce (iter outer
                  (for x below size)
                  (iter (for y below size)
                        (in outer
                            (unless (= x (1- size))
                              (collect (cons (node x y) (node (1+ x) y))
                                result-type vector))
                            (unless (= y (1- size))
                              (collect (cons (node x y) (node x (1+ y)))
                                result-type vector)))))
             '(array edge)))))

(defun nodes (input)
  (match input
    ((input edges)
     (let ((h (make-hash-table)))
       (iter (for (from . to) in-vector edges)
             (setf (gethash from h) from
                   (gethash to h) to))
       (hash-table-keys h)))))

