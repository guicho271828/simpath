#|
  This file is a part of simpath project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage simpath
  (:shadow :<>)
  (:use :cl :trivia :cl-cudd :alexandria :iterate :arrow-macros))
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

;; (defun simpath (input)
;;   (match input
;;     ((input s t edges)
;;      (with-manager (:initial-num-vars-z (length edges))
;;        (iter (for e in-vector edges with-index i)
;;              (for var = (make-var 'zdd-node :index i))
;;              ;; the details of branch pruning is not specified...

(defmacro with-renaming (bindings &body body)
  (let ((tmps (make-gensym-list (length bindings))))
    (iter (for (old new) in bindings)
          (for tmp in tmps)
          (setf body (subst tmp old body :test #'equalp)))
    (iter (for (old new) in bindings)
          (for tmp in tmps)
          (setf body (subst new tmp body :test #'equalp))))
  `(progn ,@body))

(defun test (a b)
  (* a b)
  #+nil
  (flet ((* (a b) (+ a b)))
    (* a b))
  #+nil
  (macrolet ((* (a b) `(+ ,a ,b)))
    (* a b))
  #+nil
  (symbol-macrolet ((* +))
    (* a b))
  (with-renaming ((* +)
                  (+ *))
    (* a b)
    (+ a b)))

(defconstant +new+ 0)
(defconstant +frontier+ 1)
(defconstant +closed+ 2)

(defun dump (path name f)
  (cl-cudd.baseapi:dump-dot
   (manager-pointer *manager*)
   (cl-cudd.baseapi:cudd-regular (node-pointer f))
   (namestring (make-pathname :name name :type "dot" :defaults path))))
(defun dump-zdd (path name f)
  (cl-cudd.baseapi:zdd-dump-dot
   (manager-pointer *manager*)
   (cl-cudd.baseapi:cudd-regular (node-pointer f))
   (namestring (make-pathname :name name :type "dot" :defaults path))))

(defun draw (f)
  (dump-zdd "." "dump" f)
  (uiop:run-program (format nil "dot dump.dot -Tpdf -o dump.pdf")))

(defmacro break+ (&rest args)
  (let* ((last-form (lastcar args))
         (last last-form)
         (butlast (butlast args)))
    (once-only (last)
      `(progn
         (break "~@{~a~%~<;;~@; -> ~4i~:@_~a~;~:>~2%~}"
                ,@(iter (for arg in butlast)
                        (collect `',arg)
                        (collect `(list ,arg)))
                ',last-form (list ,last))
         ,last))))

(defun mate-zdd (input)
  (ematch input
    ((input :s start :t terminal :edges edges)
     (let ((%edges (length edges))
           (%nodes (length (nodes input))))
       (labels ((mi (v1 v2) (+ (* %nodes v1) v2))
                (ei (h)     (+ (* %nodes %nodes) h))
                (m (v1 v2)
                  ;; (zdd-singleton (mi v1 v2))
                  (zdd-change (zdd-set-of-emptyset)
                              (mi v1 v2)))
                (e (h)
                  ;; (zdd-singleton (ei h))
                  (zdd-change (zdd-set-of-emptyset)
                              (ei h))))
         (with-manager (:initial-num-vars-z (+ (* %nodes %nodes) %edges))
           (let ((f (zdd-set-of-emptyset))
                 ;; frontier := visited nodes / closed nodes
                 (states (make-array %nodes
                                     :element-type 'fixnum
                                     :initial-element +new+)))
             ;; (setf (aref states start) +frontier+)
             (with-renaming ((+ zdd-union)
                             (* zdd-product-unate)
                             (/ zdd-divide-unate)
                             (! zdd-change)
                             (on  zdd-subset-1)
                             (off zdd-subset-0)
                             (% zdd-remainder-unate)
                             (_+ +)
                             (_* *))
               (iter (for i below %nodes)
                     #+wrong
                     (setf f (! f (_+ (_* %nodes i) i)))
                     #+wrong
                     (setf f (* f (zdd-singleton (mi i i))))
                     (setf f (* f (m i i))))
               ;; (break+ (draw f))
               (iter (for (i . j) in-vector edges with-index h)
                     (for p previous i)
                     (unless (first-iteration-p)
                       (when (/= i p)  ; i.e., i is a new node
                         (setf (aref states p) +closed+)
                         (iter (for s in-vector states with-index k)
                               (unless (= s +frontier+) (next-iteration))
                               (setf f (% f (m p k))))
                         (setf f (+ (/ f (m p p))
                                    (% f (m p p))))))
                     (iter (for s in-vector states with-index k)
                           (unless (= s +frontier+) (next-iteration))
                           (iter (for s in-vector states with-index l from (1+ k))
                                 (unless (= s +frontier+) (next-iteration))
                                 (setf f
                                       (+ f (-> f
                                              (/ (m i k))
                                              (/ (m j l))
                                              (* (e h))
                                              (* (m k l)))))
                                 (break+ (draw f) h 1)))
                     ;; (break+ (draw f) h 1)
                     ;; (break+ (draw f) h 2)
                     (setf (aref states i) +frontier+)
                     (setf (aref states j) +frontier+))
               (break+ (draw f))
               (setf f (/ f (m start terminal))))
             (zdd-count-minterm f))))))))

