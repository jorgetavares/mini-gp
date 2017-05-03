;;;; Copyright (c) 2011 Jorge Tavares <jorge.tavares@ieee.org>
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :mini-gp)

;;;
;;; crossover
;;;

;; subtree crossover

(defun tree-crossover (size p1 p2)
  (multiple-value-bind (o1 o2)
      (cross-subtrees (individual-tree p1) (individual-tree p2) size)
    (values (make-individual :tree (copy-tree o1) :eval-p t)
	    (make-individual :tree (copy-tree o2) :eval-p t))))

(defun cross-subtrees (p1 p2 depth)
  "Exchanges two subtrees in a random point."
  (let* ((p1-point (random (count-tree-nodes p1)))
         (p2-point (random (count-tree-nodes p2)))
         (o1 (list (copy-tree p1)))
         (o2 (list (copy-tree p2))))
    (multiple-value-bind (p1-subtree p1-fragment)
        (get-subtree (first o1) o1 p1-point)
      (multiple-value-bind
	  (p2-subtree p2-fragment)
          (get-subtree
           (first o2) o2 p2-point)
        (setf (first p1-subtree) p2-fragment)
        (setf (first p2-subtree) p1-fragment)))
    (validate-crossover p1 o1 p2 o2 depth)))

(defun get-subtree (tree point index)
  "Return a subtree."
  (if (= index 0)
      (values point (copy-tree tree) index)
    (if (consp tree)
	(do* ((tree-rest (rest tree) (rest tree-rest))
	      (arg (first tree-rest) (first tree-rest)))
	    ((not tree-rest) (values nil nil index))
	  (multiple-value-bind
	      (new-point new-tree new-index)
	      (get-subtree arg tree-rest (1- index))
	    (if (= new-index 0)
		(return (values new-point new-tree new-index))
	      (setf index new-index))))
      (values nil nil index))))

(defun validate-crossover (p1 o1 p2 o2 depth)
  "Validates the offspring. If they pass the maximum depth they are rejected."
  (let ((p1-limit (tree-depth (first o1)))
        (p2-limit (tree-depth (first o2))))
    (values
     (if (or (= 1 p1-limit) (> p1-limit depth))
         p1 (first o1))
     (if (or (= 1 p2-limit) (> p2-limit depth))
         p2 (first o2)))))

(defun count-tree-nodes (tree)
  "Count the number of nodes in a tree."
  (if (consp tree)
      (+ 1 (reduce #'+ (mapcar #'count-tree-nodes (rest tree)))) 1))

(defun tree-depth (tree)
  "Return the max depth of a tree."
  (if (consp tree)
      (+ 1 (if (rest tree)
	       (apply #'max (mapcar #'tree-depth (rest tree))) 0)) 1))


;;;
;;; mutation
;;;

;; point mutation

(defun point-mutation (individual rate fset tset tset-size)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (setf (individual-tree individual)
	(point-mutate-tree (individual-tree individual) rate fset tset tset-size))
  individual)

(defun point-mutate-tree (tree rate fset tset tset-size)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (if (or (not (consp tree))
	  (null (rest tree)))
      (mutate-terminal tree rate tset tset-size)
    (let* ((element (first tree))
	   (nargs (function-args (assoc element fset))))
      (cons (mutate-function element nargs rate fset) 
	    (loop for arg from 1 to nargs
		  collect (point-mutate-tree (nth arg tree) rate fset tset tset-size))))))

(defun mutate-terminal (terminal rate tset tset-size)
  (if (< (random 1.0) rate)
      (process-terminal (nth (random tset-size) tset))
    terminal))

(defun mutate-function (function nargs rate fset)
  (if (< (random 1.0) rate)
      (let ((filtered-fset (mapcan #'(lambda (f)
				       (when (= nargs (function-args f)) 
					 (list f))) fset)))
	(if (null filtered-fset)
	    function
	  (function-name (nth (random (length filtered-fset)) filtered-fset))))	
    function))
