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
;;; tree generators 
;;;

(defun ramped-half-and-half (limit fset tset)
  "A gp tree is created with half of probability for each method."
  (let* ((fset-vec (if (vectorp fset) fset (coerce fset 'vector)))
	 (tset-vec (if (vectorp tset) tset (coerce tset 'vector)))
	 (fset-size (length fset-vec))
	 (tset-size (length tset-vec)))
    (if (< (random 1.0) 0.5)
	(full-method-tree-generic 0 limit fset-vec fset-size tset-vec tset-size)
	(let ((combined-set (concatenate 'vector fset-vec tset-vec))
	      (combined-size (+ fset-size tset-size)))
	  (grow-method-tree-generic 0 limit fset-vec fset-size tset-vec tset-size
				    combined-set combined-size)))))


(defun full-method-tree-generic (size limit fset fset-size tset tset-size)
  "Random tree according to the Full method."
  (if (= size limit)
      (process-terminal (aref tset (random tset-size)))
      (let* ((function (aref fset (random fset-size)))
	     (name (function-name function))
	     (args (function-args function)))
	(cons name
	      (loop repeat args 
		 collect (full-method-tree-generic 
			  (1+ size) limit fset fset-size tset tset-size))))))

(defun grow-method-tree-generic (size limit fset fset-size tset tset-size
				 &optional
				 (combined-set (concatenate 'vector
						   (coerce fset 'vector)
						   (coerce tset 'vector)))
				 (combined-size (+ fset-size tset-size)))
  "Random tree according to the Grow method."
  (if (= size limit)
      (process-terminal (aref tset (random tset-size)))
      (let ((element (aref combined-set (random combined-size))))
	(if (consp element)
	    (let ((name (function-name element))
		  (args (function-args element)))
	      (cons name
		    (loop repeat args 
		       collect (grow-method-tree-generic 
				(1+ size) limit fset fset-size tset tset-size
				combined-set combined-size))))
	    (process-terminal element)))))

(defun process-terminal (terminal)
  "Return a constant or a terminal function."
  (case terminal
    (gp-constant (gp-constant))
    (gp-constant-int (gp-constant-int))
    (gp-constant-real (gp-constant-real))
    (gp-true (gp-true))
    (gp-false (gp-false))
    (otherwise (list terminal))))


;;;
;;; function and terminal sets 
;;;

(defun make-fset (&rest args)
  (loop 
     for function in args by #'cddr
     for arguments in (rest args) by #'cddr
     collect (cons function arguments)))

(defun function-name (pair)
  (car pair))

(defun function-args (pair)
  (cdr pair))


;;
;; non-terminals
;;

;; math
(defun gp-plus (a b)
  (if (and (numberp a) (numberp b))
      (+ a b)
      0))

(defun gp-minus (a b)
  (if (and (numberp a) (numberp b))
      (- a b)
      0))

(defun gp-times (a b)
  (if (and (numberp a) (numberp b))
      (* a b)
      0))

(defun gp-division (a b)
  (if (and (numberp a) (numberp b))
      (if (< (abs b) 1e-6)
	  0
	  (/ a b))
      0))

(defun gp-log (a)
  (if (numberp a)
      (if (<= (abs a) 1e-6)
	  0
	  (log (abs a)))
      0))	     

(defun gp-square-root (a)
  (if (numberp a)
      (sqrt (abs a))
      0))

(defun gp-square (a)
  (if (numberp a)
      (* a a)
      0))	
     
(defun gp-power (a b)
  (if (and (numberp a) (numberp b))
      (let ((result (expt (abs a) b)))
	(if (realp result) result 0))
      0))

;; conditionals
(defun gp-if (x y z)
  (if x y z))

(defun gp-and (x y)
  (and x y))

(defun gp-or (x y)
  (or x y))

(defun gp-not (x)
  (not x))

;; comparators
(defun gp-< (x y)
  (and (numberp x) (numberp y)
       (< x y)))

(defun gp-<= (x y)
  (and (numberp x) (numberp y)
       (<= x y)))

(defun gp-> (x y)
  (and (numberp x) (numberp y)
       (> x y)))

(defun gp->= (x y)
  (and (numberp x) (numberp y)
       (>= x y)))

(defun gp-= (x y)
  (and (numberp x) (numberp y)
       (= x y)))

(defun gp-/= (x y)
  (and (numberp x) (numberp y)
       (/= x y)))

;; others
(defun gp-random-n (n)
  (when (numberp n)
    (random n)))


;;
;; terminals
;;

;; constants (only used in a tree generation by keeping their values)
(defparameter *generate-constant* nil)

(defun gp-constant ()
  (if *generate-constant*
      (funcall *generate-constant*)
      (random 1.0)))

(defun gp-constant-int (&optional (max 100))
  (random max))

(defun gp-constant-real ()
  (random 1.0))

(defun gp-true ()
  t)

(defun gp-false ()
  nil)

;; random numbers
(defun gp-random-real ()
  (random 1.0))

(defun gp-random-10 ()
  (random 10))

(defun gp-random-100 ()
  (random 100))
