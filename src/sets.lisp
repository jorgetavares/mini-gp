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
  (let ((fset-size (length fset))
	(tset-size (length tset)))
    (if (< (random 1.0) 0.5)
	(full-method-tree-generic 0 limit fset fset-size tset tset-size)
	(grow-method-tree-generic 0 limit fset fset-size tset tset-size))))


(defun full-method-tree-generic (size limit fset fset-size tset tset-size)
  "Random tree according to the Full method."
  (if (= size limit)
      (process-terminal (nth (random tset-size) tset))
      (let* ((function (nth (random fset-size) fset))
	     (name (function-name function))
	     (args (function-args function)))
	(cons name
	      (loop repeat args 
		 collect (full-method-tree-generic 
			  (1+ size) limit fset fset-size tset tset-size))))))

(defun grow-method-tree-generic (size limit fset fset-size tset tset-size)
  "Random tree according to the Grow method."
  (if (= size limit)
      (process-terminal (nth (random tset-size) tset))
      (let* ((set (append fset tset))
	     (element (nth (random (+ fset-size tset-size)) set)))
	(if (consp element)
	    (let ((name (function-name element))
		  (args (function-args element)))
	      (cons name
		    (loop repeat args 
		       collect (grow-method-tree-generic 
				(1+ size) limit fset fset-size tset tset-size))))
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
  (when (and (numberp a) (numberp b))
    (+ a b)))

(defun gp-minus (a b)
  (when (and (numberp a) (numberp b))
    (- a b)))

(defun gp-times (a b)
  (when (and (numberp a) (numberp b))
    (* a b)))

(defun gp-divison (a b)
  (when (and (numberp a) (numberp b))
    (if (> b 0)
	(/ a b) b)))

(defun gp-log (a)
  (when (numberp a)
    (log a)))	     

(defun gp-square-root (a)
  (when (numberp a)
    (sqrt a)))

(defun gp-square (a)
  (when (numberp a)
    (* a a)))	
     
(defun gp-power (a b)
  (when (and (numberp a) (numberp b))
    (expt a b)))

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
  (funcall *generate-constant*))

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
