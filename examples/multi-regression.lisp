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

;;;;
;;;; mini-gp example: multi-valued symbolic regression
;;;;

(defpackage mini-gp-multiregression
  (:use common-lisp mini-gp)
  (:export regression 
	   var-x
	   var-y
	   int-constants
	   make-fitness-regression
	   *fset*
	   *tset*
	   *fitness-cases*
	   *x-points*
	   *y-points*))

(in-package mini-gp-multiregression)


;;;
;;; function and terminal sets
;;;

(defun var-x () 0) ; kept for tree display; actual values come from env
(defun var-y () 0)

(defparameter *fset* (make-fset 'gp-plus 2
				'gp-minus 2 
				'gp-times 2
				))

(defparameter *tset* '(var-x var-y))


;;;
;;; fitness function f(x,y) = x*x*y + x*y + y 
;;;

(defparameter *x-points* (loop for i from 0 below 10 collect (/ i 10)))
(defparameter *y-points* (loop for x in *x-points* collect (float (/ (expt x 2) 2))))

(defun make-fitness-regression (fitness-cases)
  #'(lambda (individual)
      (let* ((env (make-env '(var-x var-y)))
	     (fn (compile-tree-with-env (individual-tree individual) env)))
        (loop with expected = 0
	      repeat fitness-cases
	      do (let ((x (random 1.0))
		      (y (random 1.0)))
		   (setf (env-var env 'var-x) x
			 (env-var env 'var-y) y)
		   (setf expected (+ (* x x y) (* x y) y)))
	      sum (expt (- expected (funcall fn)) 2)))))


;;;
;;; run GP
;;;

(defparameter *multi-regression-params* (make-gp-params :total-generations 50
							:pop-size 1024
							:initial-depth 5
							:max-depth 17
							:fset *fset*
							:tset *tset*
							:fitness (make-fitness-regression 10)
							:elitism nil
							:type :steady-state
							))

(defun multi-regression (&key (params *multi-regression-params*) (runs 1)  (output :screen))
  (launch-gp *fset* *tset* :params params :runs runs :output output))
