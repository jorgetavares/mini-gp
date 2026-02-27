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
	   *y-points*
	   multi-regression))

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
        (loop repeat fitness-cases
	      sum (let ((x (random 1.0))
			(y (random 1.0)))
		    (setf (env-var env 'var-x) x
			  (env-var env 'var-y) y)
		    (let ((expected (+ (* x x y) (* x y) y)))
		      (expt (- expected (funcall fn)) 2)))))))


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
