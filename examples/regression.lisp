;;;;
;;;; mini-gp example: symbolic regression
;;;;

(defpackage mini-gp-regression
  (:use common-lisp mini-gp)
  (:export regression 
	   var-x
	   int-constants
	   make-fitness-regression
	   *fset*
	   *tset*
	   *fitness-cases*
	   *x-points*
	   *y-points*))

(in-package mini-gp-regression)


;;;
;;; function and terminal sets
;;;

(defun var-x () 0) ; kept for tree display; actual values come from env

(defun int-constants (min max)
  #'(lambda ()
      (+ min (random (1+ (- max min))))))

(defparameter *fset* (make-fset 'gp-plus 2
				'gp-minus 2 
				'gp-times 2
				'gp-division 2
				))

(defparameter *tset* '(gp-constant var-x))


;;;
;;; fitness function ( y = f(x) = x^2 / 2 )
;;;

(defparameter *x-points* (loop for i from 0 below 10 collect (/ i 10)))
(defparameter *y-points* (loop for x in *x-points* collect (float (/ (expt x 2) 2))))

(defun make-fitness-regression (fitness-cases x-points y-points)
  (declare (ignore fitness-cases))
  #'(lambda (individual)
      (let* ((env (make-env '(var-x)))
	     (fn (compile-tree-with-env (individual-tree individual) env)))
        (loop for x in x-points
	      for y in y-points
	      do (setf (env-var env 'var-x) x)
	      sum (expt (- (funcall fn) y) 2)))))


;;;
;;; run GP
;;;

(defparameter *regression-params* (make-gp-params :total-generations 10
						  :pop-size 500
						  :initial-depth 1
						  :max-depth 4
						  :fset *fset*
						  :tset *tset*
						  :fitness (make-fitness-regression 
							    10 *x-points* *y-points*)
						  :elitism nil
						  :generate-constant (int-constants -5 5)
						  :type :steady-state
						  ))

(defun regression (&key (params *regression-params*) (runs 1)  (output :screen))
  (launch-gp *fset* *tset* :params params :runs runs :output output))
