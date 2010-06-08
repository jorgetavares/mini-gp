;;;;
;;;; mini-gp example: multi-valued symbolic regression
;;;;

(use-package :mini-gp)


;;;
;;; function and terminal sets
;;;

(defparameter *X* 0)
(defparameter *Y* 0)

(defun var-x () 
  *X*)

(defun var-y () 
  *Y*)

(defparameter *fset* (mini-gp:make-fset 'mini-gp:gp-plus 2
					'mini-gp:gp-minus 2 
					'mini-gp:gp-times 2
					))

(defparameter *tset* '(var-x var-y))


;;;
;;; fitness function f(x,y) = x*x*y + x*y + y 
;;;

(defparameter *x-points* (loop for i from 0 below 10 collect (/ i 10)))
(defparameter *y-points* (loop for x in *x-points* collect (float (/ (expt x 2) 2))))

(defun make-fitness-regression (fitness-cases)
  #'(lambda (individual id generation)
      (declare (ignore id generation))
      (loop with expected = 0
	 repeat fitness-cases
	 do (progn
	      (setf *X* (random 1.0) *Y* (random 1.0))
	      (setf expected (+ (* *X* *X* *Y*) (* *X* *Y*) *Y*)))
	 sum (expt (- expected 
		      (eval (mini-gp:individual-tree individual)) 
		      ) 2))))


;;;
;;; run GP
;;;


(defparameter *multi-regression-params* (mini-gp:make-gp-params :total-generations 50
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
  (mini-gp:launch-gp *fset* *tset* :params params :runs runs :output output))
