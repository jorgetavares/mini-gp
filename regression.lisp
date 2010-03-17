;;;;
;;;; mini-gp example: symbolic regression
;;;;

(use-package :mini-gp)


;;;
;;; function and terminal sets
;;;

(defparameter *X* 0)

(defun var-x () 
  *X*)

(defun int-constants (min max)
  #'(lambda ()
      (+ min (random (1+ (- max min))))))

(setf mini-gp:*generate-constant* (int-constants -5 5))

(defparameter *fset* (mini-gp:make-fset 'mini-gp:gp-plus 2
					'mini-gp:gp-minus 2 
					'mini-gp:gp-times 2
					'mini-gp:gp-divison 2
					))

(defparameter *tset* '(mini-gp:gp-constant var-x))


;;;
;;; fitness function ( y = f(x) = x^2 / 2 )
;;;

(defparameter *x-points* (loop for i from 0 below 10 collect (/ i 10)))
(defparameter *y-points* (loop for x in *x-points* collect (float (/ (expt x 2) 2))))

(defun make-fitness-regression (fitness-cases x-points y-points)
  #'(lambda (individual id generation)
      (declare (ignore id generation))
      (loop for i from 0 below fitness-cases
	 do (setf *X* (nth i x-points))
	 sum (expt (- (eval (mini-gp:individual-tree individual)) 
		      (nth i y-points)) 2))))


;;;
;;; run GP
;;;


(defparameter *regression-params* (mini-gp:make-gp-params :total-generations 50
							  :pop-size 1000
							  :initial-depth 1
							  :max-depth 4
							  :fset *fset*
							  :tset *tset*
							  :fitness (make-fitness-regression 
								    10 *x-points* *y-points*)
							  :elitism nil
							  :type :steady-state
							  ))

(defun regression (&key (params *regression-params*) (runs 1)  (output :screen))
  (mini-gp:launch-gp *fset* *tset* :params params :runs runs :output output))
