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
;;; representation and population initialization
;;;

(defstruct individual
  (tree nil)
  (fitness 0.0 :type single-float)
  (eval-p t)) ; if eval-p is t, tree must be evaluated

(defun safe-copy-individual (individual)
  "Fresh copy of a individual structure."
  (make-individual
   :tree (copy-tree (individual-tree individual))
   :fitness (individual-fitness individual)))

(defun make-random-individual (tree-limit fset tset)
  "Return a random generate tree without being evaluated."
  (make-individual :tree (ramped-half-and-half tree-limit fset tset)))

(defun make-population (size tree-limit fset tset)
  "Return an array filled with random gp individuals."
  (make-array size 
	      :initial-contents (loop repeat size 
				      collect (make-random-individual tree-limit fset tset))))


;;;
;;; evaluation
;;;

(defun eval-population (population size fitness-function)
  "Set the fitness to every element in the population."
  (declare (type fixnum size)
	   (type simple-vector population)
	   (ignore size))
  (loop for individual across population
	do (eval-individual individual fitness-function)))

(defun eval-individual (individual fitness-function)
  "Set the fitness function of a single individual."
  (when (individual-eval-p individual)
    (setf (individual-fitness individual) 
	  (coerce (funcall fitness-function individual) 'single-float))
    (setf (individual-eval-p individual) nil)))


;;;
;;; selection
;;;

(defun tournament (tournament-size population size comparator)
  "Tournament selection: return best individual from a random set of a given size."
  (declare (type fixnum tournament-size size)
	   (type simple-vector population))
  (let ((best (aref population (random size))))
    (loop for n fixnum from 1 below tournament-size
	  do (let ((current (aref population (random size))))
	       (when (funcall comparator
			    (the single-float (individual-fitness current))
			    (the single-float (individual-fitness best)))
		 (setf best current)))
	  finally (return (copy-individual best)))))

(defun index-tournament (tournament-size population size comparator)
  "Tournament selection: return best individual from a random set of a given size."
  (declare (type fixnum tournament-size size)
	   (type simple-vector population))
  (let* ((bindex (random size))
	 (best (aref population bindex)))
    (loop for n fixnum from 1 below tournament-size
	  do (let ((index (random size)))
	       (when (funcall comparator
			    (the single-float (individual-fitness (aref population index)))
			    (the single-float (individual-fitness best)))
		 (setf best (aref population index))
		 (setf bindex index)))
	  finally (return bindex))))

(defun selection (population size tournament-size comparator &optional target)
  "Return a new population."
  (declare (type fixnum size tournament-size)
	   (type simple-vector population))
  (loop with new-population of-type simple-vector = (or target (make-array size))
	for i fixnum from 0 below size
	do (setf (aref new-population i)
		 (tournament tournament-size population size comparator))
	finally (return new-population)))


;;;
;;; elitism
;;;

(defun find-best (population size comparator)
  "Return the indicies of the best or worst individuals in the population, according to comparator."
  (declare (type fixnum size)
	   (type simple-vector population))
  (loop with best fixnum = 0
	for i fixnum from 1 below size 
	when (funcall comparator 
		      (the single-float (individual-fitness (aref population i)))
		      (the single-float (individual-fitness (aref population best))))
	do (setf best i)
	finally (return best)))

(defun elitism (population size best-individual comparator)
  "Replace a random individual with the best from the previous generation."
  (let ((worst-position (find-best population size (lambda (a b) (funcall comparator b a)))))
    (setf (aref population worst-position) 
	  (copy-individual best-individual)) population))

;;;
;;; variation
;;;

(defun apply-crossover (population size max-depth rate)
  "Apply tree crossover to the population."
  (declare (type fixnum size max-depth)
	   (type single-float rate)
	   (type simple-vector population))
  (loop for position fixnum from 0 below size by 2
	do (when (< (random 1.0) rate)
	     (multiple-value-bind (o1 o2)
		 (tree-crossover max-depth 
				 (aref population position) 
				 (aref population (1+ position)))
	       (setf (aref population position) o1 
		     (aref population (1+ position)) o2)))))

(defun apply-mutation (population size mt-rate node-rate fset-arity-map fset-by-arity tset tset-size)
  "Apply point mutation to the population."
  (declare (type fixnum size tset-size)
	   (type single-float mt-rate node-rate)
	   (type simple-vector population tset))
  (loop for position fixnum from 0 below size
	do (when (< (random 1.0) mt-rate)
	     (let ((individual (aref population position)))
	       (point-mutation individual node-rate fset-arity-map fset-by-arity tset tset-size)))))


;;;
;;; GP loops with replacement modes 
;;;

(defun run-single-gp (parameters output streams)
  "Main gp loop."
  (let* ((total-generations (gp-params-total-generations parameters))
	 (pop-size (gp-params-pop-size parameters))
	 (initial-depth (gp-params-initial-depth parameters))
	 (max-depth (gp-params-max-depth parameters))
	 (fset (gp-params-fset parameters))
	 (tset (gp-params-tset parameters))
	 (tset-vec (coerce tset 'vector))
	 (tset-size (length tset-vec))
	 (fset-by-arity (build-fset-by-arity fset))
	 (fset-arity-map (build-fset-arity-map fset))
	 (fitness (gp-params-fitness parameters))
	 (t-size (gp-params-t-size parameters))
	 (cx-rate (gp-params-cx-rate parameters))
	 (mt-rate (gp-params-mt-rate parameters))
	 (node-rate (gp-params-node-rate parameters))
	 (comparator (gp-params-comparator parameters))
	 (population (make-population pop-size initial-depth fset tset))
	 (buffer (make-array pop-size))
	 (elitism-p (gp-params-elitism parameters))
	 (best nil) (run-best nil) (new-best-p t))
    (eval-population population pop-size fitness)
    (setf best (copy-individual (aref population (find-best population pop-size comparator))))
    (setf run-best (copy-individual best))
    (output-generation 1 population pop-size best run-best new-best-p output streams)
    (loop for generation from 2 to total-generations
	  do (progn
	       (selection population pop-size t-size comparator buffer)
	       (setf new-best-p nil)
	       (apply-crossover buffer pop-size max-depth cx-rate)
	       (apply-mutation buffer pop-size mt-rate node-rate fset-arity-map fset-by-arity tset-vec tset-size)
	       (eval-population buffer pop-size fitness)
	       (when elitism-p
		 (elitism buffer pop-size best comparator))
	       (rotatef population buffer)
	       (setf best (copy-individual (aref population (find-best population pop-size comparator))))
	       (when (funcall comparator (individual-fitness best) (individual-fitness run-best))
		 (setf run-best (copy-individual best))
		 (setf new-best-p t))
	       (output-generation generation population pop-size best 
				  run-best new-best-p output streams))
	  finally (return run-best))))

(defun run-steady-state (parameters output streams)
  "Main gp loop."
  (let* ((total-generations (gp-params-total-generations parameters))
	 (pop-size (gp-params-pop-size parameters))
	 (initial-depth (gp-params-initial-depth parameters))
	 (max-depth (gp-params-max-depth parameters))
	 (fset (gp-params-fset parameters))
	 (tset (gp-params-tset parameters))
	 (tset-vec (coerce tset 'vector))
	 (tset-size (length tset-vec))
	 (fset-by-arity (build-fset-by-arity fset))
	 (fset-arity-map (build-fset-arity-map fset))
	 (fitness (gp-params-fitness parameters))
	 (t-size (gp-params-t-size parameters))
	 (cx-rate (gp-params-cx-rate parameters))
	 (mt-rate (gp-params-mt-rate parameters))
	 (node-rate (gp-params-node-rate parameters))
	 (comparator (gp-params-comparator parameters))
	 (inverse-comparator (lambda (a b) (funcall comparator b a)))
	 (population (make-population pop-size initial-depth fset tset))
	 (elitism-p (gp-params-elitism parameters))
	 (best nil) (run-best nil) (new-best-p t))
    (eval-population population pop-size fitness)
    (setf best (copy-individual (aref population (find-best population pop-size comparator))))
    (setf run-best (copy-individual best))
    (output-generation 1 population pop-size best run-best new-best-p output streams)
    (loop for generation from 2 to total-generations
	  do (progn
	       (setf new-best-p nil)
	       (loop for i from 1 to pop-size 
		     do (let ((offspring nil))
			  (if (< (random 1.0) cx-rate)
			      (let* ((parent1 (tournament t-size population pop-size comparator))
				     (parent2 (tournament t-size population pop-size comparator)))
				(multiple-value-bind (o1 o2)
				    (tree-crossover max-depth parent1 parent2)
				  (setf offspring (if (< (random 1.0) 0.5) o1 o2))))
			      (setf offspring
				    (point-mutation 
				     (tournament t-size population pop-size comparator) 
				     node-rate fset-arity-map fset-by-arity tset-vec tset-size)))
			  (eval-individual offspring fitness)
			  (setf (aref population (index-tournament t-size population pop-size
								  inverse-comparator))
				(copy-individual offspring))))
	       (when elitism-p
		 (elitism population pop-size best comparator))
	       (setf best (copy-individual (aref population (find-best population pop-size comparator))))
	       (when (funcall comparator (individual-fitness best) (individual-fitness run-best))
		 (setf run-best (copy-individual best))
		 (setf new-best-p t))
	       (output-generation generation population pop-size best 
				  run-best new-best-p output streams))
	  finally (return run-best))))

