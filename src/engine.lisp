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
  (fitness 0)
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
  (loop for individual across population
	do (eval-individual individual fitness-function)))

(defun eval-individual (individual fitness-function)
  "Set the fitness function of a single individual."
  (when (individual-eval-p individual)
    (setf (individual-fitness individual) 
	  (funcall fitness-function individual))
    (setf (individual-eval-p individual) nil)))


;;;
;;; selection
;;;

(defun tournament (tournament-size population size comparator)
  "Tournament selection: return best individual from a random set of a given size."
  (let ((best (aref population (random size))))
    (loop for n from 1 below tournament-size
	  do (let ((current (aref population (random size))))
	       (when (funcall comparator (individual-fitness current) (individual-fitness best))
		 (setf best (copy-individual current))))
	  finally (return best))))

(defun index-tournament (tournament-size population size comparator)
  "Tournament selection: return best individual from a random set of a given size."
  (let* ((bindex (random size))
	 (best (aref population bindex)))
    (loop for n from 1 below tournament-size
	  do (let ((index (random size)))
	       (when (funcall comparator (individual-fitness (aref population index)) 
			      (individual-fitness best))
		 (setf best (aref population index))
		 (setf bindex index)))
	  finally (return bindex))))

(defun selection (population size tournament-size comparator)
  "Return a new population."
  (loop with new-population = (make-array size)
	for i from 0 below size
	do (setf (aref new-population i)
		 (tournament tournament-size population size comparator))
	finally (return new-population)))


;;;
;;; elitism
;;;

(defun find-best (population size comparator)
  "Return the indicies of the best or worst individuals in the population, according to comparator."
  (loop with best = 0
	for i from 1 below size 
	when (funcall comparator 
		      (individual-fitness (aref population i)) 
		      (individual-fitness (aref population best)))
	do (setf best i)
	finally (return best)))

(defun elitism (population size best-individual)
  "Replace a random individual with the best from the previous generation."
  (let ((worst-position (find-best population size #'>)))
    (setf (aref population worst-position) 
	  (copy-individual best-individual)) population))

;;;
;;; variation
;;;

(defun apply-crossover (population size max-depth rate)
  "Apply tree crossover to the population."
  (loop for position from 0 below size by 2
	do (when (< (random 1.0) rate)
	     (multiple-value-bind (o1 o2)
		 (tree-crossover max-depth 
				 (aref population position) 
				 (aref population (1+ position)))
	       (setf (aref population position) o1 
		     (aref population (1+ position)) o2)))))

(defun apply-mutation (population size mt-rate node-rate fset tset tset-size)
  "Apply point mutation crossover to the population."
  (loop for position from 0 below size
	do (when (< (random 1.0) mt-rate)
	     (let ((individual (aref population position)))
	       (point-mutation individual node-rate fset tset tset-size)
	       (setf (individual-eval-p individual) t)))))


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
	 (tset-size (length tset))
	 (fitness (gp-params-fitness parameters))
	 (t-size (gp-params-t-size parameters))
	 (cx-rate (gp-params-cx-rate parameters))
	 (mt-rate (gp-params-mt-rate parameters))
	 (population (make-population pop-size initial-depth fset tset))
	 (elitism-p (gp-params-elitism parameters))
	 (best nil) (run-best nil) (new-best-p t))
    (eval-population population pop-size fitness)
    (setf best (copy-individual (aref population (find-best population pop-size #'<))))
    (setf run-best (copy-individual best))
    (output-generation 1 population pop-size best run-best new-best-p output streams)
    (loop for generation from 2 to total-generations
	  do (let ((new-population (selection population pop-size t-size #'<)))
	       (setf new-best-p nil)
	       (apply-crossover new-population pop-size max-depth cx-rate)
	       (apply-mutation new-population pop-size 0.1 mt-rate fset tset tset-size)
	       (eval-population new-population pop-size fitness)
	       (when elitism-p
		 (elitism new-population pop-size best))
	       (setf population new-population)
	       (setf best (copy-individual (aref population (find-best population pop-size #'<))))
	       (when (< (individual-fitness best) (individual-fitness run-best))
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
	 (tset-size (length tset))
	 (fitness (gp-params-fitness parameters))
	 (t-size (gp-params-t-size parameters))
	 (cx-rate (gp-params-cx-rate parameters))
	 (mt-rate (gp-params-mt-rate parameters))
	 (population (make-population pop-size initial-depth fset tset))
	 (elitism-p (gp-params-elitism parameters))
	 (best nil) (run-best nil) (new-best-p t))
    (eval-population population pop-size fitness)
    (setf best (copy-individual (aref population (find-best population pop-size #'<))))
    (setf run-best (copy-individual best))
    (output-generation 1 population pop-size best run-best new-best-p output streams)
    (loop for generation from 2 to total-generations
	  do (progn
	       (setf new-best-p nil)
	       (loop for i from 1 to pop-size 
		     do (let ((offspring nil))
			  (if (< (random 1.0) cx-rate)
			      (let* ((parent1 (tournament t-size population pop-size #'<))
				     (parent2 (tournament t-size population pop-size #'<)))
				(setf offspring (tree-crossover max-depth parent1 parent2)))
			    (progn 
			      (setf offspring
				    (point-mutation 
				     (tournament t-size population pop-size #'<) 
				     mt-rate fset tset tset-size))
			      (setf (individual-eval-p offspring) nil)))
			  (eval-individual offspring fitness)
			  (setf (aref population (index-tournament t-size population pop-size #'>))
				(copy-individual offspring))))
	       (when elitism-p
		 (elitism population pop-size best))
	       (setf best (copy-individual (aref population (find-best population pop-size #'<))))
	       (when (< (individual-fitness best) (individual-fitness run-best))
		 (setf run-best (copy-individual best))
		 (setf new-best-p t))
	       (output-generation generation population pop-size best 
				  run-best new-best-p output streams))
	  finally (return run-best))))

