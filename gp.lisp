(in-package #:mini-gp)

;;;
;;; function and terminal sets lists to be used for evolution
;;;

;;;
;;; function set definitions
;;;

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

(defun gp-division (a b)
  (when (and (numberp a) (numberp b))
    (if (> b 0)
	(/ a b) 0)))

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


;;;
;;; terminal set definitions
;;;

;; constants (only used in a tree generation by keeping their values)

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

;;;
;;; tree generators 
;;;

(defun ramped-half-and-half (limit fset tset)
  "A gp tree is created with half of probability for each method."
  (let ((fset-size (length fset))
	(tset-size (length tset)))
    (if (< (random 1.0) 0.5)
	(full-method-tree-generic 0 limit fset tset)
	(grow-method-tree-generic 0 limit fset tset))))

(defun make-fset (&rest args)
  (loop 
     for function in args by #'cddr
     for arguments in (rest args) by #'cddr
     collect (cons function arguments)))

(defun function-name (pair)
  (car pair))

(defun function-args (pair)
  (cdr pair))

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
    (gp-constant-int (gp-constant-int))
    (gp-constant-real (gp-constant-real))
    (gp-true (gp-true))
    (gp-false (gp-false))
    (otherwise (list terminal))))


;;;;
;;;; GP engine 
;;;;

;;;
;;; representation and population initialization
;;;

(defstruct individual
  (tree nil)
  (fitness 0))

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
;;; evaluations
;;;

(defun eval-population (population size fitness-function generation)
  "Set the fitness to every element in the population."
  (loop 
     for individual across population
     for id from 1 to size
     do (setf (individual-fitness individual) 
	      (funcall fitness-function individual id generation))))

;;;
;;; selection
;;;

(defun tournament (tournament-size population size)
  "Tournament selection: return best individual from a random set of a given size."
  (loop with best = (aref population (random size))
     for n from 1 below tournament-size
     do (let ((current (aref population (random size))))
	  (when (< (individual-fitness current) (individual-fitness best))
	    (setf best (safe-copy-individual current))))
     finally (return best)))

(defun selection (population size tournament-size)
  "Return a new population."
  (loop with new-population = (make-array size)
     for i from 0 below size
     do (setf (aref new-population i)
	      (tournament tournament-size population size))
     finally (return new-population)))


;;;
;;; elitism
;;;

(defun find-best (population size comparator)
  "Return the indicies of the best or worst individuals in the population, according to the comparator."
  (loop 
     with best = 0
     for i from 1 below size 
     when (funcall comparator 
		   (individual-fitness (aref population i)) 
		   (individual-fitness (aref population best)))
     do (setf best i)
     finally (return best)))

(defun elitism (population size best-individual)
  "Replace a random individual with the best from the previous generation."
  (let ((worst-position (find-best population size #'>)))
    (setf (aref population worst-position) (safe-copy-individual best-individual)) population))


;;;
;;; genetic operators
;;;

(defun apply-crossover (population size max-depth rate)
  "Apply tree crossover to the population."
  (loop for position from 0 below size by 2
     do (when (< (random 1.0) rate)
	  (multiple-value-bind (o1 o2)
	      (tree-crossover max-depth (aref population position) (aref population (1+ position)))
	    (setf (aref population position) o1 (aref population (1+ position)) o2)))))

(defun tree-crossover (size p1 p2)
  (multiple-value-bind (o1 o2)
      (cross-subtrees (individual-tree p1) (individual-tree p2) size)
    (values (make-individual :tree (copy-tree o1))
	    (make-individual :tree (copy-tree o2)))))

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
;;; GP engine
;;;

(defstruct gp-params
  (total-generations 100)
  (pop-size 100)
  (initial-depth 2)
  (max-depth 5)
  (fset nil)
  (tset nil)
  (fitness nil)
  (t-size 3)
  (cx-rate 0.9)
  (elitism t)
  )

(defun launch-gp (fset tset &key (id "gp") (runs 1) (output :screen) (generations 10)
		  (pop-size 10) (initial-depth 2) (max-depth 5) (elitism t)
		  (fitness-function nil) (params nil))		  
  "Start GP."
  (let* ((fitness fitness-function)
	 (gp-params (if params params
			(make-gp-params :total-generations generations
					:pop-size pop-size
					:initial-depth initial-depth
					:max-depth max-depth
					:fset fset
					:tset tset
					:fitness fitness
					:elitism elitism))))
    (gp-multiple-runs gp-params :runs runs :output output :id id)))


(defun gp-multiple-runs (parameters &key (runs 1) (output :screen) (id "gp"))
  "Run the gp engine for several runs"
  (loop for run from 1 to runs
     collect (config-gp-output parameters output run id)))

(defun config-gp-output (parameters output run id)
  "Config a GP run output (:none, :screen, :files, or both)."
  (if (member output '(:files :screen+files))
      (with-open-file (run-stream (concatenate 'string id "-run" (format nil "~D" run) ".txt")
				  :direction :output :if-exists :supersede)
	(with-open-file (best-stream (concatenate 'string id "-best" (format nil "~D" run) ".txt")
				     :direction :output :if-exists :supersede)
	  (run-single-gp parameters output (list run-stream best-stream))))
      (run-single-gp parameters output nil)))

(defun run-single-gp (parameters output streams)
  "Main gp loop."
  (let* ((total-generations (gp-params-total-generations parameters))
	 (pop-size (gp-params-pop-size parameters))
	 (initial-depth (gp-params-initial-depth parameters))
	 (max-depth (gp-params-max-depth parameters))
	 (fset (gp-params-fset parameters))
	 (tset (gp-params-tset parameters))
	 (fitness (gp-params-fitness parameters))
	 (t-size (gp-params-t-size parameters))
	 (cx-rate (gp-params-cx-rate parameters))
	 (population (make-population pop-size initial-depth fset tset))
	 (elitism-p (gp-params-elitism parameters))
	 (best nil) (run-best nil))
    (eval-population population pop-size fitness 1)
    (setf best (copy-individual (aref population (find-best population pop-size #'<))))
    (setf run-best (copy-individual best))
    (output-generation 1 population pop-size best run-best output streams)
    (loop for generation from 2 to total-generations
       do (let ((new-population (selection population pop-size t-size)))
	    (apply-crossover new-population pop-size max-depth cx-rate)
	    (eval-population new-population pop-size fitness generation)
	    (when elitism-p
	      (elitism new-population pop-size best))
	    (setf population new-population)
	    (setf best (copy-individual (aref population (find-best population pop-size #'<))))
	    (when (< (individual-fitness best) (individual-fitness run-best))
	      (setf run-best (copy-individual best)))
	    (output-generation generation population pop-size best run-best output streams))
       finally (return run-best))))

(defun output-generation (generation population pop-size best run-best output streams)
  "Shows the state of a generation"
  (unless (eql output :none)
    (let ((best-fitness (float (individual-fitness best)))
	  (avg (float (average population pop-size))))
      (when (member output '(:screen :screen+files))
	(format t "~a ~a ~a ~%" generation best-fitness avg))
      (when (member output '(:files :scree+files))
	(format (first streams) "~a ~a ~a ~%" generation best-fitness avg)
	(when (<= (individual-fitness best) (individual-fitness run-best))
	  (format (second streams) "~a ~%" (list generation run-best)))))))

(defun average (population pop-size)
  "Average of population's fitness."
  (loop for individual across population
     sum (individual-fitness individual) into total
     finally (return (/ total pop-size))))

