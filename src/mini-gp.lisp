(in-package :mini-gp)

;;;
;;; mini-gp main functions
;;;

(defstruct gp-params
  (total-generations 100 :type fixnum)
  (pop-size 100 :type fixnum)
  (initial-depth 2 :type fixnum)
  (max-depth 5 :type fixnum)
  (fset nil)
  (tset nil)
  (fitness nil)
  (t-size 3 :type fixnum)
  (cx-rate 0.9 :type single-float)
  (mt-rate 0.05 :type single-float)
  (node-rate 0.1 :type single-float)
  (comparator #'<)
  (elitism t)
  (seed nil)
  (generate-constant nil)
  (type :generational))

(defun launch-gp (fset tset &key (id "gp") (runs 1) (output :screen) 
		       (generations nil gen-p) (pop-size nil ps-p)
		       (initial-depth nil id-p) (max-depth nil md-p)
		       (t-size nil ts-p) (cx-rate nil cx-p)
		       (mt-rate nil mt-p) (node-rate nil nr-p)
		       (comparator nil cmp-p)
		       (elitism nil el-p) (fitness-function nil ff-p)
		       (generate-constant nil gc-p)
		       (params nil) (type nil ty-p) (seed nil seed-p)) 
  "Start GP.  When :params is supplied, individual keyword arguments
   override the corresponding fields in the params struct."
  (let* ((base (or params
		   (make-gp-params :fset fset :tset tset)))
	 (gp-params (make-gp-params
		     :total-generations (if gen-p generations
					   (gp-params-total-generations base))
		     :pop-size (if ps-p pop-size
				  (gp-params-pop-size base))
		     :initial-depth (if id-p initial-depth
				       (gp-params-initial-depth base))
		     :max-depth (if md-p max-depth
				    (gp-params-max-depth base))
		     :fset (gp-params-fset base)
		     :tset (gp-params-tset base)
		     :fitness (if ff-p fitness-function
				  (gp-params-fitness base))
		     :t-size (if ts-p t-size
				 (gp-params-t-size base))
		     :cx-rate (if cx-p cx-rate
				  (gp-params-cx-rate base))
		     :mt-rate (if mt-p mt-rate
				  (gp-params-mt-rate base))
		     :node-rate (if nr-p node-rate
				    (gp-params-node-rate base))
		     :comparator (if cmp-p comparator
				     (gp-params-comparator base))
		     :elitism (if el-p elitism
				  (gp-params-elitism base))
		     :seed (if seed-p seed
			       (gp-params-seed base))
		     :generate-constant (if gc-p generate-constant
					       (gp-params-generate-constant base))
		     :type (if ty-p type
			       (gp-params-type base)))))
    (gp-multiple-runs gp-params :runs runs :output output :id id 
		      :type (gp-params-type gp-params))))

(defun gp-multiple-runs (parameters &key (runs 1) (output :screen) (id "gp") (type :generational))
  "Run the gp engine for several runs."
  (validate-gp-params parameters)
  (let* ((seed (gp-params-seed parameters))
	 (*random-state* (if seed
			     (make-random-state seed)
			     (make-random-state *random-state*))))
    (loop for run from 1 to runs
	  do (format t "GP run ~a~%" run)
	  collect (time (config-gp-output parameters output run id type)))))

(defun validate-gp-params (params)
  "Validate gp-params and signal an error for invalid configurations."
  (let ((pop-size (gp-params-pop-size params))
	(fset (gp-params-fset params))
	(tset (gp-params-tset params))
	(fitness (gp-params-fitness params))
	(cx-rate (gp-params-cx-rate params))
	(mt-rate (gp-params-mt-rate params))
	(node-rate (gp-params-node-rate params))
	(total-gens (gp-params-total-generations params))
	(initial-depth (gp-params-initial-depth params))
	(max-depth (gp-params-max-depth params))
	(t-size (gp-params-t-size params)))
    (unless (and (integerp pop-size) (>= pop-size 2))
      (error "pop-size must be an integer >= 2, got ~a" pop-size))
    (when (oddp pop-size)
      (warn "pop-size ~a is odd; generational crossover pairs individuals by 2 -- the last individual will be skipped" pop-size))
    (unless fset
      (error "fset (function set) must not be empty"))
    (unless tset
      (error "tset (terminal set) must not be empty"))
    (unless fitness
      (error "fitness function must be provided"))
    (unless (functionp fitness)
      (error "fitness must be a function, got ~a" (type-of fitness)))
    (unless (and (realp cx-rate) (<= 0.0 cx-rate 1.0))
      (error "cx-rate must be a number in [0,1], got ~a" cx-rate))
    (unless (and (realp mt-rate) (<= 0.0 mt-rate 1.0))
      (error "mt-rate must be a number in [0,1], got ~a" mt-rate))
    (unless (and (realp node-rate) (<= 0.0 node-rate 1.0))
      (error "node-rate must be a number in [0,1], got ~a" node-rate))
    (unless (and (integerp total-gens) (>= total-gens 1))
      (error "total-generations must be an integer >= 1, got ~a" total-gens))
    (unless (and (integerp initial-depth) (>= initial-depth 1))
      (error "initial-depth must be an integer >= 1, got ~a" initial-depth))
    (unless (and (integerp max-depth) (>= max-depth initial-depth))
      (error "max-depth must be an integer >= initial-depth (~a), got ~a" initial-depth max-depth))
    (unless (and (integerp t-size) (>= t-size 1))
      (error "t-size (tournament size) must be an integer >= 1, got ~a" t-size))))

(defun config-gp-output (parameters output run id type)
  "Config a GP run output (:none, :screen, :files, or both)."
  (if (member output '(:files :screen+files))
      (with-open-file (run-stream (concatenate 'string id "-run" (format nil "~D" run) ".txt")
				  :direction :output :if-exists :supersede)
		      (with-open-file (best-stream (concatenate 'string id "-best" (format nil "~D" run) ".txt")
						   :direction :output :if-exists :supersede)
				      (funcall (run-gp-type type) parameters output (list run-stream best-stream))))
    (funcall (run-gp-type type) parameters output nil)))

(defun run-gp-type (type)
  "Return the appropriate generational model to launch gp."
  (case type
    (:generational #'run-single-gp)
    (:steady-state #'run-steady-state)
    (otherwise (error "Invalid generational model for GP engine."))))

(defun output-generation (generation population pop-size best run-best 
				     new-best-p output streams)
  "Shows the state of a generation"
  (unless (eql output :none)
    (let ((best-fitness (float (individual-fitness best)))
	  (avg (float (average population pop-size))))
      (when (member output '(:screen :screen+files))
	(format t "~a ~a ~a ~%" generation best-fitness avg))
      (when (member output '(:files :screen+files))
	(format (first streams) "~a ~a ~a ~%" generation best-fitness avg)
	(when new-best-p
	  (format (second streams) "~a ~%" (list generation run-best)))))))

(defun average (population pop-size)
  "Average of population's fitness."
  (declare (type fixnum pop-size)
	   (type simple-vector population))
  (loop for individual across population
	sum (the single-float (individual-fitness individual)) into total of-type single-float
	finally (return (/ total pop-size))))
