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
;;; mini-gp main functions
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
  (mt-rate 0.05)
  (elitism t)
  (type :generational))

(defun launch-gp (fset tset &key (id "gp") (runs 1) (output :screen) 
		       (generations 10) (pop-size 10) (initial-depth 2) 
		       (max-depth 5) (elitism t)
		       (fitness-function nil) (params nil) (type :generational)) 
  "Start GP."
  (let* ((fitness fitness-function)
	 (gp-params (if params 
			params
		      (make-gp-params :total-generations generations
				      :pop-size pop-size
				      :initial-depth initial-depth
				      :max-depth max-depth
				      :fset fset
				      :tset tset
				      :fitness fitness
				      :elitism elitism
				      :type type))))
    (gp-multiple-runs gp-params :runs runs :output output :id id 
		      :type (gp-params-type gp-params))))

(defun gp-multiple-runs (parameters &key (runs 1) (output :screen) (id "gp") (type :generational))
  "Run the gp engine for several runs."
  (loop for run from 1 to runs
	do (format t "GP run ~a~%" run)
	collect (time (config-gp-output parameters output run id type))))

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
  (loop for individual across population
	sum (individual-fitness individual) into total
	finally (return (/ total pop-size))))
