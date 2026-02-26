(in-package :mini-gp)

;;;
;;; crossover
;;;

;; subtree crossover

(defun tree-crossover (size p1 p2)
  (multiple-value-bind (o1 o2)
      (cross-subtrees (individual-tree p1) (individual-tree p2) size)
    (values (make-individual :tree o1 :eval-p t)
	    (make-individual :tree o2 :eval-p t))))

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
         (copy-tree p1) (first o1))
     (if (or (= 1 p2-limit) (> p2-limit depth))
         (copy-tree p2) (first o2)))))

(defun count-tree-nodes (tree)
  "Count the number of nodes in a tree."
  (if (consp tree)
      (let ((count 1))
	(dolist (child (rest tree) count)
	  (incf count (count-tree-nodes child))))
      1))

(defun tree-depth (tree)
  "Return the max depth of a tree."
  (if (consp tree)
      (let ((max-child 0))
	(dolist (child (rest tree) (1+ max-child))
	  (let ((d (tree-depth child)))
	    (when (> d max-child) (setf max-child d)))))
      1))


;;;
;;; mutation
;;;

(defun build-fset-by-arity (fset)
  "Build a hash table mapping arity to vector of fset entries."
  (let ((ht (make-hash-table)))
    (dolist (f fset)
      (push f (gethash (function-args f) ht)))
    (maphash (lambda (k v)
	       (setf (gethash k ht) (coerce v 'vector)))
	     ht)
    ht))

(defun build-fset-arity-map (fset)
  "Build a hash table mapping function symbol to its arity."
  (let ((ht (make-hash-table)))
    (dolist (f fset ht)
      (setf (gethash (function-name f) ht) (function-args f)))))

;; point mutation

(defun point-mutation (individual rate fset-arity-map fset-by-arity tset tset-size)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (setf (individual-tree individual)
	(point-mutate-tree (individual-tree individual) rate fset-arity-map fset-by-arity tset tset-size))
  (setf (individual-eval-p individual) t)
  individual)

(defun point-mutate-tree (tree rate fset-arity-map fset-by-arity tset tset-size)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (if (or (not (consp tree))
	  (null (rest tree)))
      (mutate-terminal tree rate tset tset-size)
    (let* ((element (first tree))
	   (nargs (gethash element fset-arity-map)))
      (cons (mutate-function element nargs rate fset-by-arity) 
	    (loop for child in (rest tree)
		  collect (point-mutate-tree child rate fset-arity-map fset-by-arity tset tset-size))))))

(defun mutate-terminal (terminal rate tset tset-size)
  (if (< (random 1.0) rate)
      (process-terminal (aref tset (random tset-size)))
    terminal))

(defun mutate-function (function nargs rate fset-by-arity)
  (if (< (random 1.0) rate)
      (let ((candidates (gethash nargs fset-by-arity)))
	(if (null candidates)
	    function
	    (function-name (aref candidates (random (length candidates))))))
    function))


;; subtree mutation

(defun subtree-mutation (individual max-depth fset tset)
  "Subtree mutation: replace a random subtree with a new random tree.
   Returns the (modified) individual with eval-p set to t."
  (let* ((tree (individual-tree individual))
	 (num-nodes (count-tree-nodes tree))
	 (point (random num-nodes))
	 (wrapper (list (copy-tree tree))))
    (multiple-value-bind (subtree-cell fragment)
	(get-subtree (first wrapper) wrapper point)
      (declare (ignore fragment))
      (let ((new-subtree (ramped-half-and-half (max 1 (- max-depth 1)) fset tset)))
	(setf (first subtree-cell) new-subtree)))
    (let ((result (first wrapper)))
      (if (> (tree-depth result) max-depth)
	  ;; reject: keep original tree
	  (progn
	    (setf (individual-eval-p individual) nil)
	    individual)
	  (progn
	    (setf (individual-tree individual) result)
	    (setf (individual-eval-p individual) t)
	    individual)))))
