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
;;; GP tree evaluation: interpreter and sub-tree compilation
;;;
;;; These functions replace the use of CL:EVAL for GP tree evaluation.
;;; Trees are S-expressions built from:
;;;   - Atoms (numbers, T, NIL): self-evaluating constants
;;;   - Single-element lists like (VAR-X): 0-arity terminal function calls
;;;   - Multi-element lists like (GP-PLUS (VAR-X) 5): function applications
;;;
;;; Two evaluation strategies are provided:
;;;
;;;   EVAL-TREE  - a recursive tree-walking interpreter.
;;;                Suitable for one-off evaluations.
;;;
;;;   COMPILE-TREE - compiles a GP tree into a closure (thunk) that can
;;;                  be called repeatedly without re-traversing the tree
;;;                  structure. Symbol-function lookups and arity dispatch
;;;                  happen once at compilation time. This is the preferred
;;;                  approach when a tree must be evaluated many times
;;;                  (e.g. across multiple fitness cases).
;;;

;;; --------------------------------------------------------
;;; Tree-walking interpreter
;;; --------------------------------------------------------

(defun eval-tree (tree)
  "Evaluate a GP tree by recursive interpretation.
   Atoms evaluate to themselves; lists are treated as function applications
   where the car names the function and the cdr are the argument sub-trees."
  (cond
    ;; A cons cell is a function application: (fn-name . args)
    ((consp tree)
     (let ((fn (symbol-function (first tree)))
           (args (rest tree)))
       (case (length args)
         (0 (funcall fn))
         (1 (funcall fn (eval-tree (first args))))
         (2 (funcall fn
                     (eval-tree (first args))
                     (eval-tree (second args))))
         (3 (funcall fn
                     (eval-tree (first args))
                     (eval-tree (second args))
                     (eval-tree (third args))))
         (otherwise
          (apply fn (mapcar #'eval-tree args))))))
    ;; Atoms (numbers, T, NIL, etc.) are self-evaluating.
    (t tree)))

;;; --------------------------------------------------------
;;; Sub-tree compilation
;;; --------------------------------------------------------

(defun compile-tree (tree)
  "Compile a GP tree into a zero-argument closure (thunk).
   Each sub-tree is compiled independently: function lookups and arity
   dispatch are resolved once and captured in the closure chain.
   Call the returned thunk with FUNCALL to evaluate the tree.

   Example:
     (let ((fn (compile-tree (individual-tree ind))))
       (funcall fn))  ;=> evaluation result"
  (cond
    ;; Function application
    ((consp tree)
     (let ((fn (symbol-function (first tree)))
           (compiled-args (mapcar #'compile-tree (rest tree))))
       (case (length compiled-args)
         ;; 0-arity: terminal function like (VAR-X)
         (0 (lambda () (funcall fn)))
         ;; 1-arity: e.g. (GP-SQUARE (VAR-X))
         (1 (let ((a1 (first compiled-args)))
              (lambda () (funcall fn (funcall a1)))))
         ;; 2-arity: e.g. (GP-PLUS (VAR-X) 5)
         (2 (let ((a1 (first compiled-args))
                  (a2 (second compiled-args)))
              (lambda () (funcall fn (funcall a1) (funcall a2)))))
         ;; 3-arity: e.g. (GP-IF cond then else)
         (3 (let ((a1 (first compiled-args))
                  (a2 (second compiled-args))
                  (a3 (third compiled-args)))
              (lambda () (funcall fn (funcall a1) (funcall a2) (funcall a3)))))
         ;; General case
         (otherwise
          (lambda () (apply fn (mapcar #'funcall compiled-args)))))))
    ;; Atoms: wrap constant in a thunk
    (t (let ((val tree))
         (lambda () val)))))

;;; --------------------------------------------------------
;;; Environment-based evaluation
;;; --------------------------------------------------------
;;;
;;; These variants take an environment — an alist of (symbol . value-cell)
;;; pairs where each value-cell is a cons whose car holds the current value.
;;; Variable terminals look up their bindings in the environment rather than
;;; calling global-state-mutating functions.
;;;
;;; Usage:
;;;   (let* ((env (make-env '(x y)))     ; create environment with vars X, Y
;;;          (fn  (compile-tree-with-env tree env)))
;;;     (setf (env-var env 'x) 3.0       ; set X
;;;           (env-var env 'y) 4.0)      ; set Y
;;;     (funcall fn))                     ; evaluate tree
;;;

(defun make-env (variables)
  "Create an environment alist for the given variable symbols.
   Each entry is (SYMBOL . (value)), where the cons cell serves as a
   mutable value-cell that can be shared with compiled closures."
  (mapcar (lambda (var) (cons var (cons 0 nil))) variables))

(defun env-var (env symbol)
  "Get the current value of SYMBOL in ENV."
  (car (cdr (assoc symbol env))))

(defun (setf env-var) (value env symbol)
  "Set the current value of SYMBOL in ENV."
  (setf (car (cdr (assoc symbol env))) value))

(defun eval-tree-with-env (tree env)
  "Evaluate a GP tree using ENV for variable lookups.
   Terminals that name a variable in ENV are resolved from the environment.
   Other terminals are treated as in eval-tree (atoms self-evaluate,
   single-element lists call 0-arity functions)."
  (cond
    ((consp tree)
     (let* ((head (first tree))
	    (args (rest tree))
	    (binding (assoc head env)))
       (if (and binding (null args))
	   ;; Variable terminal: (VAR-X) with VAR-X in env
	   (car (cdr binding))
	   ;; Function application
	   (let ((fn (symbol-function head)))
	     (case (length args)
	       (0 (funcall fn))
	       (1 (funcall fn (eval-tree-with-env (first args) env)))
	       (2 (funcall fn
			   (eval-tree-with-env (first args) env)
			   (eval-tree-with-env (second args) env)))
	       (3 (funcall fn
			   (eval-tree-with-env (first args) env)
			   (eval-tree-with-env (second args) env)
			   (eval-tree-with-env (third args) env)))
	       (otherwise
		(apply fn (mapcar (lambda (a) (eval-tree-with-env a env)) args))))))))
    (t tree)))

(defun compile-tree-with-env (tree env)
  "Compile a GP tree into a zero-argument closure that reads variables from ENV.
   Terminals matching a variable in ENV capture the environment's value-cell
   directly, so subsequent (setf env-var) calls are reflected in evaluations."
  (cond
    ((consp tree)
     (let* ((head (first tree))
	    (args (rest tree))
	    (binding (assoc head env)))
       (if (and binding (null args))
	   ;; Variable terminal: capture the value-cell cons
	   (let ((cell (cdr binding)))
	     (lambda () (car cell)))
	   ;; Function application
	   (let ((fn (symbol-function head))
		 (compiled-args (mapcar (lambda (a) (compile-tree-with-env a env))
					args)))
	     (case (length compiled-args)
	       (0 (lambda () (funcall fn)))
	       (1 (let ((a1 (first compiled-args)))
		    (lambda () (funcall fn (funcall a1)))))
	       (2 (let ((a1 (first compiled-args))
			(a2 (second compiled-args)))
		    (lambda () (funcall fn (funcall a1) (funcall a2)))))
	       (3 (let ((a1 (first compiled-args))
			(a2 (second compiled-args))
			(a3 (third compiled-args)))
		    (lambda () (funcall fn (funcall a1) (funcall a2) (funcall a3)))))
	       (otherwise
		(lambda () (apply fn (mapcar #'funcall compiled-args)))))))))
    (t (let ((val tree))
	 (lambda () val)))))
