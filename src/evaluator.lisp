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
