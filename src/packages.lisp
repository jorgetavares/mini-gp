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

(defpackage :mini-gp
  (:use :common-lisp)
  (:export launch-gp
	   launch-gp2
	   run-random-search
	   gp-multiple-runs
	   *generate-constant*
	   gp-constant
	   gp-plus
	   gp-minus
	   gp-times
	   gp-divison
	   gp-log
	   gp-srqt
	   gp-square-root
	   gp-power
	   gp-square
	   gp-if
	   gp-and
	   gp-or
	   gp-not
	   gp-<
	   gp-<=
	   gp->
	   gp->=
	   gp-=
	   gp-/=
	   gp-constant-int
	   gp-constant-real
	   gp-true
	   gp-false
	   gp-random-real
	   gp-random-10
	   gp-random-100
	   gp-random-n
	   individual
	   make-individual
	   individual-tree
	   individual-fitness
	   individual-info
	   individual-eval-p
	   safe-copy-individual
	   make-random-individual
	   make-fset
	   function-name
	   function-args
	   ramped-half-and-half
	   full-method-tree-generic
	   grow-method-tree-generic
	   make-population
	   make-gp-params
	   gp-params-total-generations
	   gp-params-pop-size
	   gp-params-initial-depth
	   gp-params-max-depth
	   gp-params-fset
	   gp-params-tset
	   gp-params-fitness
	   gp-params-cx-rate
	   gp-params-elitism
	   gp-params-type
	   ))
  