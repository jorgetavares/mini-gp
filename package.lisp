(defpackage #:mini-gp
  (:use #:common-lisp)
  (:export #:launch-gp
	   #:gp-plus
	   #:gp-minus
	   #:gp-times
	   #:gp-divison
	   #:gp-log
	   #:gp-srqt
	   #:gp-square-root
	   #:gp-power
	   #:gp-square
	   #:gp-if
	   #:gp-and
	   #:gp-or
	   #:gp-not
	   #:gp-<
	   #:gp-<=
	   #:gp->
	   #:gp->=
	   #:gp-=
	   #:gp-/=
	   #:gp-constant-int
	   #:gp-constant-real
	   #:gp-true
	   #:gp-false
	   #:gp-random-real
	   #:gp-random-10
	   #:gp-random-100
	   #:gp-random-n
	   #:make-idividual
	   #:individual-tree
	   #:individual-fitness
	   #:safe-copy-individual
	   #:make-random-individual
	   #:make-gp-params
	   #:gp-params-total-generations
	   #:gp-params-pop-size
	   #:gp-params-initial-depth
	   #:gp-params-max-depth
	   #:gp-params-fset
	   #:gp-params-tset
	   #:gp-params-fitness
	   #:gp-params-cx-rate
	   #:gp-params-elitism
	   ))
  