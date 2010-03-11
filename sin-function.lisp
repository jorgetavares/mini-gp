;;;;
;;;; mini-gp example: tinygp symbolic regression example (see tinygp code for more info)
;;;;

(use-package :mini-gp)

;;;
;;; function and terminal sets
;;;

(defparameter *X* 0)

(defun var-x () 
  *X*)

(defun real-constants (min max)
  #'(lambda ()
      (+ min (random (float (1+ (- max min)))))))

(setf mini-gp:*generate-constant* (real-constants -5 5))

(defparameter *fset* (mini-gp:make-fset 'mini-gp:gp-plus 2
					'mini-gp:gp-minus 2 
					'mini-gp:gp-times 2
					'mini-gp:gp-divison 2
					))

(defparameter *tset* '(mini-gp:gp-constant var-x))


;;;
;;; fitness function ( y = f(x) = sin (x) )
;;;

(defparameter *data-points* (make-array '(63 2) :initial-contents *sin-data-points*))

(defun make-fitness-sin (fitness-cases data-points)
  #'(lambda (individual id generation)
      (declare (ignore id generation))
      (loop for i from 0 below fitness-cases
	 do (setf *X* (aref data-points i 0))
	 sum (expt (- (eval (mini-gp:individual-tree individual)) 
		      (aref data-points i 1)) 2))))


;;;
;;; run GP
;;;


(defparameter *sin-params* (mini-gp:make-gp-params :total-generations 100
						   :pop-size 1000
						   :initial-depth 2
						   :max-depth 6
						   :fset *fset*
						   :tset *tset*
						   :fitness (make-fitness-sin
							     63 *data-points*)
						   :elitism nil
						   ))

(defun gp-sin (&key (params *sin-params*) (runs 1) (type :generational))
  (mini-gp:launch-gp *fset* *tset* :params params :runs runs :type type))


(defparameter *sin-data-points* '((0 0)
				  (0.1 0.0998334166468282)
				  (0.2 0.198669330795061)
				  (0.3 0.29552020666134)
				  (0.4 0.389418342308651)
				  (0.5 0.479425538604203)
				  (0.6 0.564642473395035)
				  (0.7 0.644217687237691)
				  (0.8 0.717356090899523)
				  (0.9 0.783326909627483)
				  (1 0.841470984807897)
				  (1.1 0.891207360061435)
				  (1.2 0.932039085967226)
				  (1.3 0.963558185417193)
				  (1.4 0.98544972998846)
				  (1.5 0.997494986604054)
				  (1.6 0.999573603041505)
				  (1.7 0.991664810452469)
				  (1.8 0.973847630878195)
				  (1.9 0.946300087687414)
				  (2 0.909297426825682)
				  (2.1 0.863209366648874)
				  (2.2 0.80849640381959)
				  (2.3 0.74570521217672)
				  (2.4 0.675463180551151)
				  (2.5 0.598472144103957)
				  (2.6 0.515501371821464)
				  (2.7 0.42737988023383)
				  (2.8 0.334988150155905)
				  (2.9 0.239249329213982)
				  (3 0.141120008059867)
				  (3.1 0.0415806624332905)
				  (3.2 -0.0583741434275801)
				  (3.3 -0.157745694143249)
				  (3.4 -0.255541102026832)
				  (3.5 -0.35078322768962)
				  (3.6 -0.442520443294852)
				  (3.7 -0.529836140908493)
				  (3.8 -0.611857890942719)
				  (3.9 -0.687766159183974)
				  (4 -0.756802495307928)
				  (4.1 -0.818277111064411)
				  (4.2 -0.871575772413588)
				  (4.3 -0.916165936749455)
				  (4.4 -0.951602073889516)
				  (4.5 -0.977530117665097)
				  (4.6 -0.993691003633465)
				  (4.7 -0.999923257564101)
				  (4.8 -0.996164608835841)
				  (4.9 -0.982452612624332)
				  (5 -0.958924274663138)
				  (5.1 -0.925814682327732)
				  (5.2 -0.883454655720153)
				  (5.3 -0.832267442223901)
				  (5.4 -0.772764487555987)
				  (5.5 -0.705540325570392)
				  (5.6 -0.631266637872321)
				  (5.7 -0.550685542597638)
				  (5.8 -0.464602179413757)
				  (5.9 -0.373876664830236)
				  (6 -0.279415498198926)
				  (6.1 -0.182162504272095)
				  (6.2 -0.0830894028174964)))

(defun run-tree (tree)
  (do ((point 0.0 (+ point 0.1)))
      ((> point 6.2) 'done)
    (setf *X* point)
    (format t "~a ~%" (eval tree))))


;(#S(INDIVIDUAL
;    :TREE (GP-DIVISON (GP-MINUS 3.1250563 (VAR-X))
;                      (GP-PLUS
;                       (GP-DIVISON (GP-MINUS 2.9542685 (VAR-X)) (VAR-X))
;                       (GP-DIVISON
;                        (GP-TIMES 3.1954536 (GP-DIVISON (VAR-X) (VAR-X)))
;                        (GP-MINUS 3.1486473 (GP-MINUS (VAR-X) 3.1702633)))))
;    :FITNESS 0.020194128))