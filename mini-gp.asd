(defsystem :mini-gp
  :description "mini-gp: a minimalistic Genetic Programming library."  
  :version "0.2"  
  :author "Jorge Tavares"  
  :licence "Apache-2.0"
  :serial t
  :components ((:file "src/packages")
	       (:file "src/sets")
	       (:file "src/evaluator")
	       (:file "src/operators")
	       (:file "src/engine")
	       (:file "src/mini-gp")
	       (:static-file "README.md")))
