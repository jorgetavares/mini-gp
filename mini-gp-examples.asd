(defsystem :mini-gp-examples
  :description "examples of using mini-gp."
  :version "0.2"  
  :author "Jorge Tavares"  
  :licence "Apache-2.0"
  :depends-on (mini-gp)
  :serial t
  :components ((:file "examples/regression")
	       (:file "examples/multi-regression")
	       (:file "examples/sin-function")))
