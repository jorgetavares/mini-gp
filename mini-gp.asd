(defpackage #:mini-gp-system
  (:use #:common-lisp #:asdf))  
 
(in-package #:mini-gp-system)  
 
(defsystem :mini-gp
  :description "mini-gp: a minimalistic Genetic Programming library in CL."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"  
  :components ((:file "package")
               (:file "gp")))
