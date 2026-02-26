# mini-gp

A minimalistic tree-based Genetic Programming library in Common Lisp.

mini-gp provides a simple GP system focused on evolving S-expression trees. There is no type enforcement — trees are built freely from user-defined function and terminal sets.

> **Note:** This project is no longer under active development.

## Features

- **Tree-based GP** with S-expression representation
- **Ramped half-and-half** tree initialization (full and grow methods)
- **Subtree crossover** and **subtree mutation**
- **Tournament selection**
- **Generational** and **steady-state** evolution models
- **Elitism** (optional)
- Built-in **safe arithmetic** operators (`gp-plus`, `gp-minus`, `gp-times`, `gp-division`, `gp-log`, `gp-square-root`, `gp-power`, etc.)
- Built-in **boolean/comparison** operators (`gp-if`, `gp-and`, `gp-or`, `gp-<`, `gp->`, etc.)
- **Constant generators** (integer, real, random)
- **Tree compilation** via `compile-tree` / `compile-tree-with-env` for fast fitness evaluation
- Output to screen, files, or both

## Installation

mini-gp uses [ASDF](https://asdf.common-lisp.dev/). Clone the repository into a location ASDF can find (e.g. `~/common-lisp/` or `~/quicklisp/local-projects/`), then:

```lisp
(asdf:load-system :mini-gp)
```

To load the examples as well:

```lisp
(asdf:load-system :mini-gp-examples)
```

## Quick Start

A GP run needs three things: a **function set**, a **terminal set**, and a **fitness function**.

```lisp
(use-package :mini-gp)

;; Define the function and terminal sets
(defparameter *fset* (make-fset 'gp-plus 2
                                'gp-minus 2
                                'gp-times 2
                                'gp-division 2))

(defparameter *tset* '(gp-constant var-x))

;; Define a fitness function (receives an individual, returns error)
(defun my-fitness (individual)
  (let* ((env (make-env '(var-x)))
         (fn (compile-tree-with-env (individual-tree individual) env)))
    (loop for x from 0 below 10
          sum (let ((xv (/ x 10.0)))
                (setf (env-var env 'var-x) xv)
                (expt (- (funcall fn) (* xv xv)) 2)))))

;; Launch GP
(launch-gp *fset* *tset*
            :fitness-function #'my-fitness
            :generations 50
            :pop-size 500
            :max-depth 5
            :generate-constant (lambda () (random 10))
            :output :screen)
```

## Parameters

`launch-gp` accepts the following keyword arguments:

| Parameter | Default | Description |
|---|---|---|
| `:generations` | 100 | Number of generations |
| `:pop-size` | 100 | Population size |
| `:initial-depth` | 2 | Initial tree depth |
| `:max-depth` | 5 | Maximum tree depth |
| `:fitness-function` | — | Fitness function (required) |
| `:cx-rate` | 0.9 | Crossover rate |
| `:mt-rate` | 0.05 | Mutation rate |
| `:node-rate` | 0.1 | Per-node mutation probability |
| `:t-size` | 3 | Tournament size |
| `:comparator` | `#'<` | Fitness comparator (`<` = minimise) |
| `:elitism` | `t` | Keep best individual across generations |
| `:type` | `:generational` | Evolution model (`:generational` or `:steady-state`) |
| `:generate-constant` | `nil` | Constant generator function |
| `:runs` | 1 | Number of independent runs |
| `:output` | `:screen` | Output mode (`:screen`, `:files`, `:screen+files`, `:none`) |
| `:seed` | `nil` | Random state seed |
| `:params` | `nil` | A `gp-params` struct (keyword args override its fields) |

## Examples

Three examples are included in the `examples/` directory:

- **regression** — Symbolic regression of $y = x^2 / 2$
- **multi-regression** — Multi-variable symbolic regression
- **sin-function** — Approximating the sine function

Run an example:

```lisp
(asdf:load-system :mini-gp-examples)
(mini-gp-regression:regression :runs 1 :output :screen)
```

## License

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for details.
