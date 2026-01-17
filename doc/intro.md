# Introduction

Geometric algebra is a very powerful mathematical tool that allows us
to work with spaces, lines, and many other geometrical constructs,
with the same simplicity as we normally work with vectors.

The main elements are multivectors, which can be vectors, products of
vectors, and sums of such elements. The main operation is the
geometric product, which combines multivectors into new multivectors.

With this library we can create multivectors and perform many
operations with them.

Example:

```clojure
(require '[geometric-algebra.core :as ga]
         '[geometric-algebra.infix :refer [infix]])

(ga/def-basis [1 3] :start 0) ; start with e0, the "spacetime algebra"
;; Will print:
;; Defined basis multivectors: e0 e1 e2 e3 e01 e02 e03 e12 e13 e23 e012 e013 e023 e123 e0123

(ga/def-ops)
;; Will print some warnings for replacing +, -, *, /, and then:
;; Defined operators: + - * / · ∧ ∨ × ⌋ ⌊ ∘ •

(infix (2 e1 + 3 e2) ∧ (4 e1 - 0.5 e2)) ; => -13.0 e12
```
