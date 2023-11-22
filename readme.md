# Geometric Algebra

A library to do [geometric
algebra](https://en.wikipedia.org/wiki/Geometric_algebra) in clojure.

A multivector is an element of the algebra. It is composed of a sum of
scalars, vectors, bivectors, etc., in a way similar to how complex
numbers are sums of real and imaginary components. Multivectors and
their geometric product are the simplest and most powerful tools for
mathematical analysis that I know of.


## Examples

To create the basis vectors of a geometric algebra with the given signature:

```clojure
(require '[geometric-algebra :as ga])

(def signature [3 0])
(println (ga/basis signature))
```

The output should be:

```
(1 e1 e2 e3 e12 e13 e23 e123)
```

You can add, multiply, etc., those elements to create arbitrary
multivectors.

```clojure
(require '[geometric-algebra :as ga])

(let [[+ - * / · ∧] [ga/add ga/sub ga/prod ga/div ga/dot ga/wedge]
      [e e1 e2 e12] (ga/basis [2 0])
      v (+ 3 (* 4 e12))
      w (+ 5 e1 (* 3 e2))
      a (+ (* 2 e1) (* 3 e2))
      b (- (* 4 e1) (* 0.5 e2))]
  (println "v =" v)                       ; 3 + 4*e12
  (println "w =" w)                       ; 5 + e1 + 3*e2
  (println "3*v =" (* 3 v))               ; 9 + 12*e12
  (println "v + w =" (+ v w))             ; 8 + e1 + 3*e2 + 4*e12
  (println "v - (1 + w) =" (- v (+ 1 w))) ; -3 + -1*e1 + -3*e2 + 4*e12
  (println "v * w =" (* v w))             ; 15 + 15*e1 + 5*e2 + 20*e12
  (println "w * v =" (* w v))             ; 15 + -9*e1 + 13*e2 + 20*e12
  (println "v / (2*e2) =" (/ v (* 2 e2))) ; 2*e1 + 3/2*e2
  (println "v^2 =" (ga/pow v 2))          ; -7 + 24*e12
  (println "|v| =" (ga/norm v))           ; 5.0
  (println "a · b =" (· a b))             ; 6.5
  (println "a ∧ b =" (∧ a b)))            ; -13.0*e12
```


## Signatures

A geometric algebra is characterized by its
[signature](https://en.wikipedia.org/wiki/Metric_signature).

A signature looks like `[p q]` or `[p q r]`, saying how many basis
vectors have a positive square (+1), negative (-1) and zero (0)
respectively.

When using the `basis` function to create the basis multivectors, you
can pass the signature as a vector. But you can also instead use a map
that says for each basis element its square. For example,
astrophysicists normally would use for spacetime:

```clojure
(def signature {0 -1, 1 +1, 2 +1, 3 +1}) ; t, x, y, z  with e0 = e_t
```

whereas particle physicists normally would use:

```clojure
(def signature {0 +1, 1 -1, 2 -1, 3 -1})
```

which is the same signature as `[1 3]`, or `[1 3 0]`, in the vector
notation.


## Working on the repl

When working on the repl, it is inconvenient to have to define a
symbol for each multivector basis (or keep binding them in a `let`).

There are a couple of handy macros that facilitate working with
multivectors from the repl: `def-basis` and `def-ops`. They work by
creating automatically all the symbols that we would expect.

```clojure
(require '[geometric-algebra :as ga])

(ga/def-basis [3 1])
;; Will print:
;; Defined basis multivectors: e1 e2 e3 e4 e12 e13 e14 e23 e24 e34 e123 e124 e134 e234 e1234

(ga/def-ops)
;; Will print some warnings for replacing +, -, *, /, and then:
;; Defined operators: + - * / · ∧

;; Now we can easily create multivectors and operate with them.

(* (+ 3 (* 4 e12))
   (+ 5 e1 (* 3 e2))) ; => 15 + 15*e1 + 5*e2 + 20*e12

(∧ (+ (* 2 e1) (* 3 e2))
   (- (* 4 e1) (* 0.5 e2))) ; => -13.0*e12
```


## Tests

You can run some tests with:

```sh
clj -M:test
```


## Resources

* https://bivector.net/


## See also

I developed a similar library in python, which shares the same
internal representation for a multivector:
[multivector](https://gitlab.com/jordibc/multivector).
