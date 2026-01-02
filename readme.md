# Geometric Algebra

[![Clojars Project](https://img.shields.io/clojars/v/net.clojars.jordibc/geometric-algebra.svg)](https://clojars.org/net.clojars.jordibc/geometric-algebra)
[![bb compatible](https://raw.githubusercontent.com/babashka/babashka/master/logo/badge.svg)](https://book.babashka.org#badges)

A library to do [geometric
algebra](https://en.wikipedia.org/wiki/Geometric_algebra) in clojure.

A *multivector* is an element of the algebra. It is composed of a sum
of scalars, vectors, bivectors, etc., in a way similar to how complex
numbers are sums of real and imaginary components. Multivectors and
their geometric product are the simplest and most powerful tools for
mathematical analysis that I know of.


## Using it as a calculator

With [babashka](https://babashka.org/), we can launch a quick
geometric algebra calculator with:

```sh
bb calc <signature>  # for example, "sta" as signature for spacetime algebra
```

With [rlwrap](https://github.com/hanslub42/rlwrap) we also get more
comfortable editing of expressions:

```sh
rlwrap bb calc <signature>
```


## Installation

### Clojure CLI / deps.edn

To use this library, we need to add this dependency:

```clojure
net.clojars.jordibc/geometric-algebra {:mvn/version "0.9.8"}
```

Which means, we can add that to the `:deps` in our `deps.edn` file. If
we only had that dependency, this is how `deps.edn` would look like:

```clojure
{:deps
 {net.clojars.jordibc/geometric-algebra {:mvn/version "0.9.8"}}}
```

Then we can for example run `clj`, and from there do `(require
'geometric-algebra.core)` and so on (see examples).


#### From Codeberg

We recommend to install from the [Clojars
release](https://clojars.org/net.clojars.jordibc/geometric-algebra) as
explained above, but we can also simply clone this repository (or
download one of the [Codeberg
releases](https://codeberg.org/jordibc/geometric-algebra/releases))
and run clojure from it.

We could also install a specific revision directly from Codeberg with
something like:

```
org.codeberg.jordibc/geometric-algebra
{:git/url "https://codeberg.org/jordibc/geometric-algebra.git"
 :git/sha "5daa28bc688b059d10839dec5c6b5c3698413dbc"}
```

using in `:git/sha` the value of the desired revision.


## Examples

To create the basis vectors of a geometric algebra with the given signature:

```clojure
(require '[geometric-algebra.core :as ga])

(def signature [3 0])
(println (ga/basis signature))
```

The output should be:

```
(1 e1 e2 e3 e12 e13 e23 e123)
```

We can add, multiply, etc., those elements to create arbitrary
multivectors.

```clojure
(require '[geometric-algebra.core :as ga])

(let [[+ - * / · ∧ ∨] [ga/add ga/sub ga/prod ga/div ga/dot ga/wedge ga/antiwedge]
      [e e1 e2 e12] (ga/basis [2 0])
      v (+ 3 (* 4 e12))
      w (+ 5 e1 (* 3 e2))
      a (+ (* 2 e1) (* 3 e2))
      b (- (* 4 e1) (* 0.5 e2))]
  (println "v =" v)                       ; 3 + 4 e12
  (println "w =" w)                       ; 5 + e1 + 3 e2
  (println "3*v =" (* 3 v))               ; 9 + 12 e12
  (println "v + w =" (+ v w))             ; 8 + e1 + 3 e2 + 4 e12
  (println "v - (1 + w) =" (- v (+ 1 w))) ; -3 + -1 e1 + -3 e2 + 4 e12
  (println "v * w =" (* v w))             ; 15 + 15 e1 + 5 e2 + 20 e12
  (println "w * v =" (* w v))             ; 15 + -9 e1 + 13 e2 + 20 e12
  (println "v / (2 e2) =" (/ v (* 2 e2))) ; 2 e1 + 3/2 e2
  (println "v^2 =" (ga/pow v 2))          ; -7 + 24 e12
  (println "|v| =" (ga/norm v))           ; 5.0
  (println "a · b =" (· a b))             ; 6.5
  (println "a ∧ b =" (∧ a b))             ; -13.0 e12
  (println "a ∨ b =" (∨ a b)))            ; 13.0
```


## Signatures

A geometric algebra is characterized by its
[signature](https://en.wikipedia.org/wiki/Metric_signature).

A signature looks like `[p q]` or `[p q r]`, saying how many basis
vectors have a positive square (+1), negative (-1) and zero (0)
respectively.

When using the `basis` function to create the basis multivectors, we
can pass the signature as a vector. But we can also instead use a map
that says for each basis element its square. For example,
cosmologists sometimes use for spacetime:

```clojure
(def signature {0 -1, 1 +1, 2 +1, 3 +1}) ; t, x, y, z  with e0 = e_t
```

whereas particle physicists normally use:

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
(require '[geometric-algebra.core :as ga])

(ga/def-basis [1 3] 0) ; start with e0, the "space-time algebra"
;; Will print:
;; Defined basis multivectors: e0 e1 e2 e3 e01 e02 e03 e12 e13 e23 e012 e013 e023 e123 e0123

(ga/def-ops)
;; Will print some warnings for replacing +, -, *, /, and then:
;; Defined operators: + - * / · ∧ ∨ × ⌋ ⌊ ∘ •

;; Now we can easily create multivectors and operate with them.

(* (+ 3 (* 4 e12))
   (+ 5 e1 (* 3 e2))) ; => 15 + -9 e1 + 13 e2 + 20 e12

(∧ (+ (* 2 e1) (* 3 e2))
   (- (* 4 e1) (* 0.5 e2))) ; => -13.0 e12
```

Or, using the `infix` macro to use infix notation:

```clojure
(ga/infix (3 + 4 e12) (5 + e1 + 3 e2)) ; => 15 + -9 e1 + 13 e2 + 20 e12

(ga/infix (2 e1 + 3 e2) ∧ (4 e1 - 0.5 e2)) ; => -13.0 e12
```


## Tests

We can run some tests with:

```sh
clojure -T:build test
```

Or with babashka:

```sh
bb test
```


## Resources

* https://bivector.net/


## License

This library is dual licensed under the [GNU General Public License
version 3](doc/license-gpl-3.0.md), and the [Eclipse Public License
version 1](doc/license-epl-v1.0.md).

It means that you can choose to use it under either license, whatever
is more convenient for you. You can also choose any later version of
those licenses.
