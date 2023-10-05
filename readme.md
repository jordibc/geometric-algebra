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

If you run:

```clojure
(require '[geometric-algebra :as ga])

(def a (ga/multivector [[7 [3]]
                        [6 [1 4]]
                        [4 [2 3]]]))

(println (ga/add a a))
```

It will show `14*e3 + 12*e14 + 8*e23`.


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
