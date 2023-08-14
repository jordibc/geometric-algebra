# Geometric Algebra

A library to do [geometric
algebra](https://en.wikipedia.org/wiki/Geometric_algebra) in clojure.


## Examples

If you run:

```clojure
(require '[geometric-algebra :as ga])

(def a (ga/multivector [[7 [3]]
                        [6 [1 4]]
                        [4 [2 3]]]))

(str (ga/+ a a))
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
