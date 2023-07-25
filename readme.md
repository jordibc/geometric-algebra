# Geometric Algebra

A library to do [geometric
algebra](https://en.wikipedia.org/wiki/Geometric_algebra) in python.


## Examples

To create the basis vectors of a geometric algebra with the given signature:

```py
import geometric_algebra as ga

signature = (3, 0)  # 3 dimensions, all vectors with square == +1
print(ga.basis(signature))
```

The output should be:

```
[1, e1, e2, e3, e12, e13, e23, e123]
```

You can add, multiply, etc., those elements to create arbitrary
multivectors.

```py
import geometric_algebra as ga

e, e1, e2, e12 = ga.basis((2, 0))

v = 3 + 4*e12
w = 5 + e1 + 3*e2

print('v =', v)                          # 3 + 4*e12
print('w =', w)                          # 5 + e1 + 3*e2
print('3*v =', 3*v)                      # 9 + 12*e12
print('v + w =', v + w)                  # 8 + e1 + 3*e2 + 4*e12
print('v - (1 + w) =', v - (1 + w))      # -3 + -1*e1 + -3*e2 + 4*e12
print('v * w =', v * w)                  # 15 + 15*e1 + 5*e2 + 20*e12
print('w * v =', w * v)                  # 15 + -9*e1 + 13*e2 + 20*e12
print('v / (2*e2) =', v / (2*e2))        # 2.0*e1 + 1.5*e2
print('v**2 =', v**2)                    # -7 + 24*e12
print('exp(v) =', ga.exp(v))             # -13.1... + -15.2...*e12

a = 2*e1 + 3*e2
b = 4*e1 - 0.5*e2
print('dot(a, b) =', ga.dot(a, b))       # 6.5
print('wedge(a, b) =', ga.wedge(a, b))   # -13*e12

from math import sqrt
print('norm(v) =', sqrt(v * v.T))        # 5
```


## Signatures

A geometric algebra is characterized by its
[signature](https://en.wikipedia.org/wiki/Metric_signature).

A signature looks like `(p, q)` or `(p, q, r)`, saying how many basis
vectors have a positive square (+1), negative (-1) and zero (0)
respectively.

When using the `basis()` function to create the basis multivectors,
you can pass the signature as a tuple. But you can also instead use a
dict that says for each basis element its square. For example,
astrophysicists normally use for spacetime:

```
signature = {0: -1, 1: +1, 2: +1, 3: +1}  # t, x, y, z  with e0 = e_t
```

whereas particle physicists normally use:

```
signature = {0: +1, 1: -1, 2: -1, 3: -1}
```

which is the same signature as `(1, 3)`, or `(1, 3, 0)`, in the more
common notation.


## Tests

You can run some tests with:

```sh
pytest test.py
```


## Resources

* https://bivector.net/
