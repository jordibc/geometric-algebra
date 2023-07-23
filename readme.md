# Geomtric Algebra

A library to do [geometric
algebra](https://en.wikipedia.org/wiki/Geometric_algebra) in python.


## Examples

To create the basis vectors of a geometric algebra with the given signature:

```py
import geometric_algebra as ga

signature = [1, 1]
print(ga.basis(signature))
```

The output should be:

```
[1, 1*e0, 1*e1, 1*e01]
```

You can add, multiply, etc., those elements to create arbitrary
multivectors.

You can also create your initial vectors with arbitrary indices. The
following example creates a basis that starts at `e1` (instead of
`e0`), and shows how to perform certain operations with multivectors:

```py
import geometric_algebra as ga

e, e1, e2, e12 = [ga.MultiVector([[1, e]]) for e in [[], [1], [2], [1, 2]]]

v = 3 + 4*e12
w = 5 + e1 + 3*e2

print('v =', v)
print('w =', w)
print('3*v =', 3*v)
print('v + w =', v + w)
print('v - (1 + w) =', v - (1 + w))
print('v * w =', v * w)
print('w * v =', w * v)
print('v / (2*e2) =', v / (2*e2))


from math import sqrt

print('norm(v) =', sqrt(v * v.T))
```

You should see the output:

```
v = 3 + 4*e12
w = 5 + 1*e1 + 3*e2
3*v = 9 + 12*e12
v + w = 8 + 1*e1 + 3*e2 + 4*e12
v - (1 + w) = -3 + -1*e1 + -3*e2 + 4*e12
v * w = 15 + 15*e1 + 5*e2 + 20*e12
w * v = 15 + -9*e1 + 13*e2 + 20*e12
v / (2*e2) = 2.0*e1 + 1.5*e2
norm(v) = 5.0
```


## Tests

You can run some tests with:

```sh
pytest test.py
```


## Resources

* https://bivector.net/
