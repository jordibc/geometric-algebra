# Some tests to run with:
#   pytest test.py

from math import sqrt

import geometric_algebra as ga


# For convenience.
e, e1, e2, e12 = [ga.MultiVector([[1, e]]) for e in [[], [1], [2], [1, 2]]]



def test_str():
    v = ga.MultiVector([[1, []], [5, [1, 2]], [-3, [1, 2]], [0, [2]], [0.5, []]])

    assert str(v) == '1.5 + 2*e12'


def test_simplify_element():
    a = [1, 3, 5, 1, 2]

    # Should go like this:
    #
    # a, factor = [1, 3, 5, 1, 2], +1
    #             [1, 3, 1, 5, 2], -1
    #             [1, 1, 3, 5, 2], +1
    #             [3, 5, 2], +1         # if e1*e1 = 1
    #             [3, 2, 5], -1
    #             [2, 3, 5], +1

    assert ga.simplify_element(a.copy()) == ([2, 3, 5], 1)

    # The squares of the base vectors.
    signature = [1, -1, 1, 1, 1, 1]  # meaning  e1*e1 == -1, e1*e1 == 1
    assert ga.simplify_element(a.copy(), signature) == ([2, 3, 5], -1)


def test_signature():
    # The squares of the base vectors.
    signature = [-1, 1]  # meaning  e0*e0 == -1, e1*e1 == 1

    v = ga.MultiVector([[1.5, []], [2, [1,2]]], signature)

    assert str(v) == '1.5 + 2*e12'
    assert repr(v) == str(v)
    # Alternatively, 'MultiVector([[1.5, []], [2, [1, 2]]], [-1, 1])'


def test_add():
    v = 3 + 4*e12
    assert v + v == 6 + 8*e12


def test_mul():
    v = 3 + 4*e12
    assert v * v == -7 + 24*e12


def test_norm():
    v = 3 + 4*e12
    assert sqrt(v * v.T) == 5


def test_basis():

    signature = [1, 1, 1]

    e, e0, e1, e2, e01, e02, e12, e012 = ga.basis(signature)

    assert e.blades == [[1, []]]
    assert e0.blades == [[1, [0]]]
    assert e1.blades == [[1, [1]]]
    assert e2.blades == [[1, [2]]]
    assert e01.blades == [[1, [0, 1]]]
    assert e012.blades == [[1, [0, 1, 2]]]
