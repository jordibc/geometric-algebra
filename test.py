# Some tests to run with:
#   pytest test.py

from math import sqrt
import geometric_algebra as ga


def test_str():
    v = ga.MultiVector([[1, []], [5, [1, 2]], [-3, [1, 2]], [0, [2]], [0.5, []]])

    assert str(v) == '1.5 + 2*e12'

    assert repr(v) == str(v)
    # Alternatively, 'MultiVector([[1.5, []], [2, [1, 2]]], [-1, 1])'


def test_simplify_element():
    e = [1, 3, 5, 1, 2]

    # The simplification should go like this:
    #
    # e, factor = [1, 3, 5, 1, 2], +1
    #             [1, 3, 1, 5, 2], -1
    #             [1, 1, 3, 5, 2], +1
    #             [3, 5, 2], +1         # if e1*e1 = 1
    #             [3, 2, 5], -1
    #             [2, 3, 5], +1
    assert ga.simplify_element(e) == ([2, 3, 5], 1)

    # But what if we change the squares of the base vectors?
    signature = {1: -1, 2: +1, 3: +1, 4: +1, 5: +1}  # meaning  e1*e1 == -1
    e = [1, 3, 5, 1, 2]
    assert ga.simplify_element(e, signature) == ([2, 3, 5], -1)


def test_signature():
    # The squares of the base vectors.
    signature = {1: +1, 2: +1}  # meaning  e1*e1 == +1, e2*e2 == +1

    v = ga.MultiVector([[1.5, []], [2, [1,2]]], signature)

    assert str(v) == '1.5 + 2*e12'

    e, e1, e2, e12 = ga.basis(signature)
    assert v == 1.5 + 2*e12

    # But what if we change the signature?
    signature = {1: -1, 2: +1}
    e, e1, e2, e12 = ga.basis(signature)
    assert v != 1.5 + 2*e12
    # Note that the multivectors are not equal because the signatures
    # are different, even if the string representations are the same.


def test_equal():
    e, e1, e2, e12 = ga.basis([2, 0])

    assert 1 + e1 == 0 + 0.5 + 2*e1 - e1 + 0.5
    assert e1 != 1
    assert 1 == e


def test_add():
    e, e1, e2, e12 = ga.basis([2, 0])

    v = 3 + 4*e12
    assert v + v == 6 + 8*e12


def test_mul():
    e, e1, e2, e12 = ga.basis([2, 0])

    v = 3 + 4*e12
    assert v * v == -7 + 24*e12


def test_div():
    e, e1, e2, e12 = ga.basis([2, 0])

    assert 1/e12 == -e12

    v = 3 + 4*e12
    assert v / (2 * e1) == 1.5*e1 - 2*e2

    assert 2 / (1 + e12) == 1 - e12
    assert (4 + 8*e1 + 3*e2 + 1*e12) / 2 == 2 + 4*e1 + 1.5*e2 + 0.5*e12
    assert (3 + 4*e12) / (3 + 4*e12) == 1


def test_pow():
    e, e1, e2, e12 = ga.basis([2, 0])

    assert e**2 == 1
    assert e1**2 == 1
    assert e1**3 == e1
    assert e12**2 == -1
    assert e12**5 == e12
    assert e12**0 == 1
    assert (2*e12)**-3 == 0.125*e12


def test_norm():
    e, e1, e2, e12 = ga.basis([2, 0])

    v = 3 + 4*e12
    assert sqrt(v * v.T) == 5


def test_basis():
    e, e0, e1, e2, e01, e02, e12, e012 = ga.basis([2, 1], start=0)

    assert e.blades == [[1, []]]
    assert e0.blades == [[1, [0]]]
    assert e1.blades == [[1, [1]]]
    assert e2.blades == [[1, [2]]]
    assert e01.blades == [[1, [0, 1]]]
    assert e012.blades == [[1, [0, 1, 2]]]

    assert e0 * e0 == 1
    assert e1 * e1 == 1
    assert e2 * e2 == -1

    assert e01 * e01 == -1


def test_grade_projection():
    e, e1, e2, e12 = ga.basis([2, 0])

    assert e[0] == e
    assert e[1] == 0
    assert e1[0] == 0
    assert e1[1] == e1

    v = 3 + 4*e12
    assert v[0] == 3
    assert v[1] == 0
    assert v[2] == 4*e12
    assert v[3] == 0


def test_dot():
    e, e1, e2, e12 = ga.basis([2, 0])
    assert ga.dot(e1 + 3*e2, 2*e2 + 1*e1) == 7


def test_wedge():
    e, e1, e2, e12 = ga.basis([2, 0])
    assert ga.wedge(e1 + 3*e2, 2*e2 + 2*e1) == -4*e12


def test_commutator():
    e, e1, e2, e3, e12, e13, e23, e123 = ga.basis([3, 0])
    assert ga.commutator(3*e23, 2*e12) == -6.0*e13
