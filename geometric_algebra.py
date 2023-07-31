"""
Class to represent a multivector in geometric algebra, and related functions.
"""

import math


class MultiVector:
    """
    A multivector has terms (blades) that look like [[x, base_array], ...],
    where base_array looks like [] for a scalar, [i] for ei, [1, 2] for e12...
    """

    def __init__(self, blades, signature=None):
        assert signature is None or type(signature) == dict, 'Bad signature.'
        self.signature = signature
        self.blades = simplify_blades(blades)

    def __add__(self, v):  # multivector + whatever
        return add(self, v)

    def __radd__(self, v):  # number + multivector
        return add(v, self)

    def __neg__(self):  # - self
        blades = [blade.copy() for blade in self.blades]

        for blade in blades:
            blade[0] = -blade[0]

        return MultiVector(blades, self.signature)

    def __sub__(self, v):  # multivector - whatever
        return self + -v

    def __rsub__(self, v):  # number - multivector
        assert type(v) in [int, float]
        return v + -self

    def __mul__(self, v):  # multivector * whatever  (geometric product)
        assert type(v) in [int, float] or v.signature == self.signature

        v_blades = [[v, []]] if type(v) in [int, float] else v.blades

        prod = []
        for x, ei in self.blades:
            for y, ej in v_blades:
                elem, factor = simplify_element(ei + ej, self.signature)
                prod.append([factor * x * y, elem])

        return MultiVector(prod, self.signature)

    def __rmul__(self, v):  # number * multivector
        assert type(v) in [int, float]
        return self * v

    def __truediv__(self, v):  # multivector / whatever
        if type(v) in [int, float]:
            v_inv = MultiVector([[1/v, []]], self.signature)
            return self * v_inv

        assert v.signature == self.signature
        try:
            v_r = v.reverse()
            v_norm2 = float(v * v_r)  # well, kind of norm (can be < 0)
            v_inv = v_r / v_norm2
            return self * v_inv
        except ValueError:
            raise ValueError('Multivector has no inverse: %s' % v)

    def __rtruediv__(self, v):  # number / multivector
        try:
            r = self.reverse()
            norm2 = float(self * r)  # well, kind of norm (can be < 0)
            inv = r / norm2
            return v * inv
        except ValueError:
            raise ValueError('Multivector has no inverse: %s' % self)

    def __pow__(self, n):
        return pow(self, n)

    def __matmul__(self, v):  # a @ b  (dot product)
        return dot(self, v)

    def __xor__(self, v):  # a ^ b  (wedge product)
        return wedge(self, v)

    def reverse(self):
        return reverse(self)

    @property
    def T(self):
        return reverse(self)

    def __eq__(self, v):
        if type(v) in [int, float]:
            try:
                return float(self) == v
            except ValueError:  # if we couldn't convert to float...
                return False  # no way we are equal!

        return self.blades == v.blades and self.signature == v.signature

    def __float__(self):
        if not self.blades:
            return 0.0
        elif len(self.blades) == 1 and self.blades[0][1] == []:
            return float(self.blades[0][0])
        else:
            raise ValueError('Cannot convert to float: %s' % self)

    def __int__(self):
        if not self.blades:
            return 0
        elif len(self.blades) == 1 and self.blades[0][1] == []:
            return int(self.blades[0][0])
        else:
            raise ValueError('Cannot convert to int: %s' % self)

    def __getitem__(self, r):  # grade-projection operator <A>_r
        blades = [blade for blade in self.blades if len(blade[1]) == r]
        return MultiVector(blades, self.signature)

    def __str__(self):
        if not self.blades:
            return '0'

        def blade_str(blade):
            x, e = blade
            show_e = (e != [])  # show the basis element, except for scalars
            show_x = (x != 1 or not show_e)  # do not show the number if just 1
            return ((str(x) if show_x else '') +
                    ('*' if show_x and show_e else '') +
                    (('e' + ''.join(f'{ei}' for ei in e)) if show_e else ''))

        return ' + '.join(blade_str(blade) for blade in self.blades)

    def __repr__(self):
        return self.__str__()  # so it looks nice in the interactive sessions
        # A more raw representation would be:
        #   sig_str = '' if self.signature is None else f', {self.signature}'
        #   return 'MultiVector(%s%s)' % (self.blades, sig_str)


def simplify_blades(v):
    """Return the blades of a multivector simplified.

    Example: 3 + 5*e12 + 6*e12 + 0.2  ->  3.2 + 11*e12
    """
    # The changes to v are made in-place.
    i = 0
    while i < len(v):
        if v[i][0] == 0:  # remove any terms like  0 e_
            v.pop(i)

            if i > 0:
                i -= 1  # so we compare next time from the previous element
        elif i + 1 >= len(v):
            break  # nothing left to compare, we are done
        elif v[i][1] == v[i+1][1]:  # add together terms with the same  e_
            v[i][0] += v[i+1][0]
            v.pop(i+1)
        elif (len(v[i][1]), v[i][1]) > (len(v[i+1][1]), v[i+1][1]):  # sort
            v[i], v[i+1] = v[i+1], v[i]  # 3*e12 + 5*e1  ->  5*e1 + 3*e12

            if i > 0:
                i -= 1  # so we keep comparing this element
        else:
            i += 1

    return v


def simplify_element(e, signature=None):
    """Return the simplification of a basis element, and the factor it carries.

    Example: e13512  ->  e235, +1  (if  e1*e1 == +1)
    """
    # The changes to e are made in-place.
    factor = 1

    i = 0
    while i < len(e) - 1:
        if e[i] == e[i+1]:  # repeated element -> contract
            if signature:
                factor *= signature[e[i]]

            e.pop(i)
            e.pop(i)
        elif e[i] > e[i+1]:  # unsorted order -> swap
            factor *= -1  # perpendicular vectors anticommute

            e[i], e[i+1] = e[i+1], e[i]

            if i > 0:
                i -= 1  # so we keep comparing this element
        else:  # go to the next element
            i += 1

    return e, factor


def add(a, b):
    """Return a + b."""
    is_num_a, is_num_b = type(a) in [int, float], type(b) in [int, float]

    if is_num_a and is_num_b:
        return a + b
    elif is_num_a:
        return MultiVector([[a, []]] + [blade.copy() for blade in b.blades],
                           b.signature)
    elif is_num_b:
        return MultiVector([[b, []]] + [blade.copy() for blade in a.blades],
                           a.signature)
    else:
        assert a.signature == b.signature
        return MultiVector([blade.copy() for blade in a.blades] +
                           [blade.copy() for blade in b.blades],
                           a.signature)


def pow(a, n):
    """Return a**n."""
    if not a.blades:  # a == 0
        return 0
    elif len(a.blades) == 1 and a.blades[0][1] == []:  # a is a scalar
        return float(a.blades[0][0])**n

    assert type(n) == int, 'Can only raise multivector to an integer.'

    v = 1
    for i in range(abs(n)):
        v *= a

    if n >= 0:
        return v
    else:
        return 1/v


def reverse(a):
    """Return the reverse of multivector a. For example: e12 -> e21 = -e12."""
    blades = [blade.copy() for blade in a.blades]

    for blade in blades:
        x, e = blade
        if (len(e) // 2) % 2 == 1:
            blade[0] = -x

    return MultiVector(blades, a.signature)


def dot(a, b):
    """Return the dot product (inner product) of multivectors a and b."""
    grades_a = [len(e) for _, e in a.blades]
    grades_b = [len(e) for _, e in b.blades]

    assert grades_a and grades_b, 'Dot not defined (yet) for scalars.'

    ga, gb = grades_a[0], grades_b[0]
    assert all(g == ga for g in grades_a) and all(g == gb for g in grades_b), \
        'Can only dot blades (for the moment).'

    return (a * b)[abs(ga - gb)]


def wedge(a, b):
    """Return the wedge product (exterior/outer product) of a and b."""
    grades_a = [len(e) for _, e in a.blades]
    grades_b = [len(e) for _, e in b.blades]

    assert grades_a and grades_b, 'Wedge not defined (yet) for scalars.'

    ga, gb = grades_a[0], grades_b[0]
    assert all(g == ga for g in grades_a) and all(g == gb for g in grades_b), \
        'Can only wedge blades (for the moment).'

    return (a * b)[ga + gb]


def commutator(a, b):
    """Return the commutator product of multivectors a and b."""
    return (a * b - b * a) / 2


def exp(a):
    """Return the exponentiation of multivector a."""
    if type(a) in [int, float]:
        return math.exp(a)

    if all_blades_commute(a):
        product = 1
        for x, e in a.blades:
            elem = MultiVector([[1, e]], a.signature)  # basis element

            elem2 = int(elem**2)
            if elem2 == +1:
                product *= math.cosh(x) + math.sinh(x) * elem
            elif elem2 == -1:
                product *= math.cos(x) + math.sin(x) * elem
            elif elem2 == 0:
                product *= 1 + x * elem
            else:
                raise ValueError('Weird, we should never be here.')

        return product
    elif all_blades_anticommute(a):
        blades = [MultiVector([blade], a.signature) for blade in a.blades]

        norm2 = float(sum(b**2 for b in blades))  # kind of norm (can be < 0)
        if norm2 > 0:
            x = norm2**0.5
            return math.cosh(x) + (math.sinh(x) / x) * a
        elif norm2 < 0:
            x = (-norm2)**0.5
            return math.cos(x) + (math.sin(x) / x) * a
        else:
            return 1 + a
    else:
        # If we cannot exploit symmetries, we lastly resort to summing
        # the terms of its expansion in powers of a. There must be smarter
        # ways, like with matrix diagonalization, where we do:
        #   exp(A) = exp(U D U') = U exp(D) U'  and solve trivially.
        return sum_exp_series(a)


def sum_exp_series(a, precision=1e-8, max_terms=20):
    """Return exp(a) by summing the terms in its expansion in powers of a."""
    # exp(a) = 1 + a + a**2 / 2 + a**3 / 3! + ...

    term_last = 1  # last term in the series evaluated
    partial_sum = 1  # the sum of all the terms so far
    norm = 1  # size of the last term

    for i in range(1, max_terms):
        term = term_last * a / i  # next term

        partial_sum += term  # our best approximation of exp(a) so far

        norm = sum(abs(y) for y, _ in term.blades)  # kind of norm of last term

        if norm < precision:
            break  # yay, we are done!

        term_last = term  # last term will be the new term in the next iteration
    else:
        print(('Warning: max terms reached (%d), but error (~ %g) bigger '
               'than desired precision (%g).' % (max_terms, norm, precision)))

    return partial_sum


def all_blades_commute(a):
    """Return True if all blades of multivector a commute."""
    for i in range(len(a.blades) - 1):
        ei = MultiVector([a.blades[i]], a.signature)
        for j in range(i + 1, len(a.blades)):
            ej = MultiVector([a.blades[j]], a.signature)
            if ei * ej != ej * ei:
                return False

    return True


def all_blades_anticommute(a):
    """Return True if all blades of multivector a anticommute."""
    for i in range(len(a.blades) - 1):
        ei = MultiVector([a.blades[i]], a.signature)
        for j in range(i + 1, len(a.blades)):
            ej = MultiVector([a.blades[j]], a.signature)
            if ei * ej != -ej * ei:
                return False

    return True


def basis(signature, start=None):
    """Return basis elements of a geometric algebra with the given signature."""
    # A signature looks like (p, q) or (p, q, r), saying how many basis vectors
    # have a positive square (+1), negative (-1) and zero (0) respectively.
    #
    # A signature can also be a dict that says for each basis element its
    # square. For example, astrophysicists normally use for spacetime:
    #   signature = {0: -1, 1: +1, 2: +1, 3: +1}  # t, x, y, z  with e0 = e_t
    # whereas particle physicists normally use:
    #   signature = {0: +1, 1: -1, 2: -1, 3: -1}
    # which is the same as (1, 3), or (1, 3, 0), in the more common notation.

    if type(signature) == dict:
        assert start is None, 'Cannot use start when using a dict as signature.'
        start = min(signature)
        assert sorted(signature) == list(range(start, start+len(signature))), \
            'Basis vectors have to be successive numbers.'
    else:
        start = start if start is not None else 1
        n_pos, n_neg = signature[:2]
        n_null = signature[2] if len(signature) == 3 else 0
        signature = dict(zip(range(start, start + n_pos + n_neg + n_null),
                             [+1]*n_pos + [-1]*n_neg + [0]*n_null))

    n = len(signature)  # number of vectors

    elements = []

    e = []  # current element
    while e is not None:
        elements.append(e)
        e = next_element(e, n, start)

    return [MultiVector([[1, e]], signature) for e in elements]


def is_last(e, n, start=1):
    """Is e the last of the blades with that number of vectors?"""
    # An example of last blade for n=4, with 2 vectors: [2, 3]
    return e == list(range(start + n - len(e), start + n))


def next_element(e, n, start=1):
    """Return the multivector (in dim n) base element next to e."""
    if is_last(e, n, start):
        return list(range(start, start+len(e)+1)) if len(e) < n else None

    e_next = e.copy()  # new element (we will modify it in-place)

    # Find the last position that doesn't contain its maximum possible value.
    pos = next(len(e_next) - 1 - i for i in range(len(e_next))
               if e_next[-1 - i] != start + n - 1 - i)  # max possible value

    e_next[pos] += 1  # increment at that position
    for i in range(pos + 1, len(e_next)):
        e_next[i] = e_next[i-1] + 1  # and make the following ones follow up

    return e_next
