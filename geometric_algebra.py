"""
Class to represent a multivector in geometric algebra, and related functions.
"""


class MultiVector:
    """
    A multivector has terms (blades) that look like [[x, base_array], ...],
    where base_array looks like [] for a scalar, [i] for ei, [1, 2] for e12...
    """

    def __init__(self, blades, signature=None):
        self.signature = signature
        self.blades = simplify_blades(blades)

    def __add__(self, v):  # multivector + whatever
        assert type(v) in [int, float] or v.signature == self.signature

        v_blades = [[v, []]] if type(v) in [int, float] else v.blades

        return MultiVector([blade.copy() for blade in self.blades] +
                           [blade.copy() for blade in v_blades], self.signature)

    def __radd__(self, v):  # number + multivector
        assert type(v) in [int, float]
        return self + v

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
        for term_i in self.blades:
            for term_j in v_blades:
                elem, factor = simplify_element(term_i[1] + term_j[1],
                                                self.signature)
                prod.append([factor * term_i[0] * term_j[0], elem])

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
            v_norm2 = float(v * v_r)
            v_inv = v_r / v_norm2
            return self * v_inv
        except ValueError:
            raise ValueError('Multivector has no inverse: %s' % v)

    def __rtruediv__(self, v):  # number / multivector
        try:
            r = self.reverse()
            norm2 = float(self * r)
            inv = r / norm2
            return v * inv
        except ValueError:
            raise ValueError('Multivector has no inverse: %s' % self)

    def __pow__(self, n):
        assert type(n) == int, 'Can only raise to an integer'

        v = 1
        for i in range(abs(n)):
            v *= self

        if n >= 0:
            return v
        else:
            return 1/v

    def reverse(self):
        blades = [blade.copy() for blade in self.blades]

        for blade in blades:
            x, e = blade
            if (len(e) // 2) % 2 == 1:
                blade[0] = -x

        return MultiVector(blades, self.signature)

    @property
    def T(self):
        return self.reverse()

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

    def __getitem__(self, rank):  # <A>_r
        blades = [blade for blade in self.blades if len(blade[1]) == rank]
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


def dot(a, b):
    assert a.signature == b.signature
    v = a * b
    return MultiVector([v.blades[0]], a.signature)

def wedge(a, b):
    assert a.signature == b.signature
    v = a * b
    return MultiVector([v.blades[-1]], a.signature)


def basis(signature):
    """Return basis elements of a geometric algebra with the given signature."""
    n = len(signature)  # number of vectors

    elements = []

    e = []  # current element
    while e is not None:
        elements.append(e)
        e = next_element(e, n)

    return [MultiVector([[1, e]], signature) for e in elements]


def is_last(e, n):
    """Is e the last of the blades with that number of vectors?"""
    # An example of last blade for n=4, with 2 vectors: [2, 3]
    return e == list(range(n - len(e), n))


def next_element(e, n):
    """Return the multivector (in dim n) base element next to e."""
    if is_last(e, n):
        return list(range(len(e)+1)) if len(e) < n else None

    e_next = e.copy()  # new element (we will modify it in-place)

    pos = next(len(e_next)-1-i for i in range(len(e_next))
               if e_next[-1 - i] != n - 1 - i)  # maximum possible at that pos

    e_next[pos] += 1  # increment at that position
    for i in range(pos + 1, len(e_next)):
        e_next[i] = e_next[i-1] + 1  # and make the following ones follow up

    return e_next
