"""Independent exact binary32 LN-REF-1 oracle; no host FP or compiler calls.

Design: doc/AI-COMPILER-LAYERNORM-ARITHMETIC-ORACLE-v0.1.md.
Integer/Fraction arithmetic defines every rounding boundary. This is test
infrastructure, not a source matcher, reduction dependency or kernel emitter.
"""
from fractions import Fraction
from functools import lru_cache

SIGN, INF, QNAN, MAX = 0x80000000, 0x7f800000, 0x7fc00000, 0x7f7fffff


def word(value):
    """Reject coercion/truncation rather than interpreting non-binary32 input."""
    if type(value) is not int or not 0 <= value <= 0xffffffff:
        raise ValueError("an exact unsigned binary32 word is required")
    return value


def is_nan(value):
    """NaN class is observable; payload, sign and signaling state are not."""
    return (word(value) & 0x7fffffff) > INF


def equivalent(left, right):
    """Compare all non-NaN bits, including signed zero and infinity signs."""
    return word(left) == word(right) or is_nan(left) and is_nan(right)


@lru_cache(maxsize=8192)
def finite(value):
    """Decode finite binary32 as an exact rational, without host conversion."""
    word(value)
    exponent, fraction = (value >> 23) & 255, value & 0x7fffff
    if exponent == 255:
        raise ValueError("finite binary32 required")
    significand = fraction if exponent == 0 else fraction | 0x800000
    power = -149 if exponent == 0 else exponent - 150
    result = Fraction(significand << power) if power >= 0 else Fraction(significand, 1 << -power)
    return -result if value & SIGN else result


def _quotient_rne(numerator, denominator):
    """Round a nonnegative exact quotient, with explicit midpoint/parity test."""
    whole, remainder = divmod(numerator, denominator)
    return whole + (remainder * 2 > denominator or remainder * 2 == denominator and whole & 1)


def round_fraction(value, zero_sign=0):
    """Round one exact rational to binary32 RNE, including gradual underflow.

    Quantize directly at max(floor(log2(abs(x)))-23, -149), then pack the
    rounded significand. No intermediate floating-point rounding occurs.
    """
    value = Fraction(value)
    if zero_sign not in (0, SIGN):
        raise ValueError("zero sign must be a sign bit")
    if not value:
        return zero_sign
    sign = SIGN if value < 0 else 0
    numerator, denominator = abs(value.numerator), value.denominator
    exponent = numerator.bit_length() - denominator.bit_length()
    below_power = numerator < denominator << exponent if exponent >= 0 else numerator << -exponent < denominator
    if below_power:
        exponent -= 1
    if exponent > 127:
        return sign | INF
    quantum = max(exponent - 23, -149)
    significand = _quotient_rne(numerator, denominator << quantum) if quantum >= 0 else _quotient_rne(numerator << -quantum, denominator)
    if significand < 0x800000:
        return sign | significand
    if significand == 0x1000000:
        significand >>= 1
        quantum += 1
    biased = quantum + 150
    return sign | INF if biased >= 255 else sign | (biased << 23) | (significand & 0x7fffff)


def add(left, right):
    """Exact sum then one RNE rounding; cancellation is +0 except -0 + -0."""
    word(left); word(right)
    if is_nan(left) or is_nan(right):
        return QNAN
    if left & 0x7fffffff == INF:
        return QNAN if right == left ^ SIGN else left
    if right & 0x7fffffff == INF:
        return right
    return round_fraction(finite(left) + finite(right), SIGN if left == right == SIGN else 0)


def sub(left, right):
    """Negate the second operand sign exactly, then use the addition contract."""
    return add(word(left), word(right) ^ SIGN)


def mul(left, right):
    """Exact product then one RNE rounding, with IEEE zero/infinity classes."""
    word(left); word(right)
    sign = (left ^ right) & SIGN
    a, b = left & 0x7fffffff, right & 0x7fffffff
    if is_nan(left) or is_nan(right) or (a == INF and b == 0) or (b == INF and a == 0):
        return QNAN
    if a == INF or b == INF:
        return sign | INF
    return round_fraction(finite(left) * finite(right), sign)


def div(left, right):
    """Exact rational division then RNE; no rounded reciprocal multiplication."""
    word(left); word(right)
    sign = (left ^ right) & SIGN
    a, b = left & 0x7fffffff, right & 0x7fffffff
    if is_nan(left) or is_nan(right) or (a == b == 0) or (a == b == INF):
        return QNAN
    if a == INF or b == 0:
        return sign | INF
    if a == 0 or b == INF:
        return sign
    return round_fraction(finite(left) / finite(right))


def round_sqrt_fraction(value):
    """Bracket sqrt by ordered positive binary32 words, then compare midpoint^2.

    This exact midpoint method also tests rational tie cases not representable
    as a binary32 sqrt input. Neither host sqrt nor a rounded root is used.
    """
    value = Fraction(value)
    if value < 0:
        raise ValueError("nonnegative sqrt argument required")
    overflow_midpoint = Fraction((1 << 128) - (1 << 103))
    if value >= overflow_midpoint * overflow_midpoint:
        return INF
    if value >= finite(MAX) ** 2:
        return MAX
    lower, upper = 0, MAX
    while lower + 1 < upper:
        middle = (lower + upper) // 2
        if finite(middle) ** 2 <= value:
            lower = middle
        else:
            upper = middle
    if finite(lower) ** 2 == value:
        return lower
    midpoint = (finite(lower) + finite(upper)) / 2
    if value < midpoint * midpoint:
        return lower
    if value > midpoint * midpoint:
        return upper
    return lower if not lower & 1 else upper


def sqrt(value):
    """Correctly rounded sqrt; preserve -0, propagate NaN class, reject -finite."""
    word(value)
    if is_nan(value) or value & SIGN and value != SIGN:
        return QNAN
    if value in (0, SIGN, INF):
        return value
    return round_sqrt_fraction(finite(value))


def count_bits(count):
    """The reference's exact count domain is distinct from source v1."""
    if type(count) is not int or not 1 <= count <= 2**24:
        raise ValueError("LN-REF-1 count must be in [1,2^24]")
    return round_fraction(Fraction(count))


def reference_row(values, gamma, beta, epsilon, semantic_version=2):
    """Execute the specified scalar recurrence on bits and retain all states.

    No permission, target binding, source equivalence or storage/event proof is
    inferred. In particular, an unresolved v1 node cannot call this implicitly.
    """
    if type(semantic_version) is not int or semantic_version != 2:
        raise ValueError("explicit LN-REF-1 v2 required; v1 remains unresolved")
    n = len(values)
    count = count_bits(n)
    if len(gamma) != n or len(beta) != n or not 0 < word(epsilon) < INF:
        raise ValueError("exact affine extents and positive finite epsilon required")
    for value in (*values, *gamma, *beta):
        word(value)
    result = {"count": [count], "sum_step": [], "d": [], "square": [], "var_step": [],
              "u": [], "z": [], "y": []}
    state = 0
    for value in values:
        state = add(state, value)
        result["sum_step"].append(state)
    mu = div(state, count)
    result["mu"] = [mu]
    state = 0
    for value in values:
        centered = sub(value, mu)
        square = mul(centered, centered)
        state = add(state, square)
        result["d"].append(centered)
        result["square"].append(square)
        result["var_step"].append(state)
    variance = div(state, count)
    t = add(variance, epsilon)
    h = sqrt(t)
    q = div(0x3f800000, h)
    for name, value in (("v", variance), ("t", t), ("h", h), ("q", q)):
        result[name] = [value]
    for centered, scale, bias in zip(result["d"], gamma, beta):
        u = mul(centered, q)
        z = mul(u, scale)
        result["u"].append(u)
        result["z"].append(z)
        result["y"].append(add(z, bias))
    return result
