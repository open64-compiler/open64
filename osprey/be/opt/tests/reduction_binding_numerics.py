"""R2 ordered-reduction host witnesses, not native/GPU execution.

Design: doc/AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md.
An integer binary32 oracle is independent of the host-float candidate recipes.
"""
import argparse
import json
import math
from pathlib import Path
import random
import struct


def as_bits(value):
    """Round a host value to binary32, retaining infinity on overflow."""
    try:
        return struct.unpack(">I", struct.pack(">f", value))[0]
    except OverflowError:
        return 0xff800000 if value < 0 else 0x7f800000


def as_float(bits):
    """Interpret a binary32 word; only the host-witness path uses this."""
    return struct.unpack(">f", struct.pack(">I", bits))[0]


def is_nan(bits):
    """NaN payload/sign is outside the v2 observational contract."""
    return (bits & 0x7fffffff) > 0x7f800000


def equivalent(lhs, rhs):
    """Require exact finite/infinite/signed-zero bits, or both NaN classes."""
    return lhs == rhs or is_nan(lhs) and is_nan(rhs)


def exact_add(lhs, rhs):
    """Round the exact integer sum in units of 2**-149 to nearest-even FP32."""
    if is_nan(lhs) or is_nan(rhs):
        return 0x7fc00000
    if (lhs & 0x7fffffff) == 0x7f800000:
        return 0x7fc00000 if rhs == (lhs ^ 0x80000000) else lhs
    if (rhs & 0x7fffffff) == 0x7f800000:
        return rhs

    def integer(bits):
        """Decode finite binary32 without using floating-point arithmetic."""
        exponent, fraction = (bits >> 23) & 255, bits & 0x7fffff
        magnitude = fraction if exponent == 0 else (0x800000 | fraction) << (exponent - 1)
        return -magnitude if bits >> 31 else magnitude

    total = integer(lhs) + integer(rhs)
    if total == 0:
        return 0x80000000 if lhs == rhs == 0x80000000 else 0
    sign, magnitude = (0x80000000 if total < 0 else 0), abs(total)
    if magnitude < 0x800000:
        return sign | magnitude
    shift = max(0, magnitude.bit_length() - 24)
    significand = magnitude >> shift
    remainder = magnitude - (significand << shift)
    if shift and (remainder > (1 << (shift - 1)) or
                  remainder == (1 << (shift - 1)) and significand & 1):
        significand += 1
    if significand == 0x1000000:
        significand >>= 1
        shift += 1
    exponent = shift + 1
    if exponent >= 255:
        return sign | 0x7f800000
    return sign | (exponent << 23) | (significand - 0x800000)


def oracle(words):
    """Normative positive-zero, ascending-index, rounded-add recurrence."""
    state = 0
    for word in words:
        state = exact_add(state, word)
    return state


def host_add(lhs, rhs):
    """One FP32 addition witness using binary64 then explicit binary32 rounding."""
    return as_bits(as_float(lhs) + as_float(rhs))


def direct_ordered(words):
    """One row owner reads each source element and updates the same state."""
    state = 0
    for word in words:
        state = host_add(state, word)
    return state


def staged_ordered(words, chunk):
    """Bit-preserving copy, then ascending consumption; never reset the state."""
    if type(chunk) is not int or chunk <= 0:
        raise ValueError("positive integer chunk required")
    state = 0
    for start in range(0, len(words), chunk):
        slot = list(words[start:start + chunk])
        for word in slot:
            state = host_add(state, word)
    return state


def partitioned(words, chunk):
    """Deliberately illegal under v2; retained solely as a counterexample."""
    partials = [direct_ordered(words[start:start + chunk])
                for start in range(0, len(words), chunk)]
    return direct_ordered(partials)


def witnesses():
    """Adversarial ordered cases plus a deterministic independent pair check."""
    vectors = {
        "unit": [as_bits(7.0)], "negative-zero": [0x80000000],
        "signed-zeros": [0x80000000, 0, 0x80000000],
        "subnormal": [1, 1, 0x80000001],
        "normal-boundary": [0x007fffff, 1, 0x80000001],
        "ties-even": [as_bits(1.0), as_bits(2.0 ** -24), as_bits(2.0 ** -24)],
        "cancellation": [as_bits(1e20), as_bits(1.0), as_bits(-1e20), as_bits(1.0)],
        "overflow": [0x7f7fffff, 0x7f7fffff, 0xff7fffff],
        "positive-infinity": [as_bits(1.0), 0x7f800000],
        "opposite-infinity": [0x7f800000, 0xff800000],
        "nan": [0x7fc00001, as_bits(1.0)], "signaling-nan": [0x7f800001, as_bits(1.0)],
        "tail": [as_bits((index % 11) - 5.0) for index in range(65)],
    }
    rows = []
    for name, words in vectors.items():
        expected = oracle(words)
        outcomes = [("direct_ordered", 0, direct_ordered(words))]
        outcomes += [("staged_ordered", chunk, staged_ordered(words, chunk))
                     for chunk in (1, 2, 3, 7, 16, 64, 128)]
        assert all(equivalent(actual, expected) for _, _, actual in outcomes), name
        rows.append(dict(case=name, input_bits=["%08x" % word for word in words],
                         expected_bits="%08x" % expected,
                         results=[dict(candidate=kind, chunk=chunk, bits="%08x" % actual)
                                  for kind, chunk, actual in outcomes]))
    words = vectors["cancellation"]
    assert not equivalent(oracle(words), partitioned(words, 2))
    rng = random.Random(20261027)
    for _ in range(2048):
        lhs, rhs = rng.getrandbits(32), rng.getrandbits(32)
        assert equivalent(exact_add(lhs, rhs), host_add(lhs, rhs)), (lhs, rhs)
    return dict(schema=1, cases=rows, pair_checks=2048,
                illegal_partition_counterexample=dict(input_bits=["%08x" % word for word in words],
                    ordered="%08x" % oracle(words), partitioned="%08x" % partitioned(words, 2)),
                native_execution="not_run", gpu_execution="not_run", general_real_sum_accuracy="unproved")


def main():
    """Retain the complete oracle inputs/results in a fresh review directory."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    report = witnesses()
    args.output_dir.mkdir(parents=True, exist_ok=False)
    (args.output_dir / "numerical-witness.json").write_text(json.dumps(report, indent=2) + "\n")
    (args.output_dir / "numerical.log").write_text(
        "# What it summarizes: host ordered-reduction recipes against an independent integer binary32 oracle.\n"
        "cases=13 candidate_checks=104 pair_checks=2048 partitioned_counterexample=confirmed\n"
        "acceptance=exact_bits_except_nan_class signed_zero=checked subnormal=checked overflow=checked\n"
        "source_v1_permission=not_granted general_real_sum_accuracy=unproved native_execution=not_run gpu=not_run\n")
    print("ordered reduction witnesses passed: 13 cases, 104 recipes, 2048 pairs")


if __name__ == "__main__":
    main()
