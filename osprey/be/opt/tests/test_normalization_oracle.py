"""LN1-d exact boundary and strict trace-admission tests, independent of reduction."""
from fractions import Fraction as F
from pathlib import Path
import unittest

import normalization_oracle as fp
import run_normalization_oracle_review as review


class ScalarOracleTests(unittest.TestCase):
    """Test identities which ordinary random end-to-end accuracy tests can miss."""

    def test_rounding_ties(self):
        """Cover even/odd normals, normal/subnormal crossing and zero underflow."""
        for lower in (0, 1, 2, 0x007fffff, 0x00800000, 0x3f800000, 0x3f800001):
            midpoint = (fp.finite(lower) + fp.finite(lower+1)) / 2
            wanted = lower if lower % 2 == 0 else lower+1
            self.assertEqual(fp.round_fraction(midpoint), wanted)
            self.assertEqual(fp.round_fraction(-midpoint), wanted | fp.SIGN)
            self.assertEqual(fp.round_sqrt_fraction(midpoint**2), wanted)

    def test_overflow(self):
        """The unbounded even significand wins the exact overflow midpoint tie."""
        midpoint = F((1 << 128) - (1 << 103))
        self.assertEqual(fp.round_fraction(midpoint-1), fp.MAX)
        self.assertEqual(fp.round_fraction(midpoint), fp.INF)
        self.assertEqual(fp.round_sqrt_fraction(midpoint**2-1), fp.MAX)
        self.assertEqual(fp.round_sqrt_fraction(midpoint**2), fp.INF)
        self.assertEqual(fp.add(fp.MAX, fp.MAX), fp.INF)

    def test_signed_zero(self):
        """RNE cancellation and sign propagation are observable, unlike NaN payload."""
        self.assertEqual(fp.add(fp.SIGN, fp.SIGN), fp.SIGN)
        self.assertEqual(fp.add(0, fp.SIGN), 0)
        self.assertEqual(fp.sub(fp.SIGN, 0), fp.SIGN)
        self.assertEqual(fp.mul(fp.SIGN, 0x3f800000), fp.SIGN)
        self.assertEqual(fp.div(1, 0xff7fffff), fp.SIGN)
        self.assertEqual(fp.sqrt(fp.SIGN), fp.SIGN)
        self.assertFalse(fp.equivalent(0, fp.SIGN))

    def test_exceptional_classes(self):
        """Do not substitute real-algebra shortcuts for IEEE operations."""
        for value in (fp.add(fp.INF, fp.INF | fp.SIGN), fp.mul(0, fp.INF),
                      fp.div(0, 0), fp.div(fp.INF, fp.INF), fp.sqrt(0xbf800000)):
            self.assertTrue(fp.is_nan(value))
        self.assertEqual(fp.div(0xbf800000, 0), fp.SIGN | fp.INF)
        self.assertTrue(fp.equivalent(0x7f800001, 0xffc12345))

    def test_t6_and_square_add(self):
        """Prove both critical disagreements with exact rational midpoint arithmetic."""
        h = fp.sqrt(0x40c00000)
        self.assertEqual(h, 0x401cc471)
        self.assertEqual(fp.div(0x3f800000, h), 0x3ed105eb)
        self.assertEqual(fp.round_sqrt_fraction(F(1, 6)), 0x3ed105ec)
        a, c = 0x3f800001, 0x33800000
        self.assertEqual(fp.add(fp.mul(a, a), c), 0x3f800002)
        self.assertEqual(fp.round_fraction(fp.finite(a)**2 + fp.finite(c)), 0x3f800003)
        self.assertEqual(fp.div(0x40a00000, 0x40400000), 0x3fd55555)
        self.assertEqual(fp.mul(0x40a00000, fp.div(0x3f800000, 0x40400000)), 0x3fd55556)

    def test_count_domain(self):
        """Check huge descriptor boundaries without allocating huge row buffers."""
        self.assertEqual(fp.count_bits(1), 0x3f800000)
        self.assertEqual(fp.count_bits(2**24), 0x4b800000)
        self.assertEqual(fp.finite(fp.count_bits(2**24-1)), 2**24-1)
        for bad in (0, -1, 2**24+1, True, 1.0):
            with self.assertRaises(ValueError):
                fp.count_bits(bad)

    def test_words(self):
        """Refuse coercion, oversized integers and exceptional finite decodes."""
        for bad in (-1, 2**32, True, 0.0):
            with self.assertRaises(ValueError):
                fp.word(bad)
        with self.assertRaises(ValueError):
            fp.finite(fp.INF)

    def test_row_guards(self):
        """A sample or option must not upgrade unresolved v1 or change epsilon."""
        for version in (1, 0, 3, True):
            with self.assertRaises(ValueError):
                fp.reference_row([0], [0], [0], 1, version)
        for epsilon in (0, fp.INF, fp.QNAN, fp.SIGN | 1):
            with self.assertRaises(ValueError):
                fp.reference_row([0], [0], [0], epsilon)
        with self.assertRaises(ValueError):
            fp.reference_row([0], [], [0], 1)

    def test_row_states(self):
        """First positive-zero addition, subnormal epsilon and exceptional affine matter."""
        row = [fp.SIGN]
        state = fp.reference_row(row, [0x3f800000], [0], 1)
        self.assertEqual(row, [fp.SIGN])
        self.assertEqual(state["mu"], [0])
        self.assertEqual(state["d"], [fp.SIGN])
        self.assertEqual(state["t"], [1])
        self.assertLess(state["q"][0], fp.INF)
        invalid = fp.reference_row([fp.INF], [0], [0x3f800000], 1)
        self.assertTrue(fp.is_nan(invalid["y"][0]))


class TraceAdmissionTests(unittest.TestCase):
    """A strict census must detect numerical or provenance loss, not merely count rows."""

    def setUp(self):
        """Use a tiny independent schema fixture for precise negative mutations."""
        self.lines = {"env": 10, "prim": 20, "num": 30}
        self.ops = {"p": [1, 0, 0, 0, 0]}
        self.states = {"r": {"y": [0]}}
        self.records = ["AI_NORM_ENV 1 HOST rne 0 0 masked class_only 4.2.2 10", *review.HEADERS,
                        "AI_NORM_PRIM 1 VALUE p 1 00000000 00000000 00000000 00000000 00000000 64 20"]
        self.records += [f"AI_NORM_NUM 1 VALUE r {engine} y 0 00000000 30" for engine in review.ENGINES]

    def check(self, records):
        """Audit against the expected specification, never a trace-derived census."""
        return review.audit("\n".join(records)+"\n", self.ops, self.states, {}, self.lines)

    def test_valid(self):
        """Accept only the complete recognized primitive and four-engine state set."""
        self.assertEqual(self.check(self.records)["state_comparisons"], 4)

    def test_missing_duplicate(self):
        """Both omission and duplication invalidate a supposedly passing result."""
        for records in (self.records[:-1], self.records + self.records[-1:]):
            with self.assertRaises(ValueError):
                self.check(records)

    def test_mutations(self):
        """Refuse bits, version, provenance, environment, inputs and extra fields."""
        for old, new in (("00000000 30", "80000000 30"), ("VALUE r C0", "VALUE r OTHER"),
                         ("NUM 1 VALUE", "NUM 2 VALUE"), (" 30", " 31"), ("rne 0 0", "rne 1 0"),
                         ("00000000 64 20", "00000001 64 20"), (" 64 20", " 48 20")):
            with self.subTest(old=old), self.assertRaises(ValueError):
                self.check([line.replace(old, new) for line in self.records])
        with self.assertRaises(ValueError):
            self.check(self.records[:-1]+[self.records[-1]+" extra"])

    def test_nan_payload_allowed(self):
        """Only NaN class, not sign or payload, is an observable oracle obligation."""
        self.states["r"]["y"] = [fp.QNAN]
        records = [line.replace("00000000 30", "ff800001 30") for line in self.records]
        self.check(records)

    def test_producer_lines(self):
        """Actual last-column source coordinates must be uniquely recoverable."""
        self.assertEqual(set(review.source_lines(Path(__file__).with_name("normalization_oracle_host.cxx"))), {"num", "prim", "env"})


if __name__ == "__main__":
    unittest.main()
