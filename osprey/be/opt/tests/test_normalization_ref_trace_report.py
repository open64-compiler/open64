"""Synthetic auditor tests, not native/compiler evidence.

Design: doc/AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md.
"""
from pathlib import Path
import tempfile
import unittest

from normalization_ref_trace_report import COLUMNS, HEADER, audit, write_reports


def trace(**changes):
    """Build one explicit schema fixture; native evidence is produced separately."""
    row = dict(zip(COLUMNS, ("preopt_enter 11 7 1 2 3 5 2 3 17 3727c5ac 41880000 "
               "ln_ref_1 ascending_left_fold 00000000 sum_then_div_rne exact_f32_from_static_n "
               "rne_gradual_no_traps class_only sqrt_rne_then_div_rne_f32 verified 211").split()))
    row.update(changes)
    entry = "AI_NORM_REF 1 BIND " + " ".join(row[name] for name in COLUMNS)
    row["phase"] = "preopt_exit"
    exit_row = "AI_NORM_REF 1 BIND " + " ".join(row[name] for name in COLUMNS)
    return HEADER + "\n" + entry + "\n" + HEADER + "\n" + exit_row + "\n"


class NormalizationRefTraceReportTest(unittest.TestCase):
    def test_complete_binding_and_shared_affine(self):
        self.assertEqual(len(audit(trace())), 1)
        self.assertEqual(audit(trace(beta="2"))[0]["gamma"], "2")
        from test_normalization_trace_report import fixture as legacy
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            raw = root / "mixed.t"
            raw.write_text(trace() + legacy())
            write_reports(raw, root / "reports")
            text = (root / "reports/semantic.log").read_text()
            self.assertIn("preserved_contract=common.mean_variance_norm.v1", text)
            self.assertIn("order=source_unresolved rsqrt=source_unresolved", text)

    def test_count_and_epsilon_boundaries(self):
        for width, bits in (("1", "3f800000"), ("16777216", "4b800000")):
            self.assertEqual(audit(trace(width=width, count_bits=bits, epsilon_bits="00000001"))[0]["width"], width)

    def test_each_numerical_field_is_authoritative(self):
        for field in ("version", "reference", "mean_order", "mean_identity_bits", "mean_finalize",
                      "count_conversion", "environment", "nan_policy", "rsqrt", "verified"):
            with self.subTest(field=field), self.assertRaises(ValueError):
                audit(trace(**{field: "invented"}))

    def test_missing_or_changed_header_and_schema(self):
        for text in (trace().replace(HEADER + "\n", ""), trace().replace("AI_NORM_REF 1", "AI_NORM_REF 2"),
                     trace().replace("HEADER phase", "HEADER omitted"),
                     trace().replace("verified 211", "verified target_proven 211")):
            with self.assertRaises(ValueError):
                audit(text)

    def test_wrong_count_or_unsupported_domain(self):
        for changes in (dict(count_bits="41800000"), dict(width="0"), dict(width="16777217"),
                        dict(rows="18446744073709551615"), dict(width="017")):
            with self.subTest(changes=changes), self.assertRaises(ValueError):
                audit(trace(**changes))

    def test_bad_epsilon_and_identity(self):
        for value in ("00000000", "80000000", "7f800000", "7fc00000", "3727C5AC", "1e-5"):
            with self.subTest(value=value), self.assertRaises(ValueError):
                audit(trace(epsilon_bits=value))
        for changes in (dict(result="1"), dict(x="2"), dict(pu="0"), dict(source_line="-1")):
            with self.assertRaises(ValueError):
                audit(trace(**changes))

    def test_missing_changed_or_duplicate_boundaries(self):
        rows = trace().splitlines()
        for text in ("\n".join(rows[:2]), "\n".join(rows[2:]),
                     trace() + trace(), trace().replace("BIND preopt_exit 11", "BIND preopt_exit 12"),
                     trace().replace("BIND preopt_enter", "BIND invented")):
            with self.assertRaises(ValueError):
                audit(text)

    def test_multiple_pus_do_not_alias_pending_records(self):
        self.assertEqual(len(audit(trace() + trace(pu="12"))), 2)

    def test_report_coverage_and_no_execution_claim(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            raw = root / "native.t"
            raw.write_text(trace())
            reports = root / "reports"
            write_reports(raw, reports)
            for path in reports.iterdir():
                self.assertTrue(path.read_text().startswith("# What it summarizes:"))
            summary = (reports / "optimization-summary.log").read_text()
            for phase in range(12):
                self.assertIn("P" + str(phase) + " status=", summary)
            self.assertIn("transformation=none", summary)
            self.assertIn("source_equivalence=not_claimed", summary)
            self.assertIn("native_gpu=not_run", summary)
            with self.assertRaises(FileExistsError):
                write_reports(raw, reports)


if __name__ == "__main__":
    unittest.main()
