"""Malformed numerical/scope/trace evidence must not become optimization claims.

Design: doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md.
"""
import tempfile
from pathlib import Path
import unittest
from softmax_trace_report import audit, write_reports, FORM_HEADER, FACT_HEADER


def fixture():
    """Make paired positional records with the native canonical schema."""
    lines = []
    for phase in ("preopt_enter", "preopt_exit"):
        lines.extend([FORM_HEADER, FACT_HEADER,
            "AI_SOFTMAX 1 FORM " + phase + " 1 3 3 17 1 float32 0 0 source_unresolved verified 23",
            "AI_SOFTMAX_FACT 1 FACT " + phase + " 12801 1 1 3 13570 13826 3 17 " +
            "max_exp_sum_div max_f32,sum_f32 subtract_row_max source_unresolved exp_over_sum " +
            "source_unresolved 0 0 unproved unproved unproved source_unresolved " +
            "X[r,k] Y[r,k] read_only may_alias per_row unplanned 23"])
    return "\n".join(lines) + "\n"


class SoftmaxTraceTests(unittest.TestCase):
    """Check strict joins, numerical permissions and report conventions."""

    def test_valid_and_unknown_legacy_tags(self):
        """Unrelated existing trace tags do not alter Softmax facts."""
        self.assertEqual(len(audit("AI_REDUCE 1 unrelated\n" + fixture())), 1)

    def test_missing_duplicate_or_wrong_phase(self):
        """Incomplete or repeated boundary records fail rather than disappear."""
        text = fixture()
        for bad in (text.split("AI_SOFTMAX 1 HEADER", 2)[1],
                    text.replace("preopt_exit", "preopt_enter"),
                    text.replace("preopt_exit", "bridge"),
                    text.replace("AI_SOFTMAX_FACT 1 FACT preopt_exit", "OTHER")):
            with self.subTest(bad=bad), self.assertRaises(ValueError):
                audit(bad)

    def test_numerical_permissions_never_invented(self):
        """Relaxation, finite/accuracy proof and approximate exp are not v1 facts."""
        for old, new in (("unproved", "proved"), ("source_unresolved", "fast_exp"),
                         (" 0 0 ", " 1 1 "), ("may_alias", "noalias"),
                         ("unplanned", "smem"), ("subtract_row_max", "naive_exp")):
            with self.subTest(new=new), self.assertRaises(ValueError):
                audit(fixture().replace(old, new))

    def test_domain_source_identity_and_schema(self):
        """Bad sizes, source lines, versions and changed entry/exit IDs fail."""
        for old, new in ((" 3 17 ", " 3 0 "), (" 23", " 0"),
                         (" 12801 ", " 0 "), ("AI_SOFTMAX 1", "AI_SOFTMAX 2"),
                         ("FACT preopt_exit 12801 1 1 3", "FACT preopt_exit 12801 1 2 3")):
            with self.subTest(new=new), self.assertRaises(ValueError):
                audit(fixture().replace(old, new))

    def test_same_input_result_type_is_legal(self):
        """Same-shaped Softmax can intern one TY for input and result."""
        self.assertEqual(len(audit(fixture().replace("13826", "13570"))), 1)

    def test_reports_and_no_overwrite(self):
        """Logs describe their contents and do not claim a selected executable plan."""
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            trace = root / "input.t"
            trace.write_text(fixture())
            output = root / "reports"
            write_reports(trace, output)
            for name in ("semantic.log", "optimization.log", "optimization-summary.log"):
                self.assertTrue((output / name).read_text().startswith("# What it summarizes:"))
            self.assertIn("transformation=none", (output / "optimization-summary.log").read_text())
            with self.assertRaises(FileExistsError):
                write_reports(trace, output)


if __name__ == "__main__":
    unittest.main()
