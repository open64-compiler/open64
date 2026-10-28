"""Fail closed on inaccurate normalization report claims; native design v0.1."""
import tempfile
from pathlib import Path
import unittest
from normalization_trace_report import audit, write_reports, FORM_HEADER, FACT_HEADER


def fixture():
    """Create exact paired positional rows with shared input/result TY allowed."""
    lines = []
    for phase in ("preopt_enter", "preopt_exit"):
        lines.extend([FORM_HEADER, FACT_HEADER,
            "AI_NORM 1 FORM " + phase + " 1 5 3 17 1 population_centered 3727c5ac float32 0 0 0 source_unresolved verified 23",
            "AI_NORM_FACT 1 FACT " + phase + " 12801 1 1 2 3 5 13570 13826 13826 13570 3 17 3727c5ac " +
            "centered_population mean_f32,variance_f32 17 inside_rsqrt normalize_then_scale_then_bias " +
            "source_unresolved source_unresolved 0 0 0 unproved unproved unproved source_unresolved " +
            "X[r,k] gamma[k] beta[k] Y[r,k] read_only may_alias per_row unplanned 23"])
    return "\n".join(lines) + "\n"


class NormalizationTraceTests(unittest.TestCase):
    """Check schema, population/epsilon/broadcast meaning and strict permissions."""

    def test_complete_and_other_tags(self):
        self.assertEqual(len(audit("AI_SOFTMAX 1 unrelated\n" + fixture())), 1)

    def test_schema_and_missing_pairs(self):
        for text in (fixture().replace("preopt_exit", "preopt_enter"),
                     fixture().replace("preopt_exit", "bridge"),
                     fixture().replace("AI_NORM 1", "AI_NORM 2"),
                     fixture().replace("AI_NORM_FACT 1 FACT preopt_exit", "OTHER")):
            with self.assertRaises(ValueError): audit(text)

    def test_numerical_and_affine_claims(self):
        for old, new in (("unproved", "proved"), (" 0 0 0 ", " 0 0 1 "),
                         ("source_unresolved", "fast_rsqrt"), ("inside_rsqrt", "outside_rsqrt"),
                         ("gamma[k]", "gamma[r,k]"), ("may_alias", "noalias"),
                         ("population_centered", "sample"), ("unplanned", "registers")):
            with self.subTest(new=new), self.assertRaises(ValueError):
                audit(fixture().replace(old, new))

    def test_epsilon_bits(self):
        for bad in ("00000000", "80000000", "7f800000", "7fc00000", "3727C5AC", "1e-5"):
            with self.subTest(bad=bad), self.assertRaises(ValueError):
                audit(fixture().replace("3727c5ac", bad))
        self.assertEqual(len(audit(fixture().replace("3727c5ac", "00000001"))), 1)

    def test_denominator_identity_source_and_domain(self):
        for old, new in (("variance_f32 17", "variance_f32 16"),
                         (" 23", " 0"), (" 3 17 ", " 3 0 "),
                         ("FACT preopt_exit 12801 1 1 2 3 5", "FACT preopt_exit 12801 1 1 3 2 5")):
            with self.subTest(new=new), self.assertRaises(ValueError):
                audit(fixture().replace(old, new))

    def test_reports_and_no_overwrite(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            trace = root / "input.t"
            trace.write_text(fixture())
            output = root / "reports"
            write_reports(trace, output)
            for name in ("semantic.log", "optimization.log", "optimization-summary.log"):
                self.assertTrue((output / name).read_text().startswith("# What it summarizes:"))
            self.assertIn("transformation=none", (output / "optimization-summary.log").read_text())
            with self.assertRaises(FileExistsError): write_reports(trace, output)

    def test_suite_requires_real_variant_and_control_evidence(self):
        """Suite coverage must derive from every case, not hard-coded success."""
        from run_native_normalization_review import summarize_suite
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for variant in ("default", "unit", "wide", "epsilon2"):
                case = root / variant
                (case / "trace").mkdir(parents=True)
                (case / "logs").mkdir()
                text = fixture()
                if variant in ("unit", "wide"):
                    rows, width = (1, 1) if variant == "unit" else (7, 65)
                    text = text.replace("3 17", "%d %d" % (rows, width))
                    text = text.replace("variance_f32 17", "variance_f32 " + str(width))
                if variant == "epsilon2": text = text.replace("3727c5ac", "3a83126f")
                for mode in ("preopt-on", "default-trace"):
                    (case / "trace" / (mode + ".t")).write_text(text)
                for mode in ("trace-off", "preopt-off", "O0", "O1"):
                    (case / "trace" / (mode + ".t")).write_text("")
                    (case / "logs" / (mode + ".stderr")).write_text(
                        "OPR_DSLMEANVARNORM.v1 has no executable VHO lowering route")
                write_reports(case / "trace/preopt-on.t", case / "reports")
            (root / "wide/trace/trace-off.t").write_text(fixture())
            with self.assertRaises(RuntimeError): summarize_suite(root)
            self.assertFalse((root / "optimization-summary.log").exists())
            (root / "wide/trace/trace-off.t").write_text("")
            summarize_suite(root)
            self.assertIn("contracts_verified=4", (root / "optimization-summary.log").read_text())
            with self.assertRaises(FileExistsError): summarize_suite(root)


if __name__ == "__main__":
    unittest.main()
