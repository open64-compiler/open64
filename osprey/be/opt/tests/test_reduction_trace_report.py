"""Negative trace-audit tests for the bounded native R1 contract."""
import unittest
from reduction_trace_report import HEADER, FACT_HEADER, audit, audit_characterization, write_reports
import tempfile
from pathlib import Path

ENTER = "AI_REDUCE 1 FORM preopt_enter 1 2 3 17 1 0 float32 0 verified 144"
EXIT = ENTER.replace("preopt_enter", "preopt_exit")
FACT_ENTER = ("AI_REDUCE_FACT 1 FACT preopt_enter 1 1 1 2 256 512 3 17 "
              "sum_f32_per_row 0 fp32_add identity real_add prohibited source_unresolved "
              "0 not_granted unproved unproved unproved X[r,k] Y[r] read_only may_alias per_row unplanned 144")
FACT_EXIT = FACT_ENTER.replace("preopt_enter", "preopt_exit")


def complete(enter=FACT_ENTER, leave=FACT_EXIT):
    return "\n".join((HEADER, ENTER, FACT_HEADER, enter, HEADER, EXIT, FACT_HEADER, leave))


class ReductionTraceTest(unittest.TestCase):
    def test_native_characterization(self):
        facts = audit_characterization(complete(), True)
        self.assertEqual(len(facts), 1)
        self.assertEqual(facts[0]["width"], "17")

    def test_legacy_is_not_upgraded(self):
        legacy = "\n".join((HEADER, ENTER, EXIT))
        self.assertEqual(audit_characterization(legacy), [])
        with self.assertRaises(ValueError):
            audit_characterization(legacy, True)
        with tempfile.TemporaryDirectory() as tmp:
            trace = Path(tmp) / "legacy.t"
            trace.write_text(legacy)
            reports = Path(tmp) / "reports"
            write_reports(trace, reports)
            self.assertIn("unavailable_legacy_trace", (reports / "semantic.log").read_text())

    def test_characterization_drift_and_incomplete_rejected(self):
        mutations = [FACT_EXIT + " extra", FACT_EXIT.replace(" 17 ", " 18 "),
                     FACT_EXIT.replace("512", "256"), FACT_EXIT.replace(" 1 1 1 2 ", " 1 1 3 2 "),
                     FACT_EXIT.replace("prohibited", "allowed"), FACT_EXIT.replace("may_alias", "noalias"),
                     FACT_EXIT.replace("source_unresolved", "left_to_right"),
                     FACT_EXIT.replace("unproved", "proved", 1), FACT_EXIT.replace("144", "0"), ""]
        for leave in mutations:
            with self.subTest(leave=leave), self.assertRaises(ValueError):
                audit_characterization(complete(leave=leave), True)
        with self.assertRaises(ValueError):
            audit_characterization(complete() + "\n" + FACT_EXIT, True)

    def test_reports_distinguish_logical_and_physical_facts(self):
        with tempfile.TemporaryDirectory() as tmp:
            trace = Path(tmp) / "native.t"
            trace.write_text(complete())
            reports = Path(tmp) / "reports"
            write_reports(trace, reports, True)
            detail = (reports / "semantic.log").read_text()
            self.assertIn("logical_reads=51 physical_traffic=unknown", detail)
            self.assertIn("merge_permission=prohibited", detail)
            self.assertIn("accuracy=unproved", detail)
            for file in reports.iterdir():
                self.assertTrue(file.read_text().startswith("# What it summarizes:"))

    def test_complete(self):
        self.assertEqual(len(audit("\n".join((HEADER, ENTER, HEADER, EXIT)))), 1)

    def test_reject_incomplete_or_changed(self):
        for text in ("", HEADER, "\n".join((HEADER, ENTER)),
                     "\n".join((HEADER, EXIT)),
                     "\n".join((ENTER, EXIT)),
                     "\n".join((HEADER, ENTER, ENTER, EXIT)),
                     "\n".join((HEADER, ENTER, EXIT.replace("17", "18"))),
                     "\n".join((HEADER, ENTER, EXIT.replace("144", "0"))),
                     "\n".join((HEADER, ENTER, EXIT.replace("float32", "float16"))),
                     "\n".join((HEADER, ENTER, EXIT + " extra"))):
            with self.subTest(text=text), self.assertRaises(ValueError):
                audit(text)


if __name__ == "__main__":
    unittest.main()
