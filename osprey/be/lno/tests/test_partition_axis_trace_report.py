"""Malformed trace and report regressions for native partition-axis discovery."""
import tempfile
from pathlib import Path
import unittest

from partition_axis_trace_report import COLUMNS, audit, write_reports

HEADER = "AI_PARTITION 1 HEADER " + " ".join(COLUMNS) + "\n"
FACT = "AI_PARTITION 1 FACT lno_discovery 12801 1 proved 1025 50 1025 49 0 1 3 17 ordered_sum 1 ordered 15\n"
SUMMARY = "AI_PARTITION 1 SUMMARY 12801 1 1 0\n"


class PartitionTraceTest(unittest.TestCase):
    """The report parser is a verifier, not a source of discovery evidence."""

    def test_valid_and_transposed(self):
        """Preserve explicit logical axes and their source position."""
        for axes in ("0 1", "1 0"):
            rows = audit(HEADER + FACT.replace("0 1 3 17", axes + " 3 17") + SUMMARY)
            self.assertEqual(len(rows), 1)
            self.assertEqual(rows[0]["source_line"], "15")
            self.assertEqual(rows[0]["instance_axis"], axes[0])

    def test_mutations_rejected(self):
        """Reject invented proofs, bad identity/units, missing summaries and churn."""
        original = HEADER + FACT + SUMMARY
        mutations = [original.replace("AI_PARTITION 1", "AI_PARTITION 2"),
            original.replace("0 1 3 17", "0 0 3 17"),
            original.replace("0 1 3 17", "0 1 0 17"),
            original.replace("0 1 3 17", "0 1 3 2147483647"),
            original.replace("ordered_sum 1", "unknown 1"),
            original.replace("proved 1025", "alias_unproved 1025"),
            original.replace("1025 50 1025 49", "1025 50 1025 50"),
            original.replace("1 ordered 15", "- ordered 15"),
            original.replace("1 ordered 15", "1 ordered nan"),
            original.replace("SUMMARY 12801 1 1 0", "SUMMARY 12801 2 2 0"),
            FACT + SUMMARY, HEADER + FACT, HEADER + FACT + FACT + SUMMARY,
            original + SUMMARY, original.replace("source_line", "line"), HEADER,
            original.replace("12801 1 proved", "12801 2 proved")]
        for changed in mutations:
            with self.subTest(changed=changed), self.assertRaises(ValueError):
                audit(changed)

    def test_refusal_and_empty_scope(self):
        """Unknown axes remain unknown; zero eligible regions are not success."""
        refusal = ("AI_PARTITION 1 FACT lno_discovery 12801 1 alias_unproved "
                   "- - - - - - - - ordered_sum - unknown 15\n")
        self.assertEqual(audit(HEADER + refusal + SUMMARY.replace("1 1 0", "1 0 1"))[0]["status"], "alias_unproved")
        self.assertEqual(audit(HEADER + "AI_PARTITION 1 SUMMARY 12801 0 0 0\n"), [])
        self.assertEqual(audit("unrelated phase\n"), [])

    def test_reports(self):
        """Keep the required first-line explanation and preserve raw trace."""
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            trace = root / "compile.t"
            trace.write_text(HEADER + FACT + SUMMARY)
            write_reports(trace, root / "reports")
            for name in ("semantic.log", "optimization.log", "optimization-summary.log"):
                self.assertTrue((root / "reports" / name).read_text().startswith("# What it summarizes:"))
            self.assertEqual(trace.read_text(), HEADER + FACT + SUMMARY)
            with self.assertRaises(FileExistsError):
                write_reports(trace, root / "reports")


if __name__ == "__main__":
    unittest.main()
