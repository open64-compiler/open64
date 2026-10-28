"""R2 trace schema and false-legality rejection tests.

Design: doc/AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md.
"""
from pathlib import Path
import tempfile
import unittest

from reduction_binding_trace_report import audit, audit_preopt, write_reports, NUM_HEADER, ALT_HEADER
from reduction_binding_numerics import exact_add, oracle, witnesses, staged_ordered


def fixture(bound=True, budget=32):
    """Construct an illustrative positional trace, not compiler evidence."""
    version = "2" if bound else "1"
    attrs = "ascending_left_fold 00000000 rne_gradual_no_traps class_only explicit" if bound else (
        "source_unresolved semantic_zero source_unresolved source_unresolved missing")
    num = "AI_REDUCE_NUM 1 BIND r2_query 10 1 1 3 " + version + " 3 17 " + attrs + " 123"
    rows = []
    for index, name in enumerate(("direct_ordered", "staged_ordered", "partition_tree")):
        chunk, scratch = (8, 32) if index == 1 else (0, 0)
        if index == 2:
            facts = "forbidden not_evaluated unbound 0 reassociation_forbidden"
        elif not bound:
            facts = "unbound not_evaluated unbound 0 source_binding_missing"
        elif scratch > budget:
            facts = "legal_recipe budget_rejected unbound 0 scratch_budget_exceeded"
        else:
            facts = "legal_recipe budget_ok unbound 0 ordered_recipe_preserved"
        rows.append("AI_REDUCE_ALT 1 ALT r2_query 1 %s %d %d %d %s 123" %
                    (name, chunk, scratch, budget, facts))
    query = NUM_HEADER + "\n" + num + "\n" + ALT_HEADER + "\n" + "\n".join(rows) + "\n"
    preopt = NUM_HEADER + "\n" + num.replace("r2_query", "preopt_enter") + "\n"
    preopt += NUM_HEADER + "\n" + num.replace("r2_query", "preopt_exit") + "\n"
    return query, preopt


class ReductionBindingTraceTests(unittest.TestCase):
    """Do not allow a concise report to hide changed source meaning or gates."""

    def test_v1_and_v2_budget_boundaries(self):
        for bound, budget in ((False, 32), (True, 32), (True, 31), (True, 0)):
            query, preopt = fixture(bound, budget)
            binding, rows = audit(query)
            self.assertEqual(binding["version"], "2" if bound else "1")
            self.assertEqual(audit_preopt(preopt)["width"], "17")
            self.assertEqual(len(rows), 3)

    def test_reject_invented_permissions_target_and_selection(self):
        query, _ = fixture()
        for changed in (query.replace("unbound 0", "certified 1"),
                        query.replace("reassociation_forbidden", "profitable"),
                        query.replace("ascending_left_fold", "tree"),
                        query.replace("rne_gradual_no_traps", "ftz"),
                        query.replace("class_only", "ignore_nan"),
                        query.replace("00000000", "80000000")):
            with self.assertRaises(ValueError):
                audit(changed)

    def test_reject_v1_silent_upgrade(self):
        query, _ = fixture(False)
        for changed in (query.replace("source_binding_missing", "ordered_recipe_preserved"),
                        query.replace("missing", "explicit"), query.replace("semantic_zero", "00000000")):
            with self.assertRaises(ValueError):
                audit(changed)

    def test_reject_footprint_schema_missing_and_duplicate_rows(self):
        query, _ = fixture()
        for changed in (query.replace("8 32 32", "8 28 32"),
                        query.replace("8 32 32", "18 72 32"),
                        query.replace("AI_REDUCE_ALT 1 ALT", "AI_REDUCE_ALT 2 ALT"),
                        query + query.splitlines()[-1] + "\n",
                        "\n".join(query.splitlines()[:-1]) + "\n",
                        query.replace(ALT_HEADER, "")):
            with self.assertRaises(ValueError):
                audit(changed)

    def test_reject_preopt_drift_or_incomplete_pair(self):
        _, preopt = fixture()
        with self.assertRaises(ValueError):
            audit_preopt(preopt.replace("preopt_exit", "other"))
        with self.assertRaises(ValueError):
            audit_preopt(preopt.replace("3 17", "3 18", 1))
        with self.assertRaises(ValueError):
            audit_preopt("\n".join(preopt.splitlines()[:2]))

    def test_reports_have_convention_and_no_execution_claim(self):
        query, preopt = fixture()
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "query.t").write_text(query)
            (root / "preopt.t").write_text(preopt)
            output = root / "reports"
            write_reports(root / "query.t", root / "preopt.t", output)
            for path in output.glob("*.log"):
                self.assertTrue(path.read_text().startswith("# What it summarizes:"))
            self.assertIn("profitability=not_run transformation=none",
                          (output / "optimization-summary.log").read_text())
            with self.assertRaises(FileExistsError):
                write_reports(root / "query.t", root / "preopt.t", output)

    def test_exact_binary32_oracle_and_recipe_witnesses(self):
        self.assertEqual(exact_add(0x80000000, 0x80000000), 0x80000000)
        self.assertEqual(oracle([0x80000000]), 0)
        self.assertEqual(exact_add(0x007fffff, 1), 0x00800000)
        self.assertEqual(exact_add(0x3f800000, 0x33800000), 0x3f800000)
        with self.assertRaises(ValueError):
            staged_ordered([1], 0)
        report = witnesses()
        self.assertEqual(len(report["cases"]), 13)
        self.assertEqual(report["pair_checks"], 2048)


if __name__ == "__main__":
    unittest.main()
