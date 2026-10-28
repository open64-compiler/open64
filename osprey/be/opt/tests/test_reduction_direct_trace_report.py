"""Direct fallback binding audit and partial-order rejection tests.

Design: doc/AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md.
"""
from pathlib import Path
import random
import tempfile
import unittest

from reduction_direct_trace_report import COLUMNS, HEADER, audit, events_valid, expected, write_reports
from reduction_binding_trace_report import NUM_HEADER


def fixture():
    """Illustrative contract trace, not native execution evidence."""
    values = dict(node=1, input=1, result=3, rows=3, width=17, recipe=2,
        source_match=1, contiguous=1, arch=1, instruction=1, fp_options=0, contract=101, context=201,
        in_alloc=1, in_capacity=1024, in_offset=0, in_align=16, in_lease=1, in_space=1,
        out_alloc=2, out_capacity=1024, out_offset=0, out_align=16, out_lease=2, out_space=1,
        mapping=1, threads=4, e0=2, e1=4, e2=24, e3=32, e4=0, e5=0,
        in_bytes=204, out_bytes=12, stages=15, obligations=15,
        status="bound_runtime_obligations", selected=0, source_line=123)
    return values


def trace(values):
    """Serialize the published positional schema, including source line last."""
    return HEADER + "\nAI_REDUCE_DIRECT 1 BIND " + " ".join(str(values[key]) for key in COLUMNS) + "\n"


def refusal(values, **changes):
    """Calculate a reference refusal for a deliberately changed contract."""
    result = {**values, **changes}
    verdict = expected(result)
    result.update(zip(("status", "stages", "in_bytes", "out_bytes", "obligations"), verdict))
    return result


class ReductionDirectTraceTests(unittest.TestCase):
    """Reject false target/storage/event and execution claims."""

    def test_bound_contract_and_architectures(self):
        for arch in (1, 2, 3, 4):
            row = {**fixture(), "arch": arch}
            self.assertEqual(audit(trace(row))[0]["status"], "bound_runtime_obligations")

    def test_source_and_target_refusals(self):
        base = fixture()
        cases = [(dict(recipe=1), "source_binding_missing"),
                 (dict(source_match=0), "stale_source"), (dict(arch=0), "unknown_architecture"),
                 (dict(instruction=0), "unsupported_instruction")]
        cases += [(dict(fp_options=1 << bit), "unsafe_fp_options") for bit in range(6)]
        for changes, reason in cases:
            row = refusal(base, **changes)
            self.assertEqual(audit(trace(row))[0]["status"], reason)
            self.assertEqual(row["obligations"], 0)

    def test_range_lease_and_owner_refusals(self):
        cases = [dict(in_offset=(1 << 64) - 1), dict(in_align=2), dict(in_align=12),
                 dict(out_offset=2), dict(out_capacity=11), dict(in_alloc=0), dict(in_space=0),
                 dict(contract=0), dict(context=0), dict(out_lease=0), dict(in_lease=2),
                 dict(mapping=0), dict(threads=2), dict(contiguous=0),
                 dict(rows=1 << 63), dict(width=(1 << 64) - 1)]
        for changes in cases:
            row = refusal(fixture(), **changes)
            audit(trace(row))
            self.assertNotEqual(row["status"], "bound_runtime_obligations")

    def test_same_allocation_range_boundaries(self):
        base = fixture()
        adjacent = {**base, "out_alloc": 1, "out_offset": 204}
        self.assertEqual(audit(trace(adjacent))[0]["status"], "bound_runtime_obligations")
        for offset in (0, 4, 200):
            row = refusal(base, out_alloc=1, out_offset=offset)
            self.assertEqual(audit(trace(row))[0]["status"], "overlapping_ranges")
        inconsistent = refusal(base, out_alloc=1, out_offset=204, out_capacity=512)
        self.assertEqual(audit(trace(inconsistent))[0]["status"], "invalid_range")

    def test_event_missing_cycle_unknown_and_transitive_order(self):
        for key, value in (("e0", 0), ("e1", 0), ("e2", 8), ("e3", 0),
                           ("e2", 25), ("e5", 32), ("e4", 1 << 31)):
            row = refusal(fixture(), **{key: value})
            self.assertEqual(audit(trace(row))[0]["status"], "invalid_event_order")
        transitive = {**fixture(), "e2": 8, "e3": 48}
        self.assertEqual(audit(trace(transitive))[0]["status"], "bound_runtime_obligations")
        # Compare traversal against a separate fixed-size closure across graphs.
        rng = random.Random(20261027)
        for _ in range(2048):
            edges = [rng.randrange(64) for _ in range(6)]
            reach = edges[:]
            for pivot in range(6):
                for start in range(6):
                    if reach[start] & (1 << pivot):
                        reach[start] |= reach[pivot]
            oracle = not any(reach[i] & (1 << i) for i in range(6)) and all(
                reach[a] & (1 << b) for a, b in ((0, 1), (1, 2), (2, 3), (2, 4), (3, 5)))
            self.assertEqual(events_valid(edges), bool(oracle))

    def test_invented_proof_and_schema_rejected(self):
        base = fixture()
        for change in (dict(stages=7), dict(obligations=0), dict(selected=1), dict(in_bytes=200),
                       dict(fp_options=1), dict(e1=0), dict(in_offset=3),
                       dict(arch=0), dict(status="executed")):
            with self.assertRaises(ValueError):
                audit(trace({**base, **change}))
        for malformed in (trace(base).replace(HEADER, ""),
                          trace(base).replace("DIRECT 1 BIND", "DIRECT 2 BIND"),
                          trace(base).replace(" 123\n", "\n"), ""):
            with self.assertRaises(ValueError):
                audit(malformed)

    def test_reports_preserve_convention_and_no_execution_claim(self):
        row = fixture()
        num = ("AI_REDUCE_NUM 1 BIND %s 10 1 1 3 2 3 17 ascending_left_fold "
               "00000000 rne_gradual_no_traps class_only explicit 123\n")
        preopt = NUM_HEADER + "\n" + num % "preopt_enter" + NUM_HEADER + "\n" + num % "preopt_exit"
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "direct.t").write_text(trace(row))
            (root / "preopt.t").write_text(preopt)
            write_reports(root / "direct.t", root / "preopt.t", root / "reports")
            for log in (root / "reports").glob("*.log"):
                self.assertTrue(log.read_text().startswith("# What it summarizes:"))
            summary = (root / "reports/optimization-summary.log").read_text()
            self.assertIn("runtime_enforcement=not_implemented", summary)
            self.assertIn("profitability=not_run transformation=none", summary)
            with self.assertRaises(FileExistsError):
                write_reports(root / "direct.t", root / "preopt.t", root / "reports")

    def test_preopt_identity_mismatch_rejected(self):
        row = fixture()
        num = ("AI_REDUCE_NUM 1 BIND %s 10 1 1 3 2 3 18 ascending_left_fold "
               "00000000 rne_gradual_no_traps class_only explicit 123\n")
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "direct.t").write_text(trace(row))
            (root / "preopt.t").write_text(NUM_HEADER + "\n" + num % "preopt_enter" +
                                           NUM_HEADER + "\n" + num % "preopt_exit")
            with self.assertRaises(ValueError):
                write_reports(root / "direct.t", root / "preopt.t", root / "reports")


if __name__ == "__main__":
    unittest.main()
