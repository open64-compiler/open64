"""Synthetic LN1-e join negatives; actual native evidence is produced separately.

Design: doc/AI-COMPILER-LAYERNORM-INTEGRATED-BINDING-v0.1.md.
"""
import copy
import json
from pathlib import Path
import tempfile
import unittest

from normalization_binding_report import (join_contracts, invocations, row_bindings,
                                           audit_binding, validate_variant, bound_requests,
                                           audit_case, product_hashes, sha256, write_reports)
from run_normalization_oracle_review import HEADERS, expected_states, source_lines, ENGINES
from test_normalization_ref_trace_report import trace as source_trace
from test_normalization_target_trace_report import fixture as target_trace
from test_normalization_trace_report import fixture as legacy_trace


def targets():
    """Generate baseline and two explicit negative queries for one source node."""
    return (target_trace() +
            target_trace(source_match="0", status="2", arch_id="0", stages="0", count_bits="00000000", reason="stale_source") +
            target_trace(fp_options="1", status="6", stages="1", count_bits="00000000", reason="unsafe_environment"))


def numbers(cases, producer):
    """Author synthetic exact-state rows only for exercising auditor rejection."""
    rows, _ = row_bindings(cases)
    lines = source_lines(producer)
    result = [f"AI_NORM_ENV 1 HOST rne 0 0 masked class_only 4.2.2 {lines['env']}", *HEADERS]
    for name, state in expected_states(rows).items():
        for engine in ENGINES:
            for stage, values in state.items():
                result += [f"AI_NORM_NUM 1 VALUE {name} {engine} {stage} {i} {bits:08x} {lines['num']}"
                           for i, bits in enumerate(values)]
    return "\n".join(result) + "\n"


class NormalizationBindingTests(unittest.TestCase):
    """Require a complete exact source/target/input/state/provenance join."""

    @classmethod
    def setUpClass(cls):
        """Keep a fixed synthetic source and independently recomputable state set."""
        cls.source, cls.target = source_trace(), targets()
        cls.producer = Path(__file__).with_name("normalization_oracle_host.cxx")
        cls.facts, _, _ = join_contracts(cls.source, cls.target, "sm_90")
        cls.cases = invocations(cls.facts)
        cls.numerical = numbers(cls.cases, cls.producer)

    def check(self, cases=None, numerical=None, source=None, target=None):
        """Apply the production-independent auditor to one synthetic joined case."""
        return audit_binding(self.source if source is None else source, self.target if target is None else target,
                             "sm_90", self.cases if cases is None else cases,
                             self.numerical if numerical is None else numerical, self.producer)

    def test_complete(self):
        """One native node binds four complete invocations and twelve full rows."""
        result = self.check()
        self.assertEqual((result["invocations"], result["rows"], result["target_queries"]), (4, 12, 3))
        self.assertEqual(result["state_comparisons"], 6000)

    def test_target_census(self):
        """Reject omitted, duplicated or reordered target gate witnesses."""
        for bad in (target_trace(), self.target+target_trace(), self.target.replace("12080", "12090"),
                    self.target.replace("sm_90", "sm_90a"), self.target.replace("unsafe_environment", "GPU_proven")):
            with self.subTest(bad=bad[:80]), self.assertRaises(ValueError):
                self.check(target=bad)

    def test_native_source_drift(self):
        """Changed domains/epsilon/operand/result/position cannot inherit old samples."""
        for changes in ({"width": "18", "count_bits": "41900000"}, {"epsilon_bits": "00000001"},
                        {"x": "8"}, {"beta": "2"}, {"result": "9"}, {"source_line": "212"}):
            with self.subTest(changes=changes), self.assertRaises(ValueError):
                self.check(source=source_trace(**changes))

    def test_input_envelope(self):
        """Reject missing row domains, bool/float shapes, words and foreign source fields."""
        first = next(iter(self.cases))
        for change in ("epsilon", "shape", "bool_shape", "word", "role", "extra", "missing"):
            cases = copy.deepcopy(self.cases)
            case = cases[first]
            if change == "epsilon": case["source"]["epsilon_bits"] = "3f800000"
            elif change == "shape": case["tensors"]["1"]["shape"] = [3, 18]
            elif change == "bool_shape": case["tensors"]["1"]["shape"] = [3.0, 17]
            elif change == "word": case["tensors"]["1"]["words"][0] ^= 1
            elif change == "role": case["source"]["gamma"] = "3"
            elif change == "extra": case["authorized"] = True
            else: cases.pop(first)
            with self.subTest(change=change), self.assertRaises(ValueError):
                self.check(cases=cases)

    def test_numeric_trace(self):
        """Reject missing/duplicate/wrong bits or forged numerical source coordinates."""
        lines = self.numerical.splitlines(True)
        for bad in ("".join(lines[:-1]), self.numerical+lines[-1],
                    self.numerical.replace("AI_NORM_NUM 1", "AI_NORM_NUM 2"),
                    self.numerical.replace(" y 0 ", " y 999 ")):
            with self.assertRaises(ValueError):
                self.check(numerical=bad)

    def test_v1_refused(self):
        """A coexisting v1 needs a real refusal and receives no numerical invocation."""
        v1 = target_trace(pu="12801", node="1", recipe="1", arch_id="0", status="1", stages="0",
                          count_bits="00000000", reason="source_unbound", source_line="23")
        result = self.check(source=self.source+legacy_trace(), target=self.target+v1)
        self.assertEqual(result["v1_refusals"], 1)
        with self.assertRaises(ValueError):
            self.check(source=self.source+legacy_trace())
        with self.assertRaises(ValueError):
            join_contracts(legacy_trace(), v1, "sm_90")

    def test_shared_affine(self):
        """One native operand identity must map to one payload for gamma and beta."""
        source = source_trace(beta="2")
        facts, _, _ = join_contracts(source, self.target, "sm_90")
        cases = invocations(facts)
        self.assertTrue(all(len(case["tensors"]) == 2 for case in cases.values()))
        self.check(source=source, cases=cases, numerical=numbers(cases, self.producer))
        for case in cases.values():
            case["tensors"]["3"] = {"shape": [17], "words": [0]*17}
        with self.assertRaises(ValueError):
            self.check(source=source, cases=cases)

    def test_variant_label_not_proof(self):
        """Changing a fixture name must not fabricate count/epsilon/alias coverage."""
        validate_variant(self.facts, [], "default")
        for variant in ("unit", "wide", "epsilon2", "subnormal", "shared-affine", "mixed", "unknown"):
            with self.assertRaises(ValueError):
                validate_variant(self.facts, [], variant)

    def test_row_origins(self):
        """Every row maps back to actual native IDs and its native source line."""
        rows, origins = row_bindings(self.cases)
        self.assertEqual(set(rows), set(origins))
        self.assertTrue(all(origin["source_line"] == "211" for origin in origins.values()))
        self.assertTrue(all(len(row["x"]) == 17 for row in rows.values()))
        self.assertTrue(all(row["epsilon"] == 0x3727c5ac for row in rows.values()))

    def test_json_replay_request_order(self):
        """Manifest key sorting must not change the exact retained input transcript."""
        reloaded = json.loads(json.dumps(self.cases, sort_keys=True))
        self.assertEqual(bound_requests(self.cases), bound_requests(reloaded))

    def file_fixture(self, root):
        """Synthetic bytes test product plumbing, never native mapped-reader validity."""
        case = root / "case"
        case.mkdir()
        for name, text in {"ascii/ln_ref.T": "synthetic", "ascii/preserved.T": "synthetic",
                           "trace/preopt-on.t": self.source, "trace/default-trace.t": self.source,
                           "trace/trace-off.t": "", "trace/preopt-off.t": "",
                           "target.t": self.target, "numerical/arithmetic.t": self.numerical,
                           "numerical/requests.txt": bound_requests(self.cases),
                           "source-reports/semantic.log": "# What it summarizes: synthetic source.\n",
                           "target-reports/target-binding.log": "# What it summarizes: synthetic target.\n"}.items():
            path = case / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text)
        for name in ("ln_ref.B", "ln_ref.B.preserved.B"):
            (case / name).write_bytes(b"\x7fELFsynthetic_not_native_evidence")
        host = root / "synthetic-host"
        host.write_text("not an executable")
        manifest = dict(schema="LN1-e-binding-v1", architecture="sm_90", invocations=self.cases,
                        products=product_hashes(case), host_sha256=sha256(host), host_source_sha256=sha256(self.producer))
        (case / "binding.json").write_text(json.dumps(manifest, sort_keys=True))
        return case, host

    def test_product_and_request_guard(self):
        """Catch retained-byte drift and transcript drift even with recomputed hashes."""
        with tempfile.TemporaryDirectory() as temp:
            case, host = self.file_fixture(Path(temp))
            self.assertEqual(audit_case(case, self.producer, host)["rows"], 12)
            request = case / "numerical/requests.txt"
            request.write_text(request.read_text().replace("ROW ", "BAD ", 1))
            with self.assertRaises(ValueError):
                audit_case(case, self.producer, host)
            manifest = json.loads((case / "binding.json").read_text())
            manifest["products"] = product_hashes(case)
            (case / "binding.json").write_text(json.dumps(manifest))
            with self.assertRaises(ValueError):
                audit_case(case, self.producer, host)

    def test_executable_identity_guard(self):
        """Numerical agreement does not excuse a changed checker executable."""
        with tempfile.TemporaryDirectory() as temp:
            case, host = self.file_fixture(Path(temp))
            host.write_text("different executable bytes")
            with self.assertRaises(ValueError):
                audit_case(case, self.producer, host)

    def test_report_conventions(self):
        """Keep summaries scoped, first-line descriptions and no-overwrite behavior."""
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            case, host = self.file_fixture(root)
            write_reports(case, root / "reports", self.producer, host)
            for path in (root / "reports").glob("*.log"):
                self.assertTrue(path.read_text().startswith("# What it summarizes:"))
            summary = (root / "reports/optimization-summary.log").read_text()
            self.assertIn("TRANSFORM none", summary)
            self.assertIn("GPU=not_run", summary)
            with self.assertRaises(FileExistsError):
                write_reports(case, root / "reports", self.producer, host)


if __name__ == "__main__":
    unittest.main()
