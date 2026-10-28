"""R1 graph recognition and numerical gates; BUILDING-BLOCK-INGESTION-PLAN.md."""

import copy
import importlib.util
import math
from pathlib import Path
import tempfile
import unittest

TORCH_AVAILABLE = importlib.util.find_spec("torch") is not None
if TORCH_AVAILABLE:
    import torch
    from open64_dsc.row_characterization import (
        CharacterizationError, RowNumericalPermissions, characterize_row_graph,
    )
    from models.building_blocks import make_fixture
    from models.building_block_reference import reference
    from row_semantic_report import (
        KINDS, acceptance, assess_case, build_report, capture_rows,
        execute_alternative, numerical_cases, write_report,
    )


@unittest.skipUnless(TORCH_AVAILABLE, "requires optional PyTorch dependency")
class RowSemanticCaptureTests(unittest.TestCase):
    def graph(self, kind):
        model, inputs = make_fixture(kind)
        return capture_rows(model, inputs)

    def test_reduction_then_softmax_then_layernorm_are_source_derived(self):
        for kind in KINDS:
            with self.subTest(kind=kind):
                graph = self.graph(kind)
                original = str(graph.graph)
                metadata = copy.deepcopy([node.meta for node in graph.graph.nodes])
                result = characterize_row_graph(graph)
                self.assertEqual(result["kind"], kind)
                self.assertEqual(result["domains"]["output_rows"], [0, 3])
                self.assertEqual(result["domains"]["reduction"], [0, 17])
                self.assertEqual(result["domains"]["axis"], 1)
                self.assertFalse(result["effects"]["input_writes"])
                self.assertEqual(result["transformation"], "none")
                self.assertEqual(result["preconditions"]["finite_inputs"], "not_proved")
                self.assertEqual(str(graph.graph), original)
                self.assertEqual([node.meta for node in graph.graph.nodes], metadata)
                self.assertTrue(all(not item["selected"] for item in result["alternatives"]))

    def test_dimensions_and_axis_are_not_fixture_name_lookups(self):
        class OtherName(torch.nn.Module):
            def forward(self, x):
                return torch.sum(x, dim=(1,), keepdim=False)
        graph = capture_rows(OtherName(), (torch.ones(2, 9),))
        result = characterize_row_graph(graph)
        self.assertEqual(result["domains"]["output_shape"], [2])
        self.assertEqual(result["domains"]["reduction"], [0, 9])

    def test_state_and_affine_broadcast_are_explicit(self):
        result = characterize_row_graph(self.graph("layernorm"))
        self.assertEqual(result["epsilon"], 1e-5)
        self.assertEqual(result["variance_denominator"], 17)
        self.assertEqual([a["relation"] for a in result["accesses"]],
                         ["X[r,k]", "X[r,j]", "gamma[j]", "beta[j]"])
        self.assertIn("empty-state guards", result["state"]["merge"])
        softmax = characterize_row_graph(self.graph("softmax"))
        self.assertIn("n=0", softmax["state"]["identity"])

    def test_fingerprint_invalidates_epsilon_but_not_diagnostic_names(self):
        graph = self.graph("layernorm")
        before = characterize_row_graph(graph)["graph_sha256"]
        for index, node in enumerate(graph.graph.nodes):
            node.name = "renamed_%d" % index
        self.assertEqual(characterize_row_graph(graph)["graph_sha256"], before)
        epsilon = next(node for node in graph.graph.nodes if node.op == "call_function"
                       and len(node.args) == 2 and type(node.args[1]) is float)
        epsilon.args = (epsilon.args[0], 1e-3)
        self.assertNotEqual(characterize_row_graph(graph)["graph_sha256"], before)

    def test_permission_and_numerical_acceptance_are_separate(self):
        for kind in KINDS:
            graph = self.graph(kind)
            strict = characterize_row_graph(graph)
            allowed = characterize_row_graph(graph, RowNumericalPermissions(True, True))
            self.assertTrue(any(a["status"] == "permission_rejected" for a in strict["alternatives"]))
            self.assertTrue(all(a["status"] == "requires_numerical_acceptance" for a in allowed["alternatives"]))
        one_permission = characterize_row_graph(self.graph("layernorm"), RowNumericalPermissions(True, False))
        self.assertEqual(one_permission["alternatives"][1]["missing_permissions"], ["state_reformulation"])
        with self.assertRaises(ValueError):
            RowNumericalPermissions(reassociate=1)

    def test_reject_wrong_axis_keepdim_dtype_and_missing_metadata(self):
        for change in ("axis", "keepdim", "dtype", "metadata"):
            with self.subTest(change=change):
                graph = self.graph("reduction")
                reduction = next(n for n in graph.graph.nodes if n.op == "call_function")
                if change == "axis":
                    reduction.kwargs = {"dim": 0}
                elif change == "keepdim":
                    reduction.kwargs = {"dim": -1, "keepdim": True}
                elif change == "dtype":
                    reduction.kwargs = {"dim": -1, "dtype": torch.float64}
                else:
                    list(graph.graph.nodes)[0].meta.clear()
                with self.assertRaises(CharacterizationError):
                    characterize_row_graph(graph)

    def test_reject_different_softmax_denominator_and_extra_operation(self):
        graph = self.graph("softmax")
        denominator = next(n for n in graph.graph.nodes if n.op == "call_function" and n.target is torch.sum)
        denominator.args = (list(graph.graph.nodes)[0],)
        with self.assertRaisesRegex(CharacterizationError, "same row dataflow"):
            characterize_row_graph(graph)
        graph = self.graph("reduction")
        with graph.graph.inserting_before(list(graph.graph.nodes)[-1]):
            graph.graph.call_function(torch.neg, (list(graph.graph.nodes)[0],))
        with self.assertRaisesRegex(CharacterizationError, "extra operations"):
            characterize_row_graph(graph)

    def test_reject_wrong_variance_and_nonpositive_epsilon(self):
        for change in ("square", "epsilon"):
            graph = self.graph("layernorm")
            if change == "square":
                product = next(n for n in graph.graph.nodes if n.name == "mul")
                product.args = (product.args[0], list(graph.graph.nodes)[0])
            else:
                add = next(n for n in graph.graph.nodes if len(n.args) == 2 and type(n.args[1]) is float)
                add.args = (add.args[0], 0.0)
            with self.assertRaises(CharacterizationError):
                characterize_row_graph(graph)

    def test_reject_tuple_output_and_non_row_shape(self):
        graph = self.graph("reduction")
        output = list(graph.graph.nodes)[-1]
        output.args = ((output.args[0], output.args[0]),)
        with self.assertRaises(CharacterizationError):
            characterize_row_graph(graph)
        model, _ = make_fixture("reduction")
        for value in (torch.ones(2, 3, 4), torch.empty(2, 0), torch.ones(2, 3, dtype=torch.float64)):
            with self.assertRaises(CharacterizationError):
                characterize_row_graph(capture_rows(model, (value,)))

    def test_reject_unused_softmax_input_and_inplace_operation(self):
        graph = self.graph("softmax")
        first = list(graph.graph.nodes)[0]
        with graph.graph.inserting_before(first):
            graph.graph.placeholder("unused")
        with self.assertRaises(CharacterizationError):
            characterize_row_graph(graph)
        graph = self.graph("reduction")
        with graph.graph.inserting_before(list(graph.graph.nodes)[-1]):
            graph.graph.call_method("add_", (list(graph.graph.nodes)[0], 1.0))
        with self.assertRaisesRegex(CharacterizationError, "extra operations"):
            characterize_row_graph(graph)

    def test_readonly_aliases_are_not_claimed_disjoint(self):
        model, inputs = make_fixture("layernorm")
        x = inputs[0]
        graph = capture_rows(model, (x, x[0], x[1]))
        report = characterize_row_graph(graph)
        self.assertEqual(report["effects"]["input_alias"], "may_alias; all accesses read-only")

    def test_shape_metadata_inconsistency_rejects(self):
        graph = self.graph("softmax")
        maximum = next(n for n in graph.graph.nodes if n.op == "call_function" and n.target is torch.amax)
        maximum.meta["tensor_meta"] = maximum.meta["tensor_meta"]._replace(shape=torch.Size([3, 2]))
        with self.assertRaisesRegex(CharacterizationError, "shape disagrees"):
            characterize_row_graph(graph)

    def test_acceptance_zero_reference_and_bad_thresholds(self):
        result = acceptance(torch.zeros(3), torch.zeros(3, dtype=torch.float64))
        self.assertTrue(result["accepted"])
        self.assertEqual(result["relative_l2_diagnostic"], 0)
        self.assertFalse(acceptance(torch.ones(2), torch.ones(3, dtype=torch.float64))["accepted"])
        for rtol, atol in ((-1, 1e-6), (1e-5, 0), (math.nan, 1e-6)):
            with self.assertRaises(ValueError):
                acceptance(torch.zeros(3), torch.zeros(3, dtype=torch.float64), rtol, atol)

    def test_fixture_alternatives_match_reference_only_when_permitted(self):
        for kind in KINDS:
            model, inputs = make_fixture(kind)
            record = assess_case(kind, model, inputs, RowNumericalPermissions(True, True))
            self.assertTrue(all(a["accepted"] for a in record["alternatives"]), record)
            strict = assess_case(kind, model, inputs, RowNumericalPermissions())
            self.assertTrue(any(a["reason"] == "permission_required" for a in strict["alternatives"]))

    def test_cancellation_and_overflow_are_not_hidden_by_permission(self):
        model, cases = numerical_cases("reduction")
        for name, inputs in cases:
            if name in ("cancellation", "overflow"):
                record = assess_case("reduction", model, inputs, RowNumericalPermissions(True, True))
                self.assertTrue(any(not a["accepted"] for a in record["alternatives"]))
        self.assertFalse(acceptance(torch.tensor([math.inf]), torch.tensor([1.], dtype=torch.float64))["accepted"])

    def test_constant_large_logits_and_invalid_domain(self):
        model, cases = numerical_cases("softmax")
        for name, inputs in cases:
            record = assess_case("softmax", model, inputs, RowNumericalPermissions(True, True))
            if name == "all_negative_infinity":
                self.assertEqual(record["value_domain"], "rejected_nonfinite_inputs")
                self.assertFalse(record["alternatives"])
            else:
                self.assertTrue(all(a["accepted"] for a in record["alternatives"]))
                with torch.no_grad():
                    value = execute_alternative("softmax", "online_max_sum_state", model, inputs)
                self.assertTrue((value >= 0).all().item())
                torch.testing.assert_close(value.sum(-1), torch.ones(value.shape[0]))

    def test_naive_unshifted_exp_is_a_negative_witness(self):
        _, inputs = make_fixture("softmax")
        naive = torch.exp(inputs[0])
        naive = naive / naive.sum(-1, keepdim=True)
        expected = torch.tensor(reference("softmax", [inputs[0].tolist()]), dtype=torch.float64)
        self.assertEqual(acceptance(naive, expected)["reason"], "nonfinite_result")

    def test_naive_variance_cancellation_is_a_negative_witness(self):
        model, cases = numerical_cases("layernorm")
        _, inputs = next(case for case in cases if case[0] == "large_offset_small_variance")
        x, gamma, beta = inputs
        mean = x.mean(-1, keepdim=True)
        variance = (x*x).mean(-1, keepdim=True) - mean*mean
        naive = (x-mean) * torch.rsqrt(variance.clamp_min(0)+1e-5) * gamma + beta
        expected = torch.tensor(reference("layernorm", [v.tolist() for v in inputs]), dtype=torch.float64)
        self.assertFalse(acceptance(naive, expected)["accepted"])
        torch.testing.assert_close(model(*inputs).double(), expected, rtol=2e-5, atol=2e-6)

    def test_report_records_failures_and_never_selects_or_transforms(self):
        report = build_report()
        self.assertEqual(len(report["cases"]), 12)
        self.assertEqual(report["transformation"], "none")
        for entry in report["cases"].values():
            for mode in ("strict", "permitted"):
                self.assertTrue(all(not a["selected"] for a in entry[mode]["characterization"]["alternatives"]))
                self.assertGreater(entry[mode]["source"]["function_line"], 0)
        source = report["cases"]["layernorm/squared_overflow"]["permitted"]["alternatives"][0]
        self.assertFalse(source["accepted"])

    def test_roundtrip_checksums_fresh_directory_and_reproducibility(self):
        import hashlib
        import json
        with tempfile.TemporaryDirectory() as tmp:
            first, second = Path(tmp) / "first", Path(tmp) / "second"
            write_report(first)
            write_report(second)
            self.assertEqual((first / "report.json").read_bytes(), (second / "report.json").read_bytes())
            for row in (first / "SHA256SUMS").read_text().splitlines():
                digest, path = row.split("  ", 1)
                self.assertEqual(hashlib.sha256((first / path).read_bytes()).hexdigest(), digest)
            self.assertTrue((first / "semantic-summary.log").read_text().startswith("# What it summarizes:"))
            json.loads((first / "softmax/all_negative_infinity/inputs.json").read_text())
            with self.assertRaises(FileExistsError):
                write_report(first)

    def test_cli_retained_source_replays_without_repository_package(self):
        import os
        import subprocess
        import sys
        with tempfile.TemporaryDirectory() as tmp:
            first, second = Path(tmp) / "first", Path(tmp) / "replay"
            write_report(first)
            env = os.environ.copy()
            env["PYTHONPATH"] = str(first / "source")
            subprocess.check_output([sys.executable, "row_semantic_report.py", "--artifact-dir", str(second)],
                                    cwd=first / "source", env=env, stderr=subprocess.STDOUT)
            self.assertEqual((first / "report.json").read_bytes(), (second / "report.json").read_bytes())
            self.assertEqual((first / "semantic-summary.log").read_bytes(), (second / "semantic-summary.log").read_bytes())


if __name__ == "__main__":
    unittest.main()
