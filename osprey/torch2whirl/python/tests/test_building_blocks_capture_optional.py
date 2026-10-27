"""Six isolated source/capture contracts; see BUILDING-BLOCK-INGESTION-PLAN.md."""

import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

TORCH_AVAILABLE = importlib.util.find_spec("torch") is not None
if TORCH_AVAILABLE:
    import torch
    from building_block_capture import capture, census, write_artifacts
    from models.building_blocks import NAMES, make_fixture
    from models.building_block_reference import reference
    from open64_dsc import export_to_whirl
    from open64_dsc.options import WhirlExportOptions


@unittest.skipUnless(TORCH_AVAILABLE, "requires optional PyTorch dependency")
class BuildingBlockCaptureTests(unittest.TestCase):
    def assert_reference(self, name, inputs=None):
        """Compare eager and captured source execution with an independent oracle."""
        model, default_inputs = make_fixture(name)
        inputs = default_inputs if inputs is None else inputs
        originals = [value.clone() for value in inputs]
        graph, _ = capture(name)
        expected = torch.tensor(reference(name, [value.tolist() for value in inputs]),
                                dtype=torch.float64)
        with torch.no_grad():
            actual = model(*inputs)
            torch.testing.assert_close(actual.double(), expected, rtol=2e-5, atol=2e-6)
            torch.testing.assert_close(graph(*inputs), actual, rtol=0, atol=0)
            torch.testing.assert_close(model(*inputs), actual, rtol=0, atol=0)
        self.assertEqual(actual.dtype, torch.float32)
        self.assertTrue(torch.isfinite(actual).all().item())
        for original, value in zip(originals, inputs):
            torch.testing.assert_close(value, original, rtol=0, atol=0)
        return actual

    def test_all_six_reference_capture_and_no_mutation(self):
        for name in NAMES:
            with self.subTest(name=name):
                self.assert_reference(name)

    def test_fresh_deterministic_inputs_without_rng_changes(self):
        before = torch.random.get_rng_state().clone()
        for name in NAMES:
            _, first = make_fixture(name)
            _, second = make_fixture(name)
            for lhs, rhs in zip(first, second):
                torch.testing.assert_close(lhs, rhs, rtol=0, atol=0)
                self.assertNotEqual(lhs.data_ptr(), rhs.data_ptr())
        self.assertTrue(torch.equal(before, torch.random.get_rng_state()))

    def test_constant_rows_and_softmax_shift_invariance(self):
        constant = torch.full((3, 17), 7.0)
        output = self.assert_reference("softmax", (constant,))
        torch.testing.assert_close(output, torch.full_like(output, 1 / 17))
        _, inputs = make_fixture("softmax")
        base = self.assert_reference("softmax", inputs)
        shifted = self.assert_reference("softmax", (inputs[0] + 2048,))
        torch.testing.assert_close(base, shifted, rtol=0, atol=0)

    def test_layernorm_zero_variance_retains_beta(self):
        _, inputs = make_fixture("layernorm")
        constant = torch.full_like(inputs[0], 7)
        output = self.assert_reference("layernorm", (constant, *inputs[1:]))
        torch.testing.assert_close(output, inputs[2].expand_as(output), rtol=0, atol=0)

    def test_attention_mask_excludes_future_tokens(self):
        _, inputs = make_fixture("attention")
        output = self.assert_reference("attention", inputs)
        torch.testing.assert_close(output[:, :, 0, :], inputs[2][:, :, 0, :], rtol=0, atol=0)
        changed_value = inputs[2].clone()
        changed_value[:, :, -1, :] += 20
        changed = self.assert_reference("attention", (*inputs[:2], changed_value, inputs[3]))
        torch.testing.assert_close(output[:, :, :-1], changed[:, :, :-1], rtol=0, atol=0)

    def test_moe_runtime_routes_and_zero_weights(self):
        _, inputs = make_fixture("moe")
        initial = self.assert_reference("moe", inputs)
        # Expert 2 is empty in the base case; now every assignment uses it.
        ids = torch.full_like(inputs[1], 2)
        changed = self.assert_reference("moe", (inputs[0], ids, *inputs[2:]))
        self.assertFalse(torch.equal(initial, changed))
        zero = self.assert_reference("moe", (*inputs[:2], torch.zeros_like(inputs[2]), *inputs[3:]))
        torch.testing.assert_close(zero, torch.zeros_like(zero), rtol=0, atol=0)

    def test_real_fx_census_has_not_drifted(self):
        golden = Path(__file__).parent / "golden" / "building_blocks_fx.json"
        self.assertEqual(census(), json.loads(golden.read_text()))

    def test_current_ingestion_boundary_is_explicit(self):
        model, inputs = make_fixture("gemm")
        module = export_to_whirl(model, inputs, WhirlExportOptions(backend="mock"))
        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["common.matmul"])
        # Do not paper over these gaps with synthetic operators or mock .B files.
        blockers = {"reduction": "sum", "softmax": "amax", "layernorm": "mean",
                    "attention": "transpose", "moe": "getitem"}
        for name, blocker in blockers.items():
            with self.subTest(name=name):
                model, inputs = make_fixture(name)
                with self.assertRaisesRegex(NotImplementedError, "unsupported FX graph node:.*" + blocker):
                    export_to_whirl(model, inputs, WhirlExportOptions(backend="mock"))

    def test_artifact_round_trip_and_no_overwrite(self):
        import hashlib
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "capture"
            report = write_artifacts(root)
            self.assertEqual(set(report["cases"]), set(NAMES))
            self.assertEqual(report["native_whirl"], "not_run")
            for line in (root / "SHA256SUMS").read_text().splitlines():
                digest, relative = line.split("  ", 1)
                self.assertEqual(hashlib.sha256((root / relative).read_bytes()).hexdigest(), digest)
            with self.assertRaises(FileExistsError):
                write_artifacts(root)


if __name__ == "__main__":
    unittest.main()
