"""Real PyTorch/FX checks for MLA-INGESTION-PLAN.md; no native or GPU claims."""

import hashlib
import importlib.util
import json
import math
from pathlib import Path
import tempfile
import unittest

TORCH_AVAILABLE = importlib.util.find_spec("torch") is not None
if TORCH_AVAILABLE:
    import torch
    from mla_standalone import ATOL, RTOL, capture, check_case, probe_ingestion, write_artifacts
    from models.absorbed_mla import AbsorbedMLA, make_fixture, validate_inputs
    from models.mla_reference import expanded_reference


@unittest.skipUnless(TORCH_AVAILABLE, "requires optional PyTorch dependency")
class MLACaptureTests(unittest.TestCase):
    def assert_reference(self, model, inputs):
        validate_inputs(inputs)
        originals = tuple(value.clone() for value in inputs)
        expected = tuple(torch.tensor(value, dtype=torch.float64)
                         for value in expanded_reference(
                             *[value.tolist() for value in inputs], model.alpha))
        graph, _ = capture(model, inputs)
        with torch.no_grad():
            actual = model(*inputs)
            captured = graph(*inputs)
        for value, reference, fx_value in zip(actual, expected, captured):
            torch.testing.assert_close(value.double(), reference, rtol=RTOL, atol=ATOL)
            torch.testing.assert_close(fx_value, value, rtol=0, atol=0)
        for before, after in zip(originals, inputs):
            torch.testing.assert_close(before, after, rtol=0, atol=0)
        return actual

    def test_short_and_long_against_expanded_fp64(self):
        structures = []
        for context, length in (("short", 127), ("long", 8193)):
            result, _, _, _, _, structure = check_case(context)
            self.assertEqual(result["length"], length)
            self.assertEqual(result["correctness"], "PASS")
            structures.append(structure["operators"])
        # Both contexts execute the same program, with different input extents.
        self.assertEqual(structures[0], structures[1])

    def test_one_token_matches_value_projection(self):
        model, inputs = make_fixture(length=1)
        output, _ = self.assert_reference(model, inputs)
        expected = torch.einsum("br,hvr->bhv", inputs[2][:, 0], inputs[5])
        torch.testing.assert_close(output, expected, rtol=RTOL, atol=ATOL)

    def test_nonuniform_masks_and_all_masked_head(self):
        model, inputs = make_fixture(length=17)
        inputs[-1][0, 0, 1::2] = False
        inputs[-1][0, 1] = False
        output, lse = self.assert_reference(model, inputs)
        torch.testing.assert_close(output[0, 1], torch.zeros_like(output[0, 1]), rtol=0, atol=0)
        self.assertEqual(lse[0, 1].item(), -math.inf)

    def test_masked_cache_values_do_not_leak(self):
        model, inputs = make_fixture(length=19)
        inputs[-1][..., -3:] = False
        baseline = self.assert_reference(model, inputs)
        inputs[2][:, -3:] = 1000
        inputs[3][:, -3:] = -1000
        changed = self.assert_reference(model, inputs)
        for before, after in zip(baseline, changed):
            torch.testing.assert_close(before, after, rtol=0, atol=0)

    def test_original_scale_is_preserved_after_absorption(self):
        model, inputs = make_fixture(length=23)
        actual = self.assert_reference(model, inputs)
        wrong = AbsorbedMLA(1 / math.sqrt(inputs[2].shape[-1] + inputs[1].shape[-1]))
        with torch.no_grad():
            changed = wrong(*inputs)
        self.assertGreater((actual[1] - changed[1]).abs().max().item(), 1e-4)

    def test_extreme_finite_logits_are_stable(self):
        model, inputs = make_fixture(length=17)
        inputs[0].mul_(10000)
        inputs[1].mul_(10000)
        output, lse = self.assert_reference(model, inputs)
        self.assertTrue(torch.isfinite(output).all().item())
        self.assertTrue(torch.isfinite(lse).all().item())

    def test_batch_axis_has_no_cross_request_mixing(self):
        model, one = make_fixture(length=7)
        inputs = tuple(value.repeat(2, 1, 1) if index in (0, 1, 2, 3, 6) else value
                       for index, value in enumerate(one))
        inputs[2][1].add_(0.75)
        inputs[0][1].neg_()
        output, _ = self.assert_reference(model, inputs)
        self.assertFalse(torch.equal(output[0], output[1]))

    def test_determinism_fresh_storage_and_no_global_rng_changes(self):
        before = torch.random.get_rng_state().clone()
        _, first = make_fixture(length=9)
        _, second = make_fixture(length=9)
        for lhs, rhs in zip(first, second):
            torch.testing.assert_close(lhs, rhs, rtol=0, atol=0)
            self.assertNotEqual(lhs.data_ptr(), rhs.data_ptr())
        self.assertTrue(torch.equal(before, torch.random.get_rng_state()))

    def test_invalid_fixture_inputs_rejected_before_capture(self):
        for variant in ("length", "shape", "dtype", "nonfinite", "scale"):
            with self.subTest(variant=variant), self.assertRaises(ValueError):
                if variant == "length":
                    make_fixture(length=0)
                elif variant == "scale":
                    AbsorbedMLA(math.nan)
                else:
                    _, inputs = make_fixture(length=7)
                    inputs = list(inputs)
                    if variant == "shape":
                        inputs[3] = inputs[3][:, :-1].contiguous()
                    elif variant == "dtype":
                        inputs[0] = inputs[0].double()
                    else:
                        inputs[2][0, 0, 0] = math.inf
                    validate_inputs(inputs)

    def test_complete_tensor_expression_mapping_is_explicit(self):
        if importlib.util.find_spec("open64_dsc") is None:
            self.skipTest("repository mapper is not included in the portable source bundle")
        model, inputs = make_fixture(length=7)
        result = probe_ingestion(model, inputs)
        self.assertEqual(result["status"], "mock_mapping_only")
        for required in ("common.reshape", "common.transpose", "common.matmul",
                         "common.select", "common.reduce_sum", "common.log"):
            self.assertIn(required, result["operators"])

    def test_artifacts_replay_checksums_and_no_overwrite(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "mla"
            result = write_artifacts(root, cases=("short",), lengths={"short": 7})
            self.assertEqual(result["native_whirl"], "not_run")
            self.assertEqual(json.loads((root / "report.json").read_text()), result)
            tensors = torch.load(root / "short/inputs.pt", weights_only=True)
            model, _ = make_fixture(length=7)
            replay = self.assert_reference(model, tuple(tensors.values()))
            saved = torch.load(root / "short/actual.pt", weights_only=True)
            torch.testing.assert_close(replay[0], saved["output"], rtol=0, atol=0)
            for line in (root / "SHA256SUMS").read_text().splitlines():
                digest, relative = line.split("  ", 1)
                self.assertEqual(hashlib.sha256((root / relative).read_bytes()).hexdigest(), digest)
            with self.assertRaises(FileExistsError):
                write_artifacts(root, cases=("short",), lengths={"short": 7})


if __name__ == "__main__":
    unittest.main()
