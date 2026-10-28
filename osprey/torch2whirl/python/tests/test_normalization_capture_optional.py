"""Exact centered affine ingestion, not compiled numerical execution.

Design: doc/AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md.
"""
from dataclasses import replace
from pathlib import Path
import unittest
from unittest.mock import patch

try:
    import torch
except ImportError:
    torch = None

from open64_dsc import WhirlExportOptions, export_to_whirl
from open64_dsc.verifier import verify_module, WhirlVerificationError

ATTRS = {"attr.axis": "1", "attr.accum_dtype": "float32",
         "attr.variance_kind": "population_centered", "attr.epsilon_bits": "3727c5ac",
         "attr.reassociate": "false", "attr.state_reformulation": "false",
         "attr.contract": "false", "attr.rsqrt_semantics": "source_unresolved"}

if torch is not None:
    class Centered(torch.nn.Module):
        """Pure exact source plus deliberately unsupported structural variants."""
        def __init__(self, axis=-1, variance_axis=None, keepdim=True, epsilon=1e-5, variant=""):
            super().__init__()
            self.axis = axis
            self.variance_axis = axis if variance_axis is None else variance_axis
            self.keepdim, self.epsilon, self.variant = keepdim, epsilon, variant

        def forward(self, x, gamma, beta):
            if self.variant == "mutate":
                x.add_(1)
            if self.variant == "extra":
                torch.neg(x)
            mean = torch.mean(x, dim=self.axis, keepdim=self.keepdim)
            centered = x - mean
            square = centered * centered
            if self.variant == "different-center":
                square = centered * (x - mean)
            variance = torch.mean(square, dim=self.variance_axis, keepdim=True)
            if self.variant == "sample":
                variance = torch.var(x, dim=self.axis, keepdim=True, unbiased=True)
            if self.variant == "raw-moments":
                variance = torch.mean(x * x, dim=self.axis, keepdim=True) - mean * mean
            if self.variant == "epsilon-outside":
                inverse = torch.rsqrt(variance) + self.epsilon
            else:
                inverse = torch.rsqrt(variance + self.epsilon)
            normalized = centered * inverse
            if self.variant == "affine-order":
                return (normalized + beta) * gamma
            if self.variant == "reassociate":
                return centered * (inverse * gamma) + beta
            return normalized * gamma + beta

    class Opaque(torch.nn.Module):
        def forward(self, x, gamma, beta):
            return torch.nn.functional.layer_norm(x, (17,), gamma, beta, 1e-5)

    class UnusedInput(Centered):
        def forward(self, x, gamma, beta, unused):
            return super().forward(x, gamma, beta)

    class UnrelatedName(Centered):
        pass


@unittest.skipIf(torch is None, "torch unavailable")
class NormalizationIngestionTests(unittest.TestCase):
    """Test structure, descriptor, epsilon and numerical-permission boundaries."""

    def inputs(self, rows=3, width=17):
        """Use deterministic values; values never authorize a native plan."""
        return (torch.arange(rows * width, dtype=torch.float32).reshape(rows, width),
                torch.ones(width), torch.zeros(width))

    def export(self, model=None, inputs=None):
        """Use mock handles only for source checks; native review emits real .B."""
        return export_to_whirl(Centered() if model is None else model,
            self.inputs() if inputs is None else inputs, WhirlExportOptions(backend="mock"))

    def test_exact_axes_formals_descriptors_and_order(self):
        for axis in (-1, 1, (-1,), [1]):
            values = self.inputs()
            snapshots = [(value.clone(), value._version) for value in values]
            module = self.export(Centered(axis), values)
            self.assertEqual(module.operators, ["common.mean_variance_norm"])
            self.assertEqual(module.input_count, 3)
            self.assertEqual(module.graph_operators[0].kids, ["input0", "input1", "input2"])
            self.assertEqual(module.graph_operators[0].attrs, ATTRS)
            self.assertEqual([value.value_kind for value in module.values],
                             ["formal", "formal", "formal", "operator_result"])
            self.assertEqual([value.logical_shape for value in module.tensor_types],
                             ["[3,17]", "[17]", "[17]", "[3,17]"])
            for value, (before, version) in zip(values, snapshots):
                self.assertTrue(torch.equal(value, before))
                self.assertEqual(value._version, version)

    def test_source_location_is_final_addition(self):
        metadata = self.export(UnrelatedName()).graph_operators[0].metadata
        line = int(metadata["source_line"])
        self.assertIn("return normalized * gamma + beta",
                      Path(metadata["source_file"]).read_text().splitlines()[line - 1])

    def test_epsilon_is_binary32_and_not_decimal(self):
        for epsilon, bits in ((1e-5, "3727c5ac"), (1e-3, "3a83126f"),
                              (1, "3f800000"), (2.0 ** -149, "00000001")):
            with self.subTest(epsilon=epsilon):
                attrs = self.export(Centered(epsilon=epsilon)).graph_operators[0].attrs
                self.assertEqual(attrs, {**ATTRS, "attr.epsilon_bits": bits})

    def test_bad_epsilon_rejected_before_shape_evaluation(self):
        for epsilon in (0.0, -1e-5, True, float("nan"), float("inf"),
                        1e-100, 1e100, 10 ** 400):
            with self.subTest(epsilon=epsilon):
                with patch("torch.fx.passes.shape_prop.ShapeProp.propagate") as propagate:
                    with self.assertRaises(NotImplementedError):
                        self.export(Centered(epsilon=epsilon))
                    propagate.assert_not_called()

    def test_altered_recipes_rejected_before_shape_evaluation(self):
        models = [Centered(0), Centered(variance_axis=0), Centered(keepdim=False)]
        models += [Centered(variant=name) for name in ("mutate", "extra", "sample",
            "raw-moments", "different-center", "epsilon-outside", "affine-order", "reassociate")]
        for model in models:
            values = self.inputs()
            snapshots = [(value.clone(), value._version) for value in values]
            with self.subTest(variant=model.variant, axis=model.axis):
                with patch("torch.fx.passes.shape_prop.ShapeProp.propagate") as propagate:
                    with self.assertRaises(NotImplementedError):
                        self.export(model, values)
                    propagate.assert_not_called()
                for value, (before, version) in zip(values, snapshots):
                    self.assertTrue(torch.equal(value, before))
                    self.assertEqual(value._version, version)

    def test_unsupported_descriptors_rejected_before_shape_evaluation(self):
        x, gamma, beta = self.inputs()
        cases = [(x.double(), gamma, beta), (x, gamma.double(), beta), (x, gamma, beta.double()),
                 (torch.empty(0, 17), gamma, beta), (torch.empty(3, 0), torch.empty(0), torch.empty(0)),
                 (torch.ones(17, 3).t(), gamma, beta), (torch.ones(17), gamma, beta),
                 (x, gamma.reshape(1, 17), beta), (x, gamma, beta.reshape(1, 17)),
                 (x, gamma[:1], beta), (x, gamma, torch.zeros(3, 17)),
                 (x, torch.ones(34)[::2], beta)]
        for values in cases:
            with self.subTest(shapes=[tuple(value.shape) for value in values]):
                with patch("torch.fx.passes.shape_prop.ShapeProp.propagate") as propagate:
                    with self.assertRaises(NotImplementedError):
                        self.export(inputs=values)
                    propagate.assert_not_called()

    def test_no_opaque_or_unused_argument_equivalence(self):
        with self.assertRaises(NotImplementedError):
            self.export(Opaque())
        with self.assertRaises(NotImplementedError):
            self.export(UnusedInput(), self.inputs() + (torch.ones(1),))

    def test_samples_never_grant_numerical_permissions(self):
        for row in ([float("nan"), float("inf"), -float("inf")],
                    [3e38, -3e38, 0.0], [8192.0, 8192.001, 8191.999], [7.0] * 3):
            attrs = self.export(inputs=(torch.tensor([row]), torch.ones(3), torch.zeros(3))).graph_operators[0].attrs
            self.assertEqual(attrs, ATTRS)

    def test_other_shapes_and_shared_affine_input(self):
        for rows, width in ((1, 1), (7, 65)):
            x, gamma, _ = self.inputs(rows, width)
            module = self.export(inputs=(x, gamma, gamma))
            self.assertEqual(module.tensor_types[-1].logical_shape, "[%d,%d]" % (rows, width))
            self.assertEqual(module.graph_operators[0].attrs, ATTRS)

    def test_verifier_rejects_schema_epsilon_and_descriptor_drift(self):
        module = self.export()
        original = module.graph_operators[0]
        changes = [(key, value) for key, value in (
            ("attr.axis", "-1"), ("attr.variance_kind", "sample"), ("attr.accum_dtype", "float64"),
            ("attr.reassociate", "true"), ("attr.state_reformulation", "true"),
            ("attr.contract", "true"), ("attr.rsqrt_semantics", "approx"), ("attr.extra", "1"))]
        changes += [("attr.epsilon_bits", bits) for bits in
                    ("00000000", "7f800000", "7fc00000", "80000001", "3727C5AC", "1e-5", "0x3727c5ac")]
        for key, value in changes:
            with self.subTest(key=key, value=value):
                module.graph_operators[0] = replace(original, attrs={**ATTRS, key: value})
                with self.assertRaises(WhirlVerificationError):
                    verify_module(module)
        for missing in ATTRS:
            module.graph_operators[0] = replace(original, attrs={k: v for k, v in ATTRS.items() if k != missing})
            with self.assertRaises(WhirlVerificationError):
                verify_module(module)
        module.graph_operators[0] = original
        for index, shape in ((1, "[1,17]"), (2, "[3,17]"), (3, "[3,18]")):
            saved = module.tensor_types[index]
            module.tensor_types[index] = replace(saved, rank=len(shape[1:-1].split(",")),
                logical_shape=shape, descriptor={**saved.descriptor, "rank": len(shape[1:-1].split(",")),
                                                 "logical_shape": shape})
            with self.assertRaises(WhirlVerificationError):
                verify_module(module)
            module.tensor_types[index] = saved
        verify_module(module)


if __name__ == "__main__":
    unittest.main()
