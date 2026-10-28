"""Exact stable-Softmax ingestion; native contract, not compiled execution.

Design: doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md.
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
         "attr.reassociate": "false", "attr.state_reformulation": "false",
         "attr.exp_semantics": "source_unresolved"}

if torch is not None:
    class Stable(torch.nn.Module):
        """Configurable source spellings; unsupported variants must fail closed."""
        def __init__(self, axis=-1, sum_axis=None, keepdim=True, variant=""):
            super().__init__()
            self.axis, self.sum_axis = axis, axis if sum_axis is None else sum_axis
            self.keepdim, self.variant = keepdim, variant

        def forward(self, x):
            if self.variant == "mutate":
                x.add_(1)
            if self.variant == "extra":
                torch.neg(x)
            maximum = torch.amax(x, dim=self.axis, keepdim=self.keepdim)
            shifted = x - maximum
            numerator = shifted.exp() if self.variant == "method" else torch.exp(shifted)
            source = x if self.variant == "wrong-denominator" else numerator
            if self.variant == "second-exp":
                source = torch.exp(shifted)
            denominator = torch.sum(source, dim=self.sum_axis, keepdim=True)
            if self.variant == "offset":
                denominator = denominator + 1e-5
            return numerator / denominator

    class Opaque(torch.nn.Module):
        def forward(self, x):
            return torch.softmax(x, dim=-1)

    class Naive(torch.nn.Module):
        def forward(self, x):
            numerator = torch.exp(x)
            return numerator / torch.sum(numerator, dim=-1, keepdim=True)

    class UnusedInput(torch.nn.Module):
        def forward(self, x, unused):
            numerator = torch.exp(x - torch.amax(x, dim=-1, keepdim=True))
            return numerator / torch.sum(numerator, dim=-1, keepdim=True)

    class UnrelatedName(Stable):
        pass


@unittest.skipIf(torch is None, "torch unavailable")
class SoftmaxIngestionTests(unittest.TestCase):
    """Source recipe, descriptor, numerical and pre-evaluation boundaries."""

    def export(self, model, value=None):
        """Use mock handles only for source-contract tests, never mock .B files."""
        if value is None:
            value = torch.arange(51, dtype=torch.float32).reshape(3, 17) - 25
        return export_to_whirl(model, (value,), WhirlExportOptions(backend="mock"))

    def test_exact_source_axes_and_type_contract(self):
        """Canonicalize accepted last-axis spellings without changing inputs."""
        for axis in (-1, 1, (-1,), [1]):
            with self.subTest(axis=axis):
                value = torch.arange(51, dtype=torch.float32).reshape(3, 17)
                before, version = value.clone(), value._version
                module = self.export(Stable(axis), value)
                self.assertEqual(module.operators, ["common.stable_softmax"])
                self.assertEqual(module.values[0].value_kind, "formal")
                self.assertEqual(module.tensor_types[1].logical_shape, "[3,17]")
                self.assertEqual(module.graph_operators[0].attrs, ATTRS)
                self.assertNotEqual(module.values[0].handle, module.values[1].handle)
                self.assertTrue(torch.equal(value, before))
                self.assertEqual(value._version, version)

    def test_source_position_is_division_not_class_header(self):
        """The native region location identifies its actual normalization site."""
        metadata = self.export(UnrelatedName()).graph_operators[0].metadata
        line = int(metadata["source_line"])
        self.assertIn("return numerator / denominator",
                      Path(metadata["source_file"]).read_text().splitlines()[line - 1])

    def test_samples_never_grant_numerical_permissions(self):
        """Nonfinite/extreme samples do not become range or accuracy proofs."""
        for values in ([float("inf"), -float("inf"), float("nan")],
                       [3e38, -3e38, 0.0], [0.0, 0.0, 0.0]):
            module = self.export(Stable(), torch.tensor([values], dtype=torch.float32))
            self.assertEqual(module.graph_operators[0].attrs, ATTRS)
            self.assertFalse(any(value.value_kind == "constant" for value in module.values))

    def test_reject_noncanonical_recipes_before_shape_evaluation(self):
        """Do not execute extra effects or any altered max/exp/sum dependency."""
        models = [Stable(0), Stable(keepdim=False), Stable(sum_axis=0), Naive()]
        models += [Stable(variant=name) for name in
                   ("mutate", "extra", "method", "wrong-denominator", "second-exp", "offset")]
        for model in models:
            value = torch.ones(3, 17)
            before, version = value.clone(), value._version
            with self.subTest(model=type(model).__name__, variant=getattr(model, "variant", "")):
                with patch("torch.fx.passes.shape_prop.ShapeProp.propagate") as propagate:
                    with self.assertRaises(NotImplementedError):
                        self.export(model, value)
                    propagate.assert_not_called()
                self.assertTrue(torch.equal(value, before))
                self.assertEqual(value._version, version)

    def test_reject_unsupported_descriptors_before_shape_evaluation(self):
        """Input samples supply shape/dtype only within the native static subset."""
        for value in (torch.ones(3, 17, dtype=torch.float64),
                      torch.ones(3, 17, dtype=torch.int32), torch.ones(17),
                      torch.ones(2, 3, 17), torch.empty(0, 17), torch.empty(3, 0),
                      torch.ones(17, 3).t()):
            with self.subTest(shape=value.shape, dtype=value.dtype):
                with patch("torch.fx.passes.shape_prop.ShapeProp.propagate") as propagate:
                    with self.assertRaises(NotImplementedError):
                        self.export(Stable(), value)
                    propagate.assert_not_called()

    def test_no_opaque_softmax_or_unused_input_equivalence(self):
        """No framework-name or unused-argument shortcut is allowed."""
        with self.assertRaises(NotImplementedError):
            self.export(Opaque())
        with self.assertRaises(NotImplementedError):
            export_to_whirl(UnusedInput(), (torch.ones(3, 17), torch.ones(3, 17)),
                            WhirlExportOptions(backend="mock"))

    def test_verifier_rejects_attribute_and_result_drift(self):
        """Python mirrors, but never broadens, the exact native v1 schema."""
        module = self.export(Stable())
        original = module.graph_operators[0]
        for key, value in (("attr.axis", "-1"), ("attr.accum_dtype", "float64"),
                           ("attr.reassociate", "true"), ("attr.state_reformulation", "true"),
                           ("attr.exp_semantics", "fast_exp"), ("attr.extra", "1")):
            module.graph_operators[0] = replace(original, attrs={**ATTRS, key: value})
            with self.subTest(key=key), self.assertRaises(WhirlVerificationError):
                verify_module(module)
        module.graph_operators[0] = replace(original, attrs={k: v for k, v in ATTRS.items() if k != "attr.exp_semantics"})
        with self.assertRaises(WhirlVerificationError):
            verify_module(module)
        module.graph_operators[0] = original
        result = module.tensor_types[1]
        module.tensor_types[1] = replace(result, logical_shape="[3,18]",
            descriptor={**result.descriptor, "logical_shape": "[3,18]"})
        with self.assertRaisesRegex(WhirlVerificationError, "result descriptor"):
            verify_module(module)

    def test_other_shapes_use_the_same_recipe(self):
        """No fixture-size lookup or power-of-two assumption is permitted."""
        for rows, width in ((1, 1), (7, 65)):
            module = self.export(Stable(), torch.ones(rows, width))
            self.assertEqual(module.tensor_types[1].logical_shape, "[%d,%d]" % (rows, width))
            self.assertEqual(module.graph_operators[0].attrs, ATTRS)


if __name__ == "__main__":
    unittest.main()
