"""Exact Python-to-native row-sum contract tests; no execution claim."""
from dataclasses import replace
import unittest

try:
    import torch
except ImportError:
    torch = None

from open64_dsc import WhirlExportOptions, export_to_whirl
from open64_dsc.verifier import verify_module, WhirlVerificationError


if torch is not None:
    class Sum(torch.nn.Module):
        def __init__(self, dim=-1, keepdim=False, dtype=None):
            super().__init__()
            self.dim, self.keepdim, self.dtype = dim, keepdim, dtype

        def forward(self, x):
            if self.dtype is not None:
                return torch.sum(x, dim=self.dim, keepdim=self.keepdim, dtype=self.dtype)
            return torch.sum(x, dim=self.dim, keepdim=self.keepdim)

    class Composed(torch.nn.Module):
        def forward(self, x):
            return torch.sum(x + x, dim=-1)

    class MethodSum(torch.nn.Module):
        def forward(self, x):
            return x.sum(dim=-1)

    class UnrelatedName(torch.nn.Module):
        def forward(self, x):
            return torch.sum(x, 1)


@unittest.skipIf(torch is None, "torch unavailable")
class RowSumIngestionTest(unittest.TestCase):
    def export(self, model, value=None):
        if value is None:
            value = torch.arange(51, dtype=torch.float32).reshape(3, 17)
        return export_to_whirl(model, (value,), WhirlExportOptions(backend="mock"))

    def test_axes_and_call_forms(self):
        for dim in (-1, 1, (-1,), [1]):
            with self.subTest(dim=dim):
                value = torch.randn(3, 17)
                before, version = value.clone(), value._version
                module = self.export(Sum(dim), value)
                self.assertEqual(module.operators, ["common.reduce_sum"])
                self.assertEqual(module.values[0].value_kind, "formal")
                self.assertEqual(module.tensor_types[1].logical_shape, "[3]")
                self.assertEqual(module.graph_operators[0].attrs,
                    {"attr.axis": "1", "attr.keepdim": "false",
                     "attr.accum_dtype": "float32", "attr.reassociate": "false"})
                self.assertGreater(int(module.graph_operators[0].metadata["source_line"]), 0)
                self.assertTrue(torch.equal(value, before))
                self.assertEqual(value._version, version)

    def test_no_numerical_permission_from_values(self):
        value = torch.tensor([[float("inf"), -float("inf"), 1.0]], dtype=torch.float32)
        self.assertEqual(self.export(Sum(), value).graph_operators[0].attrs["attr.reassociate"], "false")

    def test_positional_axis_and_no_model_name_dispatch(self):
        self.assertEqual(self.export(UnrelatedName()).operators, ["common.reduce_sum"])

    def test_reject_unsupported_contracts(self):
        cases = [(Sum(0), torch.randn(3, 17)), (Sum(1, True), torch.randn(3, 17)),
                 (Sum((0, 1)), torch.randn(3, 17)), (Sum(dtype=torch.float64), torch.randn(3, 17)),
                 (Sum(), torch.randn(3, 17, dtype=torch.float64)),
                 (Sum(), torch.ones(3, 17, dtype=torch.int32)),
                 (Sum(), torch.randn(17)), (Sum(), torch.randn(2, 3, 17)),
                 (Sum(), torch.empty(3, 0)), (Sum(), torch.empty(0, 17)),
                 (Sum(), torch.randn(17, 3).t()), (Composed(), torch.randn(3, 17)),
                 (MethodSum(), torch.randn(3, 17))]
        for model, value in cases:
            with self.subTest(model=type(model).__name__, shape=value.shape), self.assertRaises(NotImplementedError):
                self.export(model, value)

    def test_verifier_rejects_attributes_and_result_drift(self):
        module = self.export(Sum())
        original = module.graph_operators[0]
        for key, value in (("attr.axis", "-1"), ("attr.keepdim", "true"),
                           ("attr.accum_dtype", "float64"), ("attr.reassociate", "true"),
                           ("attr.extra", "1")):
            module.graph_operators[0] = replace(original, attrs={**original.attrs, key: value})
            with self.subTest(key=key), self.assertRaises(WhirlVerificationError):
                verify_module(module)
        module.graph_operators[0] = original
        module.values[1] = replace(module.values[1], type_name=module.tensor_types[0].name)
        with self.assertRaisesRegex(WhirlVerificationError, "result descriptor"):
            verify_module(module)


if __name__ == "__main__":
    unittest.main()
