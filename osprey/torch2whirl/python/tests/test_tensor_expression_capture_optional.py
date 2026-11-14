"""Static view/composition checks for the native tensor-expression contract."""

import importlib.util
from dataclasses import replace
import unittest
from unittest.mock import patch

AVAILABLE = importlib.util.find_spec("torch") is not None
if AVAILABLE:
    import torch
    from open64_dsc import WhirlExportOptions, export_to_whirl
    from open64_dsc.verifier import WhirlVerificationError, verify_module
    from models.absorbed_mla import make_fixture

    class View(torch.nn.Module):
        def __init__(self, kind, dim=-1):
            super().__init__()
            self.kind, self.dim = kind, dim

        def forward(self, value):
            if self.kind == "unsqueeze":
                return value.unsqueeze(self.dim)
            if self.kind == "squeeze":
                return value.squeeze(self.dim)
            return value.transpose(0, self.dim)

    class Bad(torch.nn.Module):
        def forward(self, value):
            return value.unsqueeze(1, dim=1)

    class Broadcast(torch.nn.Module):
        def forward(self, lhs, rhs):
            return torch.matmul(lhs.unsqueeze(-2), rhs).squeeze(-2)


@unittest.skipUnless(AVAILABLE, "requires optional PyTorch")
class TensorExpressionCaptureTests(unittest.TestCase):
    def export(self, model, inputs):
        return export_to_whirl(model, inputs, WhirlExportOptions(backend="mock"))

    def test_unsqueeze_positive_and_negative_axes(self):
        value = torch.ones(2, 3)
        for dim in (-3, -2, -1, 0, 1, 2):
            with self.subTest(dim=dim):
                module = self.export(View("unsqueeze", dim), (value,))
                self.assertEqual(module.operators, ["common.reshape"])
                expected = tuple(View("unsqueeze", dim)(value).shape)
                output = module.tensor_types[-1]
                self.assertEqual(output.logical_shape, "[" + ",".join(map(str, expected)) + "]")
                self.assertEqual(len([v for v in module.values if v.value_kind == "formal"]), 1)

    def test_squeeze_non_singleton_is_identity_and_transpose_permutes(self):
        for kind, dim, shape in (("squeeze", 1, (1, 3, 1)), ("squeeze", -1, (1, 3, 1)),
                                 ("transpose", -1, (2, 3, 4))):
            value = torch.ones(shape)
            module = self.export(View(kind, dim), (value,))
            expected = tuple(View(kind, dim)(value).shape)
            self.assertEqual(module.tensor_types[-1].logical_shape,
                             "[" + ",".join(map(str, expected)) + "]")
            self.assertEqual(module.operators[0], "common.transpose" if kind == "transpose" else "common.reshape")

    def test_preflight_does_not_execute_shapeprop_or_bad_axes(self):
        with patch("torch.fx.passes.shape_prop.ShapeProp.propagate",
                   side_effect=AssertionError("sample graph execution is forbidden")):
            self.export(View("unsqueeze", -1), (torch.ones(2, 3),))
            for model in (View("unsqueeze", 3), View("unsqueeze", True), Bad()):
                with self.subTest(model=type(model).__name__), self.assertRaises(NotImplementedError):
                    self.export(model, (torch.ones(2, 3),))

    def test_broadcast_matmul_preserves_rank_padded_batches(self):
        module = self.export(Broadcast(), (torch.ones(2, 3, 8), torch.ones(3, 8, 16)))
        matmul = next(op for op in module.graph_operators if op.name == "common.matmul")
        self.assertEqual(matmul.metadata["operator_version"], "3")
        self.assertEqual(matmul.attrs["attr.batch_rule"], "numpy")
        self.assertEqual(module.tensor_types[-1].logical_shape, "[2,3,16]")
        with self.assertRaises(NotImplementedError):
            self.export(Broadcast(), (torch.ones(2, 3, 8), torch.ones(5, 8, 16)))

    def test_all_mla_inputs_are_runtime_values_with_both_outputs(self):
        model, inputs = make_fixture(length=11)
        module = self.export(model, inputs)
        self.assertEqual(len(module.graph_operators), 46)
        self.assertEqual([v.name for v in module.values if v.value_kind == "formal"],
                         ["query_nope", "query_rope", "cache", "rope_cache",
                          "key_up", "value_up", "allowed"])
        self.assertEqual(module.tensor_types[-1].logical_shape, "[1,3]")
        self.assertFalse(module.tensor_payloads)
        self.assertFalse(any(v.value_kind == "tensor_constant" for v in module.values))
        self.assertEqual({o.metadata["source_contract"] for o in module.graph_operators},
                         {"tensor_expression_v1"})

    def test_bad_dtype_and_abort_retry(self):
        model, inputs = make_fixture(length=7)
        with self.assertRaises(NotImplementedError):
            self.export(model, (inputs[0].double(), *inputs[1:]))
        self.assertEqual(len(self.export(model, inputs).graph_operators), 46)

    def test_verifier_rejects_changed_version_shape_and_select_condition(self):
        model, inputs = make_fixture(length=7)
        module = self.export(model, inputs)
        ops = list(module.graph_operators)
        index = next(i for i, op in enumerate(ops) if op.name == "common.select")
        bad = replace(ops[index], kids=[ops[index].kids[1], *ops[index].kids[1:]])
        changed = ops[:]
        changed[index] = bad
        with self.assertRaises(WhirlVerificationError):
            verify_module(replace(module, graph_operators=changed))
        changed = ops[:]
        changed[0] = replace(ops[0], metadata=dict(ops[0].metadata, operator_version="9"))
        with self.assertRaises(WhirlVerificationError):
            verify_module(replace(module, graph_operators=changed))
        types = list(module.tensor_types)
        types[-1] = replace(types[-1], logical_shape="[1,4]",
                             descriptor=dict(types[-1].descriptor, logical_shape="[1,4]"))
        with self.assertRaises(WhirlVerificationError):
            verify_module(replace(module, tensor_types=types))


if __name__ == "__main__":
    unittest.main()
