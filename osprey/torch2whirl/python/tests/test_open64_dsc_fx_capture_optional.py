from __future__ import annotations

import importlib.util
import unittest

from open64_dsc import export_to_whirl


TORCH_AVAILABLE = importlib.util.find_spec("torch") is not None


@unittest.skipUnless(TORCH_AVAILABLE, "torch is not installed")
class Open64DscFxCaptureOptionalTest(unittest.TestCase):
    def test_fx_add_maps_to_common_add(self) -> None:
        import torch

        class AddModule(torch.nn.Module):
            def forward(self, lhs, rhs):
                return lhs + rhs

        lhs = torch.ones((2, 3), dtype=torch.float32)
        rhs = torch.ones((2, 3), dtype=torch.float32)
        module = export_to_whirl(AddModule(), [lhs, rhs])

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["common.add"])
        self.assertEqual(module.entry_function.body_markers[-1], "common.add")
        self.assertEqual(module.graph_operators[0].kids, ["input0", "input1"])
        self.assertEqual(module.tensor_types[0].dtype, "float32")
        self.assertEqual(module.tensor_types[0].rank, 2)
        self.assertEqual(module.tensor_types[0].logical_shape, "[2,3]")

    def test_fx_unsupported_operator_fails_loudly(self) -> None:
        import torch

        class MulModule(torch.nn.Module):
            def forward(self, lhs, rhs):
                return lhs * rhs

        lhs = torch.ones((2, 3), dtype=torch.float32)
        rhs = torch.ones((2, 3), dtype=torch.float32)

        with self.assertRaisesRegex(NotImplementedError, "unsupported FX"):
            export_to_whirl(MulModule(), [lhs, rhs])


if __name__ == "__main__":
    unittest.main()
