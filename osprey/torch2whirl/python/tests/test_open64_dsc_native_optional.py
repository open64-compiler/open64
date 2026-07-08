from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

from open64_dsc import WhirlExportOptions, export_to_whirl
from open64_dsc import load_builder, save_as_whirl


NATIVE_BACKEND_AVAILABLE = (
    importlib.util.find_spec("open64_dsc._whirl") is not None
)


class DummyModel:
    pass


@unittest.skipUnless(NATIVE_BACKEND_AVAILABLE, "open64_dsc._whirl is not built")
class Open64DscNativeOptionalTest(unittest.TestCase):
    def test_native_backend_creates_opaque_tensor_and_operator_handles(self) -> None:
        builder = load_builder("native")

        tensor_ty = builder.tensor_type(
            "native_activation_type",
            "float32",
            2,
            "[1,4]",
        )
        builder.attach_tensor_descriptor(
            tensor_ty,
            {
                "kind": "tensor",
                "dtype": "float32",
                "rank": 2,
                "logical_shape": "[1,4]",
                "lineage": "native_unit_test",
            },
        )
        symbol = builder.symbol("native_activation", tensor_ty)
        builder.attach_symbol_metadata(
            symbol,
            {
                "source_layer_name": "native_activation",
                "lowering_hint": "native_unit_test",
            },
        )
        lhs = builder.tensor_constant(
            "native_lhs",
            "float32",
            2,
            "[1,4]",
            "splat",
            "1.0",
        )
        rhs = builder.tensor_constant(
            "native_rhs",
            "float32",
            2,
            "[1,4]",
            "splat",
            "2.0",
        )
        add = builder.common_add(lhs, rhs)

        self.assertGreater(tensor_ty.value, 0)
        self.assertGreater(symbol.value, 0)
        self.assertGreater(lhs.value, 0)
        self.assertGreater(rhs.value, 0)
        self.assertGreater(add.value, 0)

    def test_native_backend_finalizes_artifact(self) -> None:
        module = export_to_whirl(
            DummyModel(),
            [object()],
            WhirlExportOptions(backend="native", model_name="native_model"),
        )

        with tempfile.TemporaryDirectory() as work_dir:
            output = Path(work_dir) / "native_model.B"
            save_as_whirl(module, str(output))

            self.assertTrue(output.exists())
            self.assertGreater(output.stat().st_size, 0)


if __name__ == "__main__":
    unittest.main()
