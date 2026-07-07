from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

from open64_dsc.backend import load_backend
from open64_dsc import WhirlExportOptions, export_to_whirl, save_as_whirl


NATIVE_BACKEND_AVAILABLE = (
    importlib.util.find_spec("open64_dsc._whirl") is not None
)


class DummyModel:
    pass


@unittest.skipUnless(NATIVE_BACKEND_AVAILABLE, "open64_dsc._whirl is not built")
class Open64DscNativeOptionalTest(unittest.TestCase):
    def test_native_backend_creates_opaque_tensor_and_operator_handles(self) -> None:
        backend = load_backend("native")

        tensor_ty = backend.create_tensor_type(
            "native_activation_type",
            "float32",
            2,
            "[1,4]",
        )
        lhs = backend.create_tensor_constant(
            "native_lhs",
            "float32",
            2,
            "[1,4]",
            "splat",
            "1.0",
        )
        rhs = backend.create_tensor_constant(
            "native_rhs",
            "float32",
            2,
            "[1,4]",
            "splat",
            "2.0",
        )
        add = backend.create_operator(
            "common.add",
            1,
            [lhs, rhs],
            {"attr.broadcast_rule": "none"},
        )

        self.assertGreater(tensor_ty, 0)
        self.assertGreater(lhs, 0)
        self.assertGreater(rhs, 0)
        self.assertGreater(add, 0)

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
