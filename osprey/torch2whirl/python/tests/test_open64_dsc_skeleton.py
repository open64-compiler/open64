from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

from open64_dsc.backend import load_backend
from open64_dsc.interpreter import WhirlExportInterpreter
from open64_dsc import WhirlExportOptions, WhirlModule
from open64_dsc import export_to_whirl, load_builder, save_as_whirl


class DummyModel:
    pass


class Open64DscSkeletonTest(unittest.TestCase):
    def test_public_export_returns_module(self) -> None:
        module = export_to_whirl(DummyModel(), [object(), object()])

        self.assertIsInstance(module, WhirlModule)
        self.assertEqual(module.model_name, "DummyModel")
        self.assertEqual(module.input_count, 2)
        self.assertEqual(module.options.backend, "mock")

    def test_options_validate_backend(self) -> None:
        with self.assertRaises(ValueError):
            WhirlExportOptions(backend="unknown")

    def test_native_backend_reports_missing_extension(self) -> None:
        if importlib.util.find_spec("open64_dsc._whirl") is None:
            with self.assertRaisesRegex(RuntimeError, "native backend is not built"):
                load_backend("native")
        else:
            self.assertEqual(load_backend("native").backend_name(), "native")

    def test_mock_backend_creates_opaque_tensor_and_operator_handles(self) -> None:
        builder = load_builder("mock")

        tensor_ty = builder.tensor_type(
            "activation_type",
            "float32",
            2,
            "[1,4]",
        )
        lhs = builder.tensor_constant(
            "lhs",
            "float32",
            2,
            "[1,4]",
            "splat",
            "1.0",
        )
        rhs = builder.tensor_constant(
            "rhs",
            "float32",
            2,
            "[1,4]",
            "splat",
            "2.0",
        )
        add = builder.common_add(lhs, rhs)

        self.assertGreater(tensor_ty.value, 0)
        self.assertGreater(lhs.value, 0)
        self.assertGreater(rhs.value, 0)
        self.assertGreater(add.value, 0)

    def test_interpreter_exposes_builder_facade(self) -> None:
        interpreter = WhirlExportInterpreter(WhirlExportOptions())
        builder = interpreter.builder()

        lhs = builder.tensor_constant(
            "interpreter_lhs",
            "float32",
            2,
            "[1,4]",
            "splat",
            "1.0",
        )
        rhs = builder.tensor_constant(
            "interpreter_rhs",
            "float32",
            2,
            "[1,4]",
            "splat",
            "2.0",
        )
        add = builder.common_add(lhs, rhs)

        self.assertGreater(add.value, 0)

    def test_save_as_whirl_uses_mock_backend(self) -> None:
        module = export_to_whirl(
            DummyModel(),
            [object()],
            WhirlExportOptions(model_name="unit_model"),
        )

        with tempfile.TemporaryDirectory() as work_dir:
            output = Path(work_dir) / "unit_model.B"
            save_as_whirl(module, str(output))

            text = output.read_text(encoding="utf-8")

        self.assertIn("format=mock", text)
        self.assertIn("model_name=unit_model", text)
        self.assertIn("input_count=1", text)


if __name__ == "__main__":
    unittest.main()
