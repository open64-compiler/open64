from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

from open64_dsc import WhirlExportOptions, export_to_whirl
from open64_dsc import load_builder, save_as_whirl
from open64_dsc.builder import ProgramUnitHandle


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

    def test_native_backend_appends_matmul_marker(self) -> None:
        builder = load_builder("native")

        pu = builder.minimal_program_unit("native_matmul_model")
        lhs = builder.tensor_constant(
            "native_matmul_lhs",
            "float32",
            2,
            "[2,3]",
            "splat",
            "1.0",
        )
        rhs = builder.tensor_constant(
            "native_matmul_rhs",
            "float32",
            2,
            "[3,4]",
            "splat",
            "2.0",
        )
        matmul = builder.common_matmul(lhs, rhs)

        builder.append_program_unit_marker(pu, lhs)
        builder.append_program_unit_marker(pu, rhs)
        builder.append_program_unit_marker(pu, matmul)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertEqual(
            [marker["opcode"] for marker in markers[-3:]],
            [
                "common.tensor_const",
                "common.tensor_const",
                "common.matmul",
            ],
        )
        self.assertIn("kid0=native_matmul_lhs", str(markers[-1]["payload"]))
        self.assertIn(
            "attr.transpose_kid0=false",
            str(markers[-1]["payload"]),
        )

    def test_native_backend_appends_relu_marker(self) -> None:
        builder = load_builder("native")

        pu = builder.minimal_program_unit("native_relu_model")
        value = builder.tensor_constant(
            "native_relu_input",
            "float32",
            2,
            "[1,4]",
            "splat",
            "1.0",
        )
        relu = builder.common_relu(value)

        builder.append_program_unit_marker(pu, value)
        builder.append_program_unit_marker(pu, relu)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertEqual(
            [marker["opcode"] for marker in markers[-2:]],
            ["common.tensor_const", "common.relu"],
        )
        self.assertIn("kid0=native_relu_input", str(markers[-1]["payload"]))

    def test_native_backend_finalizes_artifact(self) -> None:
        module = export_to_whirl(
            DummyModel(),
            [object(), object()],
            WhirlExportOptions(backend="native", model_name="native_model"),
        )
        builder = load_builder("native")
        markers = builder.inspect_program_unit_markers(
            ProgramUnitHandle(module.entry_function.handle),
        )

        self.assertEqual(
            [marker["opcode"] for marker in markers[-3:]],
            ["common.tensor_const", "common.tensor_const", "common.add"],
        )
        self.assertIn("name=input0", str(markers[-3]["payload"]))
        self.assertIn("kid0=input0", str(markers[-1]["payload"]))

        with tempfile.TemporaryDirectory() as work_dir:
            output = Path(work_dir) / "native_model.B"
            save_as_whirl(module, str(output))

            self.assertTrue(output.exists())
            self.assertGreater(output.stat().st_size, 0)


if __name__ == "__main__":
    unittest.main()
