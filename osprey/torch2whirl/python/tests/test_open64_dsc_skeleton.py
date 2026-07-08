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
        self.assertEqual(module.entry_function.name, "forward")
        self.assertGreater(module.entry_function.handle, 0)
        self.assertEqual(
            module.entry_function.body_markers,
            ["input0", "input1", "common.add"],
        )
        self.assertEqual(module.operators, ["common.add"])
        self.assertEqual(len(module.tensor_types), 2)
        self.assertEqual(len(module.values), 2)
        self.assertEqual(len(module.graph_operators), 1)
        self.assertEqual(module.graph_operators[0].kids, ["input0", "input1"])
        self.assertEqual(module.tensor_types[0].descriptor["dtype"], "float32")
        self.assertEqual(module.tensor_types[0].descriptor["rank"], 0)
        self.assertEqual(
            module.tensor_types[0].descriptor["logical_shape"],
            "[]",
        )
        self.assertEqual(
            module.values[0].metadata["source_layer_name"],
            "input0",
        )
        self.assertEqual(
            module.values[0].metadata["lowering_hint"],
            "example_input",
        )

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
        builder.attach_tensor_descriptor(
            tensor_ty,
            {
                "kind": "tensor",
                "dtype": "float32",
                "rank": 2,
                "logical_shape": "[1,4]",
                "lineage": "unit_test",
            },
        )
        symbol = builder.symbol("activation", tensor_ty)
        builder.attach_symbol_metadata(
            symbol,
            {
                "source_layer_name": "activation",
                "lowering_hint": "unit_test",
            },
        )
        pu = builder.minimal_program_unit("unit_forward")
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
        matmul = builder.common_matmul(lhs, rhs)
        relu = builder.common_relu(lhs)
        builder.append_program_unit_marker(pu, lhs)
        builder.append_program_unit_marker(pu, add)
        builder.append_program_unit_marker(pu, matmul)
        builder.append_program_unit_marker(pu, relu)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertGreater(tensor_ty.value, 0)
        self.assertGreater(symbol.value, 0)
        self.assertGreater(pu.value, 0)
        self.assertGreater(lhs.value, 0)
        self.assertGreater(rhs.value, 0)
        self.assertGreater(add.value, 0)
        self.assertGreater(matmul.value, 0)
        self.assertGreater(relu.value, 0)
        self.assertEqual(len(markers), 4)
        self.assertEqual(markers[0]["opcode"], "common.tensor_const")
        self.assertIn("name=lhs", str(markers[0]["payload"]))
        self.assertEqual(markers[1]["opcode"], "common.add")
        self.assertIn("kid0=lhs", str(markers[1]["payload"]))
        self.assertEqual(markers[2]["opcode"], "common.matmul")
        self.assertIn("attr.transpose_kid0=false", str(markers[2]["payload"]))
        self.assertEqual(markers[3]["opcode"], "common.relu")
        self.assertIn("kid0=lhs", str(markers[3]["payload"]))

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
        relu = builder.common_relu(lhs)

        self.assertGreater(add.value, 0)
        self.assertGreater(relu.value, 0)

    def test_save_as_whirl_uses_mock_backend(self) -> None:
        module = export_to_whirl(
            DummyModel(),
            [object(), object()],
            WhirlExportOptions(model_name="unit_model"),
        )

        with tempfile.TemporaryDirectory() as work_dir:
            output = Path(work_dir) / "unit_model.B"
            save_as_whirl(module, str(output))

            text = output.read_text(encoding="utf-8")

        self.assertIn("format=mock", text)
        self.assertIn("model_name=unit_model", text)
        self.assertIn("entry_function=forward", text)
        self.assertIn("entry_body_marker.0=input0", text)
        self.assertIn("entry_body_marker.1=input1", text)
        self.assertIn("entry_body_marker.2=common.add", text)
        self.assertIn("input_count=2", text)
        self.assertIn("operator.0=common.add", text)
        self.assertIn("tensor_type.0=input0_type:float32:[]", text)
        self.assertIn("tensor_descriptor.0=float32:0:[]:input0", text)
        self.assertIn("value.0=input0:input0_type:example_input", text)
        self.assertIn("value_metadata.0=input0:example_input", text)
        self.assertIn("graph_operator.0=common.add:input0,input1", text)


if __name__ == "__main__":
    unittest.main()
