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
        residual_add = builder.common_residual_add(lhs, rhs)
        relu = builder.common_relu(lhs)
        flatten = builder.common_flatten(lhs)
        output_logits = builder.common_output_logits(lhs)
        max_pool2d = builder.cnn_max_pool2d(lhs)
        global_avg_pool2d = builder.cnn_global_avg_pool2d(lhs)
        builder.append_program_unit_marker(pu, lhs)
        builder.append_program_unit_marker(pu, add)
        builder.append_program_unit_marker(pu, matmul)
        builder.append_program_unit_marker(pu, residual_add)
        builder.append_program_unit_marker(pu, relu)
        builder.append_program_unit_marker(pu, flatten)
        builder.append_program_unit_marker(pu, output_logits)
        builder.append_program_unit_marker(pu, max_pool2d)
        builder.append_program_unit_marker(pu, global_avg_pool2d)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertGreater(tensor_ty.value, 0)
        self.assertGreater(symbol.value, 0)
        self.assertGreater(pu.value, 0)
        self.assertGreater(lhs.value, 0)
        self.assertGreater(rhs.value, 0)
        self.assertGreater(add.value, 0)
        self.assertGreater(matmul.value, 0)
        self.assertGreater(residual_add.value, 0)
        self.assertGreater(relu.value, 0)
        self.assertGreater(flatten.value, 0)
        self.assertGreater(output_logits.value, 0)
        self.assertGreater(max_pool2d.value, 0)
        self.assertGreater(global_avg_pool2d.value, 0)
        self.assertEqual(len(markers), 9)
        self.assertEqual(markers[0]["opcode"], "common.tensor_const")
        self.assertIn("name=lhs", str(markers[0]["payload"]))
        self.assertEqual(markers[1]["opcode"], "common.add")
        self.assertIn("kid0=lhs", str(markers[1]["payload"]))
        self.assertEqual(markers[2]["opcode"], "common.matmul")
        self.assertIn("attr.transpose_kid0=false", str(markers[2]["payload"]))
        self.assertEqual(markers[3]["opcode"], "common.residual_add")
        self.assertIn("attr.shape_check=exact", str(markers[3]["payload"]))
        self.assertEqual(markers[4]["opcode"], "common.relu")
        self.assertIn("kid0=lhs", str(markers[4]["payload"]))
        self.assertEqual(markers[5]["opcode"], "common.flatten")
        self.assertIn("attr.start_dim=1", str(markers[5]["payload"]))
        self.assertEqual(markers[6]["opcode"], "common.output_logits")
        self.assertIn("kid0=lhs", str(markers[6]["payload"]))
        self.assertEqual(markers[7]["opcode"], "cnn.max_pool2d")
        self.assertIn("attr.kernel_shape=3,3", str(markers[7]["payload"]))
        self.assertEqual(markers[8]["opcode"], "cnn.global_avg_pool2d")
        self.assertIn("attr.output_size=1,1", str(markers[8]["payload"]))

    def test_mock_backend_creates_cnn_conv2d_marker(self) -> None:
        builder = load_builder("mock")

        value = builder.tensor_constant(
            "conv_input",
            "float32",
            4,
            "[1,3,224,224]",
            "splat",
            "1.0",
        )
        weight = builder.tensor_constant(
            "conv_weight",
            "float32",
            4,
            "[64,3,7,7]",
            "splat",
            "0.5",
        )
        bias = builder.tensor_constant(
            "conv_bias",
            "float32",
            1,
            "[64]",
            "splat",
            "0.0",
        )
        conv2d = builder.cnn_conv2d(
            value,
            weight,
            bias,
            {
                "attr.kernel_shape": "7,7",
                "attr.stride": "2,2",
                "attr.padding": "3,3",
                "attr.dilation": "1,1",
                "attr.groups": "1",
                "attr.input_layout": "NCHW",
                "attr.weight_layout": "OIHW",
                "attr.output_layout": "NCHW",
            },
        )
        pu = builder.minimal_program_unit("conv_forward")
        builder.append_program_unit_marker(pu, conv2d)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertGreater(conv2d.value, 0)
        self.assertEqual(markers[-1]["opcode"], "cnn.conv2d")
        self.assertIn("kid0=conv_input", str(markers[-1]["payload"]))
        self.assertIn("kid1=conv_weight", str(markers[-1]["payload"]))
        self.assertIn("kid2=conv_bias", str(markers[-1]["payload"]))
        self.assertIn("attr.kernel_shape=7,7", str(markers[-1]["payload"]))
        self.assertIn("attr.stride=2,2", str(markers[-1]["payload"]))

    def test_mock_backend_creates_cnn_batch_norm_infer_marker(self) -> None:
        builder = load_builder("mock")

        value = builder.tensor_constant(
            "bn_input",
            "float32",
            4,
            "[1,64,112,112]",
            "splat",
            "1.0",
        )
        scale = builder.tensor_constant(
            "bn_scale",
            "float32",
            1,
            "[64]",
            "splat",
            "1.0",
        )
        bias = builder.tensor_constant(
            "bn_bias",
            "float32",
            1,
            "[64]",
            "splat",
            "0.0",
        )
        running_mean = builder.tensor_constant(
            "bn_running_mean",
            "float32",
            1,
            "[64]",
            "splat",
            "0.0",
        )
        running_var = builder.tensor_constant(
            "bn_running_var",
            "float32",
            1,
            "[64]",
            "splat",
            "1.0",
        )
        batch_norm = builder.cnn_batch_norm_infer(
            value,
            scale,
            bias,
            running_mean,
            running_var,
            {
                "attr.epsilon": "1e-05",
                "attr.momentum": "0.1",
                "attr.training": "false",
                "attr.input_layout": "NCHW",
                "attr.channel_axis": "1",
            },
        )
        pu = builder.minimal_program_unit("bn_forward")
        builder.append_program_unit_marker(pu, batch_norm)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertGreater(batch_norm.value, 0)
        self.assertEqual(markers[-1]["opcode"], "cnn.batch_norm_infer")
        self.assertIn("kid0=bn_input", str(markers[-1]["payload"]))
        self.assertIn("kid1=bn_scale", str(markers[-1]["payload"]))
        self.assertIn("kid2=bn_bias", str(markers[-1]["payload"]))
        self.assertIn("kid3=bn_running_mean", str(markers[-1]["payload"]))
        self.assertIn("kid4=bn_running_var", str(markers[-1]["payload"]))
        self.assertIn("attr.epsilon=1e-05", str(markers[-1]["payload"]))
        self.assertIn("attr.training=false", str(markers[-1]["payload"]))

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
        residual_add = builder.common_residual_add(lhs, rhs)
        relu = builder.common_relu(lhs)
        flatten = builder.common_flatten(lhs)
        output_logits = builder.common_output_logits(lhs)
        max_pool2d = builder.cnn_max_pool2d(lhs)
        global_avg_pool2d = builder.cnn_global_avg_pool2d(lhs)
        conv2d = builder.cnn_conv2d(lhs, rhs, rhs)
        batch_norm = builder.cnn_batch_norm_infer(lhs, rhs, rhs, rhs, rhs)

        self.assertGreater(add.value, 0)
        self.assertGreater(residual_add.value, 0)
        self.assertGreater(relu.value, 0)
        self.assertGreater(flatten.value, 0)
        self.assertGreater(output_logits.value, 0)
        self.assertGreater(max_pool2d.value, 0)
        self.assertGreater(global_avg_pool2d.value, 0)
        self.assertGreater(conv2d.value, 0)
        self.assertGreater(batch_norm.value, 0)

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
