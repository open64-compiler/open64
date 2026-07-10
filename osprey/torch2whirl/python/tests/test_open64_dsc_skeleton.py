from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

from open64_dsc.backend import load_backend
from open64_dsc.cli import _load_model, _parse_shape_spec
from open64_dsc.interpreter import WhirlExportInterpreter
from open64_dsc.module import (
    WhirlOperatorRecord,
    WhirlProgramUnitRecord,
    WhirlTensorTypeRecord,
    WhirlValueRecord,
)
from open64_dsc import WhirlExportOptions, WhirlModule
from open64_dsc import WhirlVerificationError, export_to_whirl
from open64_dsc import load_builder, save_as_whirl, verify_module


class DummyModel:
    pass


class Open64DscSkeletonTest(unittest.TestCase):
    def _gatekeeper_module(
        self,
        *,
        options: WhirlExportOptions = WhirlExportOptions(),
        tensor_types=None,
        graph_operators=None,
    ) -> WhirlModule:
        default_tensor_types = [
            WhirlTensorTypeRecord(
                "input0_type",
                1,
                "float32",
                2,
                "[1,3]",
                {
                    "dtype": "float32",
                    "rank": 2,
                    "logical_shape": "[1,3]",
                    "lineage": "input0",
                },
            ),
            WhirlTensorTypeRecord(
                "input1_type",
                2,
                "float32",
                2,
                "[1,3]",
                {
                    "dtype": "float32",
                    "rank": 2,
                    "logical_shape": "[1,3]",
                    "lineage": "input1",
                },
            ),
        ]
        default_graph_operators = [
            WhirlOperatorRecord(
                "common.add",
                5,
                ["input0", "input1"],
                {"attr.broadcast_rule": "none"},
            )
        ]
        return WhirlModule(
            options=options,
            model_name="GatekeeperModel",
            input_count=2,
            entry_function=WhirlProgramUnitRecord(
                "forward",
                4,
                ["input0", "input1", "common.add"],
            ),
            graph_source="unit",
            operators=[
                operator.name
                for operator in (graph_operators or default_graph_operators)
            ],
            tensor_types=tensor_types or default_tensor_types,
            values=[
                WhirlValueRecord(
                    "input0",
                    10,
                    "input0_type",
                    "example_input",
                    metadata={"logical_shape": "[1,3]"},
                ),
                WhirlValueRecord(
                    "input1",
                    11,
                    "input1_type",
                    "example_input",
                    metadata={"logical_shape": "[1,3]"},
                ),
            ],
            graph_operators=graph_operators or default_graph_operators,
        )

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

    def test_options_default_to_verification_enabled(self) -> None:
        self.assertTrue(WhirlExportOptions().verify)

    def test_gatekeeper_accepts_valid_module(self) -> None:
        verify_module(self._gatekeeper_module())

    def test_gatekeeper_rejects_missing_descriptor_field(self) -> None:
        tensor_types = [
            WhirlTensorTypeRecord(
                "input0_type",
                1,
                "float32",
                2,
                "[1,3]",
                {"dtype": "float32", "logical_shape": "[1,3]"},
            ),
            WhirlTensorTypeRecord(
                "input1_type",
                2,
                "float32",
                2,
                "[1,3]",
                {
                    "dtype": "float32",
                    "rank": 2,
                    "logical_shape": "[1,3]",
                },
            ),
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "descriptor"):
            verify_module(self._gatekeeper_module(tensor_types=tensor_types))

    def test_gatekeeper_rejects_rank_shape_mismatch(self) -> None:
        tensor_types = [
            WhirlTensorTypeRecord(
                "input0_type",
                1,
                "float32",
                3,
                "[1,3]",
                {
                    "dtype": "float32",
                    "rank": 3,
                    "logical_shape": "[1,3]",
                },
            ),
            WhirlTensorTypeRecord(
                "input1_type",
                2,
                "float32",
                2,
                "[1,3]",
                {
                    "dtype": "float32",
                    "rank": 2,
                    "logical_shape": "[1,3]",
                },
            ),
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "rank"):
            verify_module(self._gatekeeper_module(tensor_types=tensor_types))

    def test_gatekeeper_rejects_unknown_operator(self) -> None:
        graph_operators = [
            WhirlOperatorRecord(
                "mystery.add",
                5,
                ["input0", "input1"],
                {},
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "unknown operator"):
            verify_module(self._gatekeeper_module(graph_operators=graph_operators))

    def test_gatekeeper_rejects_unresolved_operand(self) -> None:
        graph_operators = [
            WhirlOperatorRecord(
                "common.add",
                5,
                ["input0", "missing"],
                {"attr.broadcast_rule": "none"},
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "operand"):
            verify_module(self._gatekeeper_module(graph_operators=graph_operators))

    def test_gatekeeper_rejects_common_add_shape_mismatch(self) -> None:
        tensor_types = [
            WhirlTensorTypeRecord(
                "input0_type",
                1,
                "float32",
                2,
                "[1,3]",
                {
                    "dtype": "float32",
                    "rank": 2,
                    "logical_shape": "[1,3]",
                },
            ),
            WhirlTensorTypeRecord(
                "input1_type",
                2,
                "float32",
                2,
                "[2,3]",
                {
                    "dtype": "float32",
                    "rank": 2,
                    "logical_shape": "[2,3]",
                },
            ),
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "shape"):
            verify_module(self._gatekeeper_module(tensor_types=tensor_types))

    def test_gatekeeper_rejects_residual_add_broadcast(self) -> None:
        graph_operators = [
            WhirlOperatorRecord(
                "common.residual_add",
                5,
                ["input0", "input1"],
                {
                    "attr.broadcast_rule": "numpy",
                    "attr.shape_check": "exact",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "broadcast"):
            verify_module(self._gatekeeper_module(graph_operators=graph_operators))

    def test_save_as_whirl_runs_gatekeeper_by_default(self) -> None:
        module = self._gatekeeper_module(
            graph_operators=[
                WhirlOperatorRecord(
                    "common.add",
                    5,
                    ["input0", "missing"],
                    {"attr.broadcast_rule": "none"},
                )
            ]
        )

        with tempfile.TemporaryDirectory() as work_dir:
            output = Path(work_dir) / "bad_model.B"
            with self.assertRaisesRegex(WhirlVerificationError, "operand"):
                save_as_whirl(module, str(output))

    def test_save_as_whirl_allows_verification_escape_hatch(self) -> None:
        module = self._gatekeeper_module(
            options=WhirlExportOptions(verify=False),
            graph_operators=[
                WhirlOperatorRecord(
                    "common.add",
                    5,
                    ["input0", "missing"],
                    {"attr.broadcast_rule": "none"},
                )
            ],
        )

        with tempfile.TemporaryDirectory() as work_dir:
            output = Path(work_dir) / "unchecked_model.B"
            save_as_whirl(module, str(output))

            text = output.read_text(encoding="utf-8")

        self.assertIn("format=mock", text)

    def test_cli_parse_shape_spec(self) -> None:
        self.assertEqual(_parse_shape_spec("shape:1,3,224,224"), (1, 3, 224, 224))

        with self.assertRaisesRegex(ValueError, "shape"):
            _parse_shape_spec("tensor:1,3")
        with self.assertRaisesRegex(ValueError, "positive"):
            _parse_shape_spec("shape:1,0,3")
        with self.assertRaisesRegex(ValueError, "integer"):
            _parse_shape_spec("shape:1,bad,3")

    def test_cli_load_model_from_factory(self) -> None:
        with tempfile.TemporaryDirectory() as work_dir:
            model_path = Path(work_dir) / "model.py"
            model_path.write_text(
                "\n".join(
                    [
                        "class UnitModel:",
                        "    def __init__(self):",
                        "        self.eval_called = False",
                        "    def eval(self):",
                        "        self.eval_called = True",
                        "        return self",
                        "def create_model():",
                        "    return UnitModel()",
                    ]
                ) + "\n",
                encoding="utf-8",
            )

            model = _load_model(model_path, "create_model")

        self.assertEqual(model.__class__.__name__, "UnitModel")
        self.assertTrue(model.eval_called)

    def test_cli_load_model_reports_missing_factory(self) -> None:
        with tempfile.TemporaryDirectory() as work_dir:
            model_path = Path(work_dir) / "model.py"
            model_path.write_text("VALUE = 1\n", encoding="utf-8")

            with self.assertRaisesRegex(AttributeError, "model factory"):
                _load_model(model_path, "create_model")

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

    def test_mock_backend_creates_common_linear_marker(self) -> None:
        builder = load_builder("mock")

        value = builder.tensor_constant(
            "linear_input",
            "float32",
            2,
            "[1,2048]",
            "splat",
            "1.0",
        )
        weight = builder.tensor_constant(
            "linear_weight",
            "float32",
            2,
            "[1000,2048]",
            "splat",
            "0.5",
        )
        bias = builder.tensor_constant(
            "linear_bias",
            "float32",
            1,
            "[1000]",
            "splat",
            "0.0",
        )
        linear = builder.common_linear(value, weight, bias)
        pu = builder.minimal_program_unit("linear_forward")
        builder.append_program_unit_marker(pu, linear)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertGreater(linear.value, 0)
        self.assertEqual(markers[-1]["opcode"], "common.linear")
        self.assertIn("kid0=linear_input", str(markers[-1]["payload"]))
        self.assertIn("kid1=linear_weight", str(markers[-1]["payload"]))
        self.assertIn("kid2=linear_bias", str(markers[-1]["payload"]))
        self.assertIn("attr.has_bias=true", str(markers[-1]["payload"]))
        self.assertIn("attr.weight_layout=OI", str(markers[-1]["payload"]))

    def test_mock_backend_creates_external_tensor_operands(self) -> None:
        builder = load_builder("mock")

        value = builder.tensor_constant(
            "external_conv_input",
            "float32",
            4,
            "[1,3,224,224]",
            "example_input",
            "external_conv_input",
        )
        weight = builder.external_tensor_constant(
            "external_conv_weight",
            "float32",
            4,
            "[64,3,7,7]",
            "weight",
            "safetensors",
            "resnet.safetensors",
            "conv1.weight",
            128,
            37632,
            "sha256:conv-weight",
            "OIHW",
        )
        bias = builder.external_tensor_constant(
            "external_conv_bias",
            "float32",
            1,
            "[64]",
            "bias",
            "safetensors",
            "resnet.safetensors",
            "conv1.bias",
            37760,
            256,
            "sha256:conv-bias",
            "C",
        )
        conv2d = builder.cnn_conv2d(value, weight, bias)
        pu = builder.minimal_program_unit("external_conv_forward")
        builder.append_program_unit_marker(pu, weight)
        builder.append_program_unit_marker(pu, conv2d)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertGreater(weight.value, 0)
        self.assertGreater(weight.tensor_type, 0)
        self.assertGreater(weight.symbol, 0)
        self.assertEqual(weight.metadata["tensor_role"], "weight")
        self.assertEqual(weight.metadata["storage_format"], "safetensors")
        self.assertEqual(weight.metadata["storage_file"], "resnet.safetensors")
        self.assertEqual(weight.metadata["storage_tensor_key"], "conv1.weight")
        self.assertEqual(weight.metadata["storage_byte_offset"], "128")
        self.assertEqual(weight.metadata["storage_byte_length"], "37632")
        self.assertEqual(markers[-2]["opcode"], "common.tensor_const")
        self.assertIn("name=external_conv_weight", str(markers[-2]["payload"]))
        self.assertIn("value_kind=external_data", str(markers[-2]["payload"]))
        self.assertIn("safetensors://resnet.safetensors", str(markers[-2]["payload"]))
        self.assertEqual(markers[-1]["opcode"], "cnn.conv2d")
        self.assertIn("kid1=external_conv_weight", str(markers[-1]["payload"]))
        self.assertIn("kid2=external_conv_bias", str(markers[-1]["payload"]))

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
        linear = builder.common_linear(lhs, rhs, rhs)
        external = builder.external_tensor_constant(
            "interpreter_external_weight",
            "float32",
            2,
            "[4,4]",
            "weight",
            "safetensors",
            "unit.safetensors",
            "linear.weight",
            0,
            64,
        )

        self.assertGreater(add.value, 0)
        self.assertGreater(residual_add.value, 0)
        self.assertGreater(relu.value, 0)
        self.assertGreater(flatten.value, 0)
        self.assertGreater(output_logits.value, 0)
        self.assertGreater(max_pool2d.value, 0)
        self.assertGreater(global_avg_pool2d.value, 0)
        self.assertGreater(conv2d.value, 0)
        self.assertGreater(batch_norm.value, 0)
        self.assertGreater(linear.value, 0)
        self.assertGreater(external.value, 0)
        self.assertGreater(external.symbol, 0)
        self.assertEqual(external.metadata["storage_tensor_key"], "linear.weight")

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
