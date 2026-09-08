from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

from open64_dsc import WhirlExportOptions, WhirlVerificationError
from open64_dsc import export_to_whirl
from open64_dsc import load_builder, save_as_whirl
from open64_dsc.builder import ProgramUnitHandle
from open64_dsc.mapping import common
from open64_dsc.module import (
    WhirlModule,
    WhirlOperatorRecord,
    WhirlProgramUnitRecord,
    WhirlTensorTypeRecord,
    WhirlValueRecord,
)


NATIVE_BACKEND_AVAILABLE = (
    importlib.util.find_spec("open64_dsc._whirl") is not None
)


class DummyModel:
    pass


@unittest.skipUnless(NATIVE_BACKEND_AVAILABLE, "open64_dsc._whirl is not built")
class Open64DscNativeOptionalTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        builder = load_builder("native")
        probe_type = builder.tensor_type("native_g1_probe_type", "float32", 0, "[]")
        probe_value = builder.tensor_constant(
            "native_g1_probe",
            "float32",
            0,
            "[]",
            "splat",
            "1.0",
        )
        try:
            builder.common_relu(probe_value)
            builder.model_input("native_g1_input", probe_type, 0)
        except RuntimeError as exc:
            if "does not support" in str(exc):
                raise unittest.SkipTest(str(exc)) from exc
            raise

    def setUp(self) -> None:
        load_builder("native").begin_program()

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

    def test_native_tensor_identity_excludes_runtime_state_and_lineage(self) -> None:
        builder = load_builder("native")
        descriptor = {
            "kind": "tensor",
            "dtype": "float32",
            "rank": 2,
            "logical_shape": "[1,4]",
            "layout": "contiguous",
            "runtime_state": "resident",
            "lineage": "producer_a",
        }
        first = builder.tensor_type(
            "canonical_a", "float32", 2, "[1,4]", descriptor
        )
        second = builder.tensor_type(
            "canonical_b",
            "float32",
            2,
            "[1,4]",
            {**descriptor, "runtime_state": "evicted", "lineage": "producer_b"},
        )

        self.assertEqual(first.value, second.value)

    def test_native_backend_appends_add_marker(self) -> None:
        builder = load_builder("native")

        pu = builder.minimal_program_unit("native_add_model")
        lhs = builder.tensor_constant(
            "native_add_lhs",
            "float32",
            2,
            "[2,3]",
            "splat",
            "1.0",
        )
        rhs = builder.tensor_constant(
            "native_add_rhs",
            "float32",
            2,
            "[2,3]",
            "splat",
            "2.0",
        )
        add = builder.common_add(lhs, rhs)

        builder.append_program_unit_value(pu, lhs)
        builder.append_program_unit_value(pu, rhs)
        builder.append_program_unit_value(pu, add)
        markers = builder.inspect_program_unit_values(pu)

        self.assertEqual(
            [marker["opcode"] for marker in markers[-3:]],
            [
                "common.tensor_const",
                "common.tensor_const",
                "common.add",
            ],
        )
        self.assertIn("kid0=native_add_lhs", str(markers[-1]["payload"]))
        self.assertIn("kid1=native_add_rhs", str(markers[-1]["payload"]))
        self.assertIn(
            "attr.broadcast_rule=none",
            str(markers[-1]["payload"]),
        )

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
        self.assertIn("kid1=native_matmul_rhs", str(markers[-1]["payload"]))
        self.assertIn(
            "attr.transpose_kid0=false",
            str(markers[-1]["payload"]),
        )
        self.assertIn(
            "attr.transpose_kid1=false",
            str(markers[-1]["payload"]),
        )

    def test_native_backend_appends_residual_add_marker(self) -> None:
        builder = load_builder("native")

        pu = builder.minimal_program_unit("native_residual_add_model")
        lhs = builder.tensor_constant(
            "native_residual_lhs",
            "float32",
            4,
            "[1,64,56,56]",
            "splat",
            "1.0",
        )
        rhs = builder.tensor_constant(
            "native_residual_rhs",
            "float32",
            4,
            "[1,64,56,56]",
            "splat",
            "2.0",
        )
        residual_add = builder.common_residual_add(lhs, rhs)

        builder.append_program_unit_marker(pu, lhs)
        builder.append_program_unit_marker(pu, rhs)
        builder.append_program_unit_marker(pu, residual_add)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertEqual(
            [marker["opcode"] for marker in markers[-3:]],
            [
                "common.tensor_const",
                "common.tensor_const",
                "common.residual_add",
            ],
        )
        self.assertIn("kid0=native_residual_lhs", str(markers[-1]["payload"]))
        self.assertIn("kid1=native_residual_rhs", str(markers[-1]["payload"]))
        self.assertIn("attr.shape_check=exact", str(markers[-1]["payload"]))

    def test_native_backend_appends_phase7_common_unary_markers(self) -> None:
        builder = load_builder("native")

        pu = builder.minimal_program_unit("native_common_unary_model")
        value = builder.tensor_constant(
            "native_unary_input",
            "float32",
            2,
            "[1,4]",
            "splat",
            "1.0",
        )
        relu = builder.common_relu(value)
        flatten = builder.common_flatten(value)
        output_logits = builder.common_output_logits(value)

        builder.append_program_unit_marker(pu, value)
        builder.append_program_unit_marker(pu, relu)
        builder.append_program_unit_marker(pu, flatten)
        builder.append_program_unit_marker(pu, output_logits)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertEqual(
            [marker["opcode"] for marker in markers[-4:]],
            [
                "common.tensor_const",
                "common.relu",
                "common.flatten",
                "common.output_logits",
            ],
        )
        self.assertIn("kid0=native_unary_input", str(markers[-3]["payload"]))
        self.assertIn("attr.start_dim=1", str(markers[-2]["payload"]))
        self.assertIn("kid0=native_unary_input", str(markers[-1]["payload"]))

    def test_native_backend_appends_phase7_cnn_unary_markers(self) -> None:
        builder = load_builder("native")

        pu = builder.minimal_program_unit("native_cnn_unary_model")
        value = builder.tensor_constant(
            "native_cnn_input",
            "float32",
            4,
            "[1,3,224,224]",
            "splat",
            "1.0",
        )
        max_pool2d = builder.cnn_max_pool2d(value)
        global_avg_pool2d = builder.cnn_global_avg_pool2d(value)

        builder.append_program_unit_marker(pu, value)
        builder.append_program_unit_marker(pu, max_pool2d)
        builder.append_program_unit_marker(pu, global_avg_pool2d)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertEqual(
            [marker["opcode"] for marker in markers[-3:]],
            [
                "common.tensor_const",
                "cnn.max_pool2d",
                "cnn.global_avg_pool2d",
            ],
        )
        self.assertIn("kid0=native_cnn_input", str(markers[-2]["payload"]))
        self.assertIn("attr.kernel_shape=3,3", str(markers[-2]["payload"]))
        self.assertIn("kid0=native_cnn_input", str(markers[-1]["payload"]))
        self.assertIn("attr.output_size=1,1", str(markers[-1]["payload"]))

    def test_native_backend_appends_phase7_cnn_conv2d_marker(self) -> None:
        builder = load_builder("native")

        pu = builder.minimal_program_unit("native_cnn_conv2d_model")
        value = builder.tensor_constant(
            "native_conv_input",
            "float32",
            4,
            "[1,3,224,224]",
            "splat",
            "1.0",
        )
        weight = builder.tensor_constant(
            "native_conv_weight",
            "float32",
            4,
            "[64,3,7,7]",
            "splat",
            "0.5",
        )
        bias = builder.tensor_constant(
            "native_conv_bias",
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

        builder.append_program_unit_marker(pu, conv2d)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertEqual(markers[-1]["opcode"], "cnn.conv2d")
        self.assertIn("kid0=native_conv_input", str(markers[-1]["payload"]))
        self.assertIn("kid1=native_conv_weight", str(markers[-1]["payload"]))
        self.assertIn("kid2=native_conv_bias", str(markers[-1]["payload"]))
        self.assertIn("attr.kernel_shape=7,7", str(markers[-1]["payload"]))
        self.assertIn("attr.stride=2,2", str(markers[-1]["payload"]))

    def test_native_backend_appends_phase7_cnn_batch_norm_marker(self) -> None:
        builder = load_builder("native")

        pu = builder.minimal_program_unit("native_cnn_batch_norm_model")
        value = builder.tensor_constant(
            "native_bn_input",
            "float32",
            4,
            "[1,64,112,112]",
            "splat",
            "1.0",
        )
        scale = builder.tensor_constant(
            "native_bn_scale",
            "float32",
            1,
            "[64]",
            "splat",
            "1.0",
        )
        bias = builder.tensor_constant(
            "native_bn_bias",
            "float32",
            1,
            "[64]",
            "splat",
            "0.0",
        )
        running_mean = builder.tensor_constant(
            "native_bn_running_mean",
            "float32",
            1,
            "[64]",
            "splat",
            "0.0",
        )
        running_var = builder.tensor_constant(
            "native_bn_running_var",
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
        )

        builder.append_program_unit_marker(pu, batch_norm)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertEqual(markers[-1]["opcode"], "cnn.batch_norm_infer")
        self.assertIn("kid0=native_bn_input", str(markers[-1]["payload"]))
        self.assertIn("kid1=native_bn_scale", str(markers[-1]["payload"]))
        self.assertIn("kid2=native_bn_bias", str(markers[-1]["payload"]))
        self.assertIn("kid3=native_bn_running_mean", str(markers[-1]["payload"]))
        self.assertIn("kid4=native_bn_running_var", str(markers[-1]["payload"]))
        self.assertIn("attr.epsilon=1e-05", str(markers[-1]["payload"]))
        self.assertIn("attr.training=false", str(markers[-1]["payload"]))

    def test_native_backend_appends_phase7_common_linear_marker(self) -> None:
        builder = load_builder("native")

        pu = builder.minimal_program_unit("native_common_linear_model")
        value = builder.tensor_constant(
            "native_linear_input",
            "float32",
            2,
            "[1,2048]",
            "splat",
            "1.0",
        )
        weight = builder.tensor_constant(
            "native_linear_weight",
            "float32",
            2,
            "[1000,2048]",
            "splat",
            "0.5",
        )
        bias = builder.tensor_constant(
            "native_linear_bias",
            "float32",
            1,
            "[1000]",
            "splat",
            "0.0",
        )
        linear = builder.common_linear(value, weight, bias)

        builder.append_program_unit_marker(pu, linear)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertEqual(markers[-1]["opcode"], "common.linear")
        self.assertIn("kid0=native_linear_input", str(markers[-1]["payload"]))
        self.assertIn("kid1=native_linear_weight", str(markers[-1]["payload"]))
        self.assertIn("kid2=native_linear_bias", str(markers[-1]["payload"]))
        self.assertIn("attr.has_bias=true", str(markers[-1]["payload"]))
        self.assertIn("attr.weight_layout=OI", str(markers[-1]["payload"]))

    def test_native_backend_appends_external_tensor_operands(self) -> None:
        builder = load_builder("native")

        pu = builder.minimal_program_unit("native_external_tensor_model")
        value = builder.tensor_constant(
            "native_external_input",
            "float32",
            4,
            "[1,3,224,224]",
            "example_input",
            "native_external_input",
        )
        weight = builder.external_tensor_constant(
            "native_external_weight",
            "float32",
            4,
            "[64,3,7,7]",
            "weight",
            "safetensors",
            "resnet.safetensors",
            "conv1.weight",
            128,
            37632,
            "a" * 64,
            "OIHW",
        )
        bias = builder.external_tensor_constant(
            "native_external_bias",
            "float32",
            1,
            "[64]",
            "bias",
            "safetensors",
            "resnet.safetensors",
            "conv1.bias",
            37760,
            256,
            "b" * 64,
            "C",
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

        builder.append_program_unit_marker(pu, weight)
        builder.append_program_unit_marker(pu, conv2d)
        markers = builder.inspect_program_unit_markers(pu)
        weight_marker = next(
            marker
            for marker in markers
            if marker["opcode"] == "common.tensor_const"
            and "#conv1.weight" in str(marker["payload"])
        )

        self.assertGreater(weight.symbol, 0)
        self.assertEqual(weight.metadata["tensor_role"], "weight")
        self.assertEqual(weight.metadata["storage_file"], "resnet.safetensors")
        self.assertEqual(weight.metadata["storage_tensor_key"], "conv1.weight")
        self.assertEqual(weight.metadata["storage_byte_length"], "37632")
        self.assertIn("value_kind=external_data", str(weight_marker["payload"]))
        self.assertIn(
            "value=safetensors://resnet.safetensors#conv1.weight",
            str(weight_marker["payload"]),
        )
        self.assertIn("offset=128", str(weight_marker["payload"]))
        self.assertIn("length=37632", str(weight_marker["payload"]))
        self.assertEqual(markers[-1]["opcode"], "cnn.conv2d")
        self.assertIn("kid1=native_external_weight", str(markers[-1]["payload"]))
        self.assertIn("kid2=native_external_bias", str(markers[-1]["payload"]))

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
            ["common.model_input", "common.model_input", "common.add"],
        )
        self.assertIn("name=input0", str(markers[-3]["payload"]))
        self.assertIn("attr.input_ordinal=0", str(markers[-3]["payload"]))
        self.assertIn("kid0=input0", str(markers[-1]["payload"]))

        with tempfile.TemporaryDirectory() as work_dir:
            output = Path(work_dir) / "native_model.B"
            save_as_whirl(module, str(output))

            self.assertTrue(output.exists())
            self.assertGreater(output.stat().st_size, 0)

    def test_native_finalize_runs_gatekeeper_before_writing_artifact(self) -> None:
        options = WhirlExportOptions(backend="native", model_name="native_bad")
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
                "[1,4]",
                {
                    "dtype": "float32",
                    "rank": 2,
                    "logical_shape": "[1,4]",
                },
            ),
        ]
        values = [
            WhirlValueRecord("input0", 10, "input0_type", "example_input"),
            WhirlValueRecord("input1", 11, "input1_type", "example_input"),
        ]
        invalid_module = WhirlModule(
            options=options,
            model_name="native_bad",
            input_count=2,
            entry_function=WhirlProgramUnitRecord("forward", 20),
            graph_source="synthetic",
            operators=[common.ADD],
            tensor_types=tensor_types,
            values=values,
            graph_operators=[
                WhirlOperatorRecord(
                    common.ADD,
                    30,
                    ["input0", "input1"],
                    {"attr.broadcast_rule": "none"},
                )
            ],
        )

        with tempfile.TemporaryDirectory() as work_dir:
            output = Path(work_dir) / "invalid_native_model.B"
            with self.assertRaisesRegex(WhirlVerificationError, "shape"):
                save_as_whirl(invalid_module, str(output))

            self.assertFalse(output.exists())


if __name__ == "__main__":
    unittest.main()
