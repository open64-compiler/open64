from __future__ import annotations

import contextlib
import hashlib
import io
import importlib.util
import json
import struct
import tempfile
import unittest
from pathlib import Path

from open64_dsc.backend import load_backend
from open64_dsc.cli import _load_model, _parse_shape_spec, run as cli_run
from open64_dsc.interpreter import WhirlExportInterpreter
from open64_dsc.module import (
    WhirlOperatorRecord,
    WhirlProgramUnitRecord,
    WhirlTensorPayloadRecord,
    WhirlTensorTypeRecord,
    WhirlValueRecord,
)
from open64_dsc import WhirlExportOptions, WhirlModule
from open64_dsc import WhirlVerificationError, export_to_whirl
from open64_dsc import load_builder, save_as_whirl, verify_module
from open64_dsc.builder import ValueHandle, WhirlBuilder
from open64_dsc.mapping.contract import all_operator_contracts


class DummyModel:
    pass


class FailingNativeBackend:
    def backend_name(self) -> str:
        return "native"

    def create_operator(self, opcode_name, version, kids, attrs):
        raise RuntimeError("native operator unavailable")


class Open64DscSkeletonTest(unittest.TestCase):
    def _gatekeeper_module(
        self,
        *,
        options: WhirlExportOptions = WhirlExportOptions(),
        tensor_types=None,
        values=None,
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
        default_values = [
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
        ]
        resolved_values = values or default_values
        return WhirlModule(
            options=options,
            model_name="GatekeeperModel",
            input_count=len(resolved_values),
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
            values=resolved_values,
            graph_operators=graph_operators or default_graph_operators,
        )

    def _gatekeeper_tensor_type(
        self,
        name: str,
        handle: int,
        logical_shape: str,
        dtype: str = "float32",
    ) -> WhirlTensorTypeRecord:
        body = logical_shape[1:-1]
        rank = 0 if not body else len(body.split(","))
        return WhirlTensorTypeRecord(
            name,
            handle,
            dtype,
            rank,
            logical_shape,
            {
                "dtype": dtype,
                "rank": rank,
                "logical_shape": logical_shape,
                "lineage": name,
            },
        )

    def _gatekeeper_value(
        self,
        name: str,
        handle: int,
        type_name: str,
        value_kind: str = "example_input",
    ) -> WhirlValueRecord:
        return WhirlValueRecord(
            name,
            handle,
            type_name,
            value_kind,
            metadata={"logical_shape": ""},
        )

    def _external_payload_module(
        self,
        payload_data: bytes = b"\x00\x00\x80?\x00\x00\x00@",
    ) -> WhirlModule:
        checksum = hashlib.sha256(payload_data).hexdigest()
        tensor_type = WhirlTensorTypeRecord(
            "weight_type",
            1,
            "float32",
            1,
            "[2]",
            {
                "dtype": "float32",
                "rank": 1,
                "logical_shape": "[2]",
                "lineage": "weight",
            },
        )
        value = WhirlValueRecord(
            "weight",
            2,
            "weight_type",
            "external_data",
            symbol_handle=3,
            metadata={
                "source_layer_name": "weight",
                "lowering_hint": "external_tensor_constant",
                "tensor_role": "weight",
                "storage_dtype": "float32",
                "storage_shape": "[2]",
                "storage_layout": "contiguous",
                "storage_format": "safetensors",
                "storage_file": "UnitPayload.safetensors",
                "storage_tensor_key": "weight",
                "storage_byte_offset": "0",
                "storage_byte_length": str(len(payload_data)),
                "storage_checksum": checksum,
            },
        )
        return WhirlModule(
            options=WhirlExportOptions(),
            model_name="UnitPayload",
            input_count=0,
            entry_function=WhirlProgramUnitRecord("forward", 4, ["weight"]),
            graph_source="unit",
            operators=[],
            tensor_types=[tensor_type],
            values=[value],
            tensor_payloads=[
                WhirlTensorPayloadRecord(
                    "UnitPayload.safetensors",
                    "weight",
                    "float32",
                    "[2]",
                    0,
                    len(payload_data),
                    checksum,
                    payload_data,
                )
            ],
            graph_operators=[],
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
        self.assertEqual(
            module.graph_operators[0].metadata["lowering_hint"],
            "synthetic_add",
        )
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
            "model_input",
        )
        self.assertEqual(module.values[0].value_kind, "model_input")
        self.assertEqual(module.values[0].metadata["input_ordinal"], "0")

    def test_operator_record_manifest_includes_metadata(self) -> None:
        operator = WhirlOperatorRecord(
            "common.relu",
            42,
            ["input0"],
            {},
            metadata={
                "fx_node_name": "relu",
                "lowering_hint": "fx:common.relu",
            },
        )

        self.assertEqual(
            operator.to_manifest()["metadata"],
            {
                "fx_node_name": "relu",
                "lowering_hint": "fx:common.relu",
            },
        )

    def test_options_validate_backend(self) -> None:
        with self.assertRaises(ValueError):
            WhirlExportOptions(backend="unknown")

    def test_options_default_to_verification_enabled(self) -> None:
        self.assertTrue(WhirlExportOptions().verify)

    def test_published_resnet_operator_contracts_do_not_drift(self) -> None:
        contracts = all_operator_contracts()
        expected = {
            "common.model_input": (2, 0, ("attr.input_ordinal",)),
            "common.tensor_const": (1, 0, ("value_kind", "value")),
            "common.add": (1, 2, ("attr.broadcast_rule",)),
            "common.matmul": (
                1,
                2,
                ("attr.transpose_kid0", "attr.transpose_kid1"),
            ),
            "common.relu": (2, 1, ()),
            "common.flatten": (
                2,
                1,
                ("attr.start_dim", "attr.end_dim"),
            ),
            "common.residual_add": (
                2,
                2,
                (
                    "attr.broadcast_rule",
                    "attr.shape_check",
                    "attr.residual_path",
                ),
            ),
            "common.linear": (
                2,
                3,
                (
                    "attr.has_bias",
                    "attr.transpose_input",
                    "attr.transpose_weight",
                    "attr.weight_layout",
                ),
            ),
            "common.output_logits": (2, 1, ("attr.semantic",)),
            "cnn.conv2d": (
                2,
                3,
                (
                    "attr.kernel_shape",
                    "attr.stride",
                    "attr.padding",
                    "attr.dilation",
                    "attr.groups",
                    "attr.input_layout",
                    "attr.weight_layout",
                    "attr.output_layout",
                ),
            ),
            "cnn.batch_norm_infer": (
                2,
                5,
                (
                    "attr.epsilon",
                    "attr.training",
                    "attr.input_layout",
                    "attr.channel_axis",
                ),
            ),
            "cnn.max_pool2d": (
                2,
                1,
                (
                    "attr.kernel_shape",
                    "attr.stride",
                    "attr.padding",
                    "attr.dilation",
                    "attr.ceil_mode",
                ),
            ),
            "cnn.global_avg_pool2d": (
                2,
                1,
                ("attr.output_size", "attr.reduction_axes"),
            ),
        }

        self.assertEqual(set(contracts), set(expected))
        for name, (version, arity, attrs) in expected.items():
            self.assertEqual(contracts[name].version, version)
            self.assertEqual(contracts[name].arity, arity)
            self.assertEqual(tuple(contracts[name].required_attrs), attrs)

    def test_builder_uses_published_operator_versions(self) -> None:
        builder = load_builder("mock")
        lhs = builder.tensor_constant(
            "version_lhs",
            "float32",
            4,
            "[1,3,8,8]",
            "splat",
            "1.0",
        )
        rhs = builder.tensor_constant(
            "version_rhs",
            "float32",
            4,
            "[1,3,8,8]",
            "splat",
            "2.0",
        )
        add = builder.common_add(lhs, rhs)
        relu = builder.common_relu(lhs)
        residual = builder.common_residual_add(lhs, rhs)
        logits = builder.common_output_logits(lhs)
        pu = builder.minimal_program_unit("version_forward")
        for value in (add, relu, residual, logits):
            builder.append_program_unit_value(pu, value)

        annotations = builder.inspect_program_unit_values(pu)

        self.assertEqual(
            [(item["opcode"], item["version"]) for item in annotations],
            [
                ("common.add", 1),
                ("common.relu", 2),
                ("common.residual_add", 2),
                ("common.output_logits", 2),
            ],
        )
        self.assertIn(
            "attr.semantic=classifier_logits",
            str(annotations[-1]["payload"]),
        )

    def test_native_capability_failure_names_operator_version(self) -> None:
        builder = WhirlBuilder(FailingNativeBackend())

        with self.assertRaisesRegex(
            RuntimeError,
            r"native backend does not support common\.relu\.v2",
        ):
            builder.common_relu(ValueHandle(1))

    def test_native_capability_failure_names_g1_value_source(self) -> None:
        builder = WhirlBuilder(FailingNativeBackend())

        with self.assertRaisesRegex(
            RuntimeError,
            r"native backend does not support common\.model_input\.v2",
        ):
            builder.model_input("input0", ValueHandle(1), 0)  # type: ignore[arg-type]

    def test_mock_backend_creates_model_input_value(self) -> None:
        builder = load_builder("mock")
        tensor_type = builder.tensor_type("model_input_type", "float32", 4, "[1,3,8,8]")
        value = builder.model_input("input0", tensor_type, 0)
        pu = builder.minimal_program_unit("model_input_forward")

        builder.append_program_unit_value(pu, value)
        annotations = builder.inspect_program_unit_values(pu)

        self.assertEqual(annotations[-1]["opcode"], "common.model_input")
        self.assertEqual(annotations[-1]["version"], 2)
        self.assertIn("attr.input_ordinal=0", str(annotations[-1]["payload"]))

    def test_gatekeeper_accepts_valid_module(self) -> None:
        verify_module(self._gatekeeper_module())

    def test_save_as_whirl_writes_safetensors_side_file(self) -> None:
        module = self._external_payload_module()

        with tempfile.TemporaryDirectory() as work_dir:
            artifact = Path(work_dir) / "model.B"
            save_as_whirl(module, str(artifact))
            side_file = Path(work_dir) / "UnitPayload.safetensors"
            payload = side_file.read_bytes()
            text = artifact.read_text(encoding="utf-8")

        header_length = struct.unpack("<Q", payload[:8])[0]
        header = json.loads(payload[8:8 + header_length].decode("utf-8"))
        data = payload[8 + header_length:]

        self.assertEqual(header["weight"]["dtype"], "F32")
        self.assertEqual(header["weight"]["shape"], [2])
        self.assertEqual(header["weight"]["data_offsets"], [0, 8])
        self.assertEqual(
            header["weight"]["open64_sha256"],
            module.tensor_payloads[0].checksum,
        )
        self.assertEqual(data, module.tensor_payloads[0].data)
        self.assertIn(
            "tensor_payload.0=UnitPayload.safetensors:weight:float32:[2]:0:8:",
            text,
        )

    def test_gatekeeper_rejects_missing_external_payload_when_strict(self) -> None:
        module = self._external_payload_module()
        stale_payload = WhirlTensorPayloadRecord(
            "UnitPayload.safetensors",
            "other_weight",
            "float32",
            "[2]",
            0,
            len(module.tensor_payloads[0].data),
            module.tensor_payloads[0].checksum,
            module.tensor_payloads[0].data,
        )
        stale_module = WhirlModule(
            options=module.options,
            model_name=module.model_name,
            input_count=module.input_count,
            entry_function=module.entry_function,
            graph_source=module.graph_source,
            operators=module.operators,
            tensor_types=module.tensor_types,
            values=module.values,
            tensor_payloads=[stale_payload],
            graph_operators=module.graph_operators,
        )

        with self.assertRaisesRegex(WhirlVerificationError, "payload"):
            verify_module(stale_module)

    def test_gatekeeper_rejects_external_payload_checksum_mismatch(self) -> None:
        module = self._external_payload_module()
        stale_payload = WhirlTensorPayloadRecord(
            module.tensor_payloads[0].storage_file,
            module.tensor_payloads[0].tensor_key,
            module.tensor_payloads[0].dtype,
            module.tensor_payloads[0].logical_shape,
            module.tensor_payloads[0].byte_offset,
            module.tensor_payloads[0].byte_length,
            "bad-checksum",
            module.tensor_payloads[0].data,
        )
        stale_module = WhirlModule(
            options=module.options,
            model_name=module.model_name,
            input_count=module.input_count,
            entry_function=module.entry_function,
            graph_source=module.graph_source,
            operators=module.operators,
            tensor_types=module.tensor_types,
            values=module.values,
            tensor_payloads=[stale_payload],
            graph_operators=module.graph_operators,
        )

        with self.assertRaisesRegex(WhirlVerificationError, "checksum"):
            verify_module(stale_module)

    def test_gatekeeper_rejects_duplicate_external_payload_key(self) -> None:
        module = self._external_payload_module()
        duplicate_module = WhirlModule(
            options=module.options,
            model_name=module.model_name,
            input_count=module.input_count,
            entry_function=module.entry_function,
            graph_source=module.graph_source,
            operators=module.operators,
            tensor_types=module.tensor_types,
            values=module.values,
            tensor_payloads=[
                module.tensor_payloads[0],
                module.tensor_payloads[0],
            ],
            graph_operators=module.graph_operators,
        )

        with self.assertRaisesRegex(WhirlVerificationError, "duplicate"):
            verify_module(duplicate_module)

    def test_gatekeeper_rejects_overlapping_external_payload_ranges(self) -> None:
        module = self._external_payload_module()
        other_data = b"\x00\x00@@"
        other_checksum = hashlib.sha256(other_data).hexdigest()
        overlapping_payload = WhirlTensorPayloadRecord(
            module.tensor_payloads[0].storage_file,
            "other_weight",
            "float32",
            "[1]",
            4,
            len(other_data),
            other_checksum,
            other_data,
        )
        overlap_module = WhirlModule(
            options=module.options,
            model_name=module.model_name,
            input_count=module.input_count,
            entry_function=module.entry_function,
            graph_source=module.graph_source,
            operators=module.operators,
            tensor_types=module.tensor_types,
            values=module.values,
            tensor_payloads=[
                module.tensor_payloads[0],
                overlapping_payload,
            ],
            graph_operators=module.graph_operators,
        )

        with self.assertRaisesRegex(WhirlVerificationError, "overlapping"):
            verify_module(overlap_module)

    def test_gatekeeper_rejects_external_payload_shape_mismatch(self) -> None:
        module = self._external_payload_module()
        stale_payload = WhirlTensorPayloadRecord(
            module.tensor_payloads[0].storage_file,
            module.tensor_payloads[0].tensor_key,
            module.tensor_payloads[0].dtype,
            "[1,2]",
            module.tensor_payloads[0].byte_offset,
            module.tensor_payloads[0].byte_length,
            module.tensor_payloads[0].checksum,
            module.tensor_payloads[0].data,
        )
        stale_module = WhirlModule(
            options=module.options,
            model_name=module.model_name,
            input_count=module.input_count,
            entry_function=module.entry_function,
            graph_source=module.graph_source,
            operators=module.operators,
            tensor_types=module.tensor_types,
            values=module.values,
            tensor_payloads=[stale_payload],
            graph_operators=module.graph_operators,
        )

        with self.assertRaisesRegex(WhirlVerificationError, "shape"):
            verify_module(stale_module)

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
                    "attr.residual_path": "true",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "broadcast"):
            verify_module(self._gatekeeper_module(graph_operators=graph_operators))

    def test_gatekeeper_accepts_valid_matmul(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[2,3]"),
            self._gatekeeper_tensor_type("input1_type", 2, "[3,4]"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "common.matmul",
                5,
                ["input0", "input1"],
                {
                    "attr.transpose_kid0": "false",
                    "attr.transpose_kid1": "false",
                },
            )
        ]

        verify_module(
            self._gatekeeper_module(
                tensor_types=tensor_types,
                graph_operators=graph_operators,
            )
        )

    def test_gatekeeper_rejects_matmul_dimension_mismatch(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[2,3]"),
            self._gatekeeper_tensor_type("input1_type", 2, "[2,4]"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "common.matmul",
                5,
                ["input0", "input1"],
                {
                    "attr.transpose_kid0": "false",
                    "attr.transpose_kid1": "false",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "matrix"):
            verify_module(
                self._gatekeeper_module(
                    tensor_types=tensor_types,
                    graph_operators=graph_operators,
                )
            )

    def test_gatekeeper_rejects_batched_matmul_for_now(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,2,3]"),
            self._gatekeeper_tensor_type("input1_type", 2, "[1,3,4]"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "common.matmul",
                5,
                ["input0", "input1"],
                {
                    "attr.transpose_kid0": "false",
                    "attr.transpose_kid1": "false",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "batched matmul"):
            verify_module(
                self._gatekeeper_module(
                    tensor_types=tensor_types,
                    graph_operators=graph_operators,
                )
            )

    def test_gatekeeper_rejects_matmul_missing_transpose_attr(self) -> None:
        graph_operators = [
            WhirlOperatorRecord(
                "common.matmul",
                5,
                ["input0", "input1"],
                {"attr.transpose_kid0": "false"},
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "transpose_kid1"):
            verify_module(self._gatekeeper_module(graph_operators=graph_operators))

    def test_gatekeeper_accepts_valid_linear(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[4,8]"),
            self._gatekeeper_tensor_type("input1_type", 2, "[16,8]"),
            self._gatekeeper_tensor_type("input2_type", 3, "[16]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("input1", 11, "input1_type"),
            self._gatekeeper_value("input2", 12, "input2_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "common.linear",
                5,
                ["input0", "input1", "input2"],
                {
                    "attr.has_bias": "true",
                    "attr.transpose_input": "false",
                    "attr.transpose_weight": "true",
                    "attr.weight_layout": "OI",
                },
            )
        ]

        verify_module(
            self._gatekeeper_module(
                tensor_types=tensor_types,
                values=values,
                graph_operators=graph_operators,
            )
        )

    def test_gatekeeper_accepts_linear_without_bias_sentinel(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[4,8]"),
            self._gatekeeper_tensor_type("input1_type", 2, "[16,8]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("input1", 11, "input1_type"),
            WhirlValueRecord(
                "input2",
                12,
                "",
                "absent_parameter",
            ),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "common.linear",
                5,
                ["input0", "input1", "input2"],
                {
                    "attr.has_bias": "false",
                    "attr.transpose_input": "false",
                    "attr.transpose_weight": "true",
                    "attr.weight_layout": "OI",
                },
            )
        ]

        verify_module(
            self._gatekeeper_module(
                tensor_types=tensor_types,
                values=values,
                graph_operators=graph_operators,
            )
        )

    def test_gatekeeper_accepts_linear_after_pool_shape_inference(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,3,8,8]"),
            self._gatekeeper_tensor_type("input1_type", 2, "[16,48]"),
            self._gatekeeper_tensor_type("input2_type", 3, "[16]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("input1", 11, "input1_type"),
            self._gatekeeper_value("input2", 12, "input2_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.max_pool2d",
                5,
                ["input0"],
                {
                    "attr.kernel_shape": "3,3",
                    "attr.stride": "2,2",
                    "attr.padding": "1,1",
                    "attr.dilation": "1,1",
                    "attr.ceil_mode": "false",
                },
            ),
            WhirlOperatorRecord(
                "common.flatten",
                6,
                ["cnn.max_pool2d"],
                {
                    "attr.start_dim": "1",
                    "attr.end_dim": "-1",
                },
            ),
            WhirlOperatorRecord(
                "common.linear",
                7,
                ["common.flatten", "input1", "input2"],
                {
                    "attr.has_bias": "true",
                    "attr.transpose_input": "false",
                    "attr.transpose_weight": "true",
                    "attr.weight_layout": "OI",
                },
            ),
        ]

        verify_module(
            self._gatekeeper_module(
                tensor_types=tensor_types,
                values=values,
                graph_operators=graph_operators,
            )
        )

    def test_gatekeeper_rejects_linear_input_weight_mismatch(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[4,8]"),
            self._gatekeeper_tensor_type("input1_type", 2, "[16,7]"),
            self._gatekeeper_tensor_type("input2_type", 3, "[16]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("input1", 11, "input1_type"),
            self._gatekeeper_value("input2", 12, "input2_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "common.linear",
                5,
                ["input0", "input1", "input2"],
                {
                    "attr.has_bias": "true",
                    "attr.transpose_input": "false",
                    "attr.transpose_weight": "true",
                    "attr.weight_layout": "OI",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "dimensions"):
            verify_module(
                self._gatekeeper_module(
                    tensor_types=tensor_types,
                    values=values,
                    graph_operators=graph_operators,
                )
            )

    def test_gatekeeper_rejects_linear_bias_mismatch(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[4,8]"),
            self._gatekeeper_tensor_type("input1_type", 2, "[16,8]"),
            self._gatekeeper_tensor_type("input2_type", 3, "[15]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("input1", 11, "input1_type"),
            self._gatekeeper_value("input2", 12, "input2_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "common.linear",
                5,
                ["input0", "input1", "input2"],
                {
                    "attr.has_bias": "true",
                    "attr.transpose_input": "false",
                    "attr.transpose_weight": "true",
                    "attr.weight_layout": "OI",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "bias"):
            verify_module(
                self._gatekeeper_module(
                    tensor_types=tensor_types,
                    values=values,
                    graph_operators=graph_operators,
                )
            )

    def test_gatekeeper_rejects_linear_missing_transpose_attr(self) -> None:
        graph_operators = [
            WhirlOperatorRecord(
                "common.linear",
                5,
                ["input0", "input1", "input1"],
                {
                    "attr.has_bias": "true",
                    "attr.transpose_weight": "true",
                    "attr.weight_layout": "OI",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "transpose_input"):
            verify_module(self._gatekeeper_module(graph_operators=graph_operators))

    def test_gatekeeper_accepts_valid_conv2d(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,3,8,8]"),
            self._gatekeeper_tensor_type("weight_type", 2, "[16,3,3,3]"),
            self._gatekeeper_tensor_type("bias_type", 3, "[16]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("weight", 11, "weight_type"),
            self._gatekeeper_value("bias", 12, "bias_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.conv2d",
                5,
                ["input0", "weight", "bias"],
                {
                    "attr.kernel_shape": "3,3",
                    "attr.stride": "2,2",
                    "attr.padding": "1,1",
                    "attr.dilation": "1,1",
                    "attr.groups": "1",
                    "attr.input_layout": "NCHW",
                    "attr.weight_layout": "OIHW",
                    "attr.output_layout": "NCHW",
                },
            )
        ]

        verify_module(
            self._gatekeeper_module(
                tensor_types=tensor_types,
                values=values,
                graph_operators=graph_operators,
            )
        )

    def test_gatekeeper_rejects_conv2d_channel_mismatch(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,3,8,8]"),
            self._gatekeeper_tensor_type("weight_type", 2, "[16,4,3,3]"),
            self._gatekeeper_tensor_type("bias_type", 3, "[16]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("weight", 11, "weight_type"),
            self._gatekeeper_value("bias", 12, "bias_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.conv2d",
                5,
                ["input0", "weight", "bias"],
                {
                    "attr.kernel_shape": "3,3",
                    "attr.stride": "1,1",
                    "attr.padding": "0,0",
                    "attr.dilation": "1,1",
                    "attr.groups": "1",
                    "attr.input_layout": "NCHW",
                    "attr.weight_layout": "OIHW",
                    "attr.output_layout": "NCHW",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "channels"):
            verify_module(
                self._gatekeeper_module(
                    tensor_types=tensor_types,
                    values=values,
                    graph_operators=graph_operators,
                )
            )

    def test_gatekeeper_accepts_grouped_conv2d(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,4,8,8]"),
            self._gatekeeper_tensor_type("weight_type", 2, "[8,2,3,3]"),
            self._gatekeeper_tensor_type("bias_type", 3, "[8]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("weight", 11, "weight_type"),
            self._gatekeeper_value("bias", 12, "bias_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.conv2d",
                5,
                ["input0", "weight", "bias"],
                {
                    "attr.kernel_shape": "3,3",
                    "attr.stride": "1,1",
                    "attr.padding": "1,1",
                    "attr.dilation": "1,1",
                    "attr.groups": "2",
                    "attr.input_layout": "NCHW",
                    "attr.weight_layout": "OIHW",
                    "attr.output_layout": "NCHW",
                },
            )
        ]

        verify_module(
            self._gatekeeper_module(
                tensor_types=tensor_types,
                values=values,
                graph_operators=graph_operators,
            )
        )

    def test_gatekeeper_rejects_conv2d_layout_mismatch(self) -> None:
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.conv2d",
                5,
                ["input0", "input1", "input1"],
                {
                    "attr.kernel_shape": "3,3",
                    "attr.stride": "1,1",
                    "attr.padding": "0,0",
                    "attr.dilation": "1,1",
                    "attr.groups": "1",
                    "attr.input_layout": "NHWC",
                    "attr.weight_layout": "OIHW",
                    "attr.output_layout": "NCHW",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "input_layout"):
            verify_module(self._gatekeeper_module(graph_operators=graph_operators))

    def test_gatekeeper_rejects_conv2d_bias_mismatch(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,3,8,8]"),
            self._gatekeeper_tensor_type("weight_type", 2, "[16,3,3,3]"),
            self._gatekeeper_tensor_type("bias_type", 3, "[15]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("weight", 11, "weight_type"),
            self._gatekeeper_value("bias", 12, "bias_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.conv2d",
                5,
                ["input0", "weight", "bias"],
                {
                    "attr.kernel_shape": "3,3",
                    "attr.stride": "1,1",
                    "attr.padding": "0,0",
                    "attr.dilation": "1,1",
                    "attr.groups": "1",
                    "attr.input_layout": "NCHW",
                    "attr.weight_layout": "OIHW",
                    "attr.output_layout": "NCHW",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "bias"):
            verify_module(
                self._gatekeeper_module(
                    tensor_types=tensor_types,
                    values=values,
                    graph_operators=graph_operators,
                )
            )

    def test_gatekeeper_accepts_conv2d_absent_bias(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,3,8,8]"),
            self._gatekeeper_tensor_type("weight_type", 2, "[16,3,3,3]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("weight", 11, "weight_type"),
            WhirlValueRecord("bias", 12, "", "absent_parameter"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.conv2d",
                5,
                ["input0", "weight", "bias"],
                {
                    "attr.kernel_shape": "3,3",
                    "attr.stride": "1,1",
                    "attr.padding": "0,0",
                    "attr.dilation": "1,1",
                    "attr.groups": "1",
                    "attr.input_layout": "NCHW",
                    "attr.weight_layout": "OIHW",
                    "attr.output_layout": "NCHW",
                },
            )
        ]

        verify_module(
            self._gatekeeper_module(
                tensor_types=tensor_types,
                values=values,
                graph_operators=graph_operators,
            )
        )

    def test_gatekeeper_accepts_batch_norm_infer(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,16,8,8]"),
            self._gatekeeper_tensor_type("scale_type", 2, "[16]"),
            self._gatekeeper_tensor_type("bias_type", 3, "[16]"),
            self._gatekeeper_tensor_type("mean_type", 4, "[16]"),
            self._gatekeeper_tensor_type("var_type", 5, "[16]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("scale", 11, "scale_type"),
            self._gatekeeper_value("bias", 12, "bias_type"),
            self._gatekeeper_value("mean", 13, "mean_type"),
            self._gatekeeper_value("var", 14, "var_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.batch_norm_infer",
                5,
                ["input0", "scale", "bias", "mean", "var"],
                {
                    "attr.epsilon": "1e-05",
                    "attr.training": "false",
                    "attr.input_layout": "NCHW",
                    "attr.channel_axis": "1",
                },
            )
        ]

        verify_module(
            self._gatekeeper_module(
                tensor_types=tensor_types,
                values=values,
                graph_operators=graph_operators,
            )
        )

    def test_gatekeeper_rejects_batch_norm_channel_mismatch(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,16,8,8]"),
            self._gatekeeper_tensor_type("scale_type", 2, "[15]"),
            self._gatekeeper_tensor_type("bias_type", 3, "[16]"),
            self._gatekeeper_tensor_type("mean_type", 4, "[16]"),
            self._gatekeeper_tensor_type("var_type", 5, "[16]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("scale", 11, "scale_type"),
            self._gatekeeper_value("bias", 12, "bias_type"),
            self._gatekeeper_value("mean", 13, "mean_type"),
            self._gatekeeper_value("var", 14, "var_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.batch_norm_infer",
                5,
                ["input0", "scale", "bias", "mean", "var"],
                {
                    "attr.epsilon": "1e-05",
                    "attr.training": "false",
                    "attr.input_layout": "NCHW",
                    "attr.channel_axis": "1",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "channels"):
            verify_module(
                self._gatekeeper_module(
                    tensor_types=tensor_types,
                    values=values,
                    graph_operators=graph_operators,
                )
            )

    def test_gatekeeper_rejects_batch_norm_training_mode(self) -> None:
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.batch_norm_infer",
                5,
                ["input0", "input1", "input1", "input1", "input1"],
                {
                    "attr.epsilon": "1e-05",
                    "attr.training": "true",
                    "attr.input_layout": "NCHW",
                    "attr.channel_axis": "1",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "training=false"):
            verify_module(self._gatekeeper_module(graph_operators=graph_operators))

    def test_gatekeeper_accepts_pooling_contracts(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,16,8,8]"),
        ]
        values = [self._gatekeeper_value("input0", 10, "input0_type")]
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.max_pool2d",
                5,
                ["input0"],
                {
                    "attr.kernel_shape": "3,3",
                    "attr.stride": "2,2",
                    "attr.padding": "1,1",
                    "attr.dilation": "1,1",
                    "attr.ceil_mode": "false",
                },
            ),
            WhirlOperatorRecord(
                "cnn.global_avg_pool2d",
                6,
                ["cnn.max_pool2d"],
                {
                    "attr.output_size": "1,1",
                    "attr.reduction_axes": "spatial",
                },
            ),
        ]

        verify_module(
            self._gatekeeper_module(
                tensor_types=tensor_types,
                values=values,
                graph_operators=graph_operators,
            )
        )

    def test_gatekeeper_rejects_pooling_missing_attr(self) -> None:
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.max_pool2d",
                5,
                ["input0"],
                {
                    "attr.kernel_shape": "3,3",
                    "attr.stride": "2,2",
                    "attr.padding": "1,1",
                    "attr.ceil_mode": "false",
                },
            )
        ]

        with self.assertRaisesRegex(WhirlVerificationError, "dilation"):
            verify_module(self._gatekeeper_module(graph_operators=graph_operators))

    def test_gatekeeper_infers_cnn_shapes_for_residual_and_linear(self) -> None:
        tensor_types = [
            self._gatekeeper_tensor_type("input0_type", 1, "[1,3,8,8]"),
            self._gatekeeper_tensor_type("weight_type", 2, "[16,3,3,3]"),
            self._gatekeeper_tensor_type("bn_type", 3, "[16]"),
            self._gatekeeper_tensor_type("fc_weight_type", 4, "[10,16]"),
            self._gatekeeper_tensor_type("fc_bias_type", 5, "[10]"),
        ]
        values = [
            self._gatekeeper_value("input0", 10, "input0_type"),
            self._gatekeeper_value("weight", 11, "weight_type"),
            WhirlValueRecord("conv_bias", 12, "", "absent_parameter"),
            self._gatekeeper_value("scale", 13, "bn_type"),
            self._gatekeeper_value("bn_bias", 14, "bn_type"),
            self._gatekeeper_value("mean", 15, "bn_type"),
            self._gatekeeper_value("var", 16, "bn_type"),
            self._gatekeeper_value("fc_weight", 17, "fc_weight_type"),
            self._gatekeeper_value("fc_bias", 18, "fc_bias_type"),
        ]
        graph_operators = [
            WhirlOperatorRecord(
                "cnn.conv2d",
                5,
                ["input0", "weight", "conv_bias"],
                {
                    "attr.kernel_shape": "3,3",
                    "attr.stride": "1,1",
                    "attr.padding": "1,1",
                    "attr.dilation": "1,1",
                    "attr.groups": "1",
                    "attr.input_layout": "NCHW",
                    "attr.weight_layout": "OIHW",
                    "attr.output_layout": "NCHW",
                },
            ),
            WhirlOperatorRecord(
                "cnn.batch_norm_infer",
                6,
                ["cnn.conv2d", "scale", "bn_bias", "mean", "var"],
                {
                    "attr.epsilon": "1e-05",
                    "attr.training": "false",
                    "attr.input_layout": "NCHW",
                    "attr.channel_axis": "1",
                },
            ),
            WhirlOperatorRecord(
                "cnn.global_avg_pool2d",
                7,
                ["cnn.batch_norm_infer"],
                {
                    "attr.output_size": "1,1",
                    "attr.reduction_axes": "spatial",
                },
            ),
            WhirlOperatorRecord(
                "common.flatten",
                8,
                ["cnn.global_avg_pool2d"],
                {
                    "attr.start_dim": "1",
                    "attr.end_dim": "-1",
                },
            ),
            WhirlOperatorRecord(
                "common.linear",
                9,
                ["common.flatten", "fc_weight", "fc_bias"],
                {
                    "attr.has_bias": "true",
                    "attr.transpose_input": "false",
                    "attr.transpose_weight": "true",
                    "attr.weight_layout": "OI",
                },
            ),
        ]

        verify_module(
            self._gatekeeper_module(
                tensor_types=tensor_types,
                values=values,
                graph_operators=graph_operators,
            )
        )

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
        with self.assertRaisesRegex(ValueError, "positive"):
            _parse_shape_spec("shape:1,-1,3")
        with self.assertRaisesRegex(ValueError, "integer"):
            _parse_shape_spec("shape:1,bad,3")

    def test_cli_requires_sample_input(self) -> None:
        with tempfile.TemporaryDirectory() as work_dir:
            model_path = Path(work_dir) / "model.py"
            output_path = Path(work_dir) / "model.B"
            model_path.write_text(
                "\n".join(
                    [
                        "class UnitModel:",
                        "    pass",
                        "def create_model():",
                        "    return UnitModel()",
                    ]
                ) + "\n",
                encoding="utf-8",
            )

            with contextlib.redirect_stderr(io.StringIO()):
                with self.assertRaises(SystemExit) as context:
                    cli_run([str(model_path), "-o", str(output_path)])

            self.assertEqual(context.exception.code, 2)
            self.assertFalse(output_path.exists())

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

    def test_mock_backend_appends_common_add_as_program_unit_value(self) -> None:
        builder = load_builder("mock")
        pu = builder.minimal_program_unit("value_forward")
        lhs = builder.tensor_constant(
            "value_lhs",
            "float32",
            2,
            "[1,4]",
            "splat",
            "1.0",
        )
        rhs = builder.tensor_constant(
            "value_rhs",
            "float32",
            2,
            "[1,4]",
            "splat",
            "2.0",
        )
        add = builder.common_add(lhs, rhs)

        builder.append_program_unit_value(pu, lhs)
        builder.append_program_unit_value(pu, rhs)
        builder.append_program_unit_value(pu, add)
        values = builder.inspect_program_unit_values(pu)
        markers = builder.inspect_program_unit_markers(pu)

        self.assertEqual(
            [value["opcode"] for value in values[-3:]],
            [
                "common.tensor_const",
                "common.tensor_const",
                "common.add",
            ],
        )
        self.assertEqual(values, markers)
        self.assertIn("kid0=value_lhs", str(values[-1]["payload"]))
        self.assertIn("kid1=value_rhs", str(values[-1]["payload"]))

    def test_mock_backend_creates_external_tensor_operands(self) -> None:
        builder = load_builder("mock")

        value = builder.tensor_constant(
            "external_conv_input",
            "float32",
            4,
            "[1,3,224,224]",
            "model_input",
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
            "a" * 64,
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
            "b" * 64,
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
        self.assertIn("storage_format=safetensors", str(markers[-2]["payload"]))
        self.assertIn("side_file=resnet.safetensors", str(markers[-2]["payload"]))
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
        self.assertIn("value.0=input0:input0_type:model_input", text)
        self.assertIn("value_metadata.0=input0:model_input", text)
        self.assertIn("graph_operator.0=common.add:input0,input1", text)


if __name__ == "__main__":
    unittest.main()
