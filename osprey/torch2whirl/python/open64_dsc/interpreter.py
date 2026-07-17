"""Graph traversal placeholder for the future torch.export / FX bridge."""

from __future__ import annotations

from dataclasses import dataclass
import hashlib
import inspect
import os
import operator
import traceback
from typing import Any, Dict, Iterable, List, Mapping, Optional, Sequence, Set, Tuple

from .builder import (
    REGION_INPUT,
    REGION_OUTPUT,
    REGION_RESULT,
    REGION_STATE_LAYER_OWNED,
    REGION_STATE_UNIQUE_OWNERSHIP,
    STATE_EFFECT_MODIFY,
    STATE_MUTABLE_BUFFER,
    STATE_UNIQUE_OWNERSHIP,
    ProgramUnitHandle,
    RegionHandle,
    StateHandle,
    ValueHandle,
    WhirlBuilder,
    load_builder,
)
from .mapping import cnn, common, transformer
from .mapping.contract import operator_arity
from .module import (
    WhirlModule,
    WhirlOperatorRecord,
    WhirlProgramUnitRecord,
    WhirlTensorPayloadRecord,
    WhirlTensorTypeRecord,
    WhirlValueRecord,
)
from .options import WhirlExportOptions
from .python_classes import collect_python_model_classes
from .python_imports import collect_imported_python_callables


@dataclass(frozen=True)
class _MappedOperatorPlan:
    name: str
    attrs: Mapping[str, str]
    metadata: Mapping[str, str]


@dataclass(frozen=True)
class _GraphValue:
    handle: ValueHandle
    name: str


@dataclass
class _RegionState:
    inputs: Set[int] = None  # type: ignore[assignment]
    values: Set[int] = None  # type: ignore[assignment]
    declare_inputs: bool = True

    def __post_init__(self) -> None:
        if self.inputs is None:
            self.inputs = set()
        if self.values is None:
            self.values = set()


class WhirlExportInterpreter:
    def __init__(self, options: WhirlExportOptions):
        self._options = options
        self._builder: Optional[WhirlBuilder] = None

    def builder(self) -> WhirlBuilder:
        if self._builder is None:
            self._builder = load_builder(self._options.backend)
        return self._builder

    def export(self, model: Any, example_inputs: Iterable[Any]) -> WhirlModule:
        builder = self.builder()
        builder.begin_program()
        try:
            module = self._export_program(model, example_inputs)
            self._bind_model_source_positions(model, module)
            builder.verify_program()
            return module
        except Exception:
            builder.abort_program()
            raise

    def _export_program(
        self,
        model: Any,
        example_inputs: Iterable[Any],
    ) -> WhirlModule:
        inputs = list(example_inputs)
        if self._options.pu_mode == "multiple":
            return self._export_multiple_pu_program(model, inputs)

        model_name = self._model_name(model)
        entry_name = self._model_class_name(model)
        entry_pu = self.builder().minimal_program_unit(entry_name)
        tensor_types, values, handles = self._build_input_placeholders(inputs)
        tensor_payloads: List[WhirlTensorPayloadRecord] = []
        graph_operators: List[WhirlOperatorRecord] = []
        operators: List[str] = []
        body_markers: List[str] = []
        for value, handle in zip(values, handles):
            self.builder().append_program_unit_value(entry_pu, handle)
            body_markers.append(value.name)

        is_tiny_llama2_prefill = self._is_tiny_llama2_prefill_model(model)
        is_tiny_llama2_decode = self._is_tiny_llama2_decode_model(model)

        if is_tiny_llama2_decode:
            graph_source = "torch.fx+llama2_decode_semantic"
            self._emit_llama2_decode_model(
                model,
                model_name,
                entry_pu,
                handles,
                tensor_types,
                values,
                tensor_payloads,
                operators,
                body_markers,
                graph_operators,
            )
        elif is_tiny_llama2_prefill:
            graph_source = "torch.fx+llama2_semantic"
            self._emit_llama2_model(
                model,
                model_name,
                entry_pu,
                handles,
                tensor_types,
                values,
                tensor_payloads,
                operators,
                body_markers,
                graph_operators,
            )
        else:
            captured_graph = self._capture_fx_graph(model)
            graph_source = "torch.fx" if captured_graph is not None else "synthetic"

        if (
            not is_tiny_llama2_prefill and
            not is_tiny_llama2_decode and
            captured_graph is not None
        ):
            self._emit_captured_graph(
                captured_graph,
                model_name,
                entry_pu,
                handles,
                tensor_types,
                values,
                tensor_payloads,
                operators,
                body_markers,
                graph_operators,
            )
        elif (
            not is_tiny_llama2_prefill and
            not is_tiny_llama2_decode and
            len(handles) >= 2
        ):
            handle, attrs = self._emit_operator(
                common.ADD,
                handles[:2],
                {},
            )
            self.builder().append_program_unit_value(entry_pu, handle)
            operators.append(common.ADD)
            body_markers.append(common.ADD)
            graph_operators.append(
                WhirlOperatorRecord(
                    name=common.ADD,
                    handle=handle.value,
                    kids=[value.name for value in values[:2]],
                    attrs=attrs,
                    metadata={"lowering_hint": "synthetic_add"},
                )
            )

        model_module = inspect.getmodule(type(model))
        class_definitions, class_instances = collect_python_model_classes(model)
        return WhirlModule(
            options=self._options,
            model_name=model_name,
            input_count=len(inputs),
            entry_function=WhirlProgramUnitRecord(
                name=entry_name,
                handle=entry_pu.value,
                body_markers=body_markers,
            ),
            graph_source=graph_source,
            operators=operators,
            tensor_types=tensor_types,
            values=values,
            tensor_payloads=tensor_payloads,
            graph_operators=graph_operators,
            python_imports=(
                collect_imported_python_callables(model_module)
                if model_module is not None else ()
            ),
            python_class_definitions=class_definitions,
            python_class_instances=class_instances,
        )

    def _export_multiple_pu_program(
        self,
        model: Any,
        inputs: Sequence[Any],
    ) -> WhirlModule:
        if (
            not self._is_tiny_llama2_prefill_model(model) and
            not self._is_tiny_llama2_decode_model(model)
        ):
            raise NotImplementedError(
                "multiple-PU emission currently supports the tiny Llama "
                "prefill/decode fixtures"
            )

        model_name = self._model_name(model)
        entry_name = self._model_class_name(model)
        class_definitions, class_instances = collect_python_model_classes(model)
        definitions = {
            definition.class_name: definition
            for definition in class_definitions
        }
        entry_definition = definitions.get(entry_name)
        rms_definition = definitions.get("TinyRMSNorm")
        ffn_definition = definitions.get("TinyLlama2FeedForward")
        if (
            entry_definition is None or
            rms_definition is None or
            ffn_definition is None
        ):
            raise NotImplementedError(
                "multiple-PU emission requires class source definitions"
            )
        entry_identity = self._callable_identity_metadata(
            entry_definition,
            model,
            "<model>",
            entry_name,
        )
        rms_module = self._module_at_instance_path(model, "norm")
        rms_instance = self._first_instance_path(
            class_instances,
            rms_definition.canonical_name,
        )
        rms_context = f"{entry_name}.{rms_instance}"
        rms_identity = self._callable_identity_metadata(
            rms_definition,
            rms_module,
            rms_instance,
            rms_context,
        )
        ffn_instance = self._first_instance_path(
            class_instances,
            ffn_definition.canonical_name,
        )
        ffn_module = self._module_at_instance_path(model, ffn_instance)
        ffn_context = f"{entry_name}.{ffn_instance}"
        ffn_identity = self._callable_identity_metadata(
            ffn_definition,
            ffn_module,
            ffn_instance,
            ffn_context,
        )

        hidden_shape = "[1,1,32]" if self._is_tiny_llama2_decode_model(model) \
            else "[1,8,32]"
        intermediate_shape = (
            "[1,1,88]" if self._is_tiny_llama2_decode_model(model)
            else "[1,8,88]"
        )
        tensor_type = self.builder().tensor_type(
            "llama2_multi_pu_hidden_type",
            "float32",
            3,
            hidden_shape,
            {
                "dtype": "float32",
                "rank": 3,
                "logical_shape": hidden_shape,
                "layout": "BSC",
                "lineage": "python.multi_pu.hidden",
            },
        )
        scale_type = self.builder().tensor_type(
            "llama2_multi_pu_scale_type",
            "float32",
            1,
            "[32]",
            {
                "dtype": "float32",
                "rank": 1,
                "logical_shape": "[32]",
                "layout": "C",
                "lineage": "python.multi_pu.rms_norm.scale",
            },
        )
        intermediate_type = self.builder().tensor_type(
            "llama2_multi_pu_intermediate_type",
            "float32",
            3,
            intermediate_shape,
            {
                "dtype": "float32",
                "rank": 3,
                "logical_shape": intermediate_shape,
                "layout": "BSC",
                "lineage": "python.multi_pu.feed_forward.intermediate",
            },
        )
        ffn_up_weight_type = self.builder().tensor_type(
            "llama2_multi_pu_ffn_up_weight_type",
            "float32",
            2,
            "[88,32]",
            {
                "dtype": "float32",
                "rank": 2,
                "logical_shape": "[88,32]",
                "layout": "OI",
                "lineage": "python.multi_pu.feed_forward.up_weight",
            },
        )
        ffn_down_weight_type = self.builder().tensor_type(
            "llama2_multi_pu_ffn_down_weight_type",
            "float32",
            2,
            "[32,88]",
            {
                "dtype": "float32",
                "rank": 2,
                "logical_shape": "[32,88]",
                "layout": "OI",
                "lineage": "python.multi_pu.feed_forward.down_weight",
            },
        )

        rms_pu = self.builder().minimal_program_unit("TinyRMSNorm")
        self._set_pu_source_identity(rms_pu, rms_identity)
        rms_file = self.builder().register_source_file(
            rms_pu,
            rms_definition.source_file,
        )
        rms_line = rms_definition.source_line
        hidden = self.builder().declare_pu_formal(
            rms_pu,
            "hidden_states",
            0,
            tensor_type,
            rms_file,
            rms_line,
        )
        self.builder().attach_value_metadata(
            hidden,
            self._multi_pu_value_metadata(
                rms_identity,
                "hidden_states",
                "activation",
                "",
            ),
        )
        scale = self.builder().declare_pu_formal(
            rms_pu,
            "rms_norm_scale",
            1,
            scale_type,
            rms_file,
            rms_line,
        )
        scale_metadata = self._multi_pu_value_metadata(
            rms_identity,
            "rms_norm_scale",
            "rms_norm_scale",
            "weight",
        )
        self.builder().attach_value_metadata(scale, scale_metadata)
        self.builder().declare_pu_result(
            rms_pu,
            "normalized_result",
            0,
            tensor_type,
            file_id=rms_file,
            line=rms_line + 1,
        )
        normalized = self.builder().transformer_rms_norm(
            hidden,
            scale,
            self._rms_norm_attrs(rms_module),
        )
        normalized_metadata = self._multi_pu_value_metadata(
            rms_identity,
            "transformer_rms_norm",
            "operator_result",
            "",
        )
        normalized_metadata["lowering_hint"] = "llama2:transformer.rms_norm"
        normalized_metadata["semantic_name"] = "rms_norm"
        self.builder().attach_value_metadata(normalized, normalized_metadata)
        self.builder().set_value_source_position(
            normalized,
            rms_file,
            rms_line + 1,
        )
        self.builder().append_program_unit_value(rms_pu, normalized)
        self.builder().return_pu_values(rms_pu, [normalized])

        ffn_pu = self.builder().minimal_program_unit("TinyLlama2FeedForward")
        self._set_pu_source_identity(ffn_pu, ffn_identity)
        ffn_file = self.builder().register_source_file(
            ffn_pu,
            ffn_definition.source_file,
        )
        ffn_line = ffn_definition.source_line
        ffn_hidden = self.builder().declare_pu_formal(
            ffn_pu,
            "ffn_hidden_states",
            0,
            tensor_type,
            ffn_file,
            ffn_line,
        )
        self.builder().attach_value_metadata(
            ffn_hidden,
            self._multi_pu_value_metadata(
                ffn_identity,
                "ffn_hidden_states",
                "activation",
                "",
            ),
        )
        ffn_gate_weight = self.builder().declare_pu_formal(
            ffn_pu,
            "ffn_gate_weight",
            1,
            ffn_up_weight_type,
            ffn_file,
            ffn_line,
        )
        ffn_gate_metadata = self._multi_pu_value_metadata(
            ffn_identity,
            "ffn_gate_weight",
            "ffn_gate_weight",
            "gate_proj.weight",
        )
        self.builder().attach_value_metadata(ffn_gate_weight, ffn_gate_metadata)
        ffn_up_weight = self.builder().declare_pu_formal(
            ffn_pu,
            "ffn_up_weight",
            2,
            ffn_up_weight_type,
            ffn_file,
            ffn_line,
        )
        ffn_up_metadata = self._multi_pu_value_metadata(
            ffn_identity,
            "ffn_up_weight",
            "ffn_up_weight",
            "up_proj.weight",
        )
        self.builder().attach_value_metadata(ffn_up_weight, ffn_up_metadata)
        ffn_down_weight = self.builder().declare_pu_formal(
            ffn_pu,
            "ffn_down_weight",
            3,
            ffn_down_weight_type,
            ffn_file,
            ffn_line,
        )
        ffn_down_metadata = self._multi_pu_value_metadata(
            ffn_identity,
            "ffn_down_weight",
            "ffn_down_weight",
            "down_proj.weight",
        )
        self.builder().attach_value_metadata(ffn_down_weight, ffn_down_metadata)
        self.builder().declare_pu_result(
            ffn_pu,
            "ffn_output",
            0,
            tensor_type,
            file_id=ffn_file,
            line=ffn_line + 1,
        )
        ffn_gate = self.builder().common_linear_v3(
            ffn_hidden,
            ffn_gate_weight,
            self._linear_attrs(),
        )
        self.builder().attach_value_metadata(
            ffn_gate,
            self._multi_pu_operator_metadata(
                ffn_identity,
                "ffn_gate_projection",
                "llama2:common.linear",
                "gate_projection",
            ),
        )
        self.builder().set_value_source_position(
            ffn_gate,
            ffn_file,
            ffn_line + 1,
        )
        self.builder().append_program_unit_value(ffn_pu, ffn_gate)
        ffn_up = self.builder().common_linear_v3(
            ffn_hidden,
            ffn_up_weight,
            self._linear_attrs(),
        )
        self.builder().attach_value_metadata(
            ffn_up,
            self._multi_pu_operator_metadata(
                ffn_identity,
                "ffn_up_projection",
                "llama2:common.linear",
                "up_projection",
            ),
        )
        self.builder().set_value_source_position(
            ffn_up,
            ffn_file,
            ffn_line + 1,
        )
        self.builder().append_program_unit_value(ffn_pu, ffn_up)
        ffn_swiglu = self.builder().transformer_swiglu(
            ffn_gate,
            ffn_up,
            {"attr.activation": "silu"},
        )
        self.builder().attach_value_metadata(
            ffn_swiglu,
            self._multi_pu_operator_metadata(
                ffn_identity,
                "ffn_swiglu",
                "llama2:transformer.swiglu",
                "swiglu",
            ),
        )
        self.builder().set_value_source_position(
            ffn_swiglu,
            ffn_file,
            ffn_line + 1,
        )
        self.builder().append_program_unit_value(ffn_pu, ffn_swiglu)
        ffn_down = self.builder().common_linear_v3(
            ffn_swiglu,
            ffn_down_weight,
            self._linear_attrs(),
        )
        self.builder().attach_value_metadata(
            ffn_down,
            self._multi_pu_operator_metadata(
                ffn_identity,
                "ffn_down_projection",
                "llama2:common.linear",
                "down_projection",
            ),
        )
        self.builder().set_value_source_position(
            ffn_down,
            ffn_file,
            ffn_line + 1,
        )
        self.builder().append_program_unit_value(ffn_pu, ffn_down)
        self.builder().return_pu_values(ffn_pu, [ffn_down])

        entry_pu = self.builder().minimal_program_unit(entry_name)
        self._set_pu_source_identity(entry_pu, entry_identity)
        entry_file = self.builder().register_source_file(
            entry_pu,
            entry_definition.source_file,
        )
        entry_line = entry_definition.source_line
        model_hidden = self.builder().declare_pu_formal(
            entry_pu,
            "model_hidden",
            0,
            tensor_type,
            entry_file,
            entry_line,
        )
        self.builder().attach_value_metadata(
            model_hidden,
            self._multi_pu_value_metadata(
                rms_identity,
                "model_hidden",
                "call_actual",
                "",
            ),
        )
        model_norm_scale = self.builder().declare_pu_formal(
            entry_pu,
            "model_norm_scale",
            1,
            scale_type,
            entry_file,
            entry_line,
        )
        model_scale_metadata = self._multi_pu_value_metadata(
            rms_identity,
            "model_norm_scale",
            "rms_norm_scale",
            "weight",
        )
        self.builder().attach_value_metadata(
            model_norm_scale,
            model_scale_metadata,
        )
        model_ffn_gate_weight = self.builder().declare_pu_formal(
            entry_pu,
            "model_ffn_gate_weight",
            2,
            ffn_up_weight_type,
            entry_file,
            entry_line,
        )
        model_ffn_gate_metadata = self._multi_pu_value_metadata(
            ffn_identity,
            "model_ffn_gate_weight",
            "ffn_gate_weight",
            "gate_proj.weight",
        )
        self.builder().attach_value_metadata(
            model_ffn_gate_weight,
            model_ffn_gate_metadata,
        )
        model_ffn_up_weight = self.builder().declare_pu_formal(
            entry_pu,
            "model_ffn_up_weight",
            3,
            ffn_up_weight_type,
            entry_file,
            entry_line,
        )
        model_ffn_up_metadata = self._multi_pu_value_metadata(
            ffn_identity,
            "model_ffn_up_weight",
            "ffn_up_weight",
            "up_proj.weight",
        )
        self.builder().attach_value_metadata(
            model_ffn_up_weight,
            model_ffn_up_metadata,
        )
        model_ffn_down_weight = self.builder().declare_pu_formal(
            entry_pu,
            "model_ffn_down_weight",
            4,
            ffn_down_weight_type,
            entry_file,
            entry_line,
        )
        model_ffn_down_metadata = self._multi_pu_value_metadata(
            ffn_identity,
            "model_ffn_down_weight",
            "ffn_down_weight",
            "down_proj.weight",
        )
        self.builder().attach_value_metadata(
            model_ffn_down_weight,
            model_ffn_down_metadata,
        )
        self.builder().declare_pu_result(
            entry_pu,
            "model_result",
            0,
            tensor_type,
            file_id=entry_file,
            line=entry_line + 1,
        )
        call = self.builder().create_pu_call(
            entry_pu,
            rms_pu,
            [model_hidden, model_norm_scale],
            ["norm_call_result"],
            rms_definition.canonical_name,
            rms_instance,
            rms_context,
            0,
            entry_file,
            entry_line + 2,
        )
        call_result = self.builder().get_pu_call_result(call, 0, tensor_type)
        call_result_metadata = self._multi_pu_value_metadata(
            rms_identity,
            "norm_call_result",
            "call_result",
            "",
        )
        call_result_metadata["result_source"] = "TinyRMSNorm.normalized_result"
        self.builder().attach_value_metadata(call_result, call_result_metadata)
        self.builder().set_value_source_position(
            call_result,
            entry_file,
            entry_line + 2,
        )
        ffn_call = self.builder().create_pu_call(
            entry_pu,
            ffn_pu,
            [
                call_result,
                model_ffn_gate_weight,
                model_ffn_up_weight,
                model_ffn_down_weight,
            ],
            ["ffn_call_result"],
            ffn_definition.canonical_name,
            ffn_instance,
            ffn_context,
            1,
            entry_file,
            entry_line + 3,
        )
        ffn_call_result = self.builder().get_pu_call_result(
            ffn_call,
            0,
            tensor_type,
        )
        ffn_call_result_metadata = self._multi_pu_value_metadata(
            ffn_identity,
            "ffn_call_result",
            "call_result",
            "",
        )
        ffn_call_result_metadata["result_source"] = (
            "TinyLlama2FeedForward.ffn_output"
        )
        self.builder().attach_value_metadata(
            ffn_call_result,
            ffn_call_result_metadata,
        )
        self.builder().set_value_source_position(
            ffn_call_result,
            entry_file,
            entry_line + 3,
        )
        self.builder().return_pu_values(entry_pu, [ffn_call_result])

        model_module = inspect.getmodule(type(model))
        return WhirlModule(
            options=self._options,
            model_name=model_name,
            input_count=len(inputs),
            entry_function=WhirlProgramUnitRecord(
                name=entry_name,
                handle=entry_pu.value,
                body_markers=[
                    "call:TinyRMSNorm",
                    "call:TinyLlama2FeedForward",
                ],
            ),
            graph_source="torch.fx+llama2_multiple_pu_boundary",
            operators=[
                transformer.RMS_NORM,
                "call:TinyRMSNorm",
                common.LINEAR,
                common.LINEAR,
                transformer.SWIGLU,
                common.LINEAR,
                "call:TinyLlama2FeedForward",
            ],
            tensor_types=[
                WhirlTensorTypeRecord(
                    name="llama2_multi_pu_hidden_type",
                    handle=tensor_type.value,
                    dtype="float32",
                    rank=3,
                    logical_shape=hidden_shape,
                    descriptor={
                        "dtype": "float32",
                        "rank": 3,
                        "logical_shape": hidden_shape,
                        "layout": "BSC",
                        "lineage": "python.multi_pu.hidden",
                    },
                ),
                WhirlTensorTypeRecord(
                    name="llama2_multi_pu_scale_type",
                    handle=scale_type.value,
                    dtype="float32",
                    rank=1,
                    logical_shape="[32]",
                    descriptor={
                        "dtype": "float32",
                        "rank": 1,
                        "logical_shape": "[32]",
                        "layout": "C",
                        "lineage": "python.multi_pu.rms_norm.scale",
                    },
                ),
                WhirlTensorTypeRecord(
                    name="llama2_multi_pu_intermediate_type",
                    handle=intermediate_type.value,
                    dtype="float32",
                    rank=3,
                    logical_shape=intermediate_shape,
                    descriptor={
                        "dtype": "float32",
                        "rank": 3,
                        "logical_shape": intermediate_shape,
                        "layout": "BSC",
                        "lineage": (
                            "python.multi_pu.feed_forward.intermediate"
                        ),
                    },
                ),
                WhirlTensorTypeRecord(
                    name="llama2_multi_pu_ffn_up_weight_type",
                    handle=ffn_up_weight_type.value,
                    dtype="float32",
                    rank=2,
                    logical_shape="[88,32]",
                    descriptor={
                        "dtype": "float32",
                        "rank": 2,
                        "logical_shape": "[88,32]",
                        "layout": "OI",
                        "lineage": (
                            "python.multi_pu.feed_forward.up_weight"
                        ),
                    },
                ),
                WhirlTensorTypeRecord(
                    name="llama2_multi_pu_ffn_down_weight_type",
                    handle=ffn_down_weight_type.value,
                    dtype="float32",
                    rank=2,
                    logical_shape="[32,88]",
                    descriptor={
                        "dtype": "float32",
                        "rank": 2,
                        "logical_shape": "[32,88]",
                        "layout": "OI",
                        "lineage": (
                            "python.multi_pu.feed_forward.down_weight"
                        ),
                    },
                )
            ],
            values=[
                WhirlValueRecord(
                    name="hidden_states",
                    handle=hidden.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="formal",
                    metadata=self._multi_pu_value_metadata(
                        rms_identity,
                        "hidden_states",
                        "activation",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="rms_norm_scale",
                    handle=scale.value,
                    type_name="llama2_multi_pu_scale_type",
                    value_kind="formal",
                    metadata=dict(scale_metadata),
                ),
                WhirlValueRecord(
                    name="model_hidden",
                    handle=model_hidden.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="formal",
                    metadata=self._multi_pu_value_metadata(
                        rms_identity,
                        "model_hidden",
                        "call_actual",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="model_norm_scale",
                    handle=model_norm_scale.value,
                    type_name="llama2_multi_pu_scale_type",
                    value_kind="formal",
                    metadata=dict(model_scale_metadata),
                ),
                WhirlValueRecord(
                    name="norm_call_result",
                    handle=call_result.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="call_result",
                    metadata=dict(call_result_metadata),
                ),
                WhirlValueRecord(
                    name="ffn_hidden_states",
                    handle=ffn_hidden.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="formal",
                    metadata=self._multi_pu_value_metadata(
                        ffn_identity,
                        "ffn_hidden_states",
                        "activation",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="ffn_gate_weight",
                    handle=ffn_gate_weight.value,
                    type_name="llama2_multi_pu_ffn_up_weight_type",
                    value_kind="formal",
                    metadata=dict(ffn_gate_metadata),
                ),
                WhirlValueRecord(
                    name="ffn_up_weight",
                    handle=ffn_up_weight.value,
                    type_name="llama2_multi_pu_ffn_up_weight_type",
                    value_kind="formal",
                    metadata=dict(ffn_up_metadata),
                ),
                WhirlValueRecord(
                    name="ffn_down_weight",
                    handle=ffn_down_weight.value,
                    type_name="llama2_multi_pu_ffn_down_weight_type",
                    value_kind="formal",
                    metadata=dict(ffn_down_metadata),
                ),
                WhirlValueRecord(
                    name="model_ffn_gate_weight",
                    handle=model_ffn_gate_weight.value,
                    type_name="llama2_multi_pu_ffn_up_weight_type",
                    value_kind="formal",
                    metadata=dict(model_ffn_gate_metadata),
                ),
                WhirlValueRecord(
                    name="model_ffn_up_weight",
                    handle=model_ffn_up_weight.value,
                    type_name="llama2_multi_pu_ffn_up_weight_type",
                    value_kind="formal",
                    metadata=dict(model_ffn_up_metadata),
                ),
                WhirlValueRecord(
                    name="model_ffn_down_weight",
                    handle=model_ffn_down_weight.value,
                    type_name="llama2_multi_pu_ffn_down_weight_type",
                    value_kind="formal",
                    metadata=dict(model_ffn_down_metadata),
                ),
                WhirlValueRecord(
                    name="ffn_call_result",
                    handle=ffn_call_result.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="call_result",
                    metadata=dict(ffn_call_result_metadata),
                ),
            ],
            graph_operators=[
                WhirlOperatorRecord(
                    name="call:TinyRMSNorm",
                    handle=call.value,
                    kids=["model_hidden", "model_norm_scale"],
                    attrs={
                        "canonical_class_name": rms_definition.canonical_name,
                        "instance_path": rms_instance,
                        "context_identity": rms_context,
                    },
                    metadata={
                        "declaration_kind": "python_class_callable",
                        "callable_identity": rms_identity[
                            "callable_identity"
                        ],
                        "implementation_method": rms_identity[
                            "implementation_method"
                        ],
                        "implementation_fingerprint": rms_identity[
                            "implementation_fingerprint"
                        ],
                        "class_state_parameters": rms_identity[
                            "class_state_parameters"
                        ],
                        "class_state_submodules": rms_identity[
                            "class_state_submodules"
                        ],
                        "class_state_scalars": rms_identity[
                            "class_state_scalars"
                        ],
                    },
                ),
                WhirlOperatorRecord(
                    name="call:TinyLlama2FeedForward",
                    handle=ffn_call.value,
                    kids=[
                        "norm_call_result",
                        "model_ffn_gate_weight",
                        "model_ffn_up_weight",
                        "model_ffn_down_weight",
                    ],
                    attrs={
                        "canonical_class_name": (
                            ffn_definition.canonical_name
                        ),
                        "instance_path": ffn_instance,
                        "context_identity": ffn_context,
                    },
                    metadata={
                        "declaration_kind": "python_class_callable",
                        "callable_identity": ffn_identity[
                            "callable_identity"
                        ],
                        "implementation_method": ffn_identity[
                            "implementation_method"
                        ],
                        "implementation_fingerprint": ffn_identity[
                            "implementation_fingerprint"
                        ],
                        "class_state_parameters": ffn_identity[
                            "class_state_parameters"
                        ],
                        "class_state_submodules": ffn_identity[
                            "class_state_submodules"
                        ],
                        "class_state_scalars": ffn_identity[
                            "class_state_scalars"
                        ],
                    },
                )
            ],
            python_imports=(
                collect_imported_python_callables(model_module)
                if model_module is not None else ()
            ),
            python_class_definitions=class_definitions,
            python_class_instances=class_instances,
        )

    def _module_at_instance_path(self, model: Any, instance_path: str) -> Any:
        current = model
        for component in instance_path.split("."):
            if component.isdigit():
                current = current[int(component)]
            else:
                current = getattr(current, component)
        return current

    def _callable_identity_metadata(
        self,
        definition: Any,
        module: Any,
        instance_path: str,
        context_identity: str,
    ) -> Dict[str, str]:
        try:
            class_source_line = str(inspect.getsourcelines(type(module))[1])
        except (OSError, TypeError):
            class_source_line = "0"
        parameters = tuple(str(name) for name in getattr(
            module,
            "_parameters",
            {},
        ).keys())
        buffers = tuple(str(name) for name in getattr(
            module,
            "_buffers",
            {},
        ).keys())
        submodules = tuple(
            str(name)
            for name, submodule in getattr(module, "_modules", {}).items()
            if submodule is not None
        )
        scalar_state = {
            str(name): str(value)
            for name, value in vars(module).items()
            if not name.startswith("_") and
            isinstance(value, (bool, float, int, str))
        }
        return {
            "declaration_kind": "python_class_callable",
            "canonical_class_name": definition.canonical_name,
            "callable_identity": (
                f"{definition.canonical_name}."
                f"{definition.implementation_method}"
            ),
            "defining_module": str(getattr(type(module), "__module__", "")),
            "implementation_method": definition.implementation_method,
            "implementation_signature": definition.implementation_signature,
            "implementation_fingerprint": (
                definition.implementation_fingerprint
            ),
            "source_file": definition.source_file,
            "source_line": str(definition.source_line),
            "class_source_line": class_source_line,
            "instance_path": instance_path,
            "context_identity": context_identity,
            "class_state_parameters": ",".join(parameters),
            "class_state_buffers": ",".join(buffers),
            "class_state_submodules": ",".join(submodules),
            "class_state_scalars": ",".join(
                f"{name}={scalar_state[name]}"
                for name in sorted(scalar_state)
            ),
        }

    def _set_pu_source_identity(
        self,
        program_unit: ProgramUnitHandle,
        identity: Mapping[str, str],
    ) -> None:
        self.builder().set_pu_source_identity(
            program_unit,
            identity["callable_identity"],
            identity["defining_module"],
            identity["source_file"],
            int(identity["source_line"]),
        )

    def _multi_pu_value_metadata(
        self,
        identity: Mapping[str, str],
        source_layer_name: str,
        tensor_role: str,
        source_parameter: str,
    ) -> Dict[str, str]:
        metadata = {
            "declaration_kind": identity["declaration_kind"],
            "canonical_class_name": identity["canonical_class_name"],
            "callable_identity": identity["callable_identity"],
            "implementation_method": identity["implementation_method"],
            "implementation_fingerprint": (
                identity["implementation_fingerprint"]
            ),
            "source_file": identity["source_file"],
            "source_line": identity["source_line"],
            "class_source_line": identity["class_source_line"],
            "instance_path": identity["instance_path"],
            "context_identity": identity["context_identity"],
            "class_state_parameters": identity["class_state_parameters"],
            "class_state_buffers": identity["class_state_buffers"],
            "class_state_submodules": identity["class_state_submodules"],
            "class_state_scalars": identity["class_state_scalars"],
            "source_layer_name": source_layer_name,
            "tensor_role": tensor_role,
        }
        if source_parameter:
            metadata["source_parameter"] = source_parameter
            metadata["source_class_state"] = (
                f"{identity['canonical_class_name']}.{source_parameter}"
            )
            metadata["source_instance_state"] = (
                f"{identity['instance_path']}.{source_parameter}"
            )
        return metadata

    def _multi_pu_operator_metadata(
        self,
        identity: Mapping[str, str],
        source_layer_name: str,
        lowering_hint: str,
        semantic_name: str,
    ) -> Dict[str, str]:
        metadata = self._multi_pu_value_metadata(
            identity,
            source_layer_name,
            "operator_result",
            "",
        )
        metadata["lowering_hint"] = lowering_hint
        metadata["semantic_name"] = semantic_name
        return metadata

    def _first_instance_path(
        self,
        instances: Sequence[Any],
        canonical_class_name: str,
    ) -> str:
        for instance in instances:
            if (
                instance.canonical_class_name == canonical_class_name and
                instance.instance_path == "norm"
            ):
                return instance.instance_path
        for instance in instances:
            if instance.canonical_class_name == canonical_class_name:
                return instance.instance_path
        return "<unknown>"

    def _is_tiny_llama2_prefill_model(self, model: Any) -> bool:
        return (
            hasattr(model, "config") and
            hasattr(model, "token_embedding") and
            hasattr(model, "layers") and
            hasattr(model, "norm") and
            hasattr(model, "output") and
            not self._is_tiny_llama2_decode_model(model)
        )

    def _is_tiny_llama2_decode_model(self, model: Any) -> bool:
        config = getattr(model, "config", None)
        return (
            config is not None and
            hasattr(config, "decode_sequence_length") and
            hasattr(config, "cache_length") and
            hasattr(model, "token_embedding") and
            hasattr(model, "layers") and
            hasattr(model, "norm") and
            hasattr(model, "output")
        )

    def _emit_llama2_model(
        self,
        model: Any,
        model_name: str,
        entry_pu: ProgramUnitHandle,
        input_handles: Sequence[ValueHandle],
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        operators: List[str],
        body_markers: List[str],
        graph_operators: List[WhirlOperatorRecord],
    ) -> None:
        if len(input_handles) != 1:
            raise NotImplementedError("Llama 2 export expects one input_ids tensor")
        if not values or values[0].metadata.get("logical_shape") is None:
            raise ValueError("Llama 2 input_ids descriptor is missing")
        if values[0].metadata.get("logical_shape") != self._llama_input_shape(model):
            raise ValueError(
                "Llama 2 input_ids shape must match the static tiny profile: "
                f"{self._llama_input_shape(model)}"
            )
        if tensor_types[0].dtype != "int64":
            raise NotImplementedError("Llama 2 input_ids must be int64")
        if bool(getattr(model, "training", False)):
            raise NotImplementedError("Llama 2 export requires eval mode")

        config = getattr(model, "config")
        heads = self._config_int(config, "num_attention_heads")
        kv_heads = self._config_int(config, "num_kv_heads")
        if heads != kv_heads:
            raise NotImplementedError(
                "grouped-query attention is not supported by the first "
                "Llama 2 frontend profile"
            )

        attr_env: Dict[str, _GraphValue] = {}
        token_input = _GraphValue(input_handles[0], "input0")
        prefill = self.builder().region(entry_pu, transformer.PREFILL_REGION, 1)
        self._set_llama_region_context(
            prefill,
            entry_pu,
            model,
            "forward",
            0,
        )
        prefill_state = _RegionState()
        self._llama_prefill_external_inputs: List[_GraphValue] = []
        self._declare_region_input(prefill, token_input, prefill_state)

        token_weight = self._llama_external(
            model,
            "token_embedding.weight",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "token_embedding_weight",
            prefill,
            prefill_state,
        )
        current = self._emit_llama_operator(
            transformer.TOKEN_EMBEDDING,
            [token_input, token_weight],
            {
                "attr.padding_idx": "none",
                "attr.bounds_policy": "runtime_check",
            },
            self.builder().transformer_token_embedding,
            prefill,
            prefill_state,
            operators,
            body_markers,
            graph_operators,
            "token_embedding",
            "token_embedding",
        )

        for layer_index, layer in enumerate(getattr(model, "layers")):
            decoder = self.builder().region(
                entry_pu,
                transformer.DECODER_LAYER_REGION,
                1,
                parent=prefill,
            )
            self.builder().append_child_region(prefill, decoder)
            self._set_llama_region_context(
                decoder,
                entry_pu,
                model,
                f"layers.{layer_index}",
                layer_index,
            )
            layer_state = _RegionState()
            self._declare_region_input(decoder, current, layer_state)
            current = self._emit_llama_decoder_layer(
                model,
                layer,
                layer_index,
                current,
                decoder,
                layer_state,
                model_name,
                entry_pu,
                tensor_types,
                values,
                tensor_payloads,
                body_markers,
                attr_env,
                operators,
                graph_operators,
            )
            self.builder().declare_region_value(
                decoder,
                current.handle,
                REGION_OUTPUT | REGION_RESULT,
                0,
            )
            prefill_state.values.add(current.handle.value)

        for external in self._llama_prefill_external_inputs:
            self._declare_region_input(prefill, external, prefill_state)

        norm_weight = self._llama_external(
            model,
            "norm.weight",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "rms_norm_scale",
            prefill,
            prefill_state,
        )
        current = self._emit_llama_operator(
            transformer.RMS_NORM,
            [current, norm_weight],
            self._rms_norm_attrs(model.norm),
            self.builder().transformer_rms_norm,
            prefill,
            prefill_state,
            operators,
            body_markers,
            graph_operators,
            "norm",
            "final_rms_norm",
        )
        output_weight = self._llama_external(
            model,
            "output.weight",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "output_weight",
            prefill,
            prefill_state,
        )
        current = self._emit_llama_operator(
            common.LINEAR,
            [current, output_weight],
            self._linear_attrs(),
            self.builder().common_linear_v3,
            prefill,
            prefill_state,
            operators,
            body_markers,
            graph_operators,
            "output",
            "output_projection",
        )
        current = self._emit_llama_operator(
            common.OUTPUT_LOGITS,
            [current],
            {
                "attr.semantic": "token_logits",
                "attr.sequence_axis": "-2",
                "attr.vocabulary_axis": "-1",
            },
            self.builder().common_output_logits_v3,
            prefill,
            prefill_state,
            operators,
            body_markers,
            graph_operators,
            "output",
            "output_logits",
        )
        self.builder().declare_region_value(
            prefill,
            current.handle,
            REGION_OUTPUT | REGION_RESULT,
            0,
        )
        # External PU definitions must precede the region that first uses them.
        self.builder().append_program_unit_region(entry_pu, prefill)

    def _emit_llama2_decode_model(
        self,
        model: Any,
        model_name: str,
        entry_pu: ProgramUnitHandle,
        input_handles: Sequence[ValueHandle],
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        operators: List[str],
        body_markers: List[str],
        graph_operators: List[WhirlOperatorRecord],
    ) -> None:
        self._validate_llama2_decode_profile(model, tensor_types, values)

        attr_env: Dict[str, _GraphValue] = {}
        input_values = [
            _GraphValue(handle, f"input{index}")
            for index, handle in enumerate(input_handles)
        ]
        token_input = input_values[0]
        cache_position = input_values[1]
        cache_inputs = input_values[2:]
        decode = self.builder().region(entry_pu, transformer.DECODE_REGION, 1)
        self._set_llama_region_context(
            decode,
            entry_pu,
            model,
            "forward.decode",
            0,
        )
        decode_state = _RegionState()
        self._llama_prefill_external_inputs = None

        token_weight = self._llama_external(
            model,
            "token_embedding.weight",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "token_embedding_weight",
            decode,
            decode_state,
        )
        current = self._emit_llama_operator(
            transformer.TOKEN_EMBEDDING,
            [token_input, token_weight],
            {
                "attr.padding_idx": "none",
                "attr.bounds_policy": "runtime_check",
            },
            self.builder().transformer_token_embedding,
            decode,
            decode_state,
            operators,
            body_markers,
            graph_operators,
            "token_embedding",
            "token_embedding",
        )

        layer_state_flags = (
            REGION_STATE_UNIQUE_OWNERSHIP | REGION_STATE_LAYER_OWNED
        )
        for layer_index, layer in enumerate(getattr(model, "layers")):
            cache_base = layer_index * 2
            key_cache = cache_inputs[cache_base]
            value_cache = cache_inputs[cache_base + 1]
            decoder = self.builder().region(
                entry_pu,
                transformer.DECODER_LAYER_REGION,
                2,
                parent=decode,
            )
            self.builder().append_child_region(decode, decoder)
            self._set_llama_region_context(
                decoder,
                entry_pu,
                model,
                f"layers.{layer_index}",
                layer_index,
            )
            layer_region = _RegionState(declare_inputs=False)
            self._declare_region_input(decoder, current, layer_region)
            self._declare_region_input(decoder, cache_position, layer_region)
            self._declare_region_input(decoder, key_cache, layer_region)
            self._declare_region_input(decoder, value_cache, layer_region)
            key_state = self.builder().state_object(
                entry_pu,
                f"layer{layer_index}.key_cache",
                STATE_MUTABLE_BUFFER,
                STATE_UNIQUE_OWNERSHIP,
            )
            value_state = self.builder().state_object(
                entry_pu,
                f"layer{layer_index}.value_cache",
                STATE_MUTABLE_BUFFER,
                STATE_UNIQUE_OWNERSHIP,
            )
            current = self._emit_llama_decode_decoder_layer(
                model,
                layer,
                layer_index,
                current,
                cache_position,
                key_cache,
                value_cache,
                key_state,
                value_state,
                decoder,
                layer_region,
                model_name,
                entry_pu,
                tensor_types,
                values,
                tensor_payloads,
                body_markers,
                attr_env,
                operators,
                graph_operators,
            )
            self.builder().declare_region_state(
                decoder,
                key_state,
                STATE_EFFECT_MODIFY,
                0,
                layer_state_flags,
            )
            self.builder().declare_region_state(
                decoder,
                value_state,
                STATE_EFFECT_MODIFY,
                1,
                layer_state_flags,
            )
            decode_state.values.add(current.handle.value)

        norm_weight = self._llama_external(
            model,
            "norm.weight",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "rms_norm_scale",
            decode,
            decode_state,
        )
        current = self._emit_llama_operator(
            transformer.RMS_NORM,
            [current, norm_weight],
            self._rms_norm_attrs(model.norm),
            self.builder().transformer_rms_norm,
            decode,
            decode_state,
            operators,
            body_markers,
            graph_operators,
            "norm",
            "final_rms_norm",
        )
        output_weight = self._llama_external(
            model,
            "output.weight",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "output_weight",
            decode,
            decode_state,
        )
        current = self._emit_llama_operator(
            common.LINEAR,
            [current, output_weight],
            self._linear_attrs(),
            self.builder().common_linear_v3,
            decode,
            decode_state,
            operators,
            body_markers,
            graph_operators,
            "output",
            "output_projection",
        )
        current = self._emit_llama_operator(
            common.OUTPUT_LOGITS,
            [current],
            {
                "attr.semantic": "token_logits",
                "attr.sequence_axis": "-2",
                "attr.vocabulary_axis": "-1",
            },
            self.builder().common_output_logits_v3,
            decode,
            decode_state,
            operators,
            body_markers,
            graph_operators,
            "output",
            "output_logits",
        )
        self.builder().declare_region_value(
            decode,
            current.handle,
            REGION_OUTPUT | REGION_RESULT,
            0,
        )
        self.builder().append_program_unit_region(entry_pu, decode)

    def _emit_llama_decoder_layer(
        self,
        model: Any,
        layer: Any,
        layer_index: int,
        layer_input: _GraphValue,
        region: RegionHandle,
        region_state: "_RegionState",
        model_name: str,
        entry_pu: ProgramUnitHandle,
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        body_markers: List[str],
        attr_env: Dict[str, _GraphValue],
        operators: List[str],
        graph_operators: List[WhirlOperatorRecord],
    ) -> _GraphValue:
        config = getattr(model, "config")
        batch = self._config_int(config, "batch_size")
        seq = self._config_int(config, "sequence_length")
        heads = self._config_int(config, "num_attention_heads")
        head_dim = self._config_int(config, "head_dim")
        hidden = self._config_int(config, "hidden_size")
        prefix = f"layers.{layer_index}"

        attention_norm_weight = self._llama_external(
            model,
            f"{prefix}.attention_norm.weight",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "rms_norm_scale",
            region,
            region_state,
        )
        attention_input = self._emit_llama_operator(
            transformer.RMS_NORM,
            [layer_input, attention_norm_weight],
            self._rms_norm_attrs(layer.attention_norm),
            self.builder().transformer_rms_norm,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.attention_norm",
            "attention_rms_norm",
        )

        q = self._linear_projection(
            model,
            attention_input,
            f"{prefix}.attention.wq.weight",
            "attention_q_weight",
            prefix,
            "attention_q_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        k = self._linear_projection(
            model,
            attention_input,
            f"{prefix}.attention.wk.weight",
            "attention_k_weight",
            prefix,
            "attention_k_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        v = self._linear_projection(
            model,
            attention_input,
            f"{prefix}.attention.wv.weight",
            "attention_v_weight",
            prefix,
            "attention_v_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        q = self._reshape_transpose_projection(
            q,
            (batch, seq, heads, head_dim),
            (0, 2, 1, 3),
            prefix,
            "attention_q_bhsd",
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
        )
        k = q
        v = q

        cos = self._llama_external(
            model,
            f"{prefix}.attention.rotary.cos",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "rotary_cos",
            region,
            region_state,
        )
        sin = self._llama_external(
            model,
            f"{prefix}.attention.rotary.sin",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "rotary_sin",
            region,
            region_state,
        )
        q = self._emit_llama_operator(
            transformer.ROTARY_EMBEDDING,
            [q, cos, sin],
            self._rotary_attrs(),
            self.builder().transformer_rotary_embedding,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.attention.rotary",
            "attention_q_rope",
        )
        k = self._emit_llama_operator(
            transformer.ROTARY_EMBEDDING,
            [k, cos, sin],
            self._rotary_attrs(),
            self.builder().transformer_rotary_embedding,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.attention.rotary",
            "attention_k_rope",
        )
        context = self._emit_llama_operator(
            transformer.ATTENTION,
            [q, k, v],
            self._attention_attrs(heads, head_dim),
            self.builder().transformer_attention,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.attention",
            "attention",
        )
        context = self._emit_llama_operator(
            common.TRANSPOSE,
            [context],
            {"attr.permutation": "0,2,1,3"},
            lambda operand, attrs: self.builder().common_transpose(
                operand,
                (0, 2, 1, 3),
                attrs,
            ),
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            prefix,
            "attention_context_bshd",
        )
        context = self._emit_llama_operator(
            common.RESHAPE,
            [context],
            {"attr.target_shape": self._format_attr_shape((batch, seq, hidden))},
            lambda value, attrs: self.builder().common_reshape(
                value,
                (batch, seq, hidden),
                attrs,
            ),
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.attention",
            "attention_context_hidden",
        )
        attention_out = self._linear_projection(
            model,
            context,
            f"{prefix}.attention.wo.weight",
            "attention_o_weight",
            prefix,
            "attention_output_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        residual = self._emit_llama_operator(
            common.RESIDUAL_ADD,
            [layer_input, attention_out],
            {
                "attr.broadcast_rule": "none",
                "attr.shape_check": "exact",
                "attr.residual_path": "true",
            },
            self.builder().common_residual_add,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            prefix,
            "attention_residual",
        )

        ffn_norm_weight = self._llama_external(
            model,
            f"{prefix}.ffn_norm.weight",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "rms_norm_scale",
            region,
            region_state,
        )
        ffn_input = self._emit_llama_operator(
            transformer.RMS_NORM,
            [residual, ffn_norm_weight],
            self._rms_norm_attrs(layer.ffn_norm),
            self.builder().transformer_rms_norm,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.ffn_norm",
            "ffn_rms_norm",
        )
        gate = self._linear_projection(
            model,
            ffn_input,
            f"{prefix}.feed_forward.gate_proj.weight",
            "ffn_gate_weight",
            prefix,
            "ffn_gate_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        up = self._linear_projection(
            model,
            ffn_input,
            f"{prefix}.feed_forward.up_proj.weight",
            "ffn_up_weight",
            prefix,
            "ffn_up_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        swiglu = self._emit_llama_operator(
            transformer.SWIGLU,
            [gate, up],
            {"attr.activation": "silu"},
            self.builder().transformer_swiglu,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.feed_forward",
            "ffn_swiglu",
        )
        down = self._linear_projection(
            model,
            swiglu,
            f"{prefix}.feed_forward.down_proj.weight",
            "ffn_down_weight",
            prefix,
            "ffn_down_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        return self._emit_llama_operator(
            common.RESIDUAL_ADD,
            [residual, down],
            {
                "attr.broadcast_rule": "none",
                "attr.shape_check": "exact",
                "attr.residual_path": "true",
            },
            self.builder().common_residual_add,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            prefix,
            "ffn_residual",
        )

    def _emit_llama_decode_decoder_layer(
        self,
        model: Any,
        layer: Any,
        layer_index: int,
        layer_input: _GraphValue,
        cache_position: _GraphValue,
        key_cache: _GraphValue,
        value_cache: _GraphValue,
        key_state: StateHandle,
        value_state: StateHandle,
        region: RegionHandle,
        region_state: "_RegionState",
        model_name: str,
        entry_pu: ProgramUnitHandle,
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        body_markers: List[str],
        attr_env: Dict[str, _GraphValue],
        operators: List[str],
        graph_operators: List[WhirlOperatorRecord],
    ) -> _GraphValue:
        config = getattr(model, "config")
        batch = self._config_int(config, "batch_size")
        seq = self._config_int(config, "decode_sequence_length")
        heads = self._config_int(config, "num_attention_heads")
        head_dim = self._config_int(config, "head_dim")
        hidden = self._config_int(config, "hidden_size")
        prefix = f"layers.{layer_index}"

        attention_norm_weight = self._llama_external(
            model,
            f"{prefix}.attention_norm.weight",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "rms_norm_scale",
            region,
            region_state,
        )
        attention_input = self._emit_llama_operator(
            transformer.RMS_NORM,
            [layer_input, attention_norm_weight],
            self._rms_norm_attrs(layer.attention_norm),
            self.builder().transformer_rms_norm,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.attention_norm",
            "attention_rms_norm",
        )

        q = self._linear_projection(
            model,
            attention_input,
            f"{prefix}.attention.wq.weight",
            "attention_q_weight",
            prefix,
            "attention_q_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        k = self._linear_projection(
            model,
            attention_input,
            f"{prefix}.attention.wk.weight",
            "attention_k_weight",
            prefix,
            "attention_k_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        v = self._linear_projection(
            model,
            attention_input,
            f"{prefix}.attention.wv.weight",
            "attention_v_weight",
            prefix,
            "attention_v_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        q = self._reshape_transpose_projection(
            q,
            (batch, seq, heads, head_dim),
            (0, 2, 1, 3),
            prefix,
            "attention_q_bhsd",
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
        )
        k = self._reshape_transpose_projection(
            k,
            (batch, seq, heads, head_dim),
            (0, 2, 1, 3),
            prefix,
            "attention_k_bhsd",
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
        )
        v = self._reshape_transpose_projection(
            v,
            (batch, seq, heads, head_dim),
            (0, 2, 1, 3),
            prefix,
            "attention_v_bhsd",
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
        )

        cos = self._llama_external(
            model,
            f"{prefix}.attention.rotary.cos",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "rotary_cos",
            region,
            region_state,
        )
        sin = self._llama_external(
            model,
            f"{prefix}.attention.rotary.sin",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "rotary_sin",
            region,
            region_state,
        )
        q = self._emit_llama_operator(
            transformer.ROTARY_EMBEDDING,
            [q, cos, sin, cache_position],
            self._rotary_decode_attrs(),
            lambda value, cos_value, sin_value, position, attrs:
                self.builder().operator(
                    transformer.ROTARY_EMBEDDING,
                    2,
                    [value, cos_value, sin_value, position],
                    attrs,
                ),
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.attention.rotary",
            "attention_q_rope",
            2,
        )
        k = self._emit_llama_operator(
            transformer.ROTARY_EMBEDDING,
            [k, cos, sin, cache_position],
            self._rotary_decode_attrs(),
            lambda value, cos_value, sin_value, position, attrs:
                self.builder().operator(
                    transformer.ROTARY_EMBEDDING,
                    2,
                    [value, cos_value, sin_value, position],
                    attrs,
                ),
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.attention.rotary",
            "attention_k_rope",
            2,
        )
        context = self._emit_llama_operator(
            transformer.ATTENTION,
            [q, key_cache, value_cache],
            self._attention_decode_attrs(heads, head_dim),
            lambda query, key, value, attrs: self.builder().operator(
                transformer.ATTENTION,
                2,
                [query, key, value],
                attrs,
            ),
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.attention",
            "cached_attention",
            2,
        )
        self.builder().add_state_effect(
            context.handle, key_state, STATE_EFFECT_MODIFY
        )
        self.builder().add_state_effect(
            context.handle, value_state, STATE_EFFECT_MODIFY
        )
        context = self._emit_llama_operator(
            common.TRANSPOSE,
            [context],
            {"attr.permutation": "0,2,1,3"},
            lambda operand, attrs: self.builder().common_transpose(
                operand,
                (0, 2, 1, 3),
                attrs,
            ),
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            prefix,
            "attention_context_bshd",
        )
        context = self._emit_llama_operator(
            common.RESHAPE,
            [context],
            {"attr.target_shape": self._format_attr_shape((batch, seq, hidden))},
            lambda value, attrs: self.builder().common_reshape(
                value,
                (batch, seq, hidden),
                attrs,
            ),
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.attention",
            "attention_context_hidden",
        )
        attention_out = self._linear_projection(
            model,
            context,
            f"{prefix}.attention.wo.weight",
            "attention_o_weight",
            prefix,
            "attention_output_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        residual = self._emit_llama_operator(
            common.RESIDUAL_ADD,
            [layer_input, attention_out],
            {
                "attr.broadcast_rule": "none",
                "attr.shape_check": "exact",
                "attr.residual_path": "true",
            },
            self.builder().common_residual_add,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            prefix,
            "attention_residual",
        )

        ffn_norm_weight = self._llama_external(
            model,
            f"{prefix}.ffn_norm.weight",
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            "rms_norm_scale",
            region,
            region_state,
        )
        ffn_input = self._emit_llama_operator(
            transformer.RMS_NORM,
            [residual, ffn_norm_weight],
            self._rms_norm_attrs(layer.ffn_norm),
            self.builder().transformer_rms_norm,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.ffn_norm",
            "ffn_rms_norm",
        )
        gate = self._linear_projection(
            model,
            ffn_input,
            f"{prefix}.feed_forward.gate_proj.weight",
            "ffn_gate_weight",
            prefix,
            "ffn_gate_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        up = self._linear_projection(
            model,
            ffn_input,
            f"{prefix}.feed_forward.up_proj.weight",
            "ffn_up_weight",
            prefix,
            "ffn_up_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        swiglu = self._emit_llama_operator(
            transformer.SWIGLU,
            [gate, up],
            {"attr.activation": "silu"},
            self.builder().transformer_swiglu,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            f"{prefix}.feed_forward",
            "ffn_swiglu",
        )
        down = self._linear_projection(
            model,
            swiglu,
            f"{prefix}.feed_forward.down_proj.weight",
            "ffn_down_weight",
            prefix,
            "ffn_down_projection",
            region,
            region_state,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            operators,
            graph_operators,
        )
        return self._emit_llama_operator(
            common.RESIDUAL_ADD,
            [residual, down],
            {
                "attr.broadcast_rule": "none",
                "attr.shape_check": "exact",
                "attr.residual_path": "true",
            },
            self.builder().common_residual_add,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            prefix,
            "ffn_residual",
        )

    def _validate_llama2_decode_profile(
        self,
        model: Any,
        tensor_types: Sequence[WhirlTensorTypeRecord],
        values: Sequence[WhirlValueRecord],
    ) -> None:
        config = getattr(model, "config")
        if len(values) != 6:
            raise NotImplementedError(
                "Llama 2 decode export expects input_ids, cache_position, "
                "and two K/V cache tensors per decoder layer"
            )
        if bool(getattr(model, "training", False)):
            raise NotImplementedError("Llama 2 decode export requires eval mode")
        heads = self._config_int(config, "num_attention_heads")
        kv_heads = self._config_int(config, "num_kv_heads")
        if heads != kv_heads:
            raise NotImplementedError(
                "grouped-query attention is not supported by the first "
                "Llama 2 decode frontend profile"
            )
        if self._config_int(config, "decode_sequence_length") != 1:
            raise NotImplementedError(
                "Llama 2 decode export supports one-token decode only"
            )
        cache_length = self._config_int(config, "cache_length")
        if cache_length <= 0:
            raise NotImplementedError(
                "Llama 2 decode export requires a non-empty prefix cache"
            )
        if cache_length >= self._config_int(config, "max_sequence_length"):
            raise NotImplementedError(
                "Llama 2 decode cache_position must be within RoPE capacity"
            )

        expected = [
            (
                "int64",
                self._format_shape((self._config_int(config, "batch_size"), 1)),
            ),
            ("int64", self._format_shape((1,))),
        ]
        cache_shape = self._format_shape((
            self._config_int(config, "batch_size"),
            kv_heads,
            cache_length,
            self._config_int(config, "head_dim"),
        ))
        expected.extend([("float32", cache_shape)] * 4)
        types_by_name = {
            tensor_type.name: tensor_type for tensor_type in tensor_types
        }
        for ordinal, (value, (dtype, shape)) in enumerate(zip(values, expected)):
            tensor_type = types_by_name.get(value.type_name)
            if tensor_type is None:
                raise ValueError(
                    "Llama 2 decode input has no tensor type at ordinal "
                    f"{ordinal}: {value.type_name}"
                )
            if tensor_type.dtype != dtype:
                raise ValueError(
                    "Llama 2 decode input dtype mismatch at ordinal "
                    f"{ordinal}: expected {dtype}"
                )
            if value.metadata.get("logical_shape") != shape:
                raise ValueError(
                    "Llama 2 decode input shape mismatch at ordinal "
                    f"{ordinal}: expected {shape}"
                )

    def _llama_input_shape(self, model: Any) -> str:
        config = getattr(model, "config")
        return self._format_shape((
            self._config_int(config, "batch_size"),
            self._config_int(config, "sequence_length"),
        ))

    def _set_llama_region_context(
        self,
        region: RegionHandle,
        entry_pu: ProgramUnitHandle,
        model: Any,
        module_path: str,
        line_fallback: int,
    ) -> None:
        try:
            path = inspect.getsourcefile(type(model))
            line = inspect.getsourcelines(type(model))[1] + line_fallback
        except (OSError, TypeError):
            path = None
            line = 1 + line_fallback
        if path:
            file_id = self.builder().register_source_file(entry_pu, path)
            self.builder().set_region_source_position(region, file_id, line)
        self.builder().set_region_metadata(region, "module_path", module_path)
        if module_path.startswith("layers."):
            self.builder().set_region_metadata(
                region,
                "layer_ordinal",
                module_path.split(".", 1)[1],
            )

    def _config_int(self, config: Any, name: str) -> int:
        value = getattr(config, name)
        if isinstance(value, property):
            value = value.fget(config)
        return int(value)

    def _llama_external(
        self,
        model: Any,
        target: str,
        model_name: str,
        entry_pu: ProgramUnitHandle,
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        body_markers: List[str],
        attr_env: Dict[str, _GraphValue],
        role: str,
        region: RegionHandle,
        region_state: _RegionState,
    ) -> _GraphValue:
        graph_value = self._external_tensor_for_target(
            model,
            target,
            model_name,
            tensor_types,
            values,
            tensor_payloads,
            attr_env,
            role,
        )
        if graph_value.name not in body_markers:
            self.builder().append_program_unit_value(entry_pu, graph_value.handle)
            body_markers.append(graph_value.name)
        self._declare_region_input(region, graph_value, region_state)
        prefill_external_inputs = getattr(
            self,
            "_llama_prefill_external_inputs",
            None,
        )
        if (
            prefill_external_inputs is not None and
            role != "token_embedding_weight"
        ):
            known = {value.handle.value for value in prefill_external_inputs}
            if graph_value.handle.value not in known:
                prefill_external_inputs.append(graph_value)
        return graph_value

    def _declare_region_input(
        self,
        region: RegionHandle,
        value: _GraphValue,
        region_state: _RegionState,
    ) -> None:
        if value.handle.value in region_state.inputs:
            return
        if value.handle.value in region_state.values:
            return
        if not region_state.declare_inputs:
            region_state.inputs.add(value.handle.value)
            return
        self.builder().declare_region_value(
            region,
            value.handle,
            REGION_INPUT,
            len(region_state.inputs),
        )
        region_state.inputs.add(value.handle.value)

    def _emit_llama_operator(
        self,
        operator_name: str,
        operands: Sequence[_GraphValue],
        attrs: Mapping[str, str],
        emit,
        region: RegionHandle,
        region_state: _RegionState,
        operators: List[str],
        body_markers: List[str],
        graph_operators: List[WhirlOperatorRecord],
        source_module_path: str,
        semantic_name: str,
        operator_version: Optional[int] = None,
    ) -> _GraphValue:
        for operand in operands:
            self._declare_region_input(region, operand, region_state)
        handle = emit(*[operand.handle for operand in operands], attrs)
        self.builder().append_region_value(region, handle)
        region_state.values.add(handle.value)
        metadata = {
            "lowering_hint": f"llama2:{operator_name}",
            "source_module_path": source_module_path,
            "semantic_name": semantic_name,
        }
        if operator_version is not None:
            metadata["operator_version"] = str(operator_version)
        self.builder().attach_value_metadata(handle, metadata)
        operators.append(operator_name)
        body_markers.append(operator_name)
        graph_operators.append(
            WhirlOperatorRecord(
                name=operator_name,
                handle=handle.value,
                kids=[operand.name for operand in operands],
                attrs=dict(attrs),
                metadata=metadata,
            )
        )
        return _GraphValue(handle, semantic_name)

    def _linear_projection(
        self,
        model: Any,
        value: _GraphValue,
        weight_target: str,
        weight_role: str,
        source_prefix: str,
        semantic_name: str,
        region: RegionHandle,
        region_state: _RegionState,
        model_name: str,
        entry_pu: ProgramUnitHandle,
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        body_markers: List[str],
        attr_env: Dict[str, _GraphValue],
        operators: List[str],
        graph_operators: List[WhirlOperatorRecord],
    ) -> _GraphValue:
        weight = self._llama_external(
            model,
            weight_target,
            model_name,
            entry_pu,
            tensor_types,
            values,
            tensor_payloads,
            body_markers,
            attr_env,
            weight_role,
            region,
            region_state,
        )
        return self._emit_llama_operator(
            common.LINEAR,
            [value, weight],
            self._linear_attrs(),
            self.builder().common_linear_v3,
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            source_prefix,
            semantic_name,
        )

    def _reshape_transpose_projection(
        self,
        value: _GraphValue,
        shape: Sequence[int],
        permutation: Sequence[int],
        source_prefix: str,
        semantic_name: str,
        region: RegionHandle,
        region_state: _RegionState,
        operators: List[str],
        body_markers: List[str],
        graph_operators: List[WhirlOperatorRecord],
    ) -> _GraphValue:
        reshaped = self._emit_llama_operator(
            common.RESHAPE,
            [value],
            {"attr.target_shape": self._format_attr_shape(shape)},
            lambda operand, attrs: self.builder().common_reshape(
                operand,
                shape,
                attrs,
            ),
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            source_prefix,
            f"{semantic_name}_reshape",
        )
        return self._emit_llama_operator(
            common.TRANSPOSE,
            [reshaped],
            {"attr.permutation": ",".join(str(index) for index in permutation)},
            lambda operand, attrs: self.builder().common_transpose(
                operand,
                permutation,
                attrs,
            ),
            region,
            region_state,
            operators,
            body_markers,
            graph_operators,
            source_prefix,
            semantic_name,
        )

    def _linear_attrs(self) -> Mapping[str, str]:
        return {
            "attr.has_bias": "false",
            "attr.transpose_input": "false",
            "attr.transpose_weight": "true",
            "attr.weight_layout": "OI",
        }

    def _rms_norm_attrs(self, module: Any) -> Mapping[str, str]:
        return {
            "attr.axis": "-1",
            "attr.epsilon": str(getattr(module, "eps", 1.0e-5)),
            "attr.accum_dtype": "float32",
        }

    def _rotary_attrs(self) -> Mapping[str, str]:
        return {
            "attr.head_layout": "BHSD",
            "attr.sequence_axis": "2",
            "attr.feature_axis": "3",
            "attr.pairing": "half_split",
            "attr.position_mode": "zero_based_static",
            "attr.position_offset": "0",
        }

    def _rotary_decode_attrs(self) -> Mapping[str, str]:
        return {
            "attr.head_layout": "BHSD",
            "attr.sequence_axis": "2",
            "attr.feature_axis": "3",
            "attr.pairing": "half_split",
            "attr.position_mode": "explicit_operand",
        }

    def _attention_attrs(self, heads: int, head_dim: int) -> Mapping[str, str]:
        return {
            "attr.execution_mode": "full_sequence",
            "attr.mask_mode": "causal",
            "attr.head_layout": "BHSD",
            "attr.query_heads": str(heads),
            "attr.kv_heads": str(heads),
            "attr.head_dim": str(head_dim),
            "attr.scale_mode": "inverse_sqrt_head_dim",
            "attr.softmax_axis": "-1",
            "attr.softmax_accum_dtype": "float32",
            "attr.cache_mode": "none",
        }

    def _attention_decode_attrs(
        self,
        heads: int,
        head_dim: int,
    ) -> Mapping[str, str]:
        return {
            "attr.execution_mode": "single_token_decode",
            "attr.mask_mode": "implicit_prefix_causal",
            "attr.head_layout": "BHSD",
            "attr.query_heads": str(heads),
            "attr.kv_heads": str(heads),
            "attr.head_dim": str(head_dim),
            "attr.scale_mode": "inverse_sqrt_head_dim",
            "attr.softmax_axis": "-1",
            "attr.softmax_accum_dtype": "float32",
            "attr.cache_mode": "functional_append",
            "attr.cache_sequence_axis": "2",
        }

    def _bind_model_source_positions(
        self,
        model: Any,
        module: WhirlModule,
    ) -> None:
        if module.options.pu_mode == "multiple":
            return
        try:
            path = inspect.getsourcefile(type(model))
            line = inspect.getsourcelines(type(model))[1]
        except (OSError, TypeError):
            return
        if not path:
            return
        program_unit = ProgramUnitHandle(module.entry_function.handle)
        file_id = self.builder().register_source_file(program_unit, path)
        handles = [value.handle for value in module.values]
        if module.graph_source != "torch.fx":
            handles.extend(operator.handle for operator in module.graph_operators)
        for handle in handles:
            self.builder().set_value_source_position(
                ValueHandle(handle), file_id, line
            )

    def _bind_fx_source_position(
        self,
        program_unit: ProgramUnitHandle,
        node: Any,
        value: ValueHandle,
    ) -> None:
        metadata = getattr(node, "meta", {})
        path = metadata.get("open64_source_path")
        line = metadata.get("open64_source_line")
        if not isinstance(path, str) or not isinstance(line, int) or line <= 0:
            return
        file_id = self.builder().register_source_file(program_unit, path)
        self.builder().set_value_source_position(value, file_id, line)

    def _emit_captured_graph(
        self,
        captured_graph: Tuple[Any, Any],
        model_name: str,
        entry_pu: Any,
        input_handles: Sequence[ValueHandle],
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        operators: List[str],
        body_markers: List[str],
        graph_operators: List[WhirlOperatorRecord],
    ) -> None:
        graph, traced_module = captured_graph
        env: Dict[int, _GraphValue] = {}
        attr_env: Dict[str, _GraphValue] = {}
        input_index = 0
        output_source_node = None
        active_region_path: Optional[str] = None
        active_region: Optional[RegionHandle] = None
        active_region_values: Set[int] = set()
        active_region_inputs: Set[int] = set()
        active_region_last_value: Optional[ValueHandle] = None

        def finish_region() -> None:
            nonlocal active_region
            nonlocal active_region_last_value
            if active_region is not None and active_region_last_value is not None:
                self.builder().declare_region_value(
                    active_region,
                    active_region_last_value,
                    REGION_OUTPUT | REGION_RESULT,
                    len(active_region_inputs),
                )
            active_region = None
            active_region_last_value = None

        for node in getattr(graph, "nodes", ()):
            node_op = str(getattr(node, "op", ""))
            if node_op == "placeholder":
                if input_index >= len(input_handles):
                    raise ValueError("FX graph has more placeholders than inputs")
                env[id(node)] = _GraphValue(
                    input_handles[input_index],
                    f"input{input_index}",
                )
                input_index += 1
                continue

            if node_op == "get_attr":
                graph_value = self._external_tensor_for_target(
                    traced_module,
                    str(getattr(node, "target", "")),
                    model_name,
                    tensor_types,
                    values,
                    tensor_payloads,
                    attr_env,
                )
                env[id(node)] = graph_value
                self.builder().append_program_unit_value(entry_pu, graph_value.handle)
                body_markers.append(graph_value.name)
                continue

            if node_op == "output":
                output_source_node = self._fx_output_source_node(node)
                continue

            operator_plan = self._map_fx_node(node, traced_module)
            if operator_plan is None:
                raise NotImplementedError(
                    "unsupported FX graph node: "
                    f"{node_op}:{getattr(node, 'target', '')}"
                )
            metadata = getattr(node, "meta", {})
            region_path = metadata.get("open64_region_path")
            region_kind = metadata.get("open64_region_kind")
            if not isinstance(region_path, str):
                region_path = None
            if region_path != active_region_path:
                finish_region()
                active_region_path = region_path
                active_region_values.clear()
                active_region_inputs.clear()
                if region_path is not None:
                    contract = (
                        "cnn.bottleneck"
                        if region_kind == "Bottleneck"
                        else "cnn.basic_block"
                    )
                    active_region = self.builder().region(entry_pu, contract, 1)
                    self.builder().append_program_unit_region(
                        entry_pu, active_region
                    )
                    path = metadata.get("open64_source_path")
                    line = metadata.get("open64_source_line")
                    if isinstance(path, str) and isinstance(line, int):
                        file_id = self.builder().register_source_file(entry_pu, path)
                        self.builder().set_region_source_position(
                            active_region, file_id, line
                        )

            operands = self._fx_operator_operands(
                operator_plan.name,
                node,
                env,
                traced_module,
                model_name,
                entry_pu,
                tensor_types,
                values,
                tensor_payloads,
                body_markers,
                attr_env,
                active_region,
                active_region_values,
            )
            if len(operands) < self._operator_arity(operator_plan.name):
                raise ValueError(
                    f"{operator_plan.name} has too few FX operands: "
                    f"expected {self._operator_arity(operator_plan.name)} "
                    f"got {len(operands)}"
                )
            self._validate_operator_plan(
                operator_plan.name,
                operands,
                operator_plan.attrs,
                values,
            )
            handle, attrs = self._emit_operator(
                operator_plan.name,
                [operand.handle for operand in operands],
                operator_plan.attrs,
            )
            env[id(node)] = _GraphValue(handle, operator_plan.name)
            if active_region is None:
                self.builder().append_program_unit_value(entry_pu, handle)
            else:
                for operand in operands:
                    if (
                        operand.handle.value not in active_region_values
                        and operand.handle.value not in active_region_inputs
                    ):
                        self.builder().declare_region_value(
                            active_region,
                            operand.handle,
                            REGION_INPUT,
                            len(active_region_inputs),
                        )
                        active_region_inputs.add(operand.handle.value)
                self.builder().append_region_value(active_region, handle)
                active_region_values.add(handle.value)
                active_region_last_value = handle
            self._bind_fx_source_position(entry_pu, node, handle)
            operators.append(operator_plan.name)
            body_markers.append(operator_plan.name)
            graph_operators.append(
                WhirlOperatorRecord(
                    name=operator_plan.name,
                    handle=handle.value,
                    kids=[operand.name for operand in operands],
                    attrs=attrs,
                    metadata=operator_plan.metadata,
                )
            )

        finish_region()
        if output_source_node is None:
            return
        output_value = env.get(id(output_source_node))
        if output_value is None:
            return
        if output_value.name != common.LINEAR:
            return

        handle, attrs = self._emit_operator(
            common.OUTPUT_LOGITS,
            [output_value.handle],
            {"attr.semantic": "logits"},
        )
        self.builder().append_program_unit_value(entry_pu, handle)
        self._bind_fx_source_position(entry_pu, output_source_node, handle)
        operators.append(common.OUTPUT_LOGITS)
        body_markers.append(common.OUTPUT_LOGITS)
        graph_operators.append(
            WhirlOperatorRecord(
                name=common.OUTPUT_LOGITS,
                handle=handle.value,
                kids=[output_value.name],
                attrs=attrs,
                metadata={
                    "lowering_hint": "classifier_output",
                    "source_operator": output_value.name,
                },
            )
        )

    def _operator_arity(self, operator_name: str) -> int:
        return operator_arity(operator_name)

    def _emit_operator(
        self,
        operator_name: str,
        operands: Sequence[ValueHandle],
        mapped_attrs: Mapping[str, str],
    ) -> Tuple[ValueHandle, dict]:
        if operator_name == common.ADD:
            attrs = {"attr.broadcast_rule": "none", **mapped_attrs}
            return self.builder().common_add(
                operands[0],
                operands[1],
                attrs,
            ), attrs
        if operator_name == common.MATMUL:
            attrs = {
                "attr.transpose_kid0": "false",
                "attr.transpose_kid1": "false",
                **mapped_attrs,
            }
            return self.builder().common_matmul(
                operands[0],
                operands[1],
                attrs,
            ), attrs
        if operator_name == common.RESIDUAL_ADD:
            attrs = {
                "attr.broadcast_rule": "none",
                "attr.shape_check": "exact",
                "attr.residual_path": "true",
                **mapped_attrs,
            }
            return self.builder().common_residual_add(
                operands[0],
                operands[1],
                attrs,
            ), attrs
        if operator_name == common.LINEAR:
            attrs = {
                "attr.has_bias": "true",
                "attr.transpose_input": "false",
                "attr.transpose_weight": "true",
                "attr.weight_layout": "OI",
                **mapped_attrs,
            }
            return self.builder().common_linear(
                operands[0],
                operands[1],
                operands[2],
                attrs,
            ), attrs
        if operator_name == common.RELU:
            attrs = dict(mapped_attrs)
            return self.builder().common_relu(operands[0], attrs), attrs
        if operator_name == common.FLATTEN:
            attrs = {
                "attr.start_dim": "1",
                "attr.end_dim": "-1",
                **mapped_attrs,
            }
            return self.builder().common_flatten(operands[0], attrs), attrs
        if operator_name == common.OUTPUT_LOGITS:
            attrs = dict(mapped_attrs)
            return self.builder().common_output_logits(operands[0], attrs), attrs
        if operator_name == cnn.MAX_POOL2D:
            attrs = {
                "attr.kernel_shape": "3,3",
                "attr.stride": "2,2",
                "attr.padding": "1,1",
                "attr.dilation": "1,1",
                "attr.ceil_mode": "false",
                **mapped_attrs,
            }
            return self.builder().cnn_max_pool2d(operands[0], attrs), attrs
        if operator_name == cnn.GLOBAL_AVG_POOL2D:
            attrs = {
                "attr.output_size": "1,1",
                "attr.reduction_axes": "spatial",
                **mapped_attrs,
            }
            return self.builder().cnn_global_avg_pool2d(operands[0], attrs), attrs
        if operator_name == cnn.CONV2D:
            attrs = {
                "attr.kernel_shape": "3,3",
                "attr.stride": "1,1",
                "attr.padding": "0,0",
                "attr.dilation": "1,1",
                "attr.groups": "1",
                "attr.input_layout": "NCHW",
                "attr.weight_layout": "OIHW",
                "attr.output_layout": "NCHW",
                **mapped_attrs,
            }
            return self.builder().cnn_conv2d(
                operands[0],
                operands[1],
                operands[2],
                attrs,
            ), attrs
        if operator_name == cnn.BATCH_NORM_INFER:
            attrs = {
                "attr.epsilon": "1e-05",
                "attr.training": "false",
                "attr.input_layout": "NCHW",
                "attr.channel_axis": "1",
                **mapped_attrs,
            }
            return self.builder().cnn_batch_norm_infer(
                operands[0],
                operands[1],
                operands[2],
                operands[3],
                operands[4],
                attrs,
            ), attrs

        raise NotImplementedError(f"unsupported mapped operator: {operator_name}")

    def _validate_operator_plan(
        self,
        operator_name: str,
        operands: Sequence[_GraphValue],
        attrs: Mapping[str, str],
        values: Sequence[WhirlValueRecord],
    ) -> None:
        if operator_name == cnn.BATCH_NORM_INFER:
            if attrs.get("attr.training", "false") == "true":
                raise NotImplementedError(
                    "cnn.batch_norm_infer only supports inference batchnorm"
                )
        if operator_name == common.RESIDUAL_ADD:
            self._validate_residual_add_shapes(operands, values)

    def _validate_residual_add_shapes(
        self,
        operands: Sequence[_GraphValue],
        values: Sequence[WhirlValueRecord],
    ) -> None:
        if len(operands) < 2:
            return

        shapes = {
            value.name: self._logical_shape_tuple_for_type(
                value.type_name,
                values,
            )
            for value in values
        }
        lhs_shape = shapes.get(operands[0].name)
        rhs_shape = shapes.get(operands[1].name)
        if lhs_shape is None or rhs_shape is None:
            return
        if lhs_shape != rhs_shape:
            raise ValueError(
                "common.residual_add operands require exact shape match: "
                f"{operands[0].name}{lhs_shape} vs {operands[1].name}{rhs_shape}"
            )

    def _logical_shape_tuple_for_type(
        self,
        type_name: str,
        values: Sequence[WhirlValueRecord],
    ) -> Optional[Tuple[int, ...]]:
        if not type_name:
            return None
        for value in values:
            if value.type_name != type_name:
                continue
            shape = value.metadata.get("logical_shape")
            if shape is not None:
                return self._parse_logical_shape(shape)
        return None

    def _parse_logical_shape(self, logical_shape: str) -> Optional[Tuple[int, ...]]:
        if not logical_shape.startswith("[") or not logical_shape.endswith("]"):
            return None
        body = logical_shape[1:-1]
        if not body:
            return ()
        try:
            return tuple(int(part) for part in body.split(","))
        except ValueError:
            return None

    def _fx_output_source_node(self, node: Any) -> Optional[Any]:
        args = list(getattr(node, "args", ()))
        if not args:
            return None
        return self._fx_first_node(args[0])

    def _fx_first_node(self, value: Any) -> Optional[Any]:
        if hasattr(value, "op") and hasattr(value, "target"):
            return value
        if isinstance(value, Mapping):
            for item in value.values():
                node = self._fx_first_node(item)
                if node is not None:
                    return node
        if isinstance(value, Sequence) and not isinstance(value, str):
            for item in value:
                node = self._fx_first_node(item)
                if node is not None:
                    return node
        return None

    def _fx_operator_operands(
        self,
        operator_name: str,
        node: Any,
        env: Mapping[int, _GraphValue],
        traced_module: Any,
        model_name: str,
        entry_pu: Any,
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        body_markers: List[str],
        attr_env: Dict[str, _GraphValue],
        active_region: Optional[RegionHandle],
        active_region_values: Set[int],
    ) -> List[_GraphValue]:
        args = list(getattr(node, "args", ()))
        kwargs = dict(getattr(node, "kwargs", {}))
        node_op = str(getattr(node, "op", ""))

        if node_op == "call_module":
            return self._fx_module_operands(
                operator_name,
                str(getattr(node, "target", "")),
                args,
                env,
                traced_module,
                model_name,
                entry_pu,
                tensor_types,
                values,
                tensor_payloads,
                body_markers,
                attr_env,
                active_region,
                active_region_values,
            )

        if operator_name == cnn.BATCH_NORM_INFER:
            fx_values = [
                self._fx_argument(args, kwargs, 0, "input"),
                self._fx_argument(args, kwargs, 3, "weight"),
                self._fx_argument(args, kwargs, 4, "bias"),
                self._fx_argument(args, kwargs, 1, "running_mean"),
                self._fx_argument(args, kwargs, 2, "running_var"),
            ]
        else:
            fx_values = args

        operands: List[_GraphValue] = []
        for value in fx_values:
            operand = self._fx_graph_value(value, env)
            if operand is not None:
                operands.append(operand)
        return operands

    def _fx_argument(
        self,
        args: Sequence[Any],
        kwargs: Mapping[str, Any],
        index: int,
        name: str,
    ) -> Any:
        if len(args) > index:
            return args[index]
        return kwargs.get(name)

    def _fx_module_operands(
        self,
        operator_name: str,
        target: str,
        args: Sequence[Any],
        env: Mapping[int, _GraphValue],
        traced_module: Any,
        model_name: str,
        entry_pu: Any,
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        body_markers: List[str],
        attr_env: Dict[str, _GraphValue],
        active_region: Optional[RegionHandle],
        active_region_values: Set[int],
    ) -> List[_GraphValue]:
        operands: List[_GraphValue] = []
        value = self._fx_graph_value(args[0], env) if args else None
        if value is not None:
            operands.append(value)

        if operator_name in {cnn.CONV2D, common.LINEAR}:
            operands.append(
                self._module_parameter_operand(
                    traced_module,
                    f"{target}.weight",
                    model_name,
                    entry_pu,
                    tensor_types,
                    values,
                    tensor_payloads,
                    body_markers,
                    attr_env,
                    "weight",
                    active_region,
                    active_region_values,
                )
            )
            operands.append(
                self._module_parameter_operand(
                    traced_module,
                    f"{target}.bias",
                    model_name,
                    entry_pu,
                    tensor_types,
                    values,
                    tensor_payloads,
                    body_markers,
                    attr_env,
                    "bias",
                    active_region,
                    active_region_values,
                )
            )
        elif operator_name == cnn.BATCH_NORM_INFER:
            for suffix, role in (
                ("weight", "batchnorm_scale"),
                ("bias", "batchnorm_bias"),
                ("running_mean", "batchnorm_running_mean"),
                ("running_var", "batchnorm_running_var"),
            ):
                operands.append(
                    self._module_parameter_operand(
                        traced_module,
                        f"{target}.{suffix}",
                        model_name,
                        entry_pu,
                        tensor_types,
                        values,
                        tensor_payloads,
                        body_markers,
                        attr_env,
                        role,
                        active_region,
                        active_region_values,
                    )
                )

        return operands

    def _fx_graph_value(
        self,
        value: Any,
        env: Mapping[int, _GraphValue],
    ) -> Optional[_GraphValue]:
        node = self._fx_first_node(value)
        if node is None:
            return None
        return env.get(id(node))

    def _module_parameter_operand(
        self,
        traced_module: Any,
        target: str,
        model_name: str,
        entry_pu: Any,
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        body_markers: List[str],
        attr_env: Dict[str, _GraphValue],
        role: Optional[str] = None,
        active_region: Optional[RegionHandle] = None,
        active_region_values: Optional[Set[int]] = None,
    ) -> _GraphValue:
        tensor = self._resolve_attr(traced_module, target)
        if tensor is None:
            graph_value = self._absent_parameter_for_target(
                traced_module,
                target,
                tensor_types,
                values,
                role,
            )
        else:
            graph_value = self._external_tensor_for_target(
                traced_module,
                target,
                model_name,
                tensor_types,
                values,
                tensor_payloads,
                attr_env,
                role,
            )
        if graph_value.name not in body_markers:
            if active_region is None:
                self.builder().append_program_unit_value(
                    entry_pu, graph_value.handle
                )
            else:
                self.builder().append_region_value(
                    active_region, graph_value.handle
                )
                if active_region_values is not None:
                    active_region_values.add(graph_value.handle.value)
            body_markers.append(graph_value.name)
        return graph_value

    def _absent_parameter_for_target(
        self,
        traced_module: Any,
        target: str,
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        role: Optional[str] = None,
    ) -> _GraphValue:
        name = self._external_value_name(target)
        if name in {value.name for value in values}:
            for value in values:
                if value.name == name:
                    return _GraphValue(ValueHandle(value.handle), value.name)

        parameter_role = role or self._parameter_role(target)
        dtype = "float32"
        rank = 0
        logical_shape = "[]"
        value_kind = "absent_parameter"
        value_text = "none"
        if target.endswith(".bias") and parameter_role == "bias":
            weight_target = target[:-4] + "weight"
            weight = self._resolve_attr(traced_module, weight_target)
            if weight is not None:
                shape = self._input_shape(weight)
                if shape:
                    dtype = self._input_dtype(weight)
                    rank = 1
                    logical_shape = self._format_shape((shape[0],))
                    value_kind = "implicit_zero"
                    value_text = "0"

        handle = self.builder().tensor_constant(
            name,
            dtype,
            rank,
            logical_shape,
            value_kind,
            value_text,
        )
        type_name = f"{name}_type"
        tensor_type = self.builder().value_type(handle)
        descriptor = {
            "kind": "tensor",
            "dtype": dtype,
            "rank": rank,
            "logical_shape": logical_shape,
        }
        tensor_types.append(
            WhirlTensorTypeRecord(
                name=type_name,
                handle=tensor_type.value,
                dtype=dtype,
                rank=rank,
                logical_shape=logical_shape,
                descriptor=descriptor,
            )
        )
        values.append(
            WhirlValueRecord(
                name=name,
                handle=handle.value,
                type_name=type_name,
                value_kind=value_kind,
                metadata={
                    "source_layer_name": name,
                    "lowering_hint": (
                        "implicit_zero_parameter"
                        if value_kind == "implicit_zero"
                        else "module_parameter_absent"
                    ),
                    "tensor_role": parameter_role,
                    "storage_shape": logical_shape,
                },
            )
        )
        return _GraphValue(handle, name)

    def _external_tensor_for_target(
        self,
        traced_module: Any,
        target: str,
        model_name: str,
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        tensor_payloads: List[WhirlTensorPayloadRecord],
        attr_env: Dict[str, _GraphValue],
        role: Optional[str] = None,
    ) -> _GraphValue:
        if target in attr_env:
            return attr_env[target]

        tensor = self._resolve_attr(traced_module, target)
        dtype = self._input_dtype(tensor)
        shape = self._input_shape(tensor)
        logical_shape = self._format_shape(shape)
        tensor_role = role or self._parameter_role(target)
        payload_bytes = self._tensor_payload_bytes(tensor)
        byte_length = len(payload_bytes)
        byte_offset = self._next_external_offset(values)
        checksum = hashlib.sha256(payload_bytes).hexdigest()
        name = self._external_value_name(target)
        side_file = self._external_data_file(model_name)
        handle = self.builder().external_tensor_constant(
            name,
            dtype,
            len(shape),
            logical_shape,
            tensor_role,
            "safetensors",
            side_file,
            target,
            byte_offset,
            byte_length,
            "",
            self._parameter_layout(tensor_role, len(shape)),
        )
        metadata = dict(handle.metadata)
        metadata["storage_checksum"] = checksum
        tensor_types.append(
            WhirlTensorTypeRecord(
                name=f"{name}_type",
                handle=handle.tensor_type,
                dtype=dtype,
                rank=len(shape),
                logical_shape=logical_shape,
                descriptor=handle.descriptor,
            )
        )
        values.append(
            WhirlValueRecord(
                name=name,
                handle=handle.value,
                type_name=f"{name}_type",
                value_kind="external_data",
                symbol_handle=handle.symbol,
                metadata=metadata,
            )
        )
        tensor_payloads.append(
            WhirlTensorPayloadRecord(
                storage_file=side_file,
                tensor_key=target,
                dtype=dtype,
                logical_shape=logical_shape,
                byte_offset=byte_offset,
                byte_length=byte_length,
                checksum=checksum,
                data=payload_bytes,
            )
        )
        graph_value = _GraphValue(handle, name)
        attr_env[target] = graph_value
        return graph_value

    def _resolve_attr(self, owner: Any, target: str) -> Any:
        value = owner
        for part in target.split("."):
            value = getattr(value, part)
        return value

    def _external_value_name(self, target: str) -> str:
        return target.replace(".", "_")

    def _parameter_role(self, target: str) -> str:
        name = target.lower()
        if "running_mean" in name:
            return "batchnorm_running_mean"
        if "running_var" in name or "running_variance" in name:
            return "batchnorm_running_var"
        if name.endswith("weight"):
            if "bn" in name or "batchnorm" in name:
                return "batchnorm_scale"
            return "weight"
        if name.endswith("bias"):
            if "bn" in name or "batchnorm" in name:
                return "batchnorm_bias"
            return "bias"
        return "parameter"

    def _parameter_layout(self, role: str, rank: int) -> str:
        if role == "weight" and rank == 4:
            return "OIHW"
        if role == "weight" and rank == 2:
            return "OI"
        if role.startswith("batchnorm") or role == "bias":
            return "C"
        return "contiguous"

    def _next_external_offset(self, values: Sequence[WhirlValueRecord]) -> int:
        offset = 0
        for value in values:
            if value.value_kind != "external_data":
                continue
            length = value.metadata.get("storage_byte_length", "0")
            try:
                offset += int(length)
            except ValueError:
                pass
        return offset

    def _tensor_byte_length(
        self,
        tensor: Any,
        dtype: str,
        shape: Sequence[int],
    ) -> int:
        numel = getattr(tensor, "numel", None)
        if callable(numel):
            try:
                return int(numel()) * self._dtype_byte_size(dtype)
            except (TypeError, ValueError):
                pass

        element_count = 1
        for dim in shape:
            element_count *= dim
        return max(1, element_count * self._dtype_byte_size(dtype))

    def _tensor_payload_bytes(self, tensor: Any) -> bytes:
        if tensor is None:
            return b""
        value = tensor
        for method_name in ("detach", "cpu", "contiguous"):
            method = getattr(value, method_name, None)
            if callable(method):
                value = method()
        numpy_method = getattr(value, "numpy", None)
        if callable(numpy_method):
            value = numpy_method()
        tobytes = getattr(value, "tobytes", None)
        if callable(tobytes):
            return bytes(tobytes())
        buffer = getattr(value, "data", None)
        if isinstance(buffer, (bytes, bytearray)):
            return bytes(buffer)
        raise TypeError("external tensor payload does not expose raw bytes")

    def _dtype_byte_size(self, dtype: str) -> int:
        if dtype in {"float64", "int64"}:
            return 8
        if dtype in {"float32", "int32"}:
            return 4
        if dtype in {"float16", "bfloat16", "int16"}:
            return 2
        if dtype in {"bool", "int8", "uint8"}:
            return 1
        return 4

    def _capture_fx_graph(self, model: Any) -> Optional[Tuple[Any, Any]]:
        try:
            from torch.fx import GraphModule, Tracer
        except ImportError:
            return None

        try:
            source_path = inspect.getsourcefile(type(model))
        except (OSError, TypeError):
            source_path = None
        source_path = os.path.realpath(source_path) if source_path else None

        class _SourcePositionTracer(Tracer):
            def __init__(self) -> None:
                super().__init__()
                self._open64_region_stack: List[Tuple[str, str]] = []

            def call_module(
                self,
                module: Any,
                forward: Any,
                args: Tuple[Any, ...],
                kwargs: Dict[str, Any],
            ) -> Any:
                module_kind = type(module).__name__
                is_region = module_kind in {"BasicBlock", "Bottleneck"}
                if is_region:
                    self._open64_region_stack.append(
                        (self.path_of_module(module), module_kind)
                    )
                try:
                    return super().call_module(module, forward, args, kwargs)
                finally:
                    if is_region:
                        self._open64_region_stack.pop()

            def create_proxy(self, *args: Any, **kwargs: Any) -> Any:
                frames = traceback.extract_stack()
                proxy = super().create_proxy(*args, **kwargs)
                if self._open64_region_stack:
                    region_path, region_kind = self._open64_region_stack[-1]
                    proxy.node.meta["open64_region_path"] = region_path
                    proxy.node.meta["open64_region_kind"] = region_kind
                if source_path is None:
                    return proxy
                for frame in reversed(frames):
                    if os.path.realpath(frame.filename) == source_path:
                        proxy.node.meta["open64_source_path"] = source_path
                        proxy.node.meta["open64_source_line"] = frame.lineno
                        break
                return proxy

        try:
            tracer = _SourcePositionTracer()
            graph = tracer.trace(model)
            traced = GraphModule(model, graph)
        except Exception:
            return None
        return graph, traced

    def _map_fx_node(
        self,
        node: Any,
        traced_module: Optional[Any] = None,
    ) -> Optional[_MappedOperatorPlan]:
        node_op = str(getattr(node, "op", ""))
        target = getattr(node, "target", None)

        if node_op == "call_function" and target is operator.add:
            return _MappedOperatorPlan(
                common.ADD,
                {},
                self._fx_node_metadata(node, common.ADD),
            )
        if node_op == "call_function" and target is operator.matmul:
            return _MappedOperatorPlan(
                common.MATMUL,
                {},
                self._fx_node_metadata(node, common.MATMUL),
            )

        target_name = getattr(target, "__name__", str(target))
        if node_op in {"call_function", "call_method"}:
            operator_name = (
                common.FX_OPERATOR_MAP.get(target_name) or
                cnn.FX_OPERATOR_MAP.get(target_name)
            )
            if operator_name is None:
                return None
            return _MappedOperatorPlan(
                operator_name,
                self._fx_static_attrs(operator_name, node),
                self._fx_node_metadata(node, operator_name),
            )

        if node_op == "call_module" and traced_module is not None:
            module = self._resolve_attr(traced_module, str(target))
            operator_name = self._map_fx_module(module)
            if operator_name is None:
                return None
            return _MappedOperatorPlan(
                operator_name,
                self._fx_module_static_attrs(operator_name, module),
                self._fx_node_metadata(
                    node,
                    operator_name,
                    source_module=module,
                    source_module_path=str(target),
                ),
            )

        return None

    def _fx_node_metadata(
        self,
        node: Any,
        operator_name: str,
        source_module: Any = None,
        source_module_path: str = "",
    ) -> Mapping[str, str]:
        target = getattr(node, "target", "")
        metadata = {
            "fx_node_op": str(getattr(node, "op", "")),
            "fx_node_name": str(getattr(node, "name", "")),
            "fx_target": self._fx_target_text(target),
            "lowering_hint": f"fx:{operator_name}",
        }
        if source_module_path:
            metadata["source_module_path"] = source_module_path
        if source_module is not None:
            metadata["source_module_type"] = source_module.__class__.__name__
        return {
            name: value
            for name, value in metadata.items()
            if value
        }

    def _fx_target_text(self, target: Any) -> str:
        if isinstance(target, str):
            return target
        name = getattr(target, "__name__", "")
        if name:
            return name
        return str(target)

    def _map_fx_module(self, module: Any) -> Optional[str]:
        module_name = module.__class__.__name__
        if module_name == "Conv2d":
            return cnn.CONV2D
        if module_name == "BatchNorm2d":
            return cnn.BATCH_NORM_INFER
        if module_name == "ReLU":
            return common.RELU
        if module_name == "MaxPool2d":
            return cnn.MAX_POOL2D
        if module_name == "AdaptiveAvgPool2d":
            return cnn.GLOBAL_AVG_POOL2D
        if module_name == "Flatten":
            return common.FLATTEN
        if module_name == "Linear":
            return common.LINEAR
        return None

    def _fx_module_static_attrs(
        self,
        operator_name: str,
        module: Any,
    ) -> Mapping[str, str]:
        if operator_name == cnn.CONV2D:
            return {
                "attr.kernel_shape": self._format_pair_value(
                    getattr(module, "kernel_size", (3, 3)),
                    "3,3",
                ),
                "attr.stride": self._format_pair_value(
                    getattr(module, "stride", (1, 1)),
                    "1,1",
                ),
                "attr.padding": self._format_pair_value(
                    getattr(module, "padding", (0, 0)),
                    "0,0",
                ),
                "attr.dilation": self._format_pair_value(
                    getattr(module, "dilation", (1, 1)),
                    "1,1",
                ),
                "attr.groups": str(getattr(module, "groups", 1)),
                "attr.input_layout": "NCHW",
                "attr.weight_layout": "OIHW",
                "attr.output_layout": "NCHW",
            }
        if operator_name == cnn.BATCH_NORM_INFER:
            return {
                "attr.epsilon": str(getattr(module, "eps", 1e-5)),
                "attr.training": self._format_bool(
                    getattr(module, "training", False)
                ),
                "attr.input_layout": "NCHW",
                "attr.channel_axis": "1",
            }
        if operator_name == cnn.MAX_POOL2D:
            return {
                "attr.kernel_shape": self._format_pair_value(
                    getattr(module, "kernel_size", (3, 3)),
                    "3,3",
                ),
                "attr.stride": self._format_pair_value(
                    getattr(module, "stride", (2, 2)),
                    "2,2",
                ),
                "attr.padding": self._format_pair_value(
                    getattr(module, "padding", (1, 1)),
                    "1,1",
                ),
                "attr.dilation": self._format_pair_value(
                    getattr(module, "dilation", (1, 1)),
                    "1,1",
                ),
                "attr.ceil_mode": self._format_bool(
                    getattr(module, "ceil_mode", False)
                ),
            }
        if operator_name == cnn.GLOBAL_AVG_POOL2D:
            return {
                "attr.output_size": self._format_pair_value(
                    getattr(module, "output_size", (1, 1)),
                    "1,1",
                ),
                "attr.reduction_axes": "spatial",
            }
        if operator_name == common.FLATTEN:
            return {
                "attr.start_dim": str(getattr(module, "start_dim", 1)),
                "attr.end_dim": str(getattr(module, "end_dim", -1)),
            }
        if operator_name == common.LINEAR:
            return {
                "attr.has_bias": self._format_bool(
                    getattr(module, "bias", None) is not None
                ),
                "attr.transpose_input": "false",
                "attr.transpose_weight": "true",
                "attr.weight_layout": "OI",
            }
        return {}

    def _fx_static_attrs(self, operator_name: str, node: Any) -> Mapping[str, str]:
        if operator_name == cnn.CONV2D:
            args = list(getattr(node, "args", ()))
            kwargs = getattr(node, "kwargs", {})
            return {
                "attr.kernel_shape": "3,3",
                "attr.stride": self._fx_pair_attr(args, kwargs, "stride", 3, "1,1"),
                "attr.padding": self._fx_pair_attr(args, kwargs, "padding", 4, "0,0"),
                "attr.dilation": self._fx_pair_attr(
                    args,
                    kwargs,
                    "dilation",
                    5,
                    "1,1",
                ),
                "attr.groups": str(self._fx_scalar_attr(args, kwargs, "groups", 6, 1)),
                "attr.input_layout": "NCHW",
                "attr.weight_layout": "OIHW",
                "attr.output_layout": "NCHW",
            }
        if operator_name == cnn.BATCH_NORM_INFER:
            args = list(getattr(node, "args", ()))
            kwargs = getattr(node, "kwargs", {})
            return {
                "attr.epsilon": self._fx_text_attr(args, kwargs, "eps", 7, "1e-05"),
                "attr.training": self._fx_bool_attr(
                    args,
                    kwargs,
                    "training",
                    5,
                    False,
                ),
                "attr.input_layout": "NCHW",
                "attr.channel_axis": "1",
            }
        if operator_name == common.LINEAR:
            return {
                "attr.has_bias": "true",
                "attr.transpose_input": "false",
                "attr.transpose_weight": "true",
                "attr.weight_layout": "OI",
            }

        return {}

    def _format_pair_value(self, value: Any, default: str) -> str:
        if value is None:
            return default
        if isinstance(value, int):
            return f"{value},{value}"
        if isinstance(value, Sequence) and not isinstance(value, str):
            if len(value) == 1:
                return f"{value[0]},{value[0]}"
            if len(value) >= 2:
                return f"{value[0]},{value[1]}"
        return default

    def _format_bool(self, value: Any) -> str:
        return "true" if bool(value) else "false"

    def _fx_pair_attr(
        self,
        args: Sequence[Any],
        kwargs: Mapping[str, Any],
        name: str,
        index: int,
        default: str,
    ) -> str:
        value = kwargs.get(name)
        if value is None and len(args) > index:
            value = args[index]
        if value is None:
            return default
        if isinstance(value, int):
            return f"{value},{value}"
        if isinstance(value, Sequence) and not isinstance(value, str):
            if len(value) == 1:
                return f"{value[0]},{value[0]}"
            if len(value) >= 2:
                return f"{value[0]},{value[1]}"
        return default

    def _fx_scalar_attr(
        self,
        args: Sequence[Any],
        kwargs: Mapping[str, Any],
        name: str,
        index: int,
        default: int,
    ) -> int:
        value = kwargs.get(name)
        if value is None and len(args) > index:
            value = args[index]
        if isinstance(value, int):
            return value
        return default

    def _fx_text_attr(
        self,
        args: Sequence[Any],
        kwargs: Mapping[str, Any],
        name: str,
        index: int,
        default: str,
    ) -> str:
        value = kwargs.get(name)
        if value is None and len(args) > index:
            value = args[index]
        if value is None:
            return default
        return str(value)

    def _fx_bool_attr(
        self,
        args: Sequence[Any],
        kwargs: Mapping[str, Any],
        name: str,
        index: int,
        default: bool,
    ) -> str:
        value = kwargs.get(name)
        if value is None and len(args) > index:
            value = args[index]
        if isinstance(value, bool):
            return "true" if value else "false"
        return "true" if default else "false"

    def _build_input_placeholders(
        self,
        inputs: Sequence[Any],
    ) -> Tuple[List[WhirlTensorTypeRecord], List[WhirlValueRecord],
               List[ValueHandle]]:
        tensor_types: List[WhirlTensorTypeRecord] = []
        values: List[WhirlValueRecord] = []
        handles: List[ValueHandle] = []

        for ordinal, example in enumerate(inputs):
            name = f"input{ordinal}"
            type_name = f"{name}_type"
            dtype = self._input_dtype(example)
            shape = self._input_shape(example)
            logical_shape = self._format_shape(shape)
            descriptor = {
                "kind": "tensor",
                "dtype": dtype,
                "rank": len(shape),
                "logical_shape": logical_shape,
                "traits": "example_input",
                "layout": "contiguous",
                "sharding": "replicated",
                "placement": "host",
                "memory": "dense",
                "quantization": "none",
            }
            tensor_type = self.builder().tensor_type(
                type_name,
                dtype,
                len(shape),
                logical_shape,
                descriptor,
            )
            value = self.builder().model_input(name, tensor_type, ordinal)
            symbol = self.builder().value_result_symbol(value)
            metadata = {
                "source_layer_name": name,
                "lowering_hint": "model_input",
                "logical_shape": logical_shape,
                "input_ordinal": str(ordinal),
            }
            self.builder().attach_value_metadata(value, metadata)
            self.builder().attach_value_lineage(value, name)

            tensor_types.append(
                WhirlTensorTypeRecord(
                    name=type_name,
                    handle=tensor_type.value,
                    dtype=dtype,
                    rank=len(shape),
                    logical_shape=logical_shape,
                    descriptor=descriptor,
                )
            )
            values.append(
                WhirlValueRecord(
                    name=name,
                    handle=value.value,
                    type_name=type_name,
                    value_kind="model_input",
                    symbol_handle=symbol.value,
                    metadata=metadata,
                )
            )
            handles.append(value)

        return tensor_types, values, handles

    def _model_name(self, model: Any) -> str:
        if self._options.model_name:
            return self._options.model_name
        if hasattr(model, "__class__"):
            return model.__class__.__name__
        return type(model).__name__

    def _model_class_name(self, model: Any) -> str:
        if hasattr(model, "__class__"):
            return model.__class__.__name__
        return type(model).__name__

    def _external_data_file(self, model_name: str) -> str:
        if self._options.external_data_file:
            return self._options.external_data_file
        return f"{model_name}.safetensors"

    def _input_dtype(self, example: Any) -> str:
        dtype = getattr(example, "dtype", None)
        if dtype is None:
            return "float32"

        dtype_name = str(dtype)
        if dtype_name.endswith("float32"):
            return "float32"
        if dtype_name.endswith("float64"):
            return "float64"
        if dtype_name.endswith("int32"):
            return "int32"
        if dtype_name.endswith("int64"):
            return "int64"
        if dtype_name.endswith("bool"):
            return "bool"
        return dtype_name

    def _input_shape(self, example: Any) -> Tuple[int, ...]:
        shape = getattr(example, "shape", None)
        if shape is None:
            return ()

        try:
            return tuple(int(dim) for dim in shape)
        except (TypeError, ValueError):
            return ()

    def _format_shape(self, shape: Sequence[int]) -> str:
        return "[" + ",".join(str(dim) for dim in shape) + "]"

    def _format_attr_shape(self, shape: Sequence[int]) -> str:
        return ",".join(str(dim) for dim in shape)
