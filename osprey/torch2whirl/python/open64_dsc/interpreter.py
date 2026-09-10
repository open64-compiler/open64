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
    TensorTypeHandle,
    ValueHandle,
    WhirlBuilder,
    load_builder,
)
from .mapping import cnn, common, transformer
from .mapping.contract import operator_arity, operator_version
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
from .python_imports import (
    collect_python_import_census,
    resolve_reachable_imported_callables,
)


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

        class_definitions, class_instances = collect_python_model_classes(model)
        model_module = inspect.getmodule(type(model))
        import_census = (
            collect_python_import_census(model_module)
            if model_module is not None else None
        )
        python_imports = import_census.callables if import_census else ()
        python_import_diagnostics = (
            import_census.diagnostics if import_census else ()
        )
        python_reachable_imports = resolve_reachable_imported_callables(
            model_module,
            python_imports,
            class_definitions,
            class_instances,
            entry_name,
        )
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
            python_imports=python_imports,
            python_import_diagnostics=python_import_diagnostics,
            python_reachable_imports=python_reachable_imports,
            python_class_definitions=class_definitions,
            python_class_instances=class_instances,
        )

    def _export_multiple_pu_program(
        self,
        model: Any,
        inputs: Sequence[Any],
    ) -> WhirlModule:
        if self._is_resnet20_model(model):
            return self._export_resnet20_multiple_pu_program(model, inputs)

        if (
            not self._is_tiny_llama2_prefill_model(model) and
            not self._is_tiny_llama2_decode_model(model)
        ):
            raise NotImplementedError(
                "multiple-PU emission currently supports the tiny Llama "
                "prefill/decode fixtures and the SecureResNet20 fixture"
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
        rotary_definition = definitions.get("TinyRotaryEmbedding")
        attention_definition = definitions.get("TinyLlama2Attention")
        decoder_definition = definitions.get("TinyLlama2DecoderLayer")
        if (
            entry_definition is None or
            rms_definition is None or
            ffn_definition is None or
            rotary_definition is None or
            attention_definition is None or
            decoder_definition is None
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
        rotary_instance = self._first_instance_path(
            class_instances,
            rotary_definition.canonical_name,
        )
        rotary_module = self._module_at_instance_path(model, rotary_instance)
        rotary_context = f"{entry_name}.{rotary_instance}"
        rotary_identity = self._callable_identity_metadata(
            rotary_definition,
            rotary_module,
            rotary_instance,
            rotary_context,
        )
        attention_instance = self._first_instance_path(
            class_instances,
            attention_definition.canonical_name,
        )
        attention_module = self._module_at_instance_path(
            model,
            attention_instance,
        )
        attention_context = f"{entry_name}.{attention_instance}"
        attention_identity = self._callable_identity_metadata(
            attention_definition,
            attention_module,
            attention_instance,
            attention_context,
        )
        decoder_instance = self._first_instance_path(
            class_instances,
            decoder_definition.canonical_name,
        )
        decoder_module = self._module_at_instance_path(model, decoder_instance)
        decoder_context = f"{entry_name}.{decoder_instance}"
        decoder_identity = self._callable_identity_metadata(
            decoder_definition,
            decoder_module,
            decoder_instance,
            decoder_context,
        )

        sequence_length = 1 if self._is_tiny_llama2_decode_model(model) else 8
        hidden_shape = "[1,1,32]" if self._is_tiny_llama2_decode_model(model) \
            else "[1,8,32]"
        intermediate_shape = (
            "[1,1,88]" if self._is_tiny_llama2_decode_model(model)
            else "[1,8,88]"
        )
        rotary_shape = (
            "[1,4,1,8]" if self._is_tiny_llama2_decode_model(model)
            else "[1,4,8,8]"
        )
        rotary_table_shape = (
            "[1,1,1,8]" if self._is_tiny_llama2_decode_model(model)
            else "[1,1,8,8]"
        )
        logits_shape = (
            "[1,1,128]" if self._is_tiny_llama2_decode_model(model)
            else "[1,8,128]"
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
                "layout": "VC",
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
                "layout": "VC",
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
        rotary_type = self.builder().tensor_type(
            "llama2_multi_pu_rotary_bhsd_type",
            "float32",
            4,
            rotary_shape,
            {
                "dtype": "float32",
                "rank": 4,
                "logical_shape": rotary_shape,
                "layout": "VC",
                "lineage": "python.multi_pu.rotary.value",
            },
        )
        rotary_table_type = self.builder().tensor_type(
            "llama2_multi_pu_rotary_table_type",
            "float32",
            4,
            rotary_table_shape,
            {
                "dtype": "float32",
                "rank": 4,
                "logical_shape": rotary_table_shape,
                "layout": "BHSD",
                "lineage": "python.multi_pu.rotary.table",
            },
        )
        token_ids_type = self.builder().tensor_type(
            "llama2_multi_pu_token_ids_type",
            "int64",
            2,
            f"[1,{sequence_length}]",
            {
                "dtype": "int64",
                "rank": 2,
                "logical_shape": f"[1,{sequence_length}]",
                "layout": "BS",
                "lineage": "python.multi_pu.input_tokens",
            },
        )
        token_weight_type = self.builder().tensor_type(
            "llama2_multi_pu_token_weight_type",
            "float32",
            2,
            "[128,32]",
            {
                "dtype": "float32",
                "rank": 2,
                "logical_shape": "[128,32]",
                "layout": "VC",
                "lineage": "python.multi_pu.token_embedding.weight",
            },
        )
        hidden_weight_type = self.builder().tensor_type(
            "llama2_multi_pu_hidden_weight_type",
            "float32",
            2,
            "[32,32]",
            {
                "dtype": "float32",
                "rank": 2,
                "logical_shape": "[32,32]",
                "layout": "OI",
                "lineage": "python.multi_pu.hidden_projection.weight",
            },
        )
        output_weight_type = self.builder().tensor_type(
            "llama2_multi_pu_output_weight_type",
            "float32",
            2,
            "[128,32]",
            {
                "dtype": "float32",
                "rank": 2,
                "logical_shape": "[128,32]",
                "layout": "OI",
                "lineage": "python.multi_pu.output.weight",
            },
        )
        logits_type = self.builder().tensor_type(
            "llama2_multi_pu_logits_type",
            "float32",
            3,
            logits_shape,
            {
                "dtype": "float32",
                "rank": 3,
                "logical_shape": logits_shape,
                "layout": "BSV",
                "lineage": "python.multi_pu.output.logits",
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
        normalized = self.builder().operator(
            transformer.RMS_NORM,
            1,
            [hidden, scale],
            self._rms_norm_attrs(rms_module),
            result_name="normalized_result",
            result_type=tensor_type,
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
        ffn_down = self.builder().operator(
            common.LINEAR,
            3,
            [ffn_swiglu, ffn_down_weight],
            self._linear_attrs(),
            result_name="ffn_down_projection",
            result_type=tensor_type,
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

        rotary_pu = self.builder().minimal_program_unit("TinyRotaryEmbedding")
        self._set_pu_source_identity(rotary_pu, rotary_identity)
        rotary_file = self.builder().register_source_file(
            rotary_pu,
            rotary_definition.source_file,
        )
        rotary_line = rotary_definition.source_line
        rotary_value = self.builder().declare_pu_formal(
            rotary_pu,
            "rotary_value",
            0,
            rotary_type,
            rotary_file,
            rotary_line,
        )
        self.builder().attach_value_metadata(
            rotary_value,
            self._multi_pu_value_metadata(
                rotary_identity,
                "rotary_value",
                "activation",
                "",
            ),
        )
        rotary_cos = self.builder().declare_pu_formal(
            rotary_pu,
            "rotary_cos",
            1,
            rotary_table_type,
            rotary_file,
            rotary_line,
        )
        rotary_cos_metadata = self._multi_pu_buffer_metadata(
            rotary_identity,
            "rotary_cos",
            "rotary_cos",
            "cos",
        )
        self.builder().attach_value_metadata(rotary_cos, rotary_cos_metadata)
        rotary_sin = self.builder().declare_pu_formal(
            rotary_pu,
            "rotary_sin",
            2,
            rotary_table_type,
            rotary_file,
            rotary_line,
        )
        rotary_sin_metadata = self._multi_pu_buffer_metadata(
            rotary_identity,
            "rotary_sin",
            "rotary_sin",
            "sin",
        )
        self.builder().attach_value_metadata(rotary_sin, rotary_sin_metadata)
        self.builder().declare_pu_result(
            rotary_pu,
            "rotated_value",
            0,
            rotary_type,
            file_id=rotary_file,
            line=rotary_line + 1,
        )
        rotated_value = self.builder().operator(
            transformer.ROTARY_EMBEDDING,
            1,
            [rotary_value, rotary_cos, rotary_sin],
            self._rotary_attrs(),
            result_name="rotated_value",
            result_type=rotary_type,
        )
        self.builder().attach_value_metadata(
            rotated_value,
            self._multi_pu_operator_metadata(
                rotary_identity,
                "rotary_embedding",
                "llama2:transformer.rotary_embedding",
                "rotary_embedding",
            ),
        )
        self.builder().set_value_source_position(
            rotated_value,
            rotary_file,
            rotary_line + 1,
        )
        self.builder().append_program_unit_value(rotary_pu, rotated_value)
        self.builder().return_pu_values(rotary_pu, [rotated_value])

        attention_pu = self.builder().minimal_program_unit(
            "TinyLlama2Attention"
        )
        self._set_pu_source_identity(attention_pu, attention_identity)
        attention_file = self.builder().register_source_file(
            attention_pu,
            attention_definition.source_file,
        )
        attention_line = attention_definition.source_line
        attention_input = self.builder().declare_pu_formal(
            attention_pu,
            "attention_input",
            0,
            tensor_type,
            attention_file,
            attention_line,
        )
        self.builder().attach_value_metadata(
            attention_input,
            self._multi_pu_value_metadata(
                attention_identity,
                "attention_input",
                "activation",
                "",
            ),
        )
        attention_wq = self.builder().declare_pu_formal(
            attention_pu,
            "attention_wq_weight",
            1,
            hidden_weight_type,
            attention_file,
            attention_line,
        )
        attention_wq_metadata = self._multi_pu_value_metadata(
            attention_identity,
            "attention_wq_weight",
            "attention_q_weight",
            "wq.weight",
        )
        self.builder().attach_value_metadata(attention_wq,
                                             attention_wq_metadata)
        attention_wk = self.builder().declare_pu_formal(
            attention_pu,
            "attention_wk_weight",
            2,
            hidden_weight_type,
            attention_file,
            attention_line,
        )
        attention_wk_metadata = self._multi_pu_value_metadata(
            attention_identity,
            "attention_wk_weight",
            "attention_k_weight",
            "wk.weight",
        )
        self.builder().attach_value_metadata(attention_wk,
                                             attention_wk_metadata)
        attention_wv = self.builder().declare_pu_formal(
            attention_pu,
            "attention_wv_weight",
            3,
            hidden_weight_type,
            attention_file,
            attention_line,
        )
        attention_wv_metadata = self._multi_pu_value_metadata(
            attention_identity,
            "attention_wv_weight",
            "attention_v_weight",
            "wv.weight",
        )
        self.builder().attach_value_metadata(attention_wv,
                                             attention_wv_metadata)
        attention_wo = self.builder().declare_pu_formal(
            attention_pu,
            "attention_wo_weight",
            4,
            hidden_weight_type,
            attention_file,
            attention_line,
        )
        attention_wo_metadata = self._multi_pu_value_metadata(
            attention_identity,
            "attention_wo_weight",
            "attention_o_weight",
            "wo.weight",
        )
        self.builder().attach_value_metadata(attention_wo,
                                             attention_wo_metadata)
        attention_rotary_cos = self.builder().declare_pu_formal(
            attention_pu,
            "attention_rotary_cos",
            5,
            rotary_table_type,
            attention_file,
            attention_line,
        )
        attention_rotary_cos_metadata = self._multi_pu_buffer_metadata(
            rotary_identity,
            "attention_rotary_cos",
            "rotary_cos",
            "cos",
        )
        self.builder().attach_value_metadata(
            attention_rotary_cos,
            attention_rotary_cos_metadata,
        )
        attention_rotary_sin = self.builder().declare_pu_formal(
            attention_pu,
            "attention_rotary_sin",
            6,
            rotary_table_type,
            attention_file,
            attention_line,
        )
        attention_rotary_sin_metadata = self._multi_pu_buffer_metadata(
            rotary_identity,
            "attention_rotary_sin",
            "rotary_sin",
            "sin",
        )
        self.builder().attach_value_metadata(
            attention_rotary_sin,
            attention_rotary_sin_metadata,
        )
        self.builder().declare_pu_result(
            attention_pu,
            "attention_output",
            0,
            tensor_type,
            file_id=attention_file,
            line=attention_line + 1,
        )

        attention_q = self.builder().common_linear_v3(
            attention_input,
            attention_wq,
            self._linear_attrs(),
        )
        self.builder().attach_value_metadata(
            attention_q,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_q_projection",
                "llama2:common.linear",
                "attention_q_projection",
            ),
        )
        self.builder().set_value_source_position(
            attention_q,
            attention_file,
            attention_line + 1,
        )
        self.builder().append_program_unit_value(attention_pu, attention_q)
        attention_k = self.builder().common_linear_v3(
            attention_input,
            attention_wk,
            self._linear_attrs(),
        )
        self.builder().attach_value_metadata(
            attention_k,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_k_projection",
                "llama2:common.linear",
                "attention_k_projection",
            ),
        )
        self.builder().set_value_source_position(
            attention_k,
            attention_file,
            attention_line + 1,
        )
        self.builder().append_program_unit_value(attention_pu, attention_k)
        attention_v = self.builder().common_linear_v3(
            attention_input,
            attention_wv,
            self._linear_attrs(),
        )
        self.builder().attach_value_metadata(
            attention_v,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_v_projection",
                "llama2:common.linear",
                "attention_v_projection",
            ),
        )
        self.builder().set_value_source_position(
            attention_v,
            attention_file,
            attention_line + 1,
        )
        self.builder().append_program_unit_value(attention_pu, attention_v)

        bshd_shape = (1, sequence_length, 4, 8)
        bhsd_shape = (1, 4, sequence_length, 8)
        attention_q_reshape = self.builder().common_reshape(
            attention_q,
            bshd_shape,
        )
        self.builder().attach_value_metadata(
            attention_q_reshape,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_q_reshape",
                "llama2:common.reshape",
                "attention_q_reshape",
            ),
        )
        self.builder().append_program_unit_value(attention_pu,
                                                attention_q_reshape)
        attention_q_bhsd = self.builder().operator(
            common.TRANSPOSE,
            1,
            [attention_q_reshape],
            {"attr.permutation": "0,2,1,3"},
            result_name="attention_q_bhsd",
            result_type=rotary_type,
        )
        self.builder().attach_value_metadata(
            attention_q_bhsd,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_q_bhsd",
                "llama2:common.transpose",
                "attention_q_bhsd",
            ),
        )
        self.builder().append_program_unit_value(attention_pu,
                                                attention_q_bhsd)
        attention_k_reshape = self.builder().common_reshape(
            attention_k,
            bshd_shape,
        )
        self.builder().attach_value_metadata(
            attention_k_reshape,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_k_reshape",
                "llama2:common.reshape",
                "attention_k_reshape",
            ),
        )
        self.builder().append_program_unit_value(attention_pu,
                                                attention_k_reshape)
        attention_k_bhsd = self.builder().operator(
            common.TRANSPOSE,
            1,
            [attention_k_reshape],
            {"attr.permutation": "0,2,1,3"},
            result_name="attention_k_bhsd",
            result_type=rotary_type,
        )
        self.builder().attach_value_metadata(
            attention_k_bhsd,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_k_bhsd",
                "llama2:common.transpose",
                "attention_k_bhsd",
            ),
        )
        self.builder().append_program_unit_value(attention_pu,
                                                attention_k_bhsd)
        attention_v_reshape = self.builder().common_reshape(
            attention_v,
            bshd_shape,
        )
        self.builder().attach_value_metadata(
            attention_v_reshape,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_v_reshape",
                "llama2:common.reshape",
                "attention_v_reshape",
            ),
        )
        self.builder().append_program_unit_value(attention_pu,
                                                attention_v_reshape)
        attention_v_bhsd = self.builder().common_transpose(
            attention_v_reshape,
            (0, 2, 1, 3),
        )
        self.builder().attach_value_metadata(
            attention_v_bhsd,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_v_bhsd",
                "llama2:common.transpose",
                "attention_v_bhsd",
            ),
        )
        self.builder().append_program_unit_value(attention_pu,
                                                attention_v_bhsd)

        attention_query_rope_call = self.builder().create_pu_call(
            attention_pu,
            rotary_pu,
            [
                attention_q_bhsd,
                attention_rotary_cos,
                attention_rotary_sin,
            ],
            ["attention_query_rope"],
            rotary_definition.canonical_name,
            rotary_instance,
            f"{attention_context}.rotary.query",
            0,
            attention_file,
            attention_line + 1,
        )
        attention_query_rope = self.builder().get_pu_call_result(
            attention_query_rope_call,
            0,
            rotary_type,
        )
        self.builder().attach_value_metadata(
            attention_query_rope,
            self._multi_pu_operator_metadata(
                rotary_identity,
                "attention_query_rope",
                "call_result",
                "",
            ),
        )
        self.builder().set_value_source_position(
            attention_query_rope,
            attention_file,
            attention_line + 1,
        )
        attention_key_rope_call = self.builder().create_pu_call(
            attention_pu,
            rotary_pu,
            [
                attention_k_bhsd,
                attention_rotary_cos,
                attention_rotary_sin,
            ],
            ["attention_key_rope"],
            rotary_definition.canonical_name,
            rotary_instance,
            f"{attention_context}.rotary.key",
            1,
            attention_file,
            attention_line + 1,
        )
        attention_key_rope = self.builder().get_pu_call_result(
            attention_key_rope_call,
            0,
            rotary_type,
        )
        self.builder().attach_value_metadata(
            attention_key_rope,
            self._multi_pu_operator_metadata(
                rotary_identity,
                "attention_key_rope",
                "call_result",
                "",
            ),
        )
        self.builder().set_value_source_position(
            attention_key_rope,
            attention_file,
            attention_line + 1,
        )

        attention_context_value = self.builder().transformer_attention(
            attention_query_rope,
            attention_key_rope,
            attention_v_bhsd,
            self._attention_attrs(4, 8),
        )
        self.builder().attach_value_metadata(
            attention_context_value,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_context",
                "llama2:transformer.attention",
                "attention_context",
            ),
        )
        self.builder().append_program_unit_value(attention_pu,
                                                attention_context_value)
        attention_context_bshd = self.builder().common_transpose(
            attention_context_value,
            (0, 2, 1, 3),
        )
        self.builder().attach_value_metadata(
            attention_context_bshd,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_context_bshd",
                "llama2:common.transpose",
                "attention_context_bshd",
            ),
        )
        self.builder().append_program_unit_value(attention_pu,
                                                attention_context_bshd)
        attention_context_hidden = self.builder().operator(
            common.RESHAPE,
            1,
            [attention_context_bshd],
            {
                "attr.target_shape": f"1,{sequence_length},32",
            },
            result_name="attention_context_hidden",
            result_type=tensor_type,
        )
        self.builder().attach_value_metadata(
            attention_context_hidden,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_context_hidden",
                "llama2:common.reshape",
                "attention_context_hidden",
            ),
        )
        self.builder().append_program_unit_value(attention_pu,
                                                attention_context_hidden)
        attention_output = self.builder().operator(
            common.LINEAR,
            3,
            [attention_context_hidden, attention_wo],
            self._linear_attrs(),
            result_name="attention_output_projection",
            result_type=tensor_type,
        )
        self.builder().attach_value_metadata(
            attention_output,
            self._multi_pu_operator_metadata(
                attention_identity,
                "attention_output_projection",
                "llama2:common.linear",
                "attention_output_projection",
            ),
        )
        self.builder().set_value_source_position(
            attention_output,
            attention_file,
            attention_line + 1,
        )
        self.builder().append_program_unit_value(attention_pu,
                                                attention_output)
        self.builder().return_pu_values(attention_pu, [attention_output])

        decoder_pu = self.builder().minimal_program_unit(
            "TinyLlama2DecoderLayer"
        )
        self._set_pu_source_identity(decoder_pu, decoder_identity)
        decoder_file = self.builder().register_source_file(
            decoder_pu,
            decoder_definition.source_file,
        )
        decoder_line = decoder_definition.source_line
        decoder_input = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_input",
            0,
            tensor_type,
            decoder_file,
            decoder_line,
        )
        self.builder().attach_value_metadata(
            decoder_input,
            self._multi_pu_value_metadata(
                decoder_identity,
                "decoder_input",
                "activation",
                "",
            ),
        )
        decoder_attention_norm_scale = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_attention_norm_scale",
            1,
            scale_type,
            decoder_file,
            decoder_line,
        )
        decoder_attention_norm_metadata = self._multi_pu_value_metadata(
            rms_identity,
            "decoder_attention_norm_scale",
            "rms_norm_scale",
            "weight",
        )
        self.builder().attach_value_metadata(
            decoder_attention_norm_scale,
            decoder_attention_norm_metadata,
        )
        decoder_attention_wq = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_attention_wq_weight",
            2,
            hidden_weight_type,
            decoder_file,
            decoder_line,
        )
        decoder_attention_wq_metadata = self._multi_pu_value_metadata(
            attention_identity,
            "decoder_attention_wq_weight",
            "attention_q_weight",
            "wq.weight",
        )
        self.builder().attach_value_metadata(
            decoder_attention_wq,
            decoder_attention_wq_metadata,
        )
        decoder_attention_wk = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_attention_wk_weight",
            3,
            hidden_weight_type,
            decoder_file,
            decoder_line,
        )
        decoder_attention_wk_metadata = self._multi_pu_value_metadata(
            attention_identity,
            "decoder_attention_wk_weight",
            "attention_k_weight",
            "wk.weight",
        )
        self.builder().attach_value_metadata(
            decoder_attention_wk,
            decoder_attention_wk_metadata,
        )
        decoder_attention_wv = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_attention_wv_weight",
            4,
            hidden_weight_type,
            decoder_file,
            decoder_line,
        )
        decoder_attention_wv_metadata = self._multi_pu_value_metadata(
            attention_identity,
            "decoder_attention_wv_weight",
            "attention_v_weight",
            "wv.weight",
        )
        self.builder().attach_value_metadata(
            decoder_attention_wv,
            decoder_attention_wv_metadata,
        )
        decoder_attention_wo = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_attention_wo_weight",
            5,
            hidden_weight_type,
            decoder_file,
            decoder_line,
        )
        decoder_attention_wo_metadata = self._multi_pu_value_metadata(
            attention_identity,
            "decoder_attention_wo_weight",
            "attention_o_weight",
            "wo.weight",
        )
        self.builder().attach_value_metadata(
            decoder_attention_wo,
            decoder_attention_wo_metadata,
        )
        decoder_rotary_cos = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_rotary_cos",
            6,
            rotary_table_type,
            decoder_file,
            decoder_line,
        )
        decoder_rotary_cos_metadata = self._multi_pu_buffer_metadata(
            rotary_identity,
            "decoder_rotary_cos",
            "rotary_cos",
            "cos",
        )
        self.builder().attach_value_metadata(
            decoder_rotary_cos,
            decoder_rotary_cos_metadata,
        )
        decoder_rotary_sin = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_rotary_sin",
            7,
            rotary_table_type,
            decoder_file,
            decoder_line,
        )
        decoder_rotary_sin_metadata = self._multi_pu_buffer_metadata(
            rotary_identity,
            "decoder_rotary_sin",
            "rotary_sin",
            "sin",
        )
        self.builder().attach_value_metadata(
            decoder_rotary_sin,
            decoder_rotary_sin_metadata,
        )
        decoder_ffn_norm_scale = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_ffn_norm_scale",
            8,
            scale_type,
            decoder_file,
            decoder_line,
        )
        decoder_ffn_norm_metadata = self._multi_pu_value_metadata(
            rms_identity,
            "decoder_ffn_norm_scale",
            "rms_norm_scale",
            "weight",
        )
        self.builder().attach_value_metadata(
            decoder_ffn_norm_scale,
            decoder_ffn_norm_metadata,
        )
        decoder_ffn_gate = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_ffn_gate_weight",
            9,
            ffn_up_weight_type,
            decoder_file,
            decoder_line,
        )
        decoder_ffn_gate_metadata = self._multi_pu_value_metadata(
            ffn_identity,
            "decoder_ffn_gate_weight",
            "ffn_gate_weight",
            "gate_proj.weight",
        )
        self.builder().attach_value_metadata(
            decoder_ffn_gate,
            decoder_ffn_gate_metadata,
        )
        decoder_ffn_up = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_ffn_up_weight",
            10,
            ffn_up_weight_type,
            decoder_file,
            decoder_line,
        )
        decoder_ffn_up_metadata = self._multi_pu_value_metadata(
            ffn_identity,
            "decoder_ffn_up_weight",
            "ffn_up_weight",
            "up_proj.weight",
        )
        self.builder().attach_value_metadata(
            decoder_ffn_up,
            decoder_ffn_up_metadata,
        )
        decoder_ffn_down = self.builder().declare_pu_formal(
            decoder_pu,
            "decoder_ffn_down_weight",
            11,
            ffn_down_weight_type,
            decoder_file,
            decoder_line,
        )
        decoder_ffn_down_metadata = self._multi_pu_value_metadata(
            ffn_identity,
            "decoder_ffn_down_weight",
            "ffn_down_weight",
            "down_proj.weight",
        )
        self.builder().attach_value_metadata(
            decoder_ffn_down,
            decoder_ffn_down_metadata,
        )
        self.builder().declare_pu_result(
            decoder_pu,
            "decoder_output",
            0,
            tensor_type,
            file_id=decoder_file,
            line=decoder_line + 1,
        )

        decoder_attention_norm_call = self.builder().create_pu_call(
            decoder_pu,
            rms_pu,
            [decoder_input, decoder_attention_norm_scale],
            ["decoder_attention_norm_result"],
            rms_definition.canonical_name,
            "layers.0.attention_norm",
            f"{decoder_context}.attention_norm",
            0,
            decoder_file,
            decoder_line + 1,
        )
        decoder_attention_norm_result = self.builder().get_pu_call_result(
            decoder_attention_norm_call,
            0,
            tensor_type,
        )
        self.builder().attach_value_metadata(
            decoder_attention_norm_result,
            self._multi_pu_value_metadata(
                rms_identity,
                "decoder_attention_norm_result",
                "call_result",
                "",
            ),
        )
        decoder_attention_call = self.builder().create_pu_call(
            decoder_pu,
            attention_pu,
            [
                decoder_attention_norm_result,
                decoder_attention_wq,
                decoder_attention_wk,
                decoder_attention_wv,
                decoder_attention_wo,
                decoder_rotary_cos,
                decoder_rotary_sin,
            ],
            ["decoder_attention_output"],
            attention_definition.canonical_name,
            attention_instance,
            f"{decoder_context}.attention",
            1,
            decoder_file,
            decoder_line + 2,
        )
        decoder_attention_output = self.builder().get_pu_call_result(
            decoder_attention_call,
            0,
            tensor_type,
        )
        self.builder().attach_value_metadata(
            decoder_attention_output,
            self._multi_pu_value_metadata(
                attention_identity,
                "decoder_attention_output",
                "call_result",
                "",
            ),
        )
        decoder_attention_residual = self.builder().operator(
            common.RESIDUAL_ADD,
            operator_version(common.RESIDUAL_ADD),
            [decoder_input, decoder_attention_output],
            {
                "attr.broadcast_rule": "none",
                "attr.shape_check": "exact",
                "attr.residual_path": "true",
            },
            result_name="decoder_attention_residual",
            result_type=tensor_type,
        )
        self.builder().attach_value_metadata(
            decoder_attention_residual,
            self._multi_pu_operator_metadata(
                decoder_identity,
                "decoder_attention_residual",
                "llama2:common.residual_add",
                "attention_residual",
            ),
        )
        self.builder().append_program_unit_value(
            decoder_pu,
            decoder_attention_residual,
        )
        decoder_ffn_norm_call = self.builder().create_pu_call(
            decoder_pu,
            rms_pu,
            [decoder_attention_residual, decoder_ffn_norm_scale],
            ["decoder_ffn_norm_result"],
            rms_definition.canonical_name,
            "layers.0.ffn_norm",
            f"{decoder_context}.ffn_norm",
            2,
            decoder_file,
            decoder_line + 3,
        )
        decoder_ffn_norm_result = self.builder().get_pu_call_result(
            decoder_ffn_norm_call,
            0,
            tensor_type,
        )
        self.builder().attach_value_metadata(
            decoder_ffn_norm_result,
            self._multi_pu_value_metadata(
                rms_identity,
                "decoder_ffn_norm_result",
                "call_result",
                "",
            ),
        )
        decoder_ffn_call = self.builder().create_pu_call(
            decoder_pu,
            ffn_pu,
            [
                decoder_ffn_norm_result,
                decoder_ffn_gate,
                decoder_ffn_up,
                decoder_ffn_down,
            ],
            ["decoder_ffn_output"],
            ffn_definition.canonical_name,
            "layers.0.feed_forward",
            f"{decoder_context}.feed_forward",
            3,
            decoder_file,
            decoder_line + 3,
        )
        decoder_ffn_output = self.builder().get_pu_call_result(
            decoder_ffn_call,
            0,
            tensor_type,
        )
        self.builder().attach_value_metadata(
            decoder_ffn_output,
            self._multi_pu_value_metadata(
                ffn_identity,
                "decoder_ffn_output",
                "call_result",
                "",
            ),
        )
        decoder_output = self.builder().operator(
            common.RESIDUAL_ADD,
            operator_version(common.RESIDUAL_ADD),
            [decoder_attention_residual, decoder_ffn_output],
            {
                "attr.broadcast_rule": "none",
                "attr.shape_check": "exact",
                "attr.residual_path": "true",
            },
            result_name="decoder_output",
            result_type=tensor_type,
        )
        self.builder().attach_value_metadata(
            decoder_output,
            self._multi_pu_operator_metadata(
                decoder_identity,
                "decoder_output",
                "llama2:common.residual_add",
                "decoder_output",
            ),
        )
        self.builder().set_value_source_position(
            decoder_output,
            decoder_file,
            decoder_line + 4,
        )
        self.builder().append_program_unit_value(decoder_pu, decoder_output)
        self.builder().return_pu_values(decoder_pu, [decoder_output])

        entry_pu = self.builder().minimal_program_unit(entry_name)
        self._set_pu_source_identity(entry_pu, entry_identity)
        entry_file = self.builder().register_source_file(
            entry_pu,
            entry_definition.source_file,
        )
        entry_line = entry_definition.source_line
        def entry_formal(
            name: str,
            ordinal: int,
            ty: TensorTypeHandle,
            identity: Mapping[str, str],
            source_layer_name: str,
            tensor_role: str,
            source_parameter: str = "",
            source_buffer: str = "",
            source_instance_path: str = "",
        ) -> ValueHandle:
            if source_buffer:
                metadata = self._multi_pu_buffer_metadata(
                    identity,
                    source_layer_name,
                    tensor_role,
                    source_buffer,
                )
            else:
                metadata = self._multi_pu_value_metadata(
                    identity,
                    source_layer_name,
                    tensor_role,
                    source_parameter,
                )
            if source_instance_path and (source_parameter or source_buffer):
                suffix = source_parameter or source_buffer
                metadata["source_instance_state"] = (
                    f"{source_instance_path}.{suffix}"
                )
            handle = self.builder().declare_pu_formal(
                entry_pu,
                name,
                ordinal,
                ty,
                entry_file,
                entry_line,
            )
            self.builder().attach_value_metadata(handle, metadata)
            return handle

        model_token_ids = entry_formal(
            "model_token_ids",
            0,
            token_ids_type,
            entry_identity,
            "model_token_ids",
            "input_tokens",
        )
        model_token_embedding_weight = entry_formal(
            "model_token_embedding_weight",
            1,
            token_weight_type,
            entry_identity,
            "model_token_embedding_weight",
            "token_embedding_weight",
            "token_embedding.weight",
            source_instance_path="<model>",
        )
        model_layer0_attention_norm_scale = entry_formal(
            "model_layer0_attention_norm_scale",
            2,
            scale_type,
            rms_identity,
            "model_layer0_attention_norm_scale",
            "rms_norm_scale",
            "weight",
            source_instance_path="layers.0.attention_norm",
        )
        model_layer0_wq = entry_formal(
            "model_layer0_wq_weight",
            3,
            hidden_weight_type,
            attention_identity,
            "model_layer0_wq_weight",
            "attention_q_weight",
            "wq.weight",
            source_instance_path="layers.0.attention",
        )
        model_layer0_wk = entry_formal(
            "model_layer0_wk_weight",
            4,
            hidden_weight_type,
            attention_identity,
            "model_layer0_wk_weight",
            "attention_k_weight",
            "wk.weight",
            source_instance_path="layers.0.attention",
        )
        model_layer0_wv = entry_formal(
            "model_layer0_wv_weight",
            5,
            hidden_weight_type,
            attention_identity,
            "model_layer0_wv_weight",
            "attention_v_weight",
            "wv.weight",
            source_instance_path="layers.0.attention",
        )
        model_layer0_wo = entry_formal(
            "model_layer0_wo_weight",
            6,
            hidden_weight_type,
            attention_identity,
            "model_layer0_wo_weight",
            "attention_o_weight",
            "wo.weight",
            source_instance_path="layers.0.attention",
        )
        model_layer0_rotary_cos = entry_formal(
            "model_layer0_rotary_cos",
            7,
            rotary_table_type,
            rotary_identity,
            "model_layer0_rotary_cos",
            "rotary_cos",
            source_buffer="cos",
            source_instance_path="layers.0.attention.rotary",
        )
        model_layer0_rotary_sin = entry_formal(
            "model_layer0_rotary_sin",
            8,
            rotary_table_type,
            rotary_identity,
            "model_layer0_rotary_sin",
            "rotary_sin",
            source_buffer="sin",
            source_instance_path="layers.0.attention.rotary",
        )
        model_layer0_ffn_norm_scale = entry_formal(
            "model_layer0_ffn_norm_scale",
            9,
            scale_type,
            rms_identity,
            "model_layer0_ffn_norm_scale",
            "rms_norm_scale",
            "weight",
            source_instance_path="layers.0.ffn_norm",
        )
        model_layer0_ffn_gate = entry_formal(
            "model_layer0_ffn_gate_weight",
            10,
            ffn_up_weight_type,
            ffn_identity,
            "model_layer0_ffn_gate_weight",
            "ffn_gate_weight",
            "gate_proj.weight",
            source_instance_path="layers.0.feed_forward",
        )
        model_layer0_ffn_up = entry_formal(
            "model_layer0_ffn_up_weight",
            11,
            ffn_up_weight_type,
            ffn_identity,
            "model_layer0_ffn_up_weight",
            "ffn_up_weight",
            "up_proj.weight",
            source_instance_path="layers.0.feed_forward",
        )
        model_layer0_ffn_down = entry_formal(
            "model_layer0_ffn_down_weight",
            12,
            ffn_down_weight_type,
            ffn_identity,
            "model_layer0_ffn_down_weight",
            "ffn_down_weight",
            "down_proj.weight",
            source_instance_path="layers.0.feed_forward",
        )
        model_layer1_attention_norm_scale = entry_formal(
            "model_layer1_attention_norm_scale",
            13,
            scale_type,
            rms_identity,
            "model_layer1_attention_norm_scale",
            "rms_norm_scale",
            "weight",
            source_instance_path="layers.1.attention_norm",
        )
        model_layer1_wq = entry_formal(
            "model_layer1_wq_weight",
            14,
            hidden_weight_type,
            attention_identity,
            "model_layer1_wq_weight",
            "attention_q_weight",
            "wq.weight",
            source_instance_path="layers.1.attention",
        )
        model_layer1_wk = entry_formal(
            "model_layer1_wk_weight",
            15,
            hidden_weight_type,
            attention_identity,
            "model_layer1_wk_weight",
            "attention_k_weight",
            "wk.weight",
            source_instance_path="layers.1.attention",
        )
        model_layer1_wv = entry_formal(
            "model_layer1_wv_weight",
            16,
            hidden_weight_type,
            attention_identity,
            "model_layer1_wv_weight",
            "attention_v_weight",
            "wv.weight",
            source_instance_path="layers.1.attention",
        )
        model_layer1_wo = entry_formal(
            "model_layer1_wo_weight",
            17,
            hidden_weight_type,
            attention_identity,
            "model_layer1_wo_weight",
            "attention_o_weight",
            "wo.weight",
            source_instance_path="layers.1.attention",
        )
        model_layer1_rotary_cos = entry_formal(
            "model_layer1_rotary_cos",
            18,
            rotary_table_type,
            rotary_identity,
            "model_layer1_rotary_cos",
            "rotary_cos",
            source_buffer="cos",
            source_instance_path="layers.1.attention.rotary",
        )
        model_layer1_rotary_sin = entry_formal(
            "model_layer1_rotary_sin",
            19,
            rotary_table_type,
            rotary_identity,
            "model_layer1_rotary_sin",
            "rotary_sin",
            source_buffer="sin",
            source_instance_path="layers.1.attention.rotary",
        )
        model_layer1_ffn_norm_scale = entry_formal(
            "model_layer1_ffn_norm_scale",
            20,
            scale_type,
            rms_identity,
            "model_layer1_ffn_norm_scale",
            "rms_norm_scale",
            "weight",
            source_instance_path="layers.1.ffn_norm",
        )
        model_layer1_ffn_gate = entry_formal(
            "model_layer1_ffn_gate_weight",
            21,
            ffn_up_weight_type,
            ffn_identity,
            "model_layer1_ffn_gate_weight",
            "ffn_gate_weight",
            "gate_proj.weight",
            source_instance_path="layers.1.feed_forward",
        )
        model_layer1_ffn_up = entry_formal(
            "model_layer1_ffn_up_weight",
            22,
            ffn_up_weight_type,
            ffn_identity,
            "model_layer1_ffn_up_weight",
            "ffn_up_weight",
            "up_proj.weight",
            source_instance_path="layers.1.feed_forward",
        )
        model_layer1_ffn_down = entry_formal(
            "model_layer1_ffn_down_weight",
            23,
            ffn_down_weight_type,
            ffn_identity,
            "model_layer1_ffn_down_weight",
            "ffn_down_weight",
            "down_proj.weight",
            source_instance_path="layers.1.feed_forward",
        )
        model_final_norm_scale = entry_formal(
            "model_final_norm_scale",
            24,
            scale_type,
            rms_identity,
            "model_final_norm_scale",
            "rms_norm_scale",
            "weight",
            source_instance_path="norm",
        )
        model_output_weight = entry_formal(
            "model_output_weight",
            25,
            output_weight_type,
            entry_identity,
            "model_output_weight",
            "output_weight",
            "output.weight",
            source_instance_path="<model>",
        )
        model_token_embedding = self.builder().operator(
            transformer.TOKEN_EMBEDDING,
            1,
            [model_token_ids, model_token_embedding_weight],
            {
                "attr.padding_idx": "none",
                "attr.bounds_policy": "runtime_check",
            },
            result_name="model_token_embedding",
            result_type=tensor_type,
        )
        self.builder().attach_value_metadata(
            model_token_embedding,
            self._multi_pu_operator_metadata(
                entry_identity,
                "model_token_embedding",
                "llama2:transformer.token_embedding",
                "token_embedding",
            ),
        )
        self.builder().set_value_source_position(
            model_token_embedding,
            entry_file,
            entry_line + 1,
        )
        self.builder().append_program_unit_value(entry_pu,
                                                model_token_embedding)
        layer0_call = self.builder().create_pu_call(
            entry_pu,
            decoder_pu,
            [
                model_token_embedding,
                model_layer0_attention_norm_scale,
                model_layer0_wq,
                model_layer0_wk,
                model_layer0_wv,
                model_layer0_wo,
                model_layer0_rotary_cos,
                model_layer0_rotary_sin,
                model_layer0_ffn_norm_scale,
                model_layer0_ffn_gate,
                model_layer0_ffn_up,
                model_layer0_ffn_down,
            ],
            ["model_layer0_output"],
            decoder_definition.canonical_name,
            "layers.0",
            f"{entry_name}.layers.0",
            0,
            entry_file,
            entry_line + 2,
        )
        model_layer0_output = self.builder().get_pu_call_result(
            layer0_call,
            0,
            tensor_type,
        )
        self.builder().attach_value_metadata(
            model_layer0_output,
            self._multi_pu_value_metadata(
                decoder_identity,
                "model_layer0_output",
                "call_result",
                "",
            ),
        )
        layer1_call = self.builder().create_pu_call(
            entry_pu,
            decoder_pu,
            [
                model_layer0_output,
                model_layer1_attention_norm_scale,
                model_layer1_wq,
                model_layer1_wk,
                model_layer1_wv,
                model_layer1_wo,
                model_layer1_rotary_cos,
                model_layer1_rotary_sin,
                model_layer1_ffn_norm_scale,
                model_layer1_ffn_gate,
                model_layer1_ffn_up,
                model_layer1_ffn_down,
            ],
            ["model_layer1_output"],
            decoder_definition.canonical_name,
            "layers.1",
            f"{entry_name}.layers.1",
            1,
            entry_file,
            entry_line + 3,
        )
        model_layer1_output = self.builder().get_pu_call_result(
            layer1_call,
            0,
            tensor_type,
        )
        self.builder().attach_value_metadata(
            model_layer1_output,
            self._multi_pu_value_metadata(
                decoder_identity,
                "model_layer1_output",
                "call_result",
                "",
            ),
        )
        final_norm_call = self.builder().create_pu_call(
            entry_pu,
            rms_pu,
            [model_layer1_output, model_final_norm_scale],
            ["model_final_norm_result"],
            rms_definition.canonical_name,
            "norm",
            f"{entry_name}.norm",
            2,
            entry_file,
            entry_line + 4,
        )
        model_final_norm_result = self.builder().get_pu_call_result(
            final_norm_call,
            0,
            tensor_type,
        )
        self.builder().attach_value_metadata(
            model_final_norm_result,
            self._multi_pu_value_metadata(
                rms_identity,
                "model_final_norm_result",
                "call_result",
                "",
            ),
        )
        model_output_projection = self.builder().common_linear_v3(
            model_final_norm_result,
            model_output_weight,
            self._linear_attrs(),
        )
        self.builder().attach_value_metadata(
            model_output_projection,
            self._multi_pu_operator_metadata(
                entry_identity,
                "model_output_projection",
                "llama2:common.linear",
                "output_projection",
            ),
        )
        self.builder().append_program_unit_value(entry_pu,
                                                model_output_projection)
        model_output_logits = self.builder().common_output_logits_v3(
            model_output_projection,
            {
                "attr.semantic": "token_logits",
                "attr.sequence_axis": "-2",
                "attr.vocabulary_axis": "-1",
            },
        )
        self.builder().attach_value_metadata(
            model_output_logits,
            self._multi_pu_operator_metadata(
                entry_identity,
                "model_output_logits",
                "llama2:common.output_logits",
                "output_logits",
            ),
        )
        self.builder().set_value_source_position(
            model_output_logits,
            entry_file,
            entry_line + 5,
        )
        self.builder().append_program_unit_value(entry_pu, model_output_logits)
        self.builder().declare_pu_result(
            entry_pu,
            "model_result",
            0,
            self.builder().value_type(model_output_logits),
            file_id=entry_file,
            line=entry_line + 5,
        )
        self.builder().return_pu_values(entry_pu, [model_output_logits])

        model_module = inspect.getmodule(type(model))
        import_census = (
            collect_python_import_census(model_module)
            if model_module is not None else None
        )
        python_imports = import_census.callables if import_census else ()
        python_import_diagnostics = (
            import_census.diagnostics if import_census else ()
        )
        python_reachable_imports = resolve_reachable_imported_callables(
            model_module,
            python_imports,
            class_definitions,
            class_instances,
            entry_name,
        )
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
                    "call:TinyRotaryEmbedding",
                    "call:TinyLlama2Attention",
                    "call:TinyLlama2DecoderLayer",
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
                transformer.ROTARY_EMBEDDING,
                "call:TinyRotaryEmbedding",
                common.RESHAPE,
                common.TRANSPOSE,
                transformer.ATTENTION,
                "call:TinyLlama2Attention",
                common.RESIDUAL_ADD,
                "call:TinyLlama2DecoderLayer",
                common.OUTPUT_LOGITS,
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
                        "layout": "VC",
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
                    name="llama2_multi_pu_token_ids_type",
                    handle=token_ids_type.value,
                    dtype="int64",
                    rank=2,
                    logical_shape=f"[1,{sequence_length}]",
                    descriptor={
                        "dtype": "int64",
                        "rank": 2,
                        "logical_shape": f"[1,{sequence_length}]",
                        "layout": "BS",
                        "lineage": "python.multi_pu.input_tokens",
                    },
                ),
                WhirlTensorTypeRecord(
                    name="llama2_multi_pu_token_weight_type",
                    handle=token_weight_type.value,
                    dtype="float32",
                    rank=2,
                    logical_shape="[128,32]",
                    descriptor={
                        "dtype": "float32",
                        "rank": 2,
                        "logical_shape": "[128,32]",
                        "layout": "VC",
                        "lineage": "python.multi_pu.token_embedding.weight",
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
                        "layout": "VC",
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
                ),
                WhirlTensorTypeRecord(
                    name="llama2_multi_pu_rotary_bhsd_type",
                    handle=rotary_type.value,
                    dtype="float32",
                    rank=4,
                    logical_shape=rotary_shape,
                    descriptor={
                        "dtype": "float32",
                        "rank": 4,
                        "logical_shape": rotary_shape,
                        "layout": "VC",
                        "lineage": "python.multi_pu.rotary.value",
                    },
                ),
                WhirlTensorTypeRecord(
                    name="llama2_multi_pu_rotary_table_type",
                    handle=rotary_table_type.value,
                    dtype="float32",
                    rank=4,
                    logical_shape=rotary_table_shape,
                    descriptor={
                        "dtype": "float32",
                        "rank": 4,
                        "logical_shape": rotary_table_shape,
                        "layout": "BHSD",
                        "lineage": "python.multi_pu.rotary.table",
                    },
                ),
                WhirlTensorTypeRecord(
                    name="llama2_multi_pu_logits_type",
                    handle=logits_type.value,
                    dtype="float32",
                    rank=3,
                    logical_shape=logits_shape,
                    descriptor={
                        "dtype": "float32",
                        "rank": 3,
                        "logical_shape": logits_shape,
                        "layout": "BSV",
                        "lineage": "python.multi_pu.output.logits",
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
                    name="model_token_ids",
                    handle=model_token_ids.value,
                    type_name="llama2_multi_pu_token_ids_type",
                    value_kind="formal",
                    metadata=self._multi_pu_value_metadata(
                        entry_identity,
                        "model_token_ids",
                        "input_tokens",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="model_token_embedding_weight",
                    handle=model_token_embedding_weight.value,
                    type_name="llama2_multi_pu_token_weight_type",
                    value_kind="formal",
                    metadata=self._multi_pu_value_metadata(
                        entry_identity,
                        "model_token_embedding_weight",
                        "token_embedding_weight",
                        "token_embedding.weight",
                    ),
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
                    name="rotary_value",
                    handle=rotary_value.value,
                    type_name="llama2_multi_pu_rotary_bhsd_type",
                    value_kind="formal",
                    metadata=self._multi_pu_value_metadata(
                        rotary_identity,
                        "rotary_value",
                        "activation",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="rotary_cos",
                    handle=rotary_cos.value,
                    type_name="llama2_multi_pu_rotary_table_type",
                    value_kind="formal",
                    metadata=dict(rotary_cos_metadata),
                ),
                WhirlValueRecord(
                    name="rotary_sin",
                    handle=rotary_sin.value,
                    type_name="llama2_multi_pu_rotary_table_type",
                    value_kind="formal",
                    metadata=dict(rotary_sin_metadata),
                ),
                WhirlValueRecord(
                    name="attention_input",
                    handle=attention_input.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="formal",
                    metadata=self._multi_pu_value_metadata(
                        attention_identity,
                        "attention_input",
                        "activation",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="attention_q_bhsd",
                    handle=attention_q_bhsd.value,
                    type_name="llama2_multi_pu_rotary_bhsd_type",
                    value_kind="operator_result",
                    metadata=self._multi_pu_value_metadata(
                        attention_identity,
                        "attention_q_bhsd",
                        "activation",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="attention_rotary_cos",
                    handle=attention_rotary_cos.value,
                    type_name="llama2_multi_pu_rotary_table_type",
                    value_kind="formal",
                    metadata=dict(attention_rotary_cos_metadata),
                ),
                WhirlValueRecord(
                    name="attention_rotary_sin",
                    handle=attention_rotary_sin.value,
                    type_name="llama2_multi_pu_rotary_table_type",
                    value_kind="formal",
                    metadata=dict(attention_rotary_sin_metadata),
                ),
                WhirlValueRecord(
                    name="attention_query_rope",
                    handle=attention_query_rope.value,
                    type_name="llama2_multi_pu_rotary_bhsd_type",
                    value_kind="call_result",
                    metadata=self._multi_pu_value_metadata(
                        rotary_identity,
                        "attention_query_rope",
                        "call_result",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="attention_key_rope",
                    handle=attention_key_rope.value,
                    type_name="llama2_multi_pu_rotary_bhsd_type",
                    value_kind="call_result",
                    metadata=self._multi_pu_value_metadata(
                        rotary_identity,
                        "attention_key_rope",
                        "call_result",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="decoder_input",
                    handle=decoder_input.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="formal",
                    metadata=self._multi_pu_value_metadata(
                        decoder_identity,
                        "decoder_input",
                        "activation",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="decoder_attention_norm_result",
                    handle=decoder_attention_norm_result.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="call_result",
                    metadata=self._multi_pu_value_metadata(
                        rms_identity,
                        "decoder_attention_norm_result",
                        "call_result",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="decoder_attention_residual",
                    handle=decoder_attention_residual.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="operator_result",
                    metadata=self._multi_pu_value_metadata(
                        decoder_identity,
                        "decoder_attention_residual",
                        "activation",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="decoder_ffn_norm_result",
                    handle=decoder_ffn_norm_result.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="call_result",
                    metadata=self._multi_pu_value_metadata(
                        rms_identity,
                        "decoder_ffn_norm_result",
                        "call_result",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="decoder_ffn_gate_weight",
                    handle=decoder_ffn_gate.value,
                    type_name="llama2_multi_pu_ffn_up_weight_type",
                    value_kind="formal",
                    metadata=dict(decoder_ffn_gate_metadata),
                ),
                WhirlValueRecord(
                    name="decoder_ffn_up_weight",
                    handle=decoder_ffn_up.value,
                    type_name="llama2_multi_pu_ffn_up_weight_type",
                    value_kind="formal",
                    metadata=dict(decoder_ffn_up_metadata),
                ),
                WhirlValueRecord(
                    name="decoder_ffn_down_weight",
                    handle=decoder_ffn_down.value,
                    type_name="llama2_multi_pu_ffn_down_weight_type",
                    value_kind="formal",
                    metadata=dict(decoder_ffn_down_metadata),
                ),
                WhirlValueRecord(
                    name="decoder_ffn_output",
                    handle=decoder_ffn_output.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="call_result",
                    metadata=self._multi_pu_value_metadata(
                        ffn_identity,
                        "decoder_ffn_output",
                        "call_result",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="model_token_embedding",
                    handle=model_token_embedding.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="operator_result",
                    metadata=self._multi_pu_value_metadata(
                        entry_identity,
                        "model_token_embedding",
                        "activation",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="model_layer0_output",
                    handle=model_layer0_output.value,
                    type_name="llama2_multi_pu_hidden_type",
                    value_kind="call_result",
                    metadata=self._multi_pu_value_metadata(
                        decoder_identity,
                        "model_layer0_output",
                        "call_result",
                        "",
                    ),
                ),
                WhirlValueRecord(
                    name="model_output_logits",
                    handle=model_output_logits.value,
                    type_name="llama2_multi_pu_logits_type",
                    value_kind="operator_result",
                    metadata=self._multi_pu_value_metadata(
                        entry_identity,
                        "model_output_logits",
                        "output_logits",
                        "",
                    ),
                ),
            ],
            graph_operators=[
                WhirlOperatorRecord(
                    name="call:TinyRotaryEmbedding.query",
                    handle=attention_query_rope_call.value,
                    kids=[
                        "attention_q_bhsd",
                        "attention_rotary_cos",
                        "attention_rotary_sin",
                    ],
                    attrs={
                        "canonical_class_name": (
                            rotary_definition.canonical_name
                        ),
                        "instance_path": rotary_instance,
                        "context_identity": (
                            f"{attention_context}.rotary.query"
                        ),
                    },
                    metadata={
                        "declaration_kind": "python_class_callable",
                        "callable_identity": rotary_identity[
                            "callable_identity"
                        ],
                        "implementation_method": rotary_identity[
                            "implementation_method"
                        ],
                        "implementation_fingerprint": rotary_identity[
                            "implementation_fingerprint"
                        ],
                        "class_state_buffers": rotary_identity[
                            "class_state_buffers"
                        ],
                        "class_state_scalars": rotary_identity[
                            "class_state_scalars"
                        ],
                    },
                ),
                WhirlOperatorRecord(
                    name="call:TinyLlama2FeedForward.ffn_normed",
                    handle=decoder_ffn_call.value,
                    kids=[
                        "decoder_ffn_norm_result",
                        "decoder_ffn_gate_weight",
                        "decoder_ffn_up_weight",
                        "decoder_ffn_down_weight",
                    ],
                    attrs={
                        "canonical_class_name": (
                            ffn_definition.canonical_name
                        ),
                        "instance_path": "layers.0.feed_forward",
                        "context_identity": f"{decoder_context}.feed_forward",
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
                ),
                WhirlOperatorRecord(
                    name="call:TinyLlama2DecoderLayer.layers.0",
                    handle=layer0_call.value,
                    kids=[
                        "model_token_embedding",
                    ],
                    attrs={
                        "canonical_class_name": (
                            decoder_definition.canonical_name
                        ),
                        "instance_path": "layers.0",
                        "context_identity": f"{entry_name}.layers.0",
                    },
                    metadata={
                        "declaration_kind": "python_class_callable",
                        "callable_identity": decoder_identity[
                            "callable_identity"
                        ],
                        "implementation_method": decoder_identity[
                            "implementation_method"
                        ],
                        "implementation_fingerprint": decoder_identity[
                            "implementation_fingerprint"
                        ],
                        "class_state_submodules": decoder_identity[
                            "class_state_submodules"
                        ],
                        "class_state_scalars": decoder_identity[
                            "class_state_scalars"
                        ],
                    },
                )
            ],
            python_imports=python_imports,
            python_import_diagnostics=python_import_diagnostics,
            python_reachable_imports=python_reachable_imports,
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

    def _multi_pu_buffer_metadata(
        self,
        identity: Mapping[str, str],
        source_layer_name: str,
        tensor_role: str,
        source_buffer: str,
    ) -> Dict[str, str]:
        metadata = self._multi_pu_value_metadata(
            identity,
            source_layer_name,
            tensor_role,
            "",
        )
        if source_buffer:
            metadata["source_buffer"] = source_buffer
            metadata["source_class_state"] = (
                f"{identity['canonical_class_name']}.{source_buffer}"
            )
            metadata["source_instance_state"] = (
                f"{identity['instance_path']}.{source_buffer}"
            )
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

    def _is_resnet20_model(self, model: Any) -> bool:
        return (
            model.__class__.__name__ == "SecureResNet20" and
            hasattr(model, "conv1") and
            hasattr(model, "bn1") and
            hasattr(model, "layer1") and
            hasattr(model, "layer2") and
            hasattr(model, "layer3") and
            hasattr(model, "avgpool") and
            hasattr(model, "flatten") and
            hasattr(model, "fc")
        )

    def _export_resnet20_multiple_pu_program(
        self,
        model: Any,
        inputs: Sequence[Any],
    ) -> WhirlModule:
        if len(inputs) != 1:
            raise NotImplementedError("ResNet-20 export expects one image tensor")
        if bool(getattr(model, "training", False)):
            raise NotImplementedError("ResNet-20 export requires eval mode")

        model_name = self._model_name(model)
        entry_name = self._model_class_name(model)
        bn_fold_certification = bool(
            getattr(model, "open64_bn_fold_certification_no_relu", False)
        )
        class_definitions, class_instances = collect_python_model_classes(model)
        definitions = {
            definition.class_name: definition
            for definition in class_definitions
        }
        entry_definition = definitions.get(entry_name)
        block_definition = definitions.get("ResNet20Block")
        if entry_definition is None or block_definition is None:
            raise NotImplementedError(
                "ResNet-20 multiple-PU emission requires source definitions"
            )

        tensor_types: List[WhirlTensorTypeRecord] = []
        values: List[WhirlValueRecord] = []
        tensor_payloads: List[WhirlTensorPayloadRecord] = []
        operators: List[str] = []
        body_markers: List[str] = []
        graph_operators: List[WhirlOperatorRecord] = []
        attr_env: Dict[str, _GraphValue] = {}
        tensor_type_names: Dict[int, str] = {}
        entry_value_ids: Set[int] = set()
        clone_cache: Dict[
            Tuple[str, str, str, bool],
            Tuple[ProgramUnitHandle, TensorTypeHandle],
        ] = {}

        def tensor_type(
            name: str,
            shape: Sequence[int],
            layout: str,
            lineage: str,
        ) -> TensorTypeHandle:
            logical_shape = self._format_shape(shape)
            descriptor = {
                "kind": "tensor",
                "dtype": "float32",
                "rank": len(shape),
                "logical_shape": logical_shape,
                "layout": layout,
                "lineage": lineage,
            }
            handle = self.builder().tensor_type(
                name,
                "float32",
                len(shape),
                logical_shape,
                descriptor,
            )
            tensor_type_names.setdefault(handle.value, name)
            tensor_types.append(
                WhirlTensorTypeRecord(
                    name=name,
                    handle=handle.value,
                    dtype="float32",
                    rank=len(shape),
                    logical_shape=logical_shape,
                    descriptor=descriptor,
                )
            )
            return handle

        def value_record(
            name: str,
            handle: ValueHandle,
            tensor: TensorTypeHandle,
            value_kind: str,
            metadata: Mapping[str, str],
        ) -> None:
            symbol = self.builder().value_result_symbol(handle)
            values.append(
                WhirlValueRecord(
                    name=name,
                    handle=handle.value,
                    type_name=tensor_type_names.get(
                        tensor.value,
                        f"type_{tensor.value}",
                    ),
                    value_kind=value_kind,
                    symbol_handle=symbol.value,
                    metadata=dict(metadata),
                )
            )

        entry_identity = self._callable_identity_metadata(
            entry_definition,
            model,
            "<model>",
            entry_name,
        )
        entry_pu = self.builder().minimal_program_unit(entry_name)
        self._set_pu_source_identity(entry_pu, entry_identity)
        entry_file = self.builder().register_source_file(
            entry_pu,
            entry_definition.source_file,
        )
        entry_line = entry_definition.source_line
        try:
            entry_constructor_line = inspect.getsourcelines(
                type(model).__init__,
            )[1]
        except (OSError, TypeError):
            entry_constructor_line = entry_line
        try:
            block_constructor_line = inspect.getsourcelines(
                type(self._module_at_instance_path(model, "layer1.0")).__init__,
            )[1]
        except (OSError, TypeError):
            block_constructor_line = block_definition.source_line
        input_shape = self._input_shape(inputs[0])
        if input_shape != (1, 3, 32, 32):
            raise NotImplementedError(
                "ResNet-20 SYNC-2 profile expects CIFAR-10 input [1,3,32,32]"
            )
        image_type = tensor_type(
            "resnet20_input_nchw_type",
            input_shape,
            "NCHW",
            "python.resnet20.input",
        )
        image = self.builder().declare_pu_formal(
            entry_pu,
            "input0",
            0,
            image_type,
            entry_file,
            entry_line,
        )
        input_metadata = {
            "source_layer_name": "input0",
            "tensor_role": "activation",
            "lowering_hint": "model_input",
            "logical_shape": self._format_shape(input_shape),
            "input_ordinal": "0",
        }
        self.builder().attach_value_metadata(image, input_metadata)
        value_record("input0", image, image_type, "model_input", input_metadata)
        entry_value_ids.add(image.value)
        body_markers.append("input0")

        def parameter_source(target: str) -> Tuple[int, Mapping[str, str]]:
            instance_path = target.rsplit(".", 1)[0] if "." in target else target
            if target.startswith("layer"):
                definition = block_definition
                line = block_constructor_line
            else:
                definition = entry_definition
                line = entry_constructor_line
            return line, {
                "canonical_class_name": definition.canonical_name,
                "callable_identity": (
                    f"{definition.canonical_name}.__init__"
                ),
                "source_definition": "constructor",
                "source_parameter_name": target,
                "instance_path": instance_path,
            }

        def parameter(target: str, role: Optional[str] = None) -> _GraphValue:
            tensor = self._resolve_attr(model, target)
            if tensor is None:
                graph_value = self._absent_parameter_for_target(
                    model,
                    target,
                    tensor_types,
                    values,
                    role,
                )
            else:
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
            source_line, source_metadata = parameter_source(target)
            for record in values:
                if record.handle == graph_value.handle.value:
                    merged_metadata = dict(record.metadata)
                    merged_metadata.update(source_metadata)
                    record.metadata.update(source_metadata)
                    self.builder().attach_value_metadata(
                        graph_value.handle,
                        merged_metadata,
                    )
                    break
            self.builder().set_value_source_position(
                graph_value.handle,
                entry_file,
                source_line,
            )
            graph_value_type = self.builder().value_type(graph_value.handle)
            for record in values:
                if record.handle == graph_value.handle.value and record.type_name:
                    tensor_type_names.setdefault(
                        graph_value_type.value,
                        record.type_name,
                    )
            if graph_value.handle.value not in entry_value_ids:
                self.builder().append_program_unit_value(entry_pu,
                                                        graph_value.handle)
                entry_value_ids.add(graph_value.handle.value)
                body_markers.append(graph_value.name)
            return graph_value

        def emit(
            pu: ProgramUnitHandle,
            operator_name: str,
            operands: Sequence[_GraphValue],
            attrs: Mapping[str, str],
            result_name: str,
            result_type: Optional[TensorTypeHandle],
            identity: Mapping[str, str],
            source_layer: str,
            source_line: int,
            region: Optional[RegionHandle] = None,
            region_inputs: Optional[Set[int]] = None,
            region_values: Optional[Set[int]] = None,
        ) -> _GraphValue:
            self.builder().select_program_unit(pu)
            handle, emitted_attrs = self._emit_operator(
                operator_name,
                [operand.handle for operand in operands],
                attrs,
            )
            metadata = self._multi_pu_operator_metadata(
                identity,
                source_layer,
                f"resnet20:{operator_name}",
                result_name,
            )
            self.builder().attach_value_metadata(handle, metadata)
            self.builder().set_value_source_position(
                handle,
                self.builder().register_source_file(
                    pu,
                    identity["source_file"],
                ),
                source_line,
            )
            if region is None:
                self.builder().append_program_unit_value(pu, handle)
            else:
                if region_inputs is None:
                    region_inputs = set()
                if region_values is None:
                    region_values = set()
                for operand in operands:
                    if (
                        operand.handle.value not in region_inputs and
                        operand.handle.value not in region_values
                    ):
                        self.builder().declare_region_value(
                            region,
                            operand.handle,
                            REGION_INPUT,
                            len(region_inputs),
                        )
                        region_inputs.add(operand.handle.value)
                self.builder().append_region_value(region, handle)
                region_values.add(handle.value)
            operators.append(operator_name)
            body_markers.append(operator_name)
            graph_operators.append(
                WhirlOperatorRecord(
                    name=operator_name,
                    handle=handle.value,
                    kids=[operand.name for operand in operands],
                    attrs=emitted_attrs,
                    metadata=metadata,
                )
            )
            if result_type is not None:
                value_record(result_name, handle, result_type,
                             "operator_result", metadata)
            return _GraphValue(handle, result_name)

        def block_targets(path: str, has_downsample: bool) -> List[str]:
            targets = [
                f"{path}.conv1.weight",
                f"{path}.conv1.bias",
                f"{path}.bn1.weight",
                f"{path}.bn1.bias",
                f"{path}.bn1.running_mean",
                f"{path}.bn1.running_var",
                f"{path}.conv2.weight",
                f"{path}.conv2.bias",
                f"{path}.bn2.weight",
                f"{path}.bn2.bias",
                f"{path}.bn2.running_mean",
                f"{path}.bn2.running_var",
            ]
            if has_downsample:
                targets.extend([
                    f"{path}.downsample.0.weight",
                    f"{path}.downsample.0.bias",
                    f"{path}.downsample.1.weight",
                    f"{path}.downsample.1.bias",
                    f"{path}.downsample.1.running_mean",
                    f"{path}.downsample.1.running_var",
                ])
            return targets

        def block_argument_roles(has_downsample: bool) -> List[str]:
            roles = [
                "cnn.basic_block.conv1.weight",
                "cnn.basic_block.conv1.bias",
                "cnn.basic_block.bn1.scale",
                "cnn.basic_block.bn1.bias",
                "cnn.basic_block.bn1.mean",
                "cnn.basic_block.bn1.variance",
                "cnn.basic_block.conv2.weight",
                "cnn.basic_block.conv2.bias",
                "cnn.basic_block.bn2.scale",
                "cnn.basic_block.bn2.bias",
                "cnn.basic_block.bn2.mean",
                "cnn.basic_block.bn2.variance",
            ]
            if has_downsample:
                roles.extend([
                    "cnn.basic_block.downsample.conv.weight",
                    "cnn.basic_block.downsample.conv.bias",
                    "cnn.basic_block.downsample.bn.scale",
                    "cnn.basic_block.downsample.bn.bias",
                    "cnn.basic_block.downsample.bn.mean",
                    "cnn.basic_block.downsample.bn.variance",
                ])
            return roles

        def define_block_clone(
            path: str,
            block: Any,
            input_ty: TensorTypeHandle,
            output_ty: TensorTypeHandle,
            actuals: Sequence[_GraphValue],
            signature: Tuple[str, str, str, bool],
        ) -> Tuple[ProgramUnitHandle, TensorTypeHandle]:
            cached = clone_cache.get(signature)
            if cached is not None:
                return cached
            suffix = signature[0].replace(",", "x").replace("[", "").replace("]", "")
            suffix = f"{suffix}_to_{signature[1].replace(',', 'x').replace('[', '').replace(']', '')}"
            suffix = f"{suffix}_stride{signature[2].replace(',', 'x')}"
            if signature[3]:
                suffix = f"{suffix}_projection"
            else:
                suffix = f"{suffix}_identity"
            clone_name = f"ResNet20Block__{suffix}"
            clone_identity = self._callable_identity_metadata(
                block_definition,
                block,
                f"signature:{suffix}",
                f"{entry_name}.ResNet20Block[{suffix}]",
            )
            clone_pu = self.builder().minimal_program_unit(clone_name)
            self._set_pu_source_identity(clone_pu, clone_identity)
            block_file = self.builder().register_source_file(
                clone_pu,
                block_definition.source_file,
            )
            block_line = block_definition.source_line
            self.builder().select_program_unit(clone_pu)
            formal_specs = [(f"{suffix}_block_input", input_ty)]
            formal_specs.extend(
                (f"{suffix}_{target.split(f'{path}.', 1)[1].replace('.', '_')}",
                 self.builder().value_type(actual.handle))
                for target, actual in zip(block_targets(path, signature[3]), actuals)
            )
            formals: List[_GraphValue] = []
            for ordinal, (name, formal_type) in enumerate(formal_specs):
                formal = self.builder().declare_pu_formal(
                    clone_pu,
                    name,
                    ordinal,
                    formal_type,
                    block_file,
                    block_line,
                )
                metadata = self._multi_pu_value_metadata(
                    clone_identity,
                    name,
                    "activation" if ordinal == 0 else "parameter",
                    "" if ordinal == 0 else name.replace("_", "."),
                )
                self.builder().attach_value_metadata(formal, metadata)
                value_record(name, formal, formal_type, "formal", metadata)
                formals.append(_GraphValue(formal, name))
            block_region = self.builder().region(
                clone_pu,
                "cnn.basic_block",
                1,
            )
            self.builder().append_program_unit_region(clone_pu, block_region)
            self.builder().set_region_source_position(
                block_region,
                block_file,
                block_line,
            )
            self.builder().set_region_metadata(
                block_region,
                "canonical_class_name",
                block_definition.canonical_name,
            )
            self.builder().set_region_metadata(
                block_region,
                "context_specialization",
                suffix,
            )
            region_inputs: Set[int] = set()
            region_values: Set[int] = set()
            for ordinal, formal in enumerate(formals):
                self.builder().declare_region_value(
                    block_region,
                    formal.handle,
                    REGION_INPUT,
                    ordinal,
                )
                region_inputs.add(formal.handle.value)
            cursor = formals[0]
            offset = 1
            identity = clone_identity
            if signature[3]:
                ds_weight, ds_conv_bias, ds_scale, ds_bias, ds_mean, ds_var = \
                    formals[-6:]
                projected = emit(
                    clone_pu,
                    cnn.CONV2D,
                    [cursor, ds_weight, ds_conv_bias],
                    self._fx_module_static_attrs(cnn.CONV2D,
                                                 block.downsample[0]),
                    f"{suffix}_downsample_conv",
                    output_ty,
                    identity,
                    "downsample.0",
                    block_line + 1,
                    block_region,
                    region_inputs,
                    region_values,
                )
                identity_value = emit(
                    clone_pu,
                    cnn.BATCH_NORM_INFER,
                    [projected, ds_scale, ds_bias, ds_mean, ds_var],
                    self._fx_module_static_attrs(cnn.BATCH_NORM_INFER,
                                                 block.downsample[1]),
                    f"{suffix}_downsample_bn",
                    output_ty,
                    identity,
                    "downsample.1",
                    block_line + 1,
                    block_region,
                    region_inputs,
                    region_values,
                )
            else:
                identity_value = cursor
            conv1_weight, conv1_bias, bn1_scale, bn1_bias, bn1_mean, bn1_var = \
                formals[offset:offset + 6]
            conv2_weight, conv2_bias, bn2_scale, bn2_bias, bn2_mean, bn2_var = \
                formals[offset + 6:offset + 12]
            conv1 = emit(
                clone_pu,
                cnn.CONV2D,
                [cursor, conv1_weight, conv1_bias],
                self._fx_module_static_attrs(cnn.CONV2D, block.conv1),
                f"{suffix}_conv1",
                output_ty,
                identity,
                "conv1",
                block_line + 1,
                block_region,
                region_inputs,
                region_values,
            )
            bn1 = emit(
                clone_pu,
                cnn.BATCH_NORM_INFER,
                [conv1, bn1_scale, bn1_bias, bn1_mean, bn1_var],
                self._fx_module_static_attrs(cnn.BATCH_NORM_INFER, block.bn1),
                f"{suffix}_bn1",
                output_ty,
                identity,
                "bn1",
                block_line + 1,
                block_region,
                region_inputs,
                region_values,
            )
            relu1 = bn1
            if not bn_fold_certification:
                relu1 = emit(
                    clone_pu,
                    common.RELU,
                    [bn1],
                    {},
                    f"{suffix}_relu1",
                    output_ty,
                    identity,
                    "relu",
                    block_line + 1,
                    block_region,
                    region_inputs,
                    region_values,
                )
            conv2 = emit(
                clone_pu,
                cnn.CONV2D,
                [relu1, conv2_weight, conv2_bias],
                self._fx_module_static_attrs(cnn.CONV2D, block.conv2),
                f"{suffix}_conv2",
                output_ty,
                identity,
                "conv2",
                block_line + 1,
                block_region,
                region_inputs,
                region_values,
            )
            bn2 = emit(
                clone_pu,
                cnn.BATCH_NORM_INFER,
                [conv2, bn2_scale, bn2_bias, bn2_mean, bn2_var],
                self._fx_module_static_attrs(cnn.BATCH_NORM_INFER, block.bn2),
                f"{suffix}_bn2",
                output_ty,
                identity,
                "bn2",
                block_line + 1,
                block_region,
                region_inputs,
                region_values,
            )
            residual = emit(
                clone_pu,
                common.RESIDUAL_ADD,
                [bn2, identity_value],
                {},
                f"{suffix}_residual",
                output_ty,
                identity,
                "residual_add",
                block_line + 1,
                block_region,
                region_inputs,
                region_values,
            )
            relu2 = residual
            if not bn_fold_certification:
                relu2 = emit(
                    clone_pu,
                    common.RELU,
                    [residual],
                    {},
                    f"{suffix}_block_output",
                    output_ty,
                    identity,
                    "relu",
                    block_line + 1,
                    block_region,
                    region_inputs,
                    region_values,
                )
            self.builder().declare_region_value(
                block_region,
                relu2.handle,
                REGION_OUTPUT | REGION_RESULT,
                len(region_inputs),
            )
            self.builder().declare_pu_result(
                clone_pu,
                "block_result",
                0,
                output_ty,
                file_id=block_file,
                line=block_line + 1,
            )
            self.builder().return_pu_values(clone_pu, [relu2.handle])
            clone_cache[signature] = (clone_pu, output_ty)
            return clone_cache[signature]

        def emit_entry_op(
            operator_name: str,
            operands: Sequence[_GraphValue],
            attrs: Mapping[str, str],
            result_name: str,
            result_shape: Sequence[int],
            source_layer: str,
            source_line: int,
        ) -> _GraphValue:
            result_type = tensor_type(
                f"{result_name}_type",
                result_shape,
                "NCHW" if len(result_shape) == 4 else "NC",
                f"python.resnet20.{result_name}",
            )
            return emit(
                entry_pu,
                operator_name,
                operands,
                attrs,
                result_name,
                result_type,
                entry_identity,
                source_layer,
                source_line,
            )

        self.builder().select_program_unit(entry_pu)
        current = _GraphValue(image, "input0")
        stem_weight = parameter("conv1.weight", "weight")
        stem_bias = parameter("conv1.bias", "bias")
        stem_bn_scale = parameter("bn1.weight", "batchnorm_scale")
        stem_bn_bias = parameter("bn1.bias", "batchnorm_bias")
        stem_bn_mean = parameter("bn1.running_mean", "batchnorm_running_mean")
        stem_bn_var = parameter("bn1.running_var", "batchnorm_running_var")
        current = emit_entry_op(
            cnn.CONV2D,
            [current, stem_weight, stem_bias],
            self._fx_module_static_attrs(cnn.CONV2D, model.conv1),
            "stem_conv",
            (1, 16, 32, 32),
            "conv1",
            entry_line + 1,
        )
        current = emit_entry_op(
            cnn.BATCH_NORM_INFER,
            [current, stem_bn_scale, stem_bn_bias, stem_bn_mean, stem_bn_var],
            self._fx_module_static_attrs(cnn.BATCH_NORM_INFER, model.bn1),
            "stem_bn",
            (1, 16, 32, 32),
            "bn1",
            entry_line + 1,
        )
        if bn_fold_certification:
            zero_type = self.builder().value_type(current.handle)
            zero_handle = self.builder().typed_tensor_constant(
                "stem_bn_identity_zero",
                zero_type,
                "float32",
                4,
                "[1,16,32,32]",
                "implicit_zero",
                "0",
            )
            self.builder().set_value_source_position(
                zero_handle,
                self.builder().register_source_file(
                    entry_pu, entry_identity["source_file"]
                ),
                entry_line + 1,
            )
            zero_metadata = self._multi_pu_value_metadata(
                entry_identity,
                "stem_bn_identity_zero",
                "activation",
                "bn1_identity",
            )
            self.builder().attach_value_metadata(zero_handle, zero_metadata)
            self.builder().append_program_unit_value(entry_pu, zero_handle)
            value_record(
                "stem_bn_identity_zero",
                zero_handle,
                zero_type,
                "implicit_zero",
                zero_metadata,
            )
            zero = _GraphValue(zero_handle, "stem_bn_identity_zero")
            current = emit_entry_op(
                common.ADD,
                [current, zero],
                {"attr.broadcast_rule": "none"},
                "stem_bn_identity",
                (1, 16, 32, 32),
                "bn1_identity",
                entry_line + 1,
            )
        else:
            current = emit_entry_op(
                common.RELU,
                [current],
                {},
                "stem_relu",
                (1, 16, 32, 32),
                "relu",
                entry_line + 1,
            )
        block_plan = [
            ("layer1.0", (1, 16, 32, 32), (1, 16, 32, 32), "1,1", False),
            ("layer1.1", (1, 16, 32, 32), (1, 16, 32, 32), "1,1", False),
            ("layer1.2", (1, 16, 32, 32), (1, 16, 32, 32), "1,1", False),
            ("layer2.0", (1, 16, 32, 32), (1, 32, 16, 16), "2,2", True),
            ("layer2.1", (1, 32, 16, 16), (1, 32, 16, 16), "1,1", False),
            ("layer2.2", (1, 32, 16, 16), (1, 32, 16, 16), "1,1", False),
            ("layer3.0", (1, 32, 16, 16), (1, 64, 8, 8), "2,2", True),
            ("layer3.1", (1, 64, 8, 8), (1, 64, 8, 8), "1,1", False),
            ("layer3.2", (1, 64, 8, 8), (1, 64, 8, 8), "1,1", False),
        ]
        for call_ordinal, (path, in_shape, out_shape, stride, downsample) in \
                enumerate(block_plan):
            block = self._module_at_instance_path(model, path)
            actuals = [parameter(target) for target in block_targets(path, downsample)]
            input_ty = tensor_type(
                f"{path.replace('.', '_')}_input_type",
                in_shape,
                "NCHW",
                f"python.resnet20.{path}.input",
            )
            output_ty = tensor_type(
                f"{path.replace('.', '_')}_output_type",
                out_shape,
                "NCHW",
                f"python.resnet20.{path}.output",
            )
            signature = (
                self._format_shape(in_shape),
                self._format_shape(out_shape),
                stride,
                downsample,
            )
            clone_pu, clone_result_ty = define_block_clone(
                path,
                block,
                input_ty,
                output_ty,
                actuals,
                signature,
            )
            self.builder().select_program_unit(entry_pu)
            call = self.builder().create_pu_call(
                entry_pu,
                clone_pu,
                [current.handle] + [actual.handle for actual in actuals],
                [f"{path.replace('.', '_')}_output"],
                block_definition.canonical_name,
                path,
                f"{entry_name}.{path}",
                call_ordinal,
                entry_file,
                entry_line + 2 + call_ordinal,
            )
            argument_roles = [
                "cnn.basic_block.input",
                *block_argument_roles(downsample),
            ]
            for argument_ordinal, semantic_role in enumerate(argument_roles):
                self.builder().set_pu_call_argument_role(
                    call,
                    argument_ordinal,
                    argument_ordinal,
                    semantic_role,
                )
            call_result = self.builder().get_pu_call_result(
                call,
                0,
                clone_result_ty,
            )
            call_metadata = self._multi_pu_value_metadata(
                self._callable_identity_metadata(
                    block_definition,
                    block,
                    path,
                    f"{entry_name}.{path}",
                ),
                f"{path}_output",
                "call_result",
                "",
            )
            self.builder().attach_value_metadata(call_result, call_metadata)
            self.builder().set_value_source_position(
                call_result,
                entry_file,
                entry_line + 2 + call_ordinal,
            )
            result_name = f"{path.replace('.', '_')}_output"
            value_record(result_name, call_result, clone_result_ty,
                         "call_result", call_metadata)
            operators.append("call:ResNet20Block")
            body_markers.append("call:ResNet20Block")
            graph_operators.append(
                WhirlOperatorRecord(
                    name=f"call:ResNet20Block.{path}",
                    handle=call_result.value,
                    kids=[current.name] + [actual.name for actual in actuals],
                    attrs={
                        "canonical_class_name": block_definition.canonical_name,
                        "instance_path": path,
                        "context_identity": f"{entry_name}.{path}",
                        "source_ordinal": str(call_ordinal),
                        "compiler_pu_specialization": signature[0] + "->" +
                        signature[1] + ";stride=" + stride +
                        (";projection" if downsample else ";identity"),
                    },
                    metadata=call_metadata,
                )
            )
            current = _GraphValue(call_result, result_name)

        current = emit_entry_op(
            cnn.GLOBAL_AVG_POOL2D,
            [current],
            self._fx_module_static_attrs(cnn.GLOBAL_AVG_POOL2D, model.avgpool),
            "avgpool",
            (1, 64, 1, 1),
            "avgpool",
            entry_line + 12,
        )
        current = emit_entry_op(
            common.FLATTEN,
            [current],
            self._fx_module_static_attrs(common.FLATTEN, model.flatten),
            "flatten",
            (1, 64),
            "flatten",
            entry_line + 13,
        )
        fc_weight = parameter("fc.weight", "weight")
        fc_bias = parameter("fc.bias", "bias")
        current = emit_entry_op(
            common.LINEAR,
            [current, fc_weight, fc_bias],
            self._fx_module_static_attrs(common.LINEAR, model.fc),
            "fc",
            (1, 10),
            "fc",
            entry_line + 14,
        )
        current = emit_entry_op(
            common.OUTPUT_LOGITS,
            [current],
            {"attr.semantic": "logits"},
            "output_logits",
            (1, 10),
            "fc",
            entry_line + 14,
        )
        self.builder().declare_pu_result(
            entry_pu,
            "model_result",
            0,
            self.builder().value_type(current.handle),
            file_id=entry_file,
            line=entry_line + 14,
        )
        self.builder().return_pu_values(entry_pu, [current.handle])

        model_module = inspect.getmodule(type(model))
        import_census = (
            collect_python_import_census(model_module)
            if model_module is not None else None
        )
        python_imports = import_census.callables if import_census else ()
        python_import_diagnostics = (
            import_census.diagnostics if import_census else ()
        )
        python_reachable_imports = resolve_reachable_imported_callables(
            model_module,
            python_imports,
            class_definitions,
            class_instances,
            entry_name,
        )
        return WhirlModule(
            options=self._options,
            model_name=model_name,
            input_count=1,
            entry_function=WhirlProgramUnitRecord(
                name=entry_name,
                handle=entry_pu.value,
                body_markers=body_markers,
            ),
            graph_source="torch.fx+resnet20_multiple_pu_boundary",
            operators=operators,
            tensor_types=tensor_types,
            values=values,
            tensor_payloads=tensor_payloads,
            graph_operators=graph_operators,
            python_imports=python_imports,
            python_import_diagnostics=python_import_diagnostics,
            python_reachable_imports=python_reachable_imports,
            python_class_definitions=class_definitions,
            python_class_instances=class_instances,
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

        type_name = f"{name}_type"
        descriptor = {
            "kind": "tensor",
            "dtype": dtype,
            "rank": rank,
            "logical_shape": logical_shape,
            "traits": parameter_role,
            "layout": "C" if rank == 1 else "contiguous",
            "sharding": "replicated",
            "placement": "side_file",
            "memory": "external_data",
            "quantization": "none",
        }
        tensor_type = self.builder().tensor_type(
            type_name, dtype, rank, logical_shape, descriptor
        )
        handle = self.builder().typed_tensor_constant(
            name,
            tensor_type,
            dtype,
            rank,
            logical_shape,
            value_kind,
            value_text,
        )
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
            checksum,
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
                is_region = module_kind in {
                    "BasicBlock",
                    "Bottleneck",
                    "ResNet20Block",
                }
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
