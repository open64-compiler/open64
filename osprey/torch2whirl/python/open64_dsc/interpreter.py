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
    ProgramUnitHandle,
    RegionHandle,
    ValueHandle,
    WhirlBuilder,
    load_builder,
)
from .mapping import cnn, common
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


@dataclass(frozen=True)
class _MappedOperatorPlan:
    name: str
    attrs: Mapping[str, str]
    metadata: Mapping[str, str]


@dataclass(frozen=True)
class _GraphValue:
    handle: ValueHandle
    name: str


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
        model_name = self._model_name(model)
        entry_pu = self.builder().minimal_program_unit(self._options.entry)
        tensor_types, values, handles = self._build_input_placeholders(inputs)
        tensor_payloads: List[WhirlTensorPayloadRecord] = []
        graph_operators: List[WhirlOperatorRecord] = []
        operators: List[str] = []
        body_markers: List[str] = []
        captured_graph = self._capture_fx_graph(model)
        graph_source = "torch.fx" if captured_graph is not None else "synthetic"

        for value, handle in zip(values, handles):
            self.builder().append_program_unit_value(entry_pu, handle)
            body_markers.append(value.name)

        if captured_graph is not None:
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
        elif len(handles) >= 2:
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

        return WhirlModule(
            options=self._options,
            model_name=model_name,
            input_count=len(inputs),
            entry_function=WhirlProgramUnitRecord(
                name=self._options.entry,
                handle=entry_pu.value,
                body_markers=body_markers,
            ),
            graph_source=graph_source,
            operators=operators,
            tensor_types=tensor_types,
            values=values,
            tensor_payloads=tensor_payloads,
            graph_operators=graph_operators,
        )

    def _bind_model_source_positions(
        self,
        model: Any,
        module: WhirlModule,
    ) -> None:
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
