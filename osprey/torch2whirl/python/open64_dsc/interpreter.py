"""Graph traversal placeholder for the future torch.export / FX bridge."""

from __future__ import annotations

from dataclasses import dataclass
import operator
from typing import Any, Dict, Iterable, List, Mapping, Optional, Sequence, Tuple

from .builder import ValueHandle, WhirlBuilder, load_builder
from .mapping import cnn, common
from .module import (
    WhirlModule,
    WhirlOperatorRecord,
    WhirlProgramUnitRecord,
    WhirlTensorTypeRecord,
    WhirlValueRecord,
)
from .options import WhirlExportOptions


@dataclass(frozen=True)
class _MappedOperatorPlan:
    name: str
    attrs: Mapping[str, str]


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
        inputs = list(example_inputs)
        model_name = self._model_name(model)
        entry_pu = self.builder().minimal_program_unit(self._options.entry)
        tensor_types, values, handles = self._build_input_placeholders(inputs)
        graph_operators: List[WhirlOperatorRecord] = []
        operators: List[str] = []
        body_markers: List[str] = []
        captured_graph = self._capture_fx_graph(model)
        graph_source = "torch.fx" if captured_graph is not None else "synthetic"

        for value, handle in zip(values, handles):
            self.builder().append_program_unit_marker(entry_pu, handle)
            body_markers.append(value.name)

        if captured_graph is not None:
            self._emit_captured_graph(
                captured_graph,
                model_name,
                entry_pu,
                handles,
                tensor_types,
                values,
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
            self.builder().append_program_unit_marker(entry_pu, handle)
            operators.append(common.ADD)
            body_markers.append(common.ADD)
            graph_operators.append(
                WhirlOperatorRecord(
                    name=common.ADD,
                    handle=handle.value,
                    kids=[value.name for value in values[:2]],
                    attrs=attrs,
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
            graph_operators=graph_operators,
        )

    def _emit_captured_graph(
        self,
        captured_graph: Tuple[Any, Any],
        model_name: str,
        entry_pu: Any,
        input_handles: Sequence[ValueHandle],
        tensor_types: List[WhirlTensorTypeRecord],
        values: List[WhirlValueRecord],
        operators: List[str],
        body_markers: List[str],
        graph_operators: List[WhirlOperatorRecord],
    ) -> None:
        graph, traced_module = captured_graph
        env: Dict[int, _GraphValue] = {}
        attr_env: Dict[str, _GraphValue] = {}
        input_index = 0
        output_source_node = None

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
                    attr_env,
                )
                env[id(node)] = graph_value
                self.builder().append_program_unit_marker(entry_pu, graph_value.handle)
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
            operands = self._fx_operator_operands(
                operator_plan.name,
                node,
                env,
                traced_module,
                model_name,
                entry_pu,
                tensor_types,
                values,
                body_markers,
                attr_env,
            )
            if len(operands) < self._operator_arity(operator_plan.name):
                raise ValueError(f"{operator_plan.name} has too few FX operands")
            handle, attrs = self._emit_operator(
                operator_plan.name,
                [operand.handle for operand in operands],
                operator_plan.attrs,
            )
            env[id(node)] = _GraphValue(handle, operator_plan.name)
            self.builder().append_program_unit_marker(entry_pu, handle)
            operators.append(operator_plan.name)
            body_markers.append(operator_plan.name)
            graph_operators.append(
                WhirlOperatorRecord(
                    name=operator_plan.name,
                    handle=handle.value,
                    kids=[operand.name for operand in operands],
                    attrs=attrs,
                )
            )

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
            {"attr.semantic": "classifier_logits"},
        )
        self.builder().append_program_unit_marker(entry_pu, handle)
        operators.append(common.OUTPUT_LOGITS)
        body_markers.append(common.OUTPUT_LOGITS)
        graph_operators.append(
            WhirlOperatorRecord(
                name=common.OUTPUT_LOGITS,
                handle=handle.value,
                kids=[output_value.name],
                attrs=attrs,
            )
        )

    def _operator_arity(self, operator_name: str) -> int:
        if (
            operator_name in common.UNARY_OPERATORS or
            operator_name in cnn.UNARY_OPERATORS
        ):
            return 1
        if operator_name in {common.ADD, common.MATMUL, common.RESIDUAL_ADD}:
            return 2
        if (
            operator_name in common.TERNARY_OPERATORS or
            operator_name in cnn.TERNARY_OPERATORS
        ):
            return 3
        if operator_name in cnn.FIVE_INPUT_OPERATORS:
            return 5

        raise NotImplementedError(f"unsupported mapped operator: {operator_name}")

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
                "attr.momentum": "0.1",
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
        body_markers: List[str],
        attr_env: Dict[str, _GraphValue],
    ) -> List[_GraphValue]:
        args = list(getattr(node, "args", ()))
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
                body_markers,
                attr_env,
            )

        if operator_name == cnn.BATCH_NORM_INFER and len(args) >= 5:
            values = [args[0], args[3], args[4], args[1], args[2]]
        else:
            values = args

        operands: List[_GraphValue] = []
        for value in values:
            operand = self._fx_graph_value(value, env)
            if operand is not None:
                operands.append(operand)
        return operands

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
        body_markers: List[str],
        attr_env: Dict[str, _GraphValue],
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
                    body_markers,
                    attr_env,
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
                    body_markers,
                    attr_env,
                )
            )
        elif operator_name == cnn.BATCH_NORM_INFER:
            for suffix in (
                "weight",
                "bias",
                "running_mean",
                "running_var",
            ):
                operands.append(
                    self._module_parameter_operand(
                        traced_module,
                        f"{target}.{suffix}",
                        model_name,
                        entry_pu,
                        tensor_types,
                        values,
                        body_markers,
                        attr_env,
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
        body_markers: List[str],
        attr_env: Dict[str, _GraphValue],
    ) -> _GraphValue:
        tensor = self._resolve_attr(traced_module, target)
        if tensor is None:
            graph_value = self._absent_parameter_for_target(target, values)
        else:
            graph_value = self._external_tensor_for_target(
                traced_module,
                target,
                model_name,
                tensor_types,
                values,
                attr_env,
            )
        if graph_value.name not in body_markers:
            self.builder().append_program_unit_marker(entry_pu, graph_value.handle)
            body_markers.append(graph_value.name)
        return graph_value

    def _absent_parameter_for_target(
        self,
        target: str,
        values: List[WhirlValueRecord],
    ) -> _GraphValue:
        name = self._external_value_name(target)
        if name in {value.name for value in values}:
            for value in values:
                if value.name == name:
                    return _GraphValue(ValueHandle(value.handle), value.name)

        handle = self.builder().tensor_constant(
            name,
            "float32",
            0,
            "[]",
            "absent_parameter",
            "none",
        )
        values.append(
            WhirlValueRecord(
                name=name,
                handle=handle.value,
                type_name="",
                value_kind="absent_parameter",
                metadata={
                    "source_layer_name": name,
                    "lowering_hint": "module_parameter_absent",
                    "tensor_role": self._parameter_role(target),
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
        attr_env: Dict[str, _GraphValue],
    ) -> _GraphValue:
        if target in attr_env:
            return attr_env[target]

        tensor = self._resolve_attr(traced_module, target)
        dtype = self._input_dtype(tensor)
        shape = self._input_shape(tensor)
        logical_shape = self._format_shape(shape)
        role = self._parameter_role(target)
        byte_length = self._tensor_byte_length(tensor, dtype, shape)
        byte_offset = self._next_external_offset(values)
        name = self._external_value_name(target)
        side_file = f"{model_name}.safetensors"
        handle = self.builder().external_tensor_constant(
            name,
            dtype,
            len(shape),
            logical_shape,
            role,
            "safetensors",
            side_file,
            target,
            byte_offset,
            byte_length,
            "",
            self._parameter_layout(role, len(shape)),
        )
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
                metadata=handle.metadata,
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
            from torch.fx import symbolic_trace
        except ImportError:
            return None

        try:
            traced = symbolic_trace(model)
        except Exception:
            return None
        graph = getattr(traced, "graph", None)
        if graph is None:
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
            return _MappedOperatorPlan(common.ADD, {})
        if node_op == "call_function" and target is operator.matmul:
            return _MappedOperatorPlan(common.MATMUL, {})

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
            )

        if node_op == "call_module" and traced_module is not None:
            module = self._resolve_attr(traced_module, str(target))
            operator_name = self._map_fx_module(module)
            if operator_name is None:
                return None
            return _MappedOperatorPlan(
                operator_name,
                self._fx_module_static_attrs(operator_name, module),
            )

        return None

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
                "attr.momentum": str(getattr(module, "momentum", 0.1)),
                "attr.training": "false",
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
                "attr.momentum": self._fx_text_attr(
                    args,
                    kwargs,
                    "momentum",
                    6,
                    "0.1",
                ),
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
            tensor_type = self.builder().tensor_type(
                type_name,
                dtype,
                len(shape),
                logical_shape,
            )
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
                "runtime_state": "static",
                "lineage": name,
            }
            self.builder().attach_tensor_descriptor(tensor_type, descriptor)
            value = self.builder().tensor_constant(
                name,
                dtype,
                len(shape),
                logical_shape,
                "example_input",
                name,
            )
            symbol = self.builder().symbol(name, tensor_type)
            metadata = {
                "source_layer_name": name,
                "lowering_hint": "example_input",
            }
            self.builder().attach_symbol_metadata(symbol, metadata)

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
                    value_kind="example_input",
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
