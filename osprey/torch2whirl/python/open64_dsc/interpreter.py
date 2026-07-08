"""Graph traversal placeholder for the future torch.export / FX bridge."""

from __future__ import annotations

import operator
from typing import Any, Iterable, List, Optional, Sequence, Tuple

from .builder import ValueHandle, WhirlBuilder, load_builder
from .mapping import common
from .module import (
    WhirlModule,
    WhirlOperatorRecord,
    WhirlProgramUnitRecord,
    WhirlTensorTypeRecord,
    WhirlValueRecord,
)
from .options import WhirlExportOptions


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
        captured_operators = self._captured_graph_operators(model)
        graph_operators: List[WhirlOperatorRecord] = []
        operators: List[str] = []
        body_markers: List[str] = []
        graph_source = "torch.fx" if captured_operators else "synthetic"

        for value, handle in zip(values, handles):
            self.builder().append_program_unit_marker(entry_pu, handle)
            body_markers.append(value.name)

        graph_plan = captured_operators
        if not graph_plan and len(handles) >= 2:
            graph_plan = [common.ADD]

        for operator_name in graph_plan:
            if len(handles) < 2:
                raise ValueError(f"{operator_name} requires at least two inputs")
            handle, attrs = self._emit_binary_operator(
                operator_name,
                handles[0],
                handles[1],
            )
            self.builder().append_program_unit_marker(entry_pu, handle)
            operators.append(operator_name)
            body_markers.append(operator_name)
            graph_operators.append(
                WhirlOperatorRecord(
                    name=operator_name,
                    handle=handle.value,
                    kids=[values[0].name, values[1].name],
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

    def _emit_binary_operator(
        self,
        operator_name: str,
        lhs: ValueHandle,
        rhs: ValueHandle,
    ) -> Tuple[ValueHandle, dict]:
        if operator_name == common.ADD:
            attrs = {"attr.broadcast_rule": "none"}
            return self.builder().common_add(lhs, rhs, attrs), attrs
        if operator_name == common.MATMUL:
            if self.builder().backend_name() == "native":
                raise NotImplementedError(
                    "native common.matmul marker append is not ready"
                )
            attrs = {
                "attr.transpose_kid0": "false",
                "attr.transpose_kid1": "false",
            }
            return self.builder().common_matmul(lhs, rhs, attrs), attrs

        raise NotImplementedError(f"unsupported mapped operator: {operator_name}")

    def _captured_graph_operators(self, model: Any) -> List[str]:
        graph = self._capture_fx_graph(model)
        if graph is None:
            return []

        operators: List[str] = []
        for node in getattr(graph, "nodes", ()):
            node_op = str(getattr(node, "op", ""))
            if node_op in {"placeholder", "output", "get_attr"}:
                continue

            operator_name = self._map_fx_node(node)
            if operator_name is None:
                raise NotImplementedError(
                    "unsupported FX graph node: "
                    f"{node_op}:{getattr(node, 'target', '')}"
                )
            operators.append(operator_name)
        return operators

    def _capture_fx_graph(self, model: Any) -> Optional[Any]:
        try:
            from torch.fx import symbolic_trace
        except ImportError:
            return None

        try:
            traced = symbolic_trace(model)
        except Exception:
            return None
        return getattr(traced, "graph", None)

    def _map_fx_node(self, node: Any) -> Optional[str]:
        node_op = str(getattr(node, "op", ""))
        target = getattr(node, "target", None)

        if node_op == "call_function" and target is operator.add:
            return common.ADD
        if node_op == "call_function" and target is operator.matmul:
            return common.MATMUL

        target_name = getattr(target, "__name__", str(target))
        if node_op in {"call_function", "call_method"}:
            return common.FX_OPERATOR_MAP.get(target_name)

        return None

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
