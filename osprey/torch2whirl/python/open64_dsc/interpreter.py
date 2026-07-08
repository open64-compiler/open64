"""Graph traversal placeholder for the future torch.export / FX bridge."""

from __future__ import annotations

from typing import Any, Iterable, List, Optional, Sequence, Tuple

from .builder import ValueHandle, WhirlBuilder, load_builder
from .module import (
    WhirlModule,
    WhirlOperatorRecord,
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
        tensor_types, values, handles = self._build_input_placeholders(inputs)
        graph_operators: List[WhirlOperatorRecord] = []
        operators: List[str] = []

        if len(handles) >= 2:
            attrs = {"attr.broadcast_rule": "none"}
            add = self.builder().common_add(handles[0], handles[1], attrs)
            operators.append("common.add")
            graph_operators.append(
                WhirlOperatorRecord(
                    name="common.add",
                    handle=add.value,
                    kids=[values[0].name, values[1].name],
                    attrs=attrs,
                )
            )

        return WhirlModule(
            options=self._options,
            model_name=model_name,
            input_count=len(inputs),
            operators=operators,
            tensor_types=tensor_types,
            values=values,
            graph_operators=graph_operators,
        )

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
