"""Graph traversal placeholder for the future torch.export / FX bridge."""

from __future__ import annotations

from typing import Any, Iterable, List, Optional

from .builder import WhirlBuilder, load_builder
from .module import WhirlModule
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
        operators: List[str] = []

        return WhirlModule(
            options=self._options,
            model_name=model_name,
            input_count=len(inputs),
            operators=operators,
        )

    def _model_name(self, model: Any) -> str:
        if self._options.model_name:
            return self._options.model_name
        if hasattr(model, "__class__"):
            return model.__class__.__name__
        return type(model).__name__
