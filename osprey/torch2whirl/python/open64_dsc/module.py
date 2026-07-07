"""In-memory representation returned by the Python export skeleton."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Mapping, Sequence

from .options import WhirlExportOptions


@dataclass(frozen=True)
class WhirlModule:
    options: WhirlExportOptions
    model_name: str
    input_count: int
    operators: Sequence[str] = field(default_factory=list)

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "backend": self.options.backend,
            "entry": self.options.entry,
            "model_name": self.model_name,
            "input_count": self.input_count,
            "operators": list(self.operators),
        }

    def metadata(self) -> Dict[str, object]:
        return dict(self.to_manifest())
