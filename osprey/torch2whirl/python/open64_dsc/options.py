"""Export options for the Open64 Python frontend skeleton."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional


@dataclass(frozen=True)
class WhirlExportOptions:
    entry: str = "forward"
    backend: str = "mock"
    model_name: Optional[str] = None
    verify: bool = True

    def __post_init__(self) -> None:
        if not self.entry:
            raise ValueError("entry must not be empty")
        if self.backend not in {"mock", "native"}:
            raise ValueError("backend must be 'mock' or 'native'")
