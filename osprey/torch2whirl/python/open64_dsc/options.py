"""Export options for the Open64 Python frontend skeleton."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional


@dataclass(frozen=True)
class WhirlExportOptions:
    entry: str = "forward"
    backend: str = "mock"
    model_name: Optional[str] = None
    external_data_file: Optional[str] = None
    verify: bool = True
    pu_mode: str = "single"

    def __post_init__(self) -> None:
        if not self.entry:
            raise ValueError("entry must not be empty")
        if self.backend not in {"mock", "native"}:
            raise ValueError("backend must be 'mock' or 'native'")
        if self.pu_mode not in {"single", "multiple"}:
            raise ValueError("pu_mode must be 'single' or 'multiple'")
        if self.external_data_file is not None:
            if not self.external_data_file:
                raise ValueError("external_data_file must not be empty")
            if "/" in self.external_data_file or "\\" in self.external_data_file:
                raise ValueError("external_data_file must be a file name")
