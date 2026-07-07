"""Mock backend with the same shape as the future native _whirl module."""

from __future__ import annotations

from pathlib import Path
from typing import Mapping, Sequence


def backend_name() -> str:
    return "mock"


def finalize_mapped_image(path: str, module_manifest: Mapping[str, object]) -> bool:
    output_path = Path(path)
    if not str(output_path):
        return False

    lines = [
        "# open64_dsc mock WHIRL artifact",
        "format=mock",
        f"model_name={module_manifest.get('model_name', '')}",
        f"entry={module_manifest.get('entry', '')}",
        f"input_count={module_manifest.get('input_count', 0)}",
    ]

    operators = module_manifest.get("operators", ())
    if isinstance(operators, Sequence) and not isinstance(operators, str):
        for index, operator in enumerate(operators):
            lines.append(f"operator.{index}={operator}")

    output_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True
