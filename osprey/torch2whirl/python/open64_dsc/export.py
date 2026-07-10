"""Public export entry points for torch2whirl Python ingestion."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Iterable, Optional

from .backend import load_backend
from .interpreter import WhirlExportInterpreter
from .module import WhirlModule
from .options import WhirlExportOptions
from .payload import write_external_payloads
from .verifier import verify_module


def export_to_whirl(
    model: Any,
    example_inputs: Iterable[Any],
    options: Optional[WhirlExportOptions] = None,
) -> WhirlModule:
    resolved_options = options or WhirlExportOptions()
    interpreter = WhirlExportInterpreter(resolved_options)
    module = interpreter.export(model, example_inputs)
    if resolved_options.verify:
        verify_module(module)
    return module


def save_as_whirl(module: WhirlModule, path: str) -> None:
    if module.options.verify:
        verify_module(module)
    backend = load_backend(module.options.backend)
    if not backend.finalize_mapped_image(path, module.to_manifest()):
        raise RuntimeError(f"failed to write WHIRL artifact: {Path(path)}")
    write_external_payloads(module, path)
