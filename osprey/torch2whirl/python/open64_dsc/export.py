"""Public export entry points for torch2whirl Python ingestion."""

from __future__ import annotations

import os
from pathlib import Path
import tempfile
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
    output_path = Path(path)
    output_dir = output_path.parent
    temp_path: Optional[Path] = None
    backend = load_backend(module.options.backend)
    try:
        verify_program = getattr(backend, "verify_program", None)
        if verify_program is not None:
            native_result = verify_program()
            if not bool(native_result.get("valid", False)):
                diagnostic = str(native_result.get("diagnostic", ""))
                raise RuntimeError(
                    diagnostic or "native DSL program verification failed"
                )
        output_dir.mkdir(parents=True, exist_ok=True)
        with tempfile.NamedTemporaryFile(
            prefix=f".{output_path.name}.",
            suffix=".tmp",
            dir=str(output_dir),
            delete=False,
        ) as temp_file:
            temp_path = Path(temp_file.name)
        if not backend.finalize_mapped_image(str(temp_path), module.to_manifest()):
            raise RuntimeError(f"failed to write WHIRL artifact: {output_path}")
        write_external_payloads(module, path)
        os.replace(temp_path, output_path)
        temp_path = None
    except Exception:
        abort_program = getattr(backend, "abort_program", None)
        if abort_program is not None:
            abort_program()
        raise
    finally:
        if temp_path is not None:
            temp_path.unlink(missing_ok=True)
