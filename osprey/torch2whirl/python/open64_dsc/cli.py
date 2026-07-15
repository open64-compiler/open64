"""Command-line entry point for the torch2whirl Python ingestion path."""

from __future__ import annotations

import argparse
import importlib.util
from pathlib import Path
import sys
from types import ModuleType
from typing import Any, Optional, Sequence, TextIO, Tuple

from .export import export_to_whirl, save_as_whirl
from .options import WhirlExportOptions
from .verifier import WhirlVerificationError


class Torch2WhirlCliError(RuntimeError):
    pass


def _parse_shape_spec(spec: str) -> Tuple[int, ...]:
    prefix = "shape:"
    if not spec.startswith(prefix):
        raise ValueError("sample input must use shape:<dims>")

    body = spec[len(prefix):]
    if not body:
        raise ValueError("sample input shape must not be empty")

    dims = []
    for item in body.split(","):
        if not item:
            raise ValueError("sample input shape has an empty dimension")
        try:
            dim = int(item)
        except ValueError as exc:
            raise ValueError(f"sample input dimension is not an integer: {item}") from exc
        if dim <= 0:
            raise ValueError("sample input dimensions must be positive")
        dims.append(dim)

    return tuple(dims)


def _sample_input_from_spec(spec: str) -> Any:
    try:
        import torch
    except ImportError as exc:
        raise RuntimeError("torch is required for --sample-input") from exc

    return torch.ones(_parse_shape_spec(spec), dtype=torch.float32)


def _load_python_module(path: Path) -> ModuleType:
    if not path.is_file():
        raise FileNotFoundError(f"model file not found: {path}")

    module_name = f"_torch2whirl_model_{abs(hash(path.resolve()))}"
    spec = importlib.util.spec_from_file_location(module_name, path)
    if spec is None or spec.loader is None:
        raise ImportError(f"failed to load model file: {path}")

    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    try:
        spec.loader.exec_module(module)
    except Exception:
        sys.modules.pop(module_name, None)
        raise
    return module


def _load_model(path: Path, factory_name: str) -> Any:
    module = _load_python_module(path)
    factory = getattr(module, factory_name, None)
    if factory is None:
        raise AttributeError(f"model factory not found: {factory_name}")
    if not callable(factory):
        raise TypeError(f"model factory is not callable: {factory_name}")

    model = factory()
    if hasattr(model, "eval") and callable(model.eval):
        model.eval()
    return model


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="torch2whirl-python",
        description="Export a Python/PyTorch model through open64_dsc.",
    )
    parser.add_argument("model", help="Python file containing a model factory")
    parser.add_argument(
        "--entry",
        default="forward",
        help="model entry point name recorded in the WHIRL program unit",
    )
    parser.add_argument(
        "--model-factory",
        default="create_model",
        help="callable in model.py that returns the model",
    )
    parser.add_argument(
        "--sample-input",
        action="append",
        required=True,
        help="sample input descriptor, currently shape:<d0,d1,...>",
    )
    parser.add_argument(
        "--backend",
        default="mock",
        choices=("mock", "native"),
        help="open64_dsc backend to use",
    )
    parser.add_argument(
        "-o",
        "--output",
        required=True,
        help="output WHIRL artifact path",
    )
    return parser


def run(argv: Optional[Sequence[str]] = None) -> int:
    parser = _build_parser()
    args = parser.parse_args(argv)
    output_path = Path(args.output)

    try:
        model = _load_model(Path(args.model), args.model_factory)
    except Exception as exc:
        raise Torch2WhirlCliError(f"Python import/model load failed: {exc}") from exc

    try:
        sample_inputs = [
            _sample_input_from_spec(sample_spec)
            for sample_spec in args.sample_input
        ]
    except Exception as exc:
        raise Torch2WhirlCliError(f"sample input parsing failed: {exc}") from exc

    options = WhirlExportOptions(
        entry=args.entry,
        backend=args.backend,
        external_data_file=_external_data_file_for_output(output_path),
    )
    try:
        module = export_to_whirl(model, sample_inputs, options)
    except NotImplementedError as exc:
        raise Torch2WhirlCliError(f"unsupported operator: {exc}") from exc
    except WhirlVerificationError as exc:
        raise Torch2WhirlCliError(f"gatekeeper verification failed: {exc}") from exc
    except RuntimeError as exc:
        raise Torch2WhirlCliError(f"native builder or graph capture failed: {exc}") from exc

    try:
        save_as_whirl(module, str(output_path))
    except WhirlVerificationError as exc:
        raise Torch2WhirlCliError(f"gatekeeper verification failed: {exc}") from exc
    except RuntimeError as exc:
        raise Torch2WhirlCliError(f"binary finalization failed: {exc}") from exc
    return 0


def _external_data_file_for_output(output_path: Path) -> str:
    return output_path.with_suffix(".safetensors").name


def main(
    argv: Optional[Sequence[str]] = None,
    stderr: TextIO = sys.stderr,
) -> int:
    try:
        return run(argv)
    except Exception as exc:
        print(f"torch2whirl-python: error: {exc}", file=stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
