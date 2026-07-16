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
    prefix = _sample_input_prefix(spec)
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


def _sample_input_prefix(spec: str) -> str:
    for prefix in ("shape:", "int-shape:"):
        if spec.startswith(prefix):
            return prefix
    raise ValueError("sample input must use shape:<dims> or int-shape:<dims>")


def _sample_input_from_spec(spec: str) -> Any:
    try:
        import torch
    except ImportError as exc:
        raise RuntimeError("torch is required for --sample-input") from exc

    shape = _parse_shape_spec(spec)
    if _sample_input_prefix(spec) == "int-shape:":
        element_count = 1
        for dim in shape:
            element_count *= dim
        return torch.arange(element_count, dtype=torch.int64).reshape(shape)
    return torch.ones(shape, dtype=torch.float32)


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


def _load_model_from_module(module: ModuleType, factory_name: str) -> Any:
    factory = getattr(module, factory_name, None)
    if factory is None:
        raise AttributeError(f"model factory not found: {factory_name}")
    if not callable(factory):
        raise TypeError(f"model factory is not callable: {factory_name}")

    model = factory()
    if hasattr(model, "eval") and callable(model.eval):
        model.eval()
    return model


def _load_model(path: Path, factory_name: str) -> Any:
    return _load_model_from_module(_load_python_module(path), factory_name)


def _sample_inputs_from_module(module: ModuleType) -> Optional[Sequence[Any]]:
    provider = getattr(module, "open64_sample_inputs", None)
    if provider is None:
        provider = getattr(module, "create_open64_sample_inputs", None)
    if provider is None:
        return None
    if not callable(provider):
        raise TypeError("Open64 sample-input provider is not callable")
    sample_inputs = provider()
    if isinstance(sample_inputs, tuple):
        return sample_inputs
    if isinstance(sample_inputs, list):
        return tuple(sample_inputs)
    return (sample_inputs,)


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
        help=(
            "sample input descriptor: shape:<d0,d1,...> or "
            "int-shape:<d0,d1,...>; ignored when model.py provides "
            "open64_sample_inputs()"
        ),
    )
    parser.add_argument(
        "--backend",
        default="mock",
        choices=("mock", "native"),
        help="open64_dsc backend to use",
    )
    parser.add_argument(
        "--single-pu",
        dest="pu_mode",
        action="store_const",
        const="single",
        default="single",
        help=(
            "select legacy single-PU emission; currently the default and "
            "retained for future performance comparisons"
        ),
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
        module = _load_python_module(Path(args.model))
        model = _load_model_from_module(module, args.model_factory)
    except Exception as exc:
        raise Torch2WhirlCliError(f"Python import/model load failed: {exc}") from exc

    try:
        sample_inputs = _sample_inputs_from_module(module)
        if sample_inputs is None:
            if not args.sample_input:
                raise ValueError(
                    "sample input is required unless model.py provides "
                    "open64_sample_inputs()"
                )
            sample_inputs = [
                _sample_input_from_spec(sample_spec)
                for sample_spec in args.sample_input
            ]
    except Exception as exc:
        raise Torch2WhirlCliError(f"sample input parsing failed: {exc}") from exc

    options = WhirlExportOptions(
        entry=args.entry,
        backend=args.backend,
        pu_mode=args.pu_mode,
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
