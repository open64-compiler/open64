"""Class definitions and concrete instance state discovered from a model."""

from __future__ import annotations

import ast
from dataclasses import dataclass
import hashlib
import inspect
import os
import textwrap
from typing import Any, Dict, List, Mapping, Sequence, Tuple


@dataclass(frozen=True)
class PythonClassDefinition:
    canonical_name: str
    class_name: str
    source_file: str
    source_line: int
    implementation_method: str
    implementation_signature: str
    implementation_fingerprint: str

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "canonical_name": self.canonical_name,
            "class_name": self.class_name,
            "source_file": self.source_file,
            "source_line": self.source_line,
            "implementation_method": self.implementation_method,
            "implementation_signature": self.implementation_signature,
            "implementation_fingerprint": self.implementation_fingerprint,
        }


@dataclass(frozen=True)
class PythonClassInstance:
    instance_path: str
    canonical_class_name: str
    parameters: Sequence[str]
    buffers: Sequence[str]
    scalar_state: Mapping[str, str]
    submodules: Sequence[str]

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "instance_path": self.instance_path,
            "canonical_class_name": self.canonical_class_name,
            "parameters": list(self.parameters),
            "buffers": list(self.buffers),
            "scalar_state": dict(self.scalar_state),
            "submodules": list(self.submodules),
        }


def _canonical_class_name(instance: Any) -> str:
    instance_class = type(instance)
    return f"{instance_class.__module__}.{instance_class.__qualname__}"


def _implementation_identity(
    implementation: object,
) -> Tuple[str, int, str, str]:
    try:
        source_file = inspect.getsourcefile(implementation) or ""
    except (OSError, TypeError):
        source_file = ""
    if source_file:
        source_file = os.path.realpath(source_file)

    try:
        lines, source_line = inspect.getsourcelines(implementation)
        source = textwrap.dedent("".join(lines))
    except (OSError, TypeError):
        source_line = 0
        source = ""
    try:
        normalized = ast.dump(
            ast.parse(source),
            annotate_fields=True,
            include_attributes=False,
        )
    except SyntaxError:
        normalized = source

    try:
        signature = str(inspect.signature(implementation))
    except (TypeError, ValueError):
        signature = ""
    fingerprint = hashlib.sha256(normalized.encode("utf-8")).hexdigest()
    return source_file, source_line, signature, fingerprint


def _direct_names(instance: Any, method_name: str) -> Sequence[str]:
    method = getattr(instance, method_name, None)
    if not callable(method):
        return ()
    try:
        return tuple(name for name, unused in method(recurse=False))
    except TypeError:
        return ()


def _scalar_state(instance: Any) -> Mapping[str, str]:
    state: Dict[str, str] = {}
    for name, value in vars(instance).items():
        if name.startswith("_"):
            continue
        if isinstance(value, (bool, float, int, str)):
            state[name] = str(value)
    return state


def collect_python_model_classes(
    model: Any,
) -> Tuple[Sequence[PythonClassDefinition], Sequence[PythonClassInstance]]:
    """Collect model class definitions and per-instance state declarations."""

    named_modules = getattr(model, "named_modules", None)
    if not callable(named_modules):
        modules = (("", model),)
    else:
        modules = tuple(named_modules())

    definitions: Dict[str, PythonClassDefinition] = {}
    instances: List[PythonClassInstance] = []
    for path, instance in modules:
        canonical_name = _canonical_class_name(instance)
        implementation = getattr(type(instance), "forward", None)
        if inspect.isfunction(implementation):
            source_file, source_line, signature, fingerprint = (
                _implementation_identity(implementation)
            )
            if source_line != 0:
                definitions[canonical_name] = PythonClassDefinition(
                    canonical_name=canonical_name,
                    class_name=type(instance).__name__,
                    source_file=source_file,
                    source_line=source_line,
                    implementation_method="forward",
                    implementation_signature=signature,
                    implementation_fingerprint=fingerprint,
                )

        submodules = getattr(instance, "_modules", {})
        instances.append(
            PythonClassInstance(
                instance_path=str(path) or "<model>",
                canonical_class_name=canonical_name,
                parameters=_direct_names(instance, "named_parameters"),
                buffers=_direct_names(instance, "named_buffers"),
                scalar_state=_scalar_state(instance),
                submodules=tuple(
                    str(name)
                    for name, submodule in submodules.items()
                    if submodule is not None
                ),
            )
        )

    return (
        tuple(definitions[name] for name in sorted(definitions)),
        tuple(instances),
    )
