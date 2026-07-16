"""Discovery records for Python callables made visible by imports."""

from __future__ import annotations

import ast
from dataclasses import dataclass
import hashlib
import inspect
import os
import textwrap
from types import ModuleType
from typing import Dict, List, Mapping, Sequence, Tuple


@dataclass(frozen=True)
class PythonImportedCallable:
    canonical_name: str
    import_names: Sequence[str]
    kind: str
    module_name: str
    qualified_name: str
    source_file: str
    source_line: int
    signature: str
    implementation_fingerprint: str

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "canonical_name": self.canonical_name,
            "import_names": list(self.import_names),
            "kind": self.kind,
            "module_name": self.module_name,
            "qualified_name": self.qualified_name,
            "source_file": self.source_file,
            "source_line": self.source_line,
            "signature": self.signature,
            "implementation_fingerprint": self.implementation_fingerprint,
        }


def _source_identity(function: object) -> Tuple[str, int, str]:
    try:
        source_file = inspect.getsourcefile(function) or ""
    except (OSError, TypeError):
        source_file = ""
    if source_file:
        source_file = os.path.realpath(source_file)

    try:
        source_lines, source_line = inspect.getsourcelines(function)
        source = textwrap.dedent("".join(source_lines))
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
    fingerprint = hashlib.sha256(normalized.encode("utf-8")).hexdigest()
    return source_file, source_line, fingerprint


def _signature(function: object) -> str:
    try:
        return str(inspect.signature(function))
    except (TypeError, ValueError):
        return ""


def _function_record(
    function: object,
    import_names: Sequence[str],
    kind: str,
) -> PythonImportedCallable:
    module_name = str(getattr(function, "__module__", ""))
    qualified_name = str(getattr(function, "__qualname__", ""))
    source_file, source_line, fingerprint = _source_identity(function)
    return PythonImportedCallable(
        canonical_name=f"{module_name}.{qualified_name}",
        import_names=tuple(sorted(import_names)),
        kind=kind,
        module_name=module_name,
        qualified_name=qualified_name,
        source_file=source_file,
        source_line=source_line,
        signature=_signature(function),
        implementation_fingerprint=fingerprint,
    )


def _class_functions(
    import_name: str,
    imported_class: type,
) -> List[Tuple[str, object]]:
    functions: List[Tuple[str, object]] = []
    for member_name, member in vars(imported_class).items():
        function = member
        if isinstance(member, (classmethod, staticmethod)):
            function = member.__func__
        if inspect.isfunction(function):
            functions.append((f"{import_name}.{member_name}", function))
    return functions


def collect_imported_python_callables(
    module: ModuleType,
) -> Sequence[PythonImportedCallable]:
    """Collect Python-defined callables bound from another module.

    This is a declaration catalog, not yet the certified reachable-PU graph.
    Multiple aliases are folded into one canonical definition record.
    """

    aliases: Dict[Tuple[str, str], List[str]] = {}
    definitions: Dict[Tuple[str, str], Tuple[object, str]] = {}

    for import_name, imported in vars(module).items():
        defining_module = str(getattr(imported, "__module__", ""))
        if not defining_module or defining_module == module.__name__:
            continue

        candidates: List[Tuple[str, object, str]] = []
        if inspect.isfunction(imported):
            candidates.append((import_name, imported, "function"))
        elif inspect.isclass(imported):
            candidates.extend(
                (name, function, "method")
                for name, function in _class_functions(import_name, imported)
            )

        for alias, function, kind in candidates:
            key = (
                str(getattr(function, "__module__", "")),
                str(getattr(function, "__qualname__", "")),
            )
            aliases.setdefault(key, []).append(alias)
            definitions[key] = (function, kind)

    records = [
        _function_record(definitions[key][0], names, definitions[key][1])
        for key, names in aliases.items()
    ]
    return tuple(sorted(records, key=lambda record: record.canonical_name))
