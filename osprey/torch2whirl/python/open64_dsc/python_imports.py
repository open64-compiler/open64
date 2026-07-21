"""Discovery records for Python callables made visible by imports."""

from __future__ import annotations

import ast
from dataclasses import dataclass, field
import hashlib
import inspect
import os
import textwrap
from types import ModuleType
from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple

from .python_classes import PythonClassDefinition, PythonClassInstance


@dataclass(frozen=True)
class PythonImportDiagnostic:
    code: str
    message: str
    import_name: str = ""
    source_file: str = ""
    source_line: int = 0

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "code": self.code,
            "message": self.message,
            "import_name": self.import_name,
            "source_file": self.source_file,
            "source_line": self.source_line,
        }


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
    importing_module: str = ""
    import_aliases: Mapping[str, str] = field(default_factory=dict)
    reexport_sources: Mapping[str, str] = field(default_factory=dict)
    declaration_kind: str = "imported_callable"

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "canonical_name": self.canonical_name,
            "import_names": list(self.import_names),
            "kind": self.kind,
            "module_name": self.module_name,
            "qualified_name": self.qualified_name,
            "source_file": self.source_file,
            "source_line": self.source_line,
            "definition_file": self.source_file,
            "definition_line": self.source_line,
            "signature": self.signature,
            "implementation_fingerprint": self.implementation_fingerprint,
            "importing_module": self.importing_module,
            "import_aliases": dict(self.import_aliases),
            "reexport_sources": dict(self.reexport_sources),
            "declaration_kind": self.declaration_kind,
        }


@dataclass(frozen=True)
class PythonReachableImport:
    canonical_name: str
    imported_spelling: str
    defining_module: str
    importing_module: str
    instance_paths: Sequence[str]
    context_identities: Sequence[str]
    definition_file: str
    definition_line: int
    declaration_kind: str
    class_state_members: Sequence[str] = field(default_factory=tuple)
    state_to_formal_mapping: Mapping[str, str] = field(default_factory=dict)

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "canonical_name": self.canonical_name,
            "imported_spelling": self.imported_spelling,
            "defining_module": self.defining_module,
            "importing_module": self.importing_module,
            "instance_paths": list(self.instance_paths),
            "context_identities": list(self.context_identities),
            "definition_file": self.definition_file,
            "definition_line": self.definition_line,
            "declaration_kind": self.declaration_kind,
            "class_state_members": list(self.class_state_members),
            "state_to_formal_mapping": dict(self.state_to_formal_mapping),
        }


@dataclass(frozen=True)
class PythonImportCensus:
    callables: Sequence[PythonImportedCallable]
    diagnostics: Sequence[PythonImportDiagnostic]

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "callables": [
                callable_record.to_manifest()
                for callable_record in self.callables
            ],
            "diagnostics": [
                diagnostic.to_manifest()
                for diagnostic in self.diagnostics
            ],
        }


@dataclass
class _ImportFacts:
    importing_module: str
    source_file: str
    imported_spelling: Dict[str, str] = field(default_factory=dict)
    alias_sources: Dict[str, str] = field(default_factory=dict)
    module_imports: Dict[str, str] = field(default_factory=dict)
    declaration_kinds: Dict[str, str] = field(default_factory=dict)
    diagnostics: List[PythonImportDiagnostic] = field(default_factory=list)


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


def _has_source_file(item: object) -> bool:
    try:
        return bool(inspect.getsourcefile(item))
    except (OSError, TypeError):
        return False


def _module_source(module: ModuleType) -> Tuple[str, Optional[ast.Module]]:
    try:
        source_file = inspect.getsourcefile(module) or ""
    except (OSError, TypeError):
        source_file = ""
    if source_file:
        source_file = os.path.realpath(source_file)
    try:
        source = inspect.getsource(module)
    except (OSError, TypeError):
        return source_file, None
    try:
        return source_file, ast.parse(source)
    except SyntaxError:
        return source_file, None


def _name_root(node: ast.AST) -> str:
    current = node
    while isinstance(current, ast.Attribute):
        current = current.value
    if isinstance(current, ast.Name):
        return current.id
    return ""


def _target_name(node: ast.AST) -> str:
    if isinstance(node, ast.Name):
        return node.id
    return ""


def _record_diagnostic(
    facts: _ImportFacts,
    code: str,
    message: str,
    import_name: str,
    line: int,
) -> None:
    facts.diagnostics.append(
        PythonImportDiagnostic(
            code=code,
            message=message,
            import_name=import_name,
            source_file=facts.source_file,
            source_line=line,
        )
    )


def _collect_import_facts(module: ModuleType) -> _ImportFacts:
    source_file, tree = _module_source(module)
    facts = _ImportFacts(str(module.__name__), source_file)
    if tree is None:
        _record_diagnostic(
            facts,
            "source_unavailable",
            "module source is unavailable for import analysis",
            str(module.__name__),
            0,
        )
        return facts

    seen_import_names: Dict[str, str] = {}
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                local_name = alias.asname or alias.name.split(".", 1)[0]
                facts.imported_spelling[local_name] = alias.name
                facts.module_imports[local_name] = alias.name
                facts.declaration_kinds[local_name] = "module_import"
                prior = seen_import_names.setdefault(local_name, alias.name)
                if prior != alias.name:
                    _record_diagnostic(
                        facts,
                        "import_name_conflict",
                        "one local name is bound by multiple imports",
                        local_name,
                        node.lineno,
                    )
        elif isinstance(node, ast.ImportFrom):
            module_name = node.module or ""
            for alias in node.names:
                if alias.name == "*":
                    _record_diagnostic(
                        facts,
                        "wildcard_import",
                        "wildcard import cannot be traced to one definition",
                        module_name,
                        node.lineno,
                    )
                    continue
                local_name = alias.asname or alias.name
                spelling = f"{module_name}.{alias.name}"
                facts.imported_spelling[local_name] = spelling
                facts.declaration_kinds[local_name] = "direct_import"
                prior = seen_import_names.setdefault(local_name, spelling)
                if prior != spelling:
                    _record_diagnostic(
                        facts,
                        "import_name_conflict",
                        "one local name is bound by multiple imports",
                        local_name,
                        node.lineno,
                    )
        elif isinstance(node, (ast.Assign, ast.AnnAssign, ast.AugAssign)):
            targets: Sequence[ast.AST]
            value: Optional[ast.AST]
            if isinstance(node, ast.Assign):
                targets = node.targets
                value = node.value
            elif isinstance(node, ast.AnnAssign):
                targets = (node.target,)
                value = node.value
            else:
                targets = (node.target,)
                value = None

            for target in targets:
                root = _name_root(target)
                if root in facts.imported_spelling:
                    _record_diagnostic(
                        facts,
                        "monkey_patch_import",
                        "assignment mutates an imported binding",
                        root,
                        node.lineno,
                    )

            if value is not None:
                source_alias = _target_name(value)
                if source_alias in facts.imported_spelling:
                    for target in targets:
                        target_alias = _target_name(target)
                        if not target_alias:
                            continue
                        facts.imported_spelling[target_alias] = (
                            facts.imported_spelling[source_alias]
                        )
                        facts.alias_sources[target_alias] = source_alias
                        facts.declaration_kinds[target_alias] = "reexport_alias"
        elif isinstance(node, ast.Call):
            if isinstance(node.func, ast.Name) and node.func.id == "__import__":
                _record_diagnostic(
                    facts,
                    "dynamic_import",
                    "dynamic __import__ is not a stable import identity",
                    "__import__",
                    node.lineno,
                )
            elif (
                isinstance(node.func, ast.Attribute) and
                node.func.attr == "import_module" and
                isinstance(node.func.value, ast.Name) and
                node.func.value.id == "importlib"
            ):
                _record_diagnostic(
                    facts,
                    "dynamic_import",
                    "importlib.import_module is not a stable import identity",
                    "importlib.import_module",
                    node.lineno,
                )

    return facts


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


def _callable_candidates(
    import_name: str,
    imported: object,
) -> Sequence[Tuple[str, object, str]]:
    if inspect.isfunction(imported):
        return ((import_name, imported, "function"),)
    if inspect.isclass(imported):
        return tuple(
            (name, function, "method")
            for name, function in _class_functions(import_name, imported)
        )
    return ()


def _module_candidates(
    import_name: str,
    imported_module: ModuleType,
) -> Sequence[Tuple[str, object, str]]:
    candidates: List[Tuple[str, object, str]] = []
    for member_name, member in vars(imported_module).items():
        if str(getattr(member, "__module__", "")) != imported_module.__name__:
            continue
        candidates.extend(
            _callable_candidates(f"{import_name}.{member_name}", member)
        )
    return tuple(candidates)


def _canonical_key(function: object) -> Tuple[str, str]:
    return (
        str(getattr(function, "__module__", "")),
        str(getattr(function, "__qualname__", "")),
    )


def _function_record(
    function: object,
    import_names: Sequence[str],
    kind: str,
    importing_module: str,
    import_aliases: Mapping[str, str],
    reexport_sources: Mapping[str, str],
    declaration_kinds: Sequence[str],
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
        importing_module=importing_module,
        import_aliases=dict(sorted(import_aliases.items())),
        reexport_sources=dict(sorted(reexport_sources.items())),
        declaration_kind="+".join(sorted(set(declaration_kinds))),
    )


def _import_spelling(
    facts: _ImportFacts,
    import_name: str,
    imported: object,
) -> str:
    root = import_name.split(".", 1)[0]
    if root in facts.imported_spelling:
        suffix = import_name[len(root):]
        return f"{facts.imported_spelling[root]}{suffix}"
    module_name = str(getattr(imported, "__module__", ""))
    qualified_name = str(getattr(imported, "__qualname__", import_name))
    return f"{module_name}.{qualified_name}"


def _declaration_kind(facts: _ImportFacts, import_name: str) -> str:
    root = import_name.split(".", 1)[0]
    return facts.declaration_kinds.get(root, "imported_callable")


def collect_python_import_census(
    module: ModuleType,
) -> PythonImportCensus:
    """Collect import-visible Python callables and diagnostics.

    The census is review metadata.  It does not claim that every imported
    callable is reached by the exported model.
    """

    facts = _collect_import_facts(module)
    aliases: Dict[Tuple[str, str], List[str]] = {}
    definitions: Dict[Tuple[str, str], Tuple[object, str]] = {}
    alias_spellings: Dict[Tuple[str, str], Dict[str, str]] = {}
    reexport_sources: Dict[Tuple[str, str], Dict[str, str]] = {}
    declaration_kinds: Dict[Tuple[str, str], List[str]] = {}

    for import_name, imported in vars(module).items():
        defining_module = str(getattr(imported, "__module__", ""))
        is_external_callable = (
            defining_module and defining_module != module.__name__
        )
        candidates: Sequence[Tuple[str, object, str]] = ()
        if isinstance(imported, ModuleType):
            if import_name not in facts.module_imports:
                continue
            candidates = _module_candidates(import_name, imported)
        elif is_external_callable:
            candidates = _callable_candidates(import_name, imported)

        if (
            is_external_callable and
            callable(imported) and
            not candidates and
            not _has_source_file(imported)
        ):
            facts.diagnostics.append(
                PythonImportDiagnostic(
                    code="source_unavailable",
                    message="callable source is unavailable for import review",
                    import_name=import_name,
                    source_file=facts.source_file,
                    source_line=0,
                )
            )

        for alias, function, kind in candidates:
            key = _canonical_key(function)
            aliases.setdefault(key, []).append(alias)
            definitions[key] = (function, kind)
            alias_spellings.setdefault(key, {})[alias] = _import_spelling(
                facts,
                alias,
                function,
            )
            root = alias.split(".", 1)[0]
            if root in facts.alias_sources:
                reexport_sources.setdefault(key, {})[alias] = (
                    facts.alias_sources[root]
                )
            declaration_kinds.setdefault(key, []).append(
                _declaration_kind(facts, alias)
            )

    records = [
        _function_record(
            definitions[key][0],
            names,
            definitions[key][1],
            facts.importing_module,
            alias_spellings.get(key, {}),
            reexport_sources.get(key, {}),
            declaration_kinds.get(key, ("imported_callable",)),
        )
        for key, names in aliases.items()
    ]
    return PythonImportCensus(
        callables=tuple(sorted(records, key=lambda record: record.canonical_name)),
        diagnostics=tuple(
            sorted(
                facts.diagnostics,
                key=lambda diagnostic: (
                    diagnostic.source_file,
                    diagnostic.source_line,
                    diagnostic.code,
                    diagnostic.import_name,
                ),
            )
        ),
    )


def collect_imported_python_callables(
    module: ModuleType,
) -> Sequence[PythonImportedCallable]:
    """Collect Python-defined callables bound from another module.

    This compatibility wrapper returns only callable records.  Use
    `collect_python_import_census` when diagnostics are needed.
    """

    return collect_python_import_census(module).callables


def _state_members(instance: Optional[PythonClassInstance]) -> Sequence[str]:
    if instance is None:
        return ()
    members = list(instance.parameters)
    members.extend(instance.buffers)
    members.extend(sorted(instance.scalar_state))
    return tuple(members)


def _state_to_formal_mapping(
    instance: Optional[PythonClassInstance],
) -> Mapping[str, str]:
    if instance is None:
        return {}
    mapping: Dict[str, str] = {}
    for member in instance.parameters:
        mapping[f"self.{member}"] = f"{instance.instance_path}.{member}"
    for member in instance.buffers:
        mapping[f"self.{member}"] = f"{instance.instance_path}.{member}"
    for member in sorted(instance.scalar_state):
        mapping[f"self.{member}"] = instance.scalar_state[member]
    return mapping


def resolve_reachable_imported_callables(
    importing_module: Optional[ModuleType],
    imported_callables: Sequence[PythonImportedCallable],
    class_definitions: Sequence[PythonClassDefinition],
    class_instances: Sequence[PythonClassInstance],
    entry_class_name: str,
) -> Sequence[PythonReachableImport]:
    """Resolve which imported class callables are reached by model instances."""

    if importing_module is None:
        return ()
    importing_module_name = str(importing_module.__name__)
    callables_by_name = {
        callable_record.canonical_name: callable_record
        for callable_record in imported_callables
    }
    definitions_by_name = {
        definition.canonical_name: definition
        for definition in class_definitions
    }
    instances_by_class: Dict[str, List[PythonClassInstance]] = {}
    for instance in class_instances:
        if instance.canonical_class_name.startswith(
            f"{importing_module_name}."
        ):
            continue
        instances_by_class.setdefault(
            instance.canonical_class_name,
            [],
        ).append(instance)

    reachable: List[PythonReachableImport] = []
    for canonical_class_name, instances in sorted(instances_by_class.items()):
        callable_name = f"{canonical_class_name}.forward"
        imported_record = callables_by_name.get(callable_name)
        definition = definitions_by_name.get(canonical_class_name)
        if imported_record is None or definition is None:
            continue
        imported_spelling = (
            imported_record.import_names[0]
            if imported_record.import_names else callable_name
        )
        instance_paths = tuple(instance.instance_path for instance in instances)
        context_identities = tuple(
            entry_class_name if path == "<model>" else f"{entry_class_name}.{path}"
            for path in instance_paths
        )
        primary_instance = instances[0] if instances else None
        reachable.append(
            PythonReachableImport(
                canonical_name=callable_name,
                imported_spelling=imported_spelling,
                defining_module=imported_record.module_name,
                importing_module=importing_module_name,
                instance_paths=instance_paths,
                context_identities=context_identities,
                definition_file=imported_record.source_file,
                definition_line=imported_record.source_line,
                declaration_kind=imported_record.declaration_kind,
                class_state_members=_state_members(primary_instance),
                state_to_formal_mapping=_state_to_formal_mapping(
                    primary_instance
                ),
            )
        )
    return tuple(reachable)
