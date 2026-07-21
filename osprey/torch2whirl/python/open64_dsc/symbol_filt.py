"""Demangle torch2whirl Python/DSL review symbols.

This is the torch2whirl companion to tools like c++filt.  It does not attempt
to demangle C++ ABI names; it explains the Python callable, PU, owner, and
callsite spellings emitted by the frontend and retained by ir_b2a review
traces.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import re
import sys
from typing import Mapping, Optional, Sequence, TextIO


_CALL_PREFIX = "__WHIRL_DSL_CALL__:"
_IDENT = r"[A-Za-z_][A-Za-z0-9_]*"
_PYTHON_QUALIFIED_NAME = re.compile(
    rf"^{_IDENT}(?:\.{_IDENT}|(?:\.[0-9]+))*$"
)
_CALLABLE_FORWARD = re.compile(
    rf"^(?P<class>{_IDENT}(?:\.{_IDENT})*)\.forward$"
)
_IMPORT_FIELD_PREFIXES = (
    "imported_spelling=",
    "alias_chain=",
    "defining_module=",
    "importing_module=",
    "reexport_source=",
    "declaration_kind=",
)


@dataclass(frozen=True)
class DemangledSymbol:
    original: str
    kind: str
    text: str
    fields: Mapping[str, str]


def _split_record_fields(record: str) -> Mapping[str, str]:
    fields = {}
    for item in record.split(";"):
        item = item.strip()
        if not item:
            continue
        if "=" not in item:
            continue
        key, value = item.split("=", 1)
        key = key.strip()
        value = value.strip()
        if key:
            fields[key] = value
    return fields


def _format_call_record(original: str, fields: Mapping[str, str]) -> str:
    pieces = ["python-call"]
    for key in ("context", "instance", "callee", "class"):
        value = fields.get(key)
        if value:
            pieces.append(f"{key}={value}")
    ordinal = fields.get("ordinal") or fields.get("source_ordinal")
    if ordinal:
        pieces.append(f"ordinal={ordinal}")
    if len(pieces) == 1:
        return original
    return " ".join(pieces)


def _format_python_context(symbol: str) -> Optional[str]:
    if not _PYTHON_QUALIFIED_NAME.match(symbol):
        return None
    if ".layers." in symbol or ".attention" in symbol or ".feed_forward" in symbol:
        return f"python-context {symbol}"
    return None


def _format_import_identity(original: str) -> Optional[DemangledSymbol]:
    fields = _split_record_fields(original)
    if (
        not fields and
        any(original.startswith(prefix) for prefix in _IMPORT_FIELD_PREFIXES)
    ):
        key, value = original.split("=", 1)
        fields = {key.strip(): value.strip()}
    if not any(key in fields for key in (
        "imported_spelling",
        "alias_chain",
        "defining_module",
        "importing_module",
        "reexport_source",
        "declaration_kind",
    )):
        return None

    pieces = ["python-import"]
    for key in (
        "imported_spelling",
        "alias_chain",
        "defining_module",
        "importing_module",
        "reexport_source",
        "declaration_kind",
    ):
        value = fields.get(key)
        if value:
            pieces.append(f"{key}={value}")
    return DemangledSymbol(
        original,
        "python-import",
        " ".join(pieces),
        fields,
    )


def demangle(symbol: str) -> DemangledSymbol:
    """Return a demangled representation, or the input unchanged."""

    original = symbol.strip()
    if not original:
        return DemangledSymbol(symbol, "empty", symbol, {})

    import_identity = _format_import_identity(original)
    if import_identity is not None:
        return import_identity

    if original.startswith(_CALL_PREFIX):
        fields = _split_record_fields(original[len(_CALL_PREFIX):])
        text = _format_call_record(original, fields)
        return DemangledSymbol(original, "dsl-call", text, fields)

    if original.startswith("call:"):
        callee = original[len("call:"):]
        return DemangledSymbol(
            original,
            "call-marker",
            f"python-call callee={callee}",
            {"callee": callee},
        )

    for prefix, kind, label in (
        ("metadata=owner_pu=", "owner-pu", "python-owner-pu"),
        ("owner_pu=", "owner-pu", "python-owner-pu"),
        ("definition=", "definition", "python-definition"),
    ):
        if original.startswith(prefix):
            value = original[len(prefix):]
            return DemangledSymbol(
                original,
                kind,
                f"{label} {value}",
                {kind: value},
            )

    callable_match = _CALLABLE_FORWARD.match(original)
    if callable_match is not None:
        return DemangledSymbol(
            original,
            "python-callable",
            f"python-callable {original} method=forward",
            {"class": callable_match.group("class"), "method": "forward"},
        )

    context = _format_python_context(original)
    if context is not None:
        return DemangledSymbol(
            original,
            "python-context",
            context,
            {"context": original},
        )

    return DemangledSymbol(original, "unknown", original, {})


def demangle_text(symbol: str) -> str:
    return demangle(symbol).text


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="torch2whirl-filt",
        description="Demangle torch2whirl Python/DSL review symbols.",
    )
    parser.add_argument(
        "symbols",
        nargs="*",
        help="symbols or review records to demangle; stdin is used if omitted",
    )
    parser.add_argument(
        "--kind",
        action="store_true",
        help="prefix each output line with the recognized symbol kind",
    )
    return parser


def _emit(symbol: str, show_kind: bool, out: TextIO) -> None:
    demangled = demangle(symbol)
    if show_kind:
        print(f"{demangled.kind}: {demangled.text}", file=out)
    else:
        print(demangled.text, file=out)


def run(
    argv: Optional[Sequence[str]] = None,
    stdin: TextIO = sys.stdin,
    stdout: TextIO = sys.stdout,
) -> int:
    parser = _build_parser()
    args = parser.parse_args(argv)
    if args.symbols:
        for symbol in args.symbols:
            _emit(symbol, args.kind, stdout)
        return 0

    for line in stdin:
        _emit(line.rstrip("\n"), args.kind, stdout)
    return 0


def main() -> int:
    return run()


if __name__ == "__main__":
    raise SystemExit(main())
