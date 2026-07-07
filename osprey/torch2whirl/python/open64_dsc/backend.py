"""Backend loader for the Python frontend skeleton."""

from __future__ import annotations

from types import ModuleType

from . import _mock_whirl


def load_backend(name: str) -> ModuleType:
    if name == "mock":
        return _mock_whirl

    if name == "native":
        try:
            from . import _whirl  # type: ignore
        except ImportError as exc:
            raise RuntimeError("open64_dsc._whirl native backend is not built") from exc
        return _whirl

    raise ValueError(f"unknown Open64 DSC backend: {name}")
