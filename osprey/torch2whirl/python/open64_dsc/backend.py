"""Backend loader for the Python frontend skeleton."""

from __future__ import annotations

from typing import Mapping, Protocol, cast

from . import _mock_whirl


class WhirlBackend(Protocol):
    def backend_name(self) -> str:
        ...

    def finalize_mapped_image(
        self,
        path: str,
        module_manifest: Mapping[str, object],
    ) -> bool:
        ...


def load_backend(name: str) -> WhirlBackend:
    if name == "mock":
        return cast(WhirlBackend, _mock_whirl)

    if name == "native":
        try:
            from . import _whirl  # type: ignore
        except ImportError as exc:
            raise RuntimeError("open64_dsc._whirl native backend is not built") from exc
        return cast(WhirlBackend, _whirl)

    raise ValueError(f"unknown Open64 DSC backend: {name}")
