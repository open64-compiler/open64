"""Backend loader for the Python frontend skeleton."""

from __future__ import annotations

from typing import Mapping, Sequence, Protocol, cast

from . import _mock_whirl


class WhirlBackend(Protocol):
    def backend_name(self) -> str:
        ...

    def create_tensor_type(
        self,
        name: str,
        dtype: str,
        rank: int,
        logical_shape: str,
    ) -> int:
        ...

    def attach_tensor_descriptor(
        self,
        tensor_type: int,
        descriptor: Mapping[str, object],
    ) -> bool:
        ...

    def create_tensor_constant(
        self,
        name: str,
        dtype: str,
        rank: int,
        logical_shape: str,
        value_kind: str,
        value: str,
    ) -> int:
        ...

    def create_operator(
        self,
        opcode_name: str,
        version: int,
        kids: Sequence[int],
        attrs: Mapping[str, str],
    ) -> int:
        ...

    def create_symbol(
        self,
        name: str,
        tensor_type: int,
    ) -> int:
        ...

    def attach_symbol_metadata(
        self,
        symbol: int,
        metadata: Mapping[str, str],
    ) -> bool:
        ...

    def create_minimal_program_unit(
        self,
        name: str,
    ) -> int:
        ...

    def append_program_unit_marker(
        self,
        program_unit: int,
        marker: int,
    ) -> bool:
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
