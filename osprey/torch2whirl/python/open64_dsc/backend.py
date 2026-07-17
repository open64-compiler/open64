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

    def intern_tensor_type(
        self,
        name: str,
        descriptor: Mapping[str, object],
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

    def create_model_input(
        self,
        name: str,
        tensor_type: int,
        input_ordinal: int,
    ) -> int:
        ...

    def create_external_tensor_constant(
        self,
        name: str,
        tensor_type: int,
        reference: Mapping[str, object],
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

    def create_operator_with_result(
        self,
        opcode_name: str,
        version: int,
        kids: Sequence[int],
        attrs: Mapping[str, str],
        result_name: str,
        result_type: int,
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

    def attach_value_metadata(
        self,
        value: int,
        metadata: Mapping[str, str],
    ) -> bool:
        ...

    def attach_value_lineage(self, value: int, lineage: str) -> bool:
        ...

    def get_value_type(self, value: int) -> int:
        ...

    def get_value_result_symbol(self, value: int) -> int:
        ...

    def begin_program(self) -> bool:
        ...

    def abort_program(self) -> None:
        ...

    def create_minimal_program_unit(
        self,
        name: str,
    ) -> int:
        ...

    def select_program_unit(self, program_unit: int) -> bool:
        ...

    def set_pu_source_identity(
        self,
        program_unit: int,
        canonical_definition_name: str,
        defining_module: str,
        defining_file: str,
        defining_line: int,
        flags: int,
    ) -> bool:
        ...

    def declare_pu_formal(
        self,
        program_unit: int,
        name: str,
        ordinal: int,
        tensor_type: int,
        file_id: int,
        line: int,
        column: int,
        statement_begin: bool,
        basic_block_begin: bool,
    ) -> int:
        ...

    def declare_pu_result(
        self,
        program_unit: int,
        name: str,
        ordinal: int,
        tensor_type: int,
        role: int,
        file_id: int,
        line: int,
        column: int,
        statement_begin: bool,
        basic_block_begin: bool,
    ) -> int:
        ...

    def return_pu_values(
        self,
        program_unit: int,
        values: Sequence[int],
    ) -> bool:
        ...

    def create_pu_call(
        self,
        caller: int,
        callee: int,
        arguments: Sequence[int],
        result_names: Sequence[str],
        canonical_class_name: str,
        instance_path: str,
        context_identity: str,
        call_ordinal: int,
        file_id: int,
        line: int,
        column: int,
        statement_begin: bool,
        basic_block_begin: bool,
    ) -> int:
        ...

    def get_pu_call_result(self, call: int, ordinal: int) -> int:
        ...

    def register_source_file(self, program_unit: int, path: str) -> int:
        ...

    def set_value_source_position(
        self,
        value: int,
        file_id: int,
        line: int,
        column: int,
        statement_begin: bool,
        basic_block_begin: bool,
    ) -> bool:
        ...

    def create_region(
        self,
        program_unit: int,
        parent_region: int,
        contract_name: str,
        contract_version: int,
    ) -> int:
        ...

    def append_region_value(self, region: int, value: int) -> bool:
        ...

    def append_program_unit_region(
        self, program_unit: int, region: int
    ) -> bool:
        ...

    def append_child_region(
        self, parent_region: int, child_region: int
    ) -> bool:
        ...

    def declare_region_value(
        self,
        region: int,
        value: int,
        roles: int,
        ordinal: int,
        flags: int,
    ) -> bool:
        ...

    def declare_state_object(
        self,
        program_unit: int,
        name: str,
        kind: int,
        flags: int,
    ) -> int:
        ...

    def add_state_effect(
        self,
        value: int,
        state: int,
        effect_kind: int,
    ) -> bool:
        ...

    def declare_region_state(
        self,
        region: int,
        state: int,
        effect_kind: int,
        ordinal: int,
        flags: int,
    ) -> bool:
        ...

    def set_region_source_position(
        self,
        region: int,
        file_id: int,
        line: int,
        column: int,
        statement_begin: bool,
        basic_block_begin: bool,
    ) -> bool:
        ...

    def set_region_metadata(
        self,
        region: int,
        key: str,
        value: str,
    ) -> bool:
        ...

    def verify_program(self) -> Mapping[str, object]:
        ...

    def append_program_unit_marker(
        self,
        program_unit: int,
        marker: int,
    ) -> bool:
        ...

    def append_program_unit_value(
        self,
        program_unit: int,
        value: int,
    ) -> bool:
        ...

    def inspect_program_unit_markers(
        self,
        program_unit: int,
    ) -> Sequence[Mapping[str, object]]:
        ...

    def inspect_program_unit_values(
        self,
        program_unit: int,
    ) -> Sequence[Mapping[str, object]]:
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
