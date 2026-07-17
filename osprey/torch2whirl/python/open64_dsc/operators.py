"""Import-facing operator definitions for torch2whirl frontends.

The native WHIRL representation and verifier remain owned by common/com.
This module exposes only stable logical names, versions, arities, and
attribute spellings that Python frontend code and tests may import.
"""

from __future__ import annotations

from types import MappingProxyType
from typing import Mapping, Sequence

from .mapping import cnn, common, transformer
from .mapping.contract import OperatorContract
from .mapping.contract import all_operator_contracts
from .mapping.contract import operator_contract


OperatorDefinitions = Mapping[str, OperatorContract]


COMMON_OPERATORS: OperatorDefinitions = MappingProxyType({
    name: contract
    for name, contract in all_operator_contracts().items()
    if name.startswith("common.")
})

TRANSFORMER_OPERATORS: OperatorDefinitions = MappingProxyType({
    name: contract
    for name, contract in all_operator_contracts().items()
    if name.startswith("transformer.")
})

CNN_OPERATORS: OperatorDefinitions = MappingProxyType({
    name: contract
    for name, contract in all_operator_contracts().items()
    if name.startswith("cnn.")
})

LLAMA2_PREFILL_OPERATORS: OperatorDefinitions = MappingProxyType({
    transformer.TOKEN_EMBEDDING: operator_contract(
        transformer.TOKEN_EMBEDDING,
    ),
    transformer.RMS_NORM: operator_contract(transformer.RMS_NORM),
    transformer.ROTARY_EMBEDDING: operator_contract(
        transformer.ROTARY_EMBEDDING,
    ),
    transformer.ATTENTION: operator_contract(transformer.ATTENTION),
    transformer.SWIGLU: operator_contract(transformer.SWIGLU),
    common.LINEAR: operator_contract(common.LINEAR, 3),
    common.RESHAPE: operator_contract(common.RESHAPE),
    common.TRANSPOSE: operator_contract(common.TRANSPOSE),
    common.RESIDUAL_ADD: operator_contract(common.RESIDUAL_ADD),
    common.OUTPUT_LOGITS: operator_contract(common.OUTPUT_LOGITS, 3),
})

LLAMA2_DECODE_OPERATORS: OperatorDefinitions = MappingProxyType({
    transformer.TOKEN_EMBEDDING: operator_contract(
        transformer.TOKEN_EMBEDDING,
    ),
    transformer.RMS_NORM: operator_contract(transformer.RMS_NORM),
    transformer.ROTARY_EMBEDDING: operator_contract(
        transformer.ROTARY_EMBEDDING,
        2,
    ),
    transformer.ATTENTION: operator_contract(transformer.ATTENTION, 2),
    transformer.SWIGLU: operator_contract(transformer.SWIGLU),
    common.LINEAR: operator_contract(common.LINEAR, 3),
    common.RESHAPE: operator_contract(common.RESHAPE),
    common.TRANSPOSE: operator_contract(common.TRANSPOSE),
    common.OUTPUT_LOGITS: operator_contract(common.OUTPUT_LOGITS, 3),
})

LLAMA2_MULTIPLE_PU_OPERATORS: OperatorDefinitions = MappingProxyType({
    transformer.RMS_NORM: operator_contract(transformer.RMS_NORM),
    transformer.ROTARY_EMBEDDING: operator_contract(
        transformer.ROTARY_EMBEDDING,
    ),
    transformer.ATTENTION: operator_contract(transformer.ATTENTION),
    transformer.SWIGLU: operator_contract(transformer.SWIGLU),
    common.LINEAR: operator_contract(common.LINEAR, 3),
    common.RESHAPE: operator_contract(common.RESHAPE),
    common.TRANSPOSE: operator_contract(common.TRANSPOSE),
    common.RESIDUAL_ADD: operator_contract(common.RESIDUAL_ADD),
    common.OUTPUT_LOGITS: operator_contract(common.OUTPUT_LOGITS, 3),
})


def operator_names(definitions: OperatorDefinitions) -> Sequence[str]:
    return tuple(definitions)


__all__ = [
    "CNN_OPERATORS",
    "COMMON_OPERATORS",
    "LLAMA2_DECODE_OPERATORS",
    "LLAMA2_MULTIPLE_PU_OPERATORS",
    "LLAMA2_PREFILL_OPERATORS",
    "OperatorDefinitions",
    "TRANSFORMER_OPERATORS",
    "operator_names",
]
