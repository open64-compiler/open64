"""Static source-expression mirror of the common/com native contract.

Design: doc/AI-COMPILER-NATIVE-TENSOR-EXPRESSION-CONTRACT-v0.1.md.
No target choice or runtime evaluation. C++ repeats checks before allocation
and the mapped gatekeeper repeats them after reopening.
"""

import math
import re

from .mapping.contract import operator_contract


VERSIONS = {name: 1 for name in (
    "common.add", "common.mul", "common.div", "common.sub", "common.exp",
    "common.log", "common.reduce_max", "common.isfinite", "common.greater",
    "common.select", "common.logical_not", "common.fill_like",
    "common.reshape", "common.transpose")}
VERSIONS.update({"common.matmul": 3, "common.reduce_sum": 3})


def broadcast(*shapes):
    """Infer trailing-axis broadcast, rejecting incompatible positive extents."""
    result = ()
    for shape in shapes:
        rank = max(len(result), len(shape))
        left = (1,) * (rank-len(result)) + result
        right = (1,) * (rank-len(shape)) + tuple(shape)
        if any(a != b and a != 1 and b != 1 for a, b in zip(left, right)):
            raise ValueError("incompatible broadcast")
        result = tuple(max(a, b) for a, b in zip(left, right))
    return result


def indices(text):
    """Read canonical comma-separated integers, without dynamic dimensions."""
    return tuple(int(part) for part in text.split(","))


def infer_expression(name, version, attrs, operands):
    """Validate exact schema/types and infer a bounded static logical result."""
    from .quantized_contract import SCHEMAS, DTYPES, infer_quantized
    if (name, version) in SCHEMAS:
        return infer_quantized(name, version, attrs, operands, broadcast)
    if VERSIONS.get(name) != version:
        raise ValueError("unsupported tensor expression version")
    contract = operator_contract(name, version)
    if len(operands) != contract.arity or set(attrs) != set(contract.required_attrs):
        raise ValueError("tensor expression schema or arity mismatch")
    for dtype, shape in operands:
        allowed = DTYPES | {"bool"} if name in ("common.reshape", "common.transpose") else {"float32", "bool"}
        if dtype not in allowed or not 1 <= len(shape) <= 16 or any(n <= 0 for n in shape):
            raise ValueError("positive static rank 1 through 16 FP32 or bool required")
    dtype, shape = operands[0]
    expected = shape
    result_dtype = dtype
    if name in ("common.reshape", "common.transpose"):
        if name.endswith("reshape"):
            expected = indices(attrs["attr.target_shape"])
            if math.prod(expected) != math.prod(shape):
                raise ValueError("reshape element count mismatch")
        else:
            permutation = indices(attrs["attr.permutation"])
            if sorted(permutation) != list(range(len(shape))):
                raise ValueError("invalid permutation")
            expected = tuple(shape[i] for i in permutation)
    elif name == "common.logical_not":
        if dtype != "bool":
            raise ValueError("logical_not requires bool")
    elif name == "common.select":
        if dtype != "bool" or any(item[0] != "float32" for item in operands[1:]):
            raise ValueError("select requires bool, float32, float32")
        if attrs["attr.broadcast_rule"] != "numpy":
            raise ValueError("numpy broadcast required")
        result_dtype = "float32"
        expected = broadcast(*(item[1] for item in operands))
    else:
        if any(item[0] != "float32" for item in operands):
            raise ValueError("float32 arithmetic required")
        if name in ("common.add", "common.sub", "common.mul", "common.div", "common.greater"):
            if attrs["attr.broadcast_rule"] != "numpy":
                raise ValueError("numpy broadcast required")
            expected = broadcast(*(item[1] for item in operands))
            if name == "common.greater":
                result_dtype = "bool"
        elif name in ("common.exp", "common.log"):
            if attrs["attr.fp_semantics"] != "source_unresolved":
                raise ValueError("source numerical semantics required")
        elif name == "common.isfinite":
            result_dtype = "bool"
        elif name == "common.fill_like":
            bits = attrs["attr.value_bits"]
            if not re.fullmatch("[0-9a-f]{8}", bits):
                raise ValueError("canonical binary32 bits required")
            value = int(bits, 16)
            if value & 0x7f800000 == 0x7f800000 and value & 0x7fffff:
                raise ValueError("NaN fill literal is unsupported")
        elif name in ("common.reduce_sum", "common.reduce_max"):
            axis = int(attrs["attr.axis"])
            if (axis != len(shape)-1 or attrs["attr.keepdim"] not in ("true", "false") or
                    attrs["attr.accum_dtype"] != "float32" or attrs["attr.reassociate"] != "false"):
                raise ValueError("last-axis source reduction required")
            expected = shape[:-1] + ((1,) if attrs["attr.keepdim"] == "true" else ())
        elif name == "common.matmul":
            left, right = operands[0][1], operands[1][1]
            if (len(left) < 2 or len(right) < 2 or left[-1] != right[-2] or
                    attrs["attr.batch_rule"] != "numpy" or attrs["attr.accum_dtype"] != "float32" or
                    attrs["attr.transpose_kid0"] != "false" or attrs["attr.transpose_kid1"] != "false"):
                raise ValueError("invalid broadcast matmul")
            expected = broadcast(left[:-2], right[:-2]) + (left[-2], right[-1])
    if not 1 <= len(expected) <= 16 or any(n <= 0 for n in expected):
        raise ValueError("positive static result required")
    return result_dtype, tuple(expected)
