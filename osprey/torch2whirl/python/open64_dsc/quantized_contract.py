"""Q1 source mirror of common/com typed quantized contracts.

These predicates admit source representation, not executable GPU kernels.
Runtime bounds/integral/range/unique-index guards remain required semantics.
Native C++ repeats dtype/shape/schema validation and mapped reopening checks it.
"""
import math
import re
import struct

SCHEMAS = {
    ("common.abs", 1): (1, ()),
    ("common.round_even", 1): (1, ("rounding",)),
    ("common.clamp", 1): (1, ("has_min", "min_bits", "has_max", "max_bits")),
    ("common.convert", 1): (1, ("src_dtype", "dst_dtype", "mode")),
    ("common.pack_u4", 1): (1, ("axis", "nibble_order", "code_bits")),
    ("common.unpack_u4", 1): (1, ("axis", "nibble_order", "code_bits")),
    ("common.gather", 1): (2, ("axis", "bounds", "alias_rule")),
    ("common.scatter", 2): (3, ("axis", "bounds", "conflicts", "alias_rule")),
    ("common.add", 2): (2, ("broadcast_rule", "integer_domain")),
    ("common.mul", 2): (2, ("broadcast_rule", "integer_domain")),
    ("common.div", 2): (2, ("broadcast_rule", "integer_domain", "rounding")),
    ("common.rem", 2): (2, ("broadcast_rule", "integer_domain", "rounding")),
    ("common.fill_like", 2): (1, ("value_i64",)),
}
DTYPES = frozenset(("float32", "int8", "uint8", "int64"))


def require(ok, message):
    if not ok:
        raise ValueError("quantized source: " + message)


def finite_bits(text):
    require(type(text) is str and re.fullmatch("[0-9a-f]{8}", text), "finite F32 bits")
    value = struct.unpack(">f", bytes.fromhex(text))[0]
    require(math.isfinite(value), "finite F32 bounds")
    return value


def infer_quantized(name, version, attrs, operands, broadcast):
    arity, fields = SCHEMAS[name, version]
    require(len(operands) == arity and set(attrs) == {"attr."+f for f in fields}, "schema/arity")
    require(all(dtype in DTYPES and 1 <= len(shape) <= 16 and
                all(type(n) is int and n > 0 for n in shape) for dtype, shape in operands), "dtype/shape")
    dtype, shape = operands[0]
    shape = tuple(shape)
    if name in ("common.abs", "common.round_even", "common.clamp"):
        require(dtype == "float32", "float32 unary required")
        if name == "common.round_even":
            require(attrs["attr.rounding"] == "ties_to_even", "RNE required")
        if name == "common.clamp":
            lo, hi = attrs["attr.has_min"], attrs["attr.has_max"]
            lower, upper = finite_bits(attrs["attr.min_bits"]), finite_bits(attrs["attr.max_bits"])
            require(lo in ("true", "false") and hi in ("true", "false") and
                    (lo == "true" or hi == "true") and
                    (lo == "true" or attrs["attr.min_bits"] == "00000000") and
                    (hi == "true" or attrs["attr.max_bits"] == "00000000") and
                    (lo != "true" or hi != "true" or lower <= upper), "clamp bounds")
    elif name == "common.convert":
        dst, mode = attrs["attr.dst_dtype"], attrs["attr.mode"]
        require(attrs["attr.src_dtype"] == dtype and
                ((dtype == "float32" and dst in ("int8", "uint8") and mode == "checked_integral") or
                 (dtype in ("int8", "uint8") and dst == "float32" and mode == "exact")), "conversion")
        dtype = dst
    elif name in ("common.pack_u4", "common.unpack_u4"):
        require(dtype == "uint8" and attrs == {"attr.axis": str(len(shape)-1),
                "attr.nibble_order": "even_low", "attr.code_bits": "4"}, "U4 layout")
        if name == "common.pack_u4":
            require(shape[-1] % 2 == 0, "U4 even columns")
            shape = shape[:-1] + (shape[-1]//2,)
        else:
            require(shape[-1] <= (2**64-1)//2, "unpack extent overflow")
            shape = shape[:-1] + (shape[-1]*2,)
    elif name in ("common.gather", "common.scatter"):
        axis = attrs["attr.axis"]
        require(axis in ("0", "1") and int(axis) < len(shape) and operands[1][0] == "int64" and
                len(operands[1][1]) == 1 and attrs["attr.bounds"] == "guarded_nonnegative" and
                attrs["attr.alias_rule"] == "fresh_result", "gather/scatter indices")
        expected = list(shape); expected[int(axis)] = operands[1][1][0]
        if name == "common.gather":
            shape = tuple(expected)
        else:
            require(axis == "0" and attrs["attr.conflicts"] == "unique_indices" and
                    operands[2] == (dtype, tuple(expected)), "functional scatter shape/conflicts")
    else:
        require(all(t[0] == "int64" for t in operands), "int64 index operation")
        if name == "common.fill_like":
            text = attrs["attr.value_i64"]
            require(type(text) is str and re.fullmatch("0|-?[1-9][0-9]*", text) and
                    -(2**63) <= int(text) < 2**63, "canonical int64 literal")
        else:
            require(attrs["attr.broadcast_rule"] == "numpy" and
                    attrs["attr.integer_domain"] == "nonnegative_i64_checked", "checked index arithmetic")
            if name in ("common.div", "common.rem"):
                require(attrs["attr.rounding"] == "floor", "floor index arithmetic")
            shape = broadcast(*(t[1] for t in operands))
    return dtype, shape
