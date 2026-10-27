"""Independent inspection of MMA3 native source contracts and emitted bodies."""
import re

COLUMNS = ("id", "owner", "contract", "source", "input", "product", "partial", "outer", "output", "permissions",
           "max_span", "evidence_kind", "evidence_id", "acceptance", "generation", "M", "N", "K", "threads", "disjoint", "source_line")


def contract(trace):
    records = []
    actions = []
    for line in trace.splitlines():
        if not line.startswith("AI_SIMT_BASELINE "):
            continue
        tokens = line.split()
        if tokens[1] != "1":
            raise ValueError("unsupported typed SIMT schema")
        if tokens[2] == "CONTRACT":
            values = list(map(int, tokens[3:]))
            record = dict(zip(COLUMNS, values))
            if (len(values) != len(COLUMNS) or record["source"] not in (5, 6)
                    or record["input"] != record["source"]
                    or any(record[key] != 1 for key in ("product", "partial", "outer", "output"))
                    or record["permissions"] not in (0, 1) or record["max_span"] < record["K"]
                    or any(record[key] <= 0 for key in COLUMNS if key != "permissions")
                    or any(record[key] >= 2147483647 for key in ("M", "N", "K"))
                    or record["M"] * record["N"] > ((1 << 63) - 1) // 4
                    or record["threads"] > 1024 or record["disjoint"] != 1 or record["evidence_kind"] not in (1, 2, 3)):
                raise ValueError("invalid typed numerical/source contract")
            records.append(values)
        elif tokens[2] == "ACTION":
            if (len(tokens) != 14 or tokens[4] not in ("serial_reference", "direct_simt")
                    or tokens[5:13] != ["loads_u2=2", "widen_f4=2", "product_f4=1", "add_f4=1",
                                         "store_f4=1", "order=increasing_k", "contraction=none", "timing=unknown"]):
                raise ValueError("invalid typed action evidence")
            actions.append(tokens[3:])
        else:
            raise ValueError("unknown typed trace record")
    if (len(records) != 2 or records[0] != records[1] or len(actions) != 2
            or [row[1] for row in actions] != ["serial_reference", "direct_simt"]
            or any(int(row[0]) != records[0][0] or int(row[-1]) != records[0][-1] for row in actions)):
        raise ValueError("missing/duplicate/mismatched typed boundary evidence")
    return dict(zip(COLUMNS, records[0]))


def audit(source, dump, record, execution):
    """Inspect the actual canonical IR and source, not just ACTION counters."""
    if not dump.strip() or "MMA3_TYPED_SIMT_V1" not in dump or "LOC 1 " not in dump:
        raise ValueError("missing mapped/source cross-reference evidence")
    for text in (source, dump):
        identity = re.search(r"MMA3_TYPED_SIMT_V1[^\n]*", text)
        if not identity:
            raise ValueError("missing typed source identity")
        fields = dict(re.findall(r"(\w+)=(\d+)", identity.group()))
        checks = {key: str(record[key]) for key in ("id", "owner", "contract", "source", "input", "product", "partial",
                                                   "outer", "output", "permissions", "max_span", "acceptance", "generation", "M", "N", "K", "threads", "disjoint")}
        evidence = f"evidence={record['evidence_kind']}:{record['evidence_id']}"
        if (any(fields.get(key) != value for key, value in checks.items())
                or fields.get("execution") != ("1" if execution == "serial" else "2")
                or not re.search(r"\b" + re.escape(evidence) + r"(?:\s|$)", identity.group())):
            raise ValueError("source/WHIRL numerical identity differs from accepted contract")
    expected = {"U4U2ILOAD": 2, "F4TAS": 2, "F4MPY": 1, "F4ADD": 1, "F4ISTORE": 1, "RETURN": 1}
    for opcode, count in expected.items():
        if len(re.findall(r"^\s*" + opcode + r"(?:\s|$)", dump, re.M)) != count:
            raise ValueError(f"typed WHIRL operation changed: {opcode}")
    if re.search(r"^\s*(?:F[248](?:MADD|MSUB)|CALL|INTRINSIC_CALL|DSL\w+)(?:\s|$)", dump, re.M):
        raise ValueError("typed body has fused, runtime or unlowered work")
    for parameter, storage in (("input_a", "_UINT16"), ("input_b", "_UINT16"), ("output", "_IEEE32")):
        if not re.search(r"\b" + storage + r"\s*\*\s*" + parameter + r"\b", source):
            raise ValueError("typed buffer ABI changed")
    for scalar in ("rounded_product", "accumulator"):
        if not re.search(r"volatile\s+_IEEE32\s+" + scalar + r"\s*;", source):
            raise ValueError("lost explicit FP32 rounding boundary")
    compact = re.sub(r"\s+", "", source)
    if (source.count("__uint_as_float(") != 2 or "rounded_product=a_value*b_value;" not in compact
            or "accumulator=accumulator+rounded_product;" not in compact
            or f"reduction_index<{record['K']}" not in compact or "reduction_index=reduction_index+1" not in compact
            or "accumulator=0.0F;" not in compact):
        raise ValueError("widening, separate product or source K order changed")
    if any(token in source for token in ("__half", "__nv_bfloat", "__shared__", "__syncthreads", "__open64_cuda_tma", "_U4SHL", "_U4EXTRACT_BITS")):
        raise ValueError("unexpected native-half/staged operation or missing emitter lowering")
    if execution == "simt":
        if ("blockIdx" not in source or "threadIdx" not in source
                or f"row<{record['M']}LL" not in compact or f"column<{record['N']}LL" not in compact):
            raise ValueError("lost SIMT ownership/edge guard")
    elif "for(row=" not in compact or "for(column=" not in compact:
        raise ValueError("lost serial source topology")
    return expected
