"""Independent inspection of real MMA4-H mapped source and executable PTX."""
import json
import re

ASM = re.compile(r'__asm__\s+volatile\s*\("((?:[^"\\]|\\.)*)"\s*:\s*:\s*(.*?)\s*:\s*"memory"\s*\)', re.S)


def parse(trace):
    contracts = [row for row in trace.splitlines() if row.startswith("AI_WGMMA 1 CONTRACT ")]
    actions = [row for row in trace.splitlines() if row.startswith("AI_WGMMA 1 ACTION ")]
    if len(contracts) != 1 or len(actions) != 1:
        raise ValueError("missing/duplicate executable WGMMA evidence")
    fields = re.findall(r"(\w+)=(\d+)", contracts[0])
    record = {name: int(value) for name, value in fields}
    required = {"id", "owner", "generation", "contract", "acceptance", "freedoms", "profile", "arch", "ptx", "toolkit",
                "input", "output", "permissions", "max_span", "evidence_kind", "evidence_id", "M", "N", "K", "threads", "smem", "LBO", "SBO", "swizzle"}
    if len(fields) != len(record) or set(record) != required:
        raise ValueError("unsupported/duplicate numerical identity fields")
    if (any(record.get(key, 0) <= 0 for key in ("id", "owner", "generation", "contract", "acceptance", "M", "N", "K"))
            or record.get("input") not in (5, 6) or record.get("output") != 1
            or record.get("profile") != record["input"] - 4 or record.get("arch") != 2
            or record.get("freedoms") != 31 or record.get("ptx", 0) < 800 or record.get("toolkit", 0) < 12000
            or record["permissions"] != 1 or record["max_span"] < record["K"]
            or record["evidence_kind"] not in (1, 2, 3) or record["evidence_id"] <= 0
            or record.get("threads") != 128 or record.get("smem") != 4096
            or record.get("LBO") != 128 or record.get("SBO") != 256 or record.get("swizzle") != 0
            or any(record[key] % divisor for key, divisor in (("M", 64), ("N", 64), ("K", 16)))
            or any(record[key] >= 2147483647 for key in ("M", "N", "K"))
            or record["M"] * record["N"] > ((1 << 63) - 1) // 4
            or "timing=unknown" not in contracts[0]):
        raise ValueError("invalid target/numerical/layout/shape contract")
    expected = (f"AI_WGMMA 1 ACTION atom=m64n64k16 iterations={record['K'] // 16} "
                f"cta_tiles={record['M'] // 64 * (record['N'] // 64)} proxy=1 barriers=2 register_fence=1 mma=1 "
                "commit=1 wait_zero=1 fragment_loads=32 fragment_stores=32 default=SIMT timing=unknown")
    if actions[0] != expected:
        raise ValueError("unsupported/inconsistent operation evidence")
    return record


def pack(mn, reduction):
    return (mn // 8) * 128 + (reduction // 8) * 64 + (mn % 8) * 8 + reduction % 8


def fragment(thread, reg):
    return (thread // 32 * 16 + thread % 32 // 4 + reg % 4 // 2 * 8,
            reg // 4 * 8 + thread % 4 * 2 + reg % 2)


def audit(source, dump, record):
    compact = re.sub(r"\s+", "", source)
    if "LOC 1 " not in dump or "MMA4_H_V1" not in dump:
        raise ValueError("missing real mapped/source evidence")
    for text in (source, dump):
        identity = re.search(r"MMA4_H_V1[^\n]*", text)
        values = dict(re.findall(r"(\w+)=(\d+)", identity.group())) if identity else {}
        if any(values.get(key) != str(value) for key, value in record.items()):
            raise ValueError("source/WHIRL accepted identity changed")
    for name in ("mma_shared_a", "mma_shared_b"):
        if f"__shared____align__(16)_UINT16{name}[1024LL];" not in compact:
            raise ValueError("lost typed/aligned shared allocation")
    for name, storage in (("input_a", "_UINT16"), ("input_b", "_UINT16"), ("output", "_IEEE32")):
        if storage + "*" + name not in compact:
            raise ValueError("changed source ABI/precision")
    if (source.count("__syncthreads();") != 2 or "_IEEE32mma_fragment[32LL];" not in compact
            or f"mma_base<{record['K']}LL;mma_base=mma_base+16LL" not in compact
            or "mma_copy<1024LL;mma_copy=mma_copy+128LL" not in compact
            or any(token in source for token in ("tma_", "tcgen05", "__half", "__nv_bfloat"))):
        raise ValueError("unsupported staging/iteration/precision")
    for field, value in (("blockDim).x", 128), ("blockDim).y", 1), ("blockDim).z", 1), ("gridDim).z", 1)):
        if f"({field})=={value}LL" not in compact:
            raise ValueError("missing uniform launch guard")
    if any(f"((blockIdx).{axis})<{record[key] // 64}LL" not in compact for axis, key in (("x", "N"), ("y", "M"))):
        raise ValueError("missing output tile guard")
    assembly = list(ASM.finditer(source))
    if len(assembly) != 2:
        raise ValueError("lost volatile assembly operands/memory effects")
    proxy, atom = [re.sub(r"\s+", "", json.loads('"' + match[1] + '"')) for match in assembly]
    if proxy != "fence.proxy.async.shared::cta;" or assembly[0][2].strip():
        raise ValueError("wrong proxy publication")
    operands = re.findall(r'"l"\(\s*(\w+)\s*\)', assembly[1][2])
    if operands != ["mma_fragment", "mma_shared_a", "mma_shared_b"]:
        raise ValueError("wrong fragment/shared operand ownership")
    dtype = "f16" if record["input"] == 5 else "bf16"
    beginning = ("{.reg.b64desc_a,desc_b;.reg.f32acc<32>;cvta.to.shared.u64desc_a,%1;cvta.to.shared.u64desc_b,%2;"
                 "shr.u64desc_a,desc_a,4;shr.u64desc_b,desc_b,4;and.b64desc_a,desc_a,16383;and.b64desc_b,desc_b,16383;"
                 "or.b64desc_a,desc_a,68720001024;or.b64desc_b,desc_b,68720001024;")
    expected = beginning + "".join(f"ld.f32acc{reg},[%0+{reg * 4}];" for reg in range(32))
    expected += f"wgmma.fence.sync.aligned;wgmma.mma_async.sync.aligned.m64n64k16.f32.{dtype}.{dtype}" + "{"
    expected += ",".join(f"acc{reg}" for reg in range(32)) + "},desc_a,desc_b,1,1,1,0,0;"
    expected += "wgmma.commit_group.sync.aligned;wgmma.wait_group.sync.aligned0;"
    expected += "".join(f"st.f32[%0+{reg * 4}],acc{reg};" for reg in range(32)) + "}"
    if atom != expected:
        raise ValueError("changed descriptor/atom/register/completion mechanism")
    if (source.find("__syncthreads();", assembly[0].end()) > assembly[1].start()
            or source.find("__syncthreads();", assembly[1].end()) < assembly[1].end()):
        raise ValueError("missing publication/reuse join")
    for opcode, count in (("ASM_STMT", 2), ("ASM_INPUT", 3), ("U4U2ILOAD", 2), ("VCALL", 2), ("RETURN", 1)):
        if len(re.findall(r"^\s*" + opcode + r"\b", dump, re.M)) != count:
            raise ValueError("changed actual WHIRL operation: " + opcode)
    if re.search(r"^\s*F4(?:MPY|ADD)\b", dump, re.M):
        raise ValueError("scalar stand-in instead of executable MMA")
    if {pack(mn, reduction) for mn in range(64) for reduction in range(16)} != set(range(1024)):
        raise ValueError("packing not a bijection")
    if {fragment(thread, reg) for thread in range(128) for reg in range(32)} != {(row, column) for row in range(64) for column in range(64)}:
        raise ValueError("fragments do not cover every output exactly")
    return dict(physical_shared_elements=2048, output_fragment_elements=4096,
                descriptor_bits=(128 >> 4) << 16 | (256 >> 4) << 32, native_gpu="unknown", timing="unknown")


def host_source(source):
    """Substitute only already-audited completed target operations for tests."""
    substitutions = ["VHO_DSL_wgmma_host_proxy()", "VHO_DSL_wgmma_host_atom(mma_fragment, mma_shared_a, mma_shared_b)"]
    matches = list(ASM.finditer(source))
    if len(matches) != 2:
        raise ValueError("cannot substitute unaudited operation shape")
    for match, replacement in reversed(list(zip(matches, substitutions))):
        source = source[:match.start()] + replacement + source[match.end():]
    return source
