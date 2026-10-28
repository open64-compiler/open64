"""Independent MMA5-H executable-source, lifetime and complete-cost checks."""
import json
import re

from wgmma_report import ASM, fragment, pack


def fields(line):
    return {name: int(value) for name, value in re.findall(r"(\w+)=(\d+)", line)}


def atom(dtype, groups):
    registers = ",".join("acc" + str(index) for index in range(32))
    text = "{.reg.b64desc_a,desc_b;.reg.f32acc<32>;"
    text += "cvta.to.shared.u64desc_a,%1;cvta.to.shared.u64desc_b,%2;"
    text += "shr.u64desc_a,desc_a,4;shr.u64desc_b,desc_b,4;"
    text += "and.b64desc_a,desc_a,16383;and.b64desc_b,desc_b,16383;"
    text += "or.b64desc_a,desc_a,68720001024;or.b64desc_b,desc_b,68720001024;"
    text += "".join(f"ld.f32acc{index},[%0+{index * 4}];" for index in range(32))
    text += "wgmma.fence.sync.aligned;"
    for group in range(groups):
        if group:
            text += "add.u64desc_a,desc_a,128;add.u64desc_b,desc_b,128;"
        text += f"wgmma.mma_async.sync.aligned.m64n64k16.f32.{dtype}.{dtype}" + "{" + registers
        text += "},desc_a,desc_b,1,1,1,0,0;wgmma.commit_group.sync.aligned;"
    text += "wgmma.wait_group.sync.aligned0;"
    return text + "".join(f"st.f32[%0+{index * 4}],acc{index};" for index in range(32)) + "}"


def tma_issue(record):
    text = "{.reg.b32raw_a,raw_b,ready,dst;.reg.b64src;"
    text += "cvta.to.shared.u64src,%0;cvt.u32.u64raw_a,src;cvta.to.shared.u64src,%1;cvt.u32.u64raw_b,src;"
    text += "cvta.to.shared.u64src,%4;cvt.u32.u64ready,src;"
    text += "mbarrier.init.shared::cta.b64[ready],1;fence.proxy.async.shared::cta;"
    text += "mbarrier.arrive.expect_tx.shared::cta.b64_,[ready],4096;"
    for name, rows, size, stride, operand in (("a", 64, 32, record["K"] * 2, 2), ("b", 16, 128, record["N"] * 2, 3)):
        for row in range(rows):
            text += f"add.u32dst,raw_{name},{row * size};add.u64src,%{operand},{row * stride};"
            text += f"cp.async.bulk.shared::cta.global.mbarrier::complete_tx::bytes[dst],[src],{size},[ready];"
    return text + "}"


def tma_wait():
    return ("{.reg.b64address;.reg.b32ready;.reg.predcomplete;cvta.to.shared.u64address,%0;cvt.u32.u64ready,address;"
            "ready_loop:mbarrier.try_wait.parity.acquire.cta.shared::cta.b64complete,[ready],0;@!completebraready_loop;"
            "mbarrier.inval.shared::cta.b64[ready];}")


def parse(trace):
    contracts = [row for row in trace.splitlines() if row.startswith("AI_WGMMA_PIPELINE 1 CONTRACT ")]
    actions = [row for row in trace.splitlines() if row.startswith("AI_WGMMA_PIPELINE 1 ACTION ")]
    if len(contracts) != 1 or len(actions) != 1:
        raise ValueError("missing/duplicate pipeline evidence")
    record = fields(contracts[0])
    if (record.get("delivery") not in (1, 2) or record.get("input") not in (5, 6)
            or record.get("arch") != 2 or record.get("freedoms") != 31 or record.get("permissions") != 1
            or record.get("profile") != record["input"] - 4 or record.get("output") != 1
            or record.get("slots") != 2 or record.get("groups") != 2 or record.get("threads") != 128
            or record.get("smem") != (16400 if record["delivery"] == 2 else 8192)
            or record.get("shared") != record["smem"] or record.get("LBO") != 128 or record.get("SBO") != 256
            or record.get("swizzle") != 0 or any(record.get(key, 0) <= 0 for key in ("id", "owner", "generation", "acceptance", "contract", "M", "N", "K"))
            or any(record[key] % divisor for key, divisor in (("M", 64), ("N", 64), ("K", 16)))
            or record.get("max_span", 0) < record["K"] or record.get("ptx", 0) < (806 if record["delivery"] == 2 else 800)
            or record.get("toolkit", 0) < (12080 if record["delivery"] == 2 else 12000)):
        raise ValueError("invalid pipeline target/numerical/storage contract")
    steps = record["K"] // 16
    expected = (f"AI_WGMMA_PIPELINE 1 ACTION batches={(steps + 1) // 2} atoms={steps} slots=2 max_groups=2 "
                f"tail_groups={1 if steps % 2 else 2} delivery={record['delivery']} ready_join={int(record['delivery'] == 2)} "
                f"mma_done=wait_zero reuse=CTA_after_MMA_done accumulator_read=after_wait default=SIMT timing=unknown source_line={record['source_line']}")
    if actions[0] != expected or "timing=unknown" not in contracts[0]:
        raise ValueError("inconsistent actual pipeline actions")
    return record


def audit(source, dump, record):
    compact = re.sub(r"\s+", "", source)
    for text in (source, dump):
        identity = re.search(r"MMA5_H_V1[^\n]*", text)
        if not identity or fields(identity.group()) != record:
            raise ValueError("stale executable identity")
    if "LOC 1 " not in dump or re.search(r"^\s*F4(?:ADD|MPY)\b", dump, re.M):
        raise ValueError("missing source or scalar stand-in")
    allocations = [("mma_shared_a", "_UINT16", 2048), ("mma_shared_b", "_UINT16", 2048)]
    tma = record["delivery"] == 2
    if tma:
        allocations += [("mma_raw_a", "_UINT16", 2048), ("mma_raw_b", "_UINT16", 2048), ("mma_ready", "_UINT64", 2)]
    for name, dtype, count in allocations:
        if f"__shared____align__(16){dtype}{name}[{count}LL];" not in compact:
            raise ValueError("changed physical shared reservation")
    if (f"mma_base<{record['K']}LL;mma_base=mma_base+32LL" not in compact
            or source.count("__syncthreads();") != (3 if tma else 2)
            or "_IEEE32mma_fragment[32LL];" not in compact
            or compact.count("mma_copy<1024LL;mma_copy=mma_copy+128LL") != 2):
        raise ValueError("changed batching/repacking/joins")
    for axis, value in (("x", 128), ("y", 1), ("z", 1)):
        if f"(blockDim).{axis})=={value}LL" not in compact:
            raise ValueError("missing uniform launch guard")
    for key in ("input_a", "input_b"):
        if key + ")&15ULL" not in compact:
            raise ValueError("missing aligned input guard")
    assembly = list(ASM.finditer(source))
    expected = ([tma_issue(record)] * 2 + [tma_wait()] * 2 if tma else [])
    dtype = "f16" if record["input"] == 5 else "bf16"
    expected += ["fence.proxy.async.shared::cta;", atom(dtype, 2), atom(dtype, 1)]
    actual = [re.sub(r"\s+", "", json.loads('"' + match[1] + '"')) for match in assembly]
    if actual != expected:
        raise ValueError("changed real copy/descriptor/fence/group/completion instruction")
    for match in assembly[-2:]:
        if re.sub(r"\s+", "", match[2]) != '"l"(mma_fragment),"l"(mma_shared_a),"l"(mma_shared_b)':
            raise ValueError("changed accumulator/operand ownership")
    for opcode, count in (("ASM_STMT", len(expected)), ("ASM_INPUT", 18 if tma else 6), ("VCALL", 3 if tma else 2)):
        if len(re.findall(r"^\s*" + opcode + r"\b", dump, re.M)) != count:
            raise ValueError("wrong real WHIRL carrier count")
    if ({pack(row, col) for row in range(64) for col in range(16)} != set(range(1024))
            or len({fragment(thread, reg) for thread in range(128) for reg in range(32)}) != 4096):
        raise ValueError("bad independent fragment/packing oracle")
    return dict(shared_bytes=record["smem"], slots=2, max_groups=2, native_gpu="unknown", timing="unknown")


def host_source(source, record):
    matches = list(ASM.finditer(source))
    replacements = []
    for match in matches:
        text = json.loads('"' + match[1] + '"')
        arguments = re.findall(r'"l"\(\s*(.*?)\s*\)\s*(?:,|$)', match[2], re.S)
        if "cp.async.bulk" in text:
            arguments = ["reinterpret_cast<" + ("unsigned long long" if index == 4 else "unsigned short") + " *>(" + value + ")"
                         for index, value in enumerate(arguments)]
            replacements.append("VHO_DSL_pipeline_host_issue(" + ",".join(arguments) + f",{record['K']},{record['N']})")
        elif "mbarrier.try_wait" in text:
            arguments = ["reinterpret_cast<unsigned long long *>(" + value + ")" for value in arguments]
            replacements.append("VHO_DSL_pipeline_host_wait(" + ",".join(arguments) + ")")
        elif "wgmma.mma_async" in text:
            replacements.append("VHO_DSL_pipeline_host_batch(" + ",".join(arguments) + f",{text.count('wgmma.mma_async')})")
        else:
            replacements.append("VHO_DSL_wgmma_host_proxy()")
    for match, replacement in reversed(list(zip(matches, replacements))):
        source = source[:match.start()] + replacement + source[match.end():]
    return source


def model_oracle(trace):
    scenarios = {}
    for line in trace.splitlines():
        if not line.startswith("AI_MMA_PIPELINE_MODEL 1 "):
            continue
        kind = line.split()[2]
        row = fields(line)
        scenario = scenarios.setdefault(row["scenario"], dict(metrics={}, candidates={}, costs={}))
        if kind == "INPUT":
            scenario["input"] = row
        elif kind == "METRIC":
            scenario["metrics"][row["metric"]] = row
        elif kind == "CANDIDATE":
            scenario["candidates"][row["id"]] = row
        elif kind == "COST":
            row["terms"] = [int(value) for value in re.search(r"terms=([0-9,]+)", line)[1].split(",")]
            scenario["costs"][row["id"], row["corner"]] = row
    checked = 0
    for scenario in scenarios.values():
        inp, metrics = scenario["input"], scenario["metrics"]
        for candidate_id, candidate in scenario["candidates"].items():
            simt, tma = candidate_id == 0, candidate_id == 3
            expected_eligible = simt or candidate["shared"] <= inp["limit"]
            if inp["sms"] and expected_eligible:
                rounded_shared = ((candidate["shared"] + 1024 + 127) // 128) * 128
                limits = [32, 16, 16, inp["registers"] // 12288, inp["smem_budget"] // rounded_shared]
                resident = min(min(limits), inp["cap"])
                limiting = sum(1 << index for index, limit in enumerate(limits) if limit == min(limits))
                limiting |= 32 if resident == inp["cap"] else 0
                if (candidate["registers"], candidate["rounded_shared"], candidate["resident"], candidate["limiting"]) != (12288, rounded_shared, resident, limiting):
                    raise ValueError("independent occupancy mismatch")
                expected_eligible = bool(resident and inp["queue"] >= candidate["groups"])
            if candidate["eligible"] != expected_eligible:
                raise ValueError("independent queue/resource eligibility mismatch")
            known = bool(inp["sms"] and expected_eligible and all(row["known"] for row in metrics.values()))
            if candidate["known"] != known:
                raise ValueError("unknown timing became a cost")
            for corner in range(3):
                cost = scenario["costs"][candidate_id, corner]
                if not known:
                    if cost["kernel"] or cost["invocation"] or any(cost["terms"]):
                        raise ValueError("cost published for unknown/rejected tuple")
                    continue
                def metric(index):
                    point = "nominal" if corner == 1 else ("lower" if (corner == 0) == (index > 6) else "upper")
                    return metrics[index][point]
                def service(amount, index):
                    return (amount + metric(index) - 1) // metric(index)
                blocks_x = (inp["N"] + 127) // 128 if simt else inp["N"] // 64
                ctas = blocks_x * (inp["M"] if simt else inp["M"] // 64)
                capacity = inp["sms"] * candidate["resident"]
                total = [0] * 8
                for first in range(0, ctas, capacity):
                    per_sm = [[0] * 8 for _ in range(inp["sms"])]
                    inputs, outputs = 0, 0
                    for offset in range(min(capacity, ctas - first)):
                        count = min(128, inp["N"] - ((first + offset) % blocks_x) * 128) if simt else 4096
                        steps = inp["K"] // 16
                        batches = inp["K"] if simt else (steps + candidate["slots"] - 1) // candidate["slots"]
                        flops = count * inp["K"] * 2
                        inputs += count * inp["K"] * 4 if simt else steps * 4096 + (batches + 1) * 32768
                        outputs += count * 4
                        row = per_sm[offset % inp["sms"]]
                        row[1] += service(0 if simt else steps * (16384 if tma else 8192), 1)
                        row[2] += service(flops, 3 if simt else 2)
                        row[3] += 0 if simt else steps * (81 if tma else 1) * metric(8)
                        row[4] += 0 if simt else batches * (3 if tma else 2) * metric(10)
                        row[5] += (0 if simt else steps * metric(9)) + (inp["K"] if simt else batches if tma else steps) * metric(7)
                        row[6] += service(flops if simt else 0, 6) + service(count, 4) + metric(11)
                        if not simt:
                            row[6] += metric(12) + metric(13) + steps * metric(14)
                    longest = max(per_sm, key=sum)
                    for term in range(1, 7):
                        total[term] += longest[term]
                    total[0] += service(inputs, 0) + service(outputs, 0)
                total[7] = metric(15)
                if cost["terms"] != total or cost["kernel"] != sum(total[:7]) or cost["invocation"] != sum(total):
                    raise ValueError("independent complete-plan cost mismatch")
                checked += 1
        known_ids = [index for index, candidate in scenario["candidates"].items() if candidate["known"]]
        nominal = min(known_ids, key=lambda index: (scenario["costs"][index, 1]["invocation"], index)) if known_ids else 0
        separated = bool(known_ids) and all(index == nominal or not candidate["eligible"] or (
            candidate["known"] and scenario["costs"][nominal, 2]["invocation"] < scenario["costs"][index, 0]["invocation"])
            for index, candidate in scenario["candidates"].items())
        if (inp["nominal"], inp["bounded"], inp["emitted"]) != (nominal, nominal if separated else 0, 1):
            raise ValueError("nominal/bounded/default selection mismatch")
    if set(scenarios) != set(range(8)):
        raise ValueError("incomplete scenario evidence")
    return dict(scenarios=len(scenarios), independently_checked_cost_corners=checked, emitted="SIMT", native_timing="unknown")


def protocol_events(steps, tma):
    events = []
    for base in range(0, steps, 2):
        slots = range(min(2, steps - base))
        if tma:
            events += [("copy_issue", slot) for slot in slots]
            events += [("copy_ready", slot) for slot in slots]
        events += [("pack", slot) for slot in slots]
        events += [("publish", -1)]
        for slot in slots:
            events += [("mma_issue", slot), ("commit", -1)]
        events += [("wait_zero", -1), ("fragment_read", -1), ("reuse_join", -1)]
    return events


def verify_protocol(events, tma, group_limit=2):
    raw, packed = ["free"] * 2, ["free"] * 2
    published = False
    uncommitted = groups = 0
    complete = read = False
    for event, slot in events:
        if event == "copy_issue":
            if not tma or raw[slot] != "free" or packed[slot] != "free":
                raise ValueError("raw slot overwritten while a reader is live")
            raw[slot] = "pending"
        elif event == "copy_ready":
            if raw[slot] != "pending":
                raise ValueError("copy completion without issue")
            raw[slot] = "ready"
        elif event == "pack":
            if packed[slot] != "free" or (tma and raw[slot] != "ready") or published:
                raise ValueError("premature operand pack/reuse")
            packed[slot] = "packed"
        elif event == "publish":
            if published or "packed" not in packed or uncommitted or groups:
                raise ValueError("invalid operand publication")
            published = True
        elif event == "mma_issue":
            if not published or packed[slot] != "packed" or uncommitted or complete:
                raise ValueError("MMA without a published operand generation")
            packed[slot] = "reading"
            uncommitted = 1
        elif event == "commit":
            if not uncommitted or groups >= group_limit:
                raise ValueError("missing issue or outstanding-group overflow")
            uncommitted = 0
            groups += 1
        elif event == "wait_zero":
            if uncommitted or not groups or complete:
                raise ValueError("wait does not cover issued MMA")
            groups = 0
            complete = True
        elif event == "fragment_read":
            if not complete or groups or uncommitted:
                raise ValueError("copy readiness is not accumulator completion")
            read = True
        elif event == "reuse_join":
            if not complete or not read or groups or uncommitted or "packed" in packed:
                raise ValueError("reuse before all asynchronous readers complete")
            raw, packed = ["free"] * 2, ["free"] * 2
            published = complete = read = False
        else:
            raise ValueError("unknown event")
    if groups or uncommitted or published or raw != ["free"] * 2 or packed != ["free"] * 2:
        raise ValueError("incomplete pipeline teardown")
    return True
