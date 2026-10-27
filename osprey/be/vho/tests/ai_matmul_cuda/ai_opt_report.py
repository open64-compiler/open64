"""Validate AI optimization trace records and derive readable phase reports."""

from __future__ import annotations

import argparse
import hashlib
from functools import lru_cache
from pathlib import Path
from readiness_report import readiness_cost


PHASE_LOGS = (
    ("semantic.log", "Matmul regions found, tensor roles, shapes, dtypes, and unresolved semantic facts."),
    ("locality.log", "Tensor lifetimes, reuse, traffic baseline, and alias uncertainty."),
    ("fusion.log", "Fusion candidates, legality, costs, and decisions; out of scope for the first matmul case."),
    ("layout.log", "Logical layout alternatives, conversion costs, and access implications."),
    ("placement.log", "Device ownership, shard alternatives, and rejected placements."),
    ("communication.log", "Required transfers or collectives, exposed cost, and unresolved boundaries."),
    ("residency.log", "Memory-level footprints, capacity, occupancy constraints, and rejected residency plans."),
    ("tile.log", "Matmul baseline, G0-G11 tile refinements, legality, resources, estimated costs, and tile selection."),
    ("fetch-pipeline.log", "Movement engines, stages, prefetch distance, barriers, overlap, and fallback."),
    ("physical-plan.log", "Complete plan selection, actual rewrite, before/after IR identity, and verification."),
    ("runtime-variant.log", "Certified variants, guards, guard cost, and fallback."),
    ("feedback.log", "Measured versus predicted results and cost-model corrections when profile data exists."),
)

PHASE_STATUSES = frozenset(("disabled_by_option", "disabled_by_policy",
                            "not_applicable", "no_eligible_region",
                            "analyzed_no_rewrite", "rewritten", "failed"))


def source_warp_sector_bytes(row: dict) -> tuple[int, int, int, int]:
    m, n, k = row["grid"]["problem"]
    bm, bn, bk = row["cta"]
    threads = row["threads"]
    columns = row["layout"]["thread_columns"]
    tm = bm * columns // threads
    tn = bn // columns
    if threads % 32 or min(bm, bn, bk, tm, tn) <= 0:
        raise ValueError("invalid source warp geometry")

    def local_sectors(operand, vm, vn, vk, alignment):
        sectors = 0
        elements = bm * bk if operand == 0 else bk * bn
        issues = tm * tn if operand == 2 else (elements + threads - 1) // threads
        for warp in range(threads // 32):
            for issue in range(issues):
                touched = set()
                for lane in range(32):
                    tid = warp * 32 + lane
                    if operand == 2:
                        row_index = (tid // columns) * tm + issue // tn
                        col_index = (tid % columns) * tn + issue % tn
                        if row_index >= vm or col_index >= vn:
                            continue
                        word = row_index * n + col_index
                    else:
                        offset = tid + issue * threads
                        if offset >= elements:
                            continue
                        width = bk if operand == 0 else bn
                        row_index, col_index = divmod(offset, width)
                        if (operand == 0 and (row_index >= vm or col_index >= vk)) or (
                                operand == 1 and (row_index >= vk or col_index >= vn)):
                            continue
                        word = row_index * (k if operand == 0 else n) + col_index
                    touched.add((word + alignment) // 8)
                sectors += len(touched)
        return sectors

    def classes(extent, tile):
        return ((tile, extent // tile), (extent % tile, int(extent % tile != 0)))

    worst = []
    aligned = 0
    for operand in range(3):
        totals = []
        for alignment in range(8):
            total = 0
            for vm, mc in classes(m, bm):
                for vn, nc in classes(n, bn):
                    for vk, kc in (((bk, 1),) if operand == 2 else classes(k, bk)):
                        if mc and nc and kc:
                            total += mc * nc * kc * local_sectors(
                                operand, vm, vn, vk, alignment)
            totals.append(total * 32)
        aligned += totals[0]
        worst.append(max(totals))
    return (*worst, aligned)


def source_smem_work(row: dict) -> dict:
    """Count distinct scalar words per bank, preserving same-word broadcasts."""
    if row["family"] == "baseline":
        return dict.fromkeys(("a_copy", "b_copy", "a_read", "b_read",
                              "b_issues", "address"), 0)
    bm, bn, bk = row["cta"]
    columns = row["layout"]["thread_columns"]
    threads = row["threads"]
    tm, tn = bm * columns // threads, bn // columns
    transpose = row["layout"]["a_layout"] == "transposed_padded"
    xor = row["smem"]["b_layout"] == "xor_columns"

    def excess(words):
        banks = [set() for _ in range(32)]
        for word in words:
            banks[word % 32].add(word)
        return max(map(len, banks), default=0) - 1 if words else 0

    counts = [0, 0, 0, 0]
    b_copies = 0
    for warp in range(threads // 32):
        tids = range(warp * 32, (warp + 1) * 32)
        for operand, elements, width in ((0, bm * bk, bk), (1, bk * bn, bn)):
            for start in range(0, elements, threads):
                words = []
                for tid in tids:
                    offset = start + tid
                    if offset >= elements:
                        continue
                    r, c = divmod(offset, width)
                    words.append(c * (bm + 1) + r if operand == 0 and transpose
                                 else r * width + (c ^ (c // 32) if xor else c)
                                 if operand == 1 else offset)
                counts[operand] += excess(words)
                b_copies += int(operand == 1 and bool(words))
        for i in range(tm):
            words = [(tid // columns) * tm + i for tid in tids]
            counts[2] += excess(words if transpose else [r * bk for r in words])
        for j in range(tn):
            cols = [(tid % columns) * tn + j for tid in tids]
            counts[3] += tm * excess([c ^ (c // 32) for c in cols] if xor else cols)
    iterations = row["grid"]["ctas"] * row["grid"]["k_iterations"]
    steps = iterations * bk
    b_issues = steps * (threads // 32) * tm * tn
    return dict(a_copy=counts[0] * iterations, b_copy=counts[1] * iterations,
                a_read=counts[2] * steps, b_read=counts[3] * steps,
                b_issues=b_issues, address=2 * (b_copies * iterations + b_issues) if xor else 0)


@lru_cache(maxsize=128)
def direct_sector_bytes(m, n, k, bm, bn, bk, tm, tn, threads):
    """Count direct register-map warp issues; never infer a cache hit rate."""
    def classes(extent, block):
        return [(block, extent // block), (extent % block, int(extent % block != 0))]
    columns = bn // tn
    warps = [[((tid // columns) * tm, (tid % columns) * tn)
              for tid in range(warp * 32, (warp + 1) * 32)] for warp in range(threads // 32)]
    worst = []
    for operand in (0, 1):
        alignments = []
        for alignment in range(8):
            total = 0
            for vm, mc in classes(m, bm):
                for vn, nc in classes(n, bn):
                    for vk, kc in classes(k, bk):
                        if not mc * nc * kc:
                            continue
                        requests = 0
                        for q in range(vk):
                            for warp in warps:
                                for i in range(tm):
                                    for j in range(1 if operand == 0 else tn):
                                        words = [(row + i) * k + q if operand == 0 else q * n + col + j
                                                 for row, col in warp if row + i < vm and
                                                 (operand == 0 or col + j < vn)]
                                        requests += len({(word + alignment) // 8 for word in words})
                        total += requests * mc * nc * kc * 32
            alignments.append(total)
        worst.append(max(alignments))
    return tuple(worst)


def verify_joint(mode, rows, decision, candidates, direct_access=None, resources=None,
                 readiness=None, profile=None):
    """Independently reproduce bounded whole-plan costs and deterministic choice."""
    by_tile = {row["tile"]: row for row in candidates}
    model = mode.get("model", 1) if mode else 1
    if (mode is None or decision is None or mode["budget"] != (48 if model >= 2 else 32)
            or mode["tiles"] != len(candidates) or mode["tma"] not in (0, 1)
            or not rows or len(rows) > mode["budget"] or decision["count"] != len(rows)):
        raise ValueError("incomplete joint search census")
    expected_pairs = []
    expected_fetch = {}
    for tile in candidates:
        if tile["family"] == "baseline" or tile["legality"] != "proven":
            continue
        kinds = ["S1"]
        if model >= 2 and mode["direct"] and tile["layout"]["a_layout"] == "row_major" and tile["smem"]["b_layout"] == "row_major":
            kinds.append("D")
        if mode["tma"]:
            kinds.append("T2")
        for fetch, kind in enumerate(kinds, 1):
            expected_pairs.append((tile["tile"], kind))
            expected_fetch[(tile["tile"], kind)] = fetch
    if [(row["tile"], row.get("kind", "T2" if row["engine"] == "tma_like" else "S1")) for row in rows] != expected_pairs:
        raise ValueError("joint candidate budget/coverage mismatch")
    derived = {}
    for ordinal, row in enumerate(rows, 1):
        tile = by_tile[row["tile"]]
        grid, gmem, smem = tile["grid"], tile["gmem"], tile["smem"]
        tma = row["engine"] == "tma_like"
        direct = row.get("kind") == "D"
        ctas, steps = grid["ctas"], grid["k_iterations"]
        m, n, k = grid["problem"]
        bm, bn, bk = tile["cta"]
        allocation = tile["shared"] * (2 if tma else 1)
        raw_bytes = ((m * k * grid["grid"][1] + k * n * grid["grid"][0]) * 4
                     if tma else gmem["a"] + gmem["b"])
        raw = max(1, raw_bytes // 1024)
        compute = max(1, tile["flops"] // 1024) + smem["address"] // 1024
        if direct:
            columns = tile["layout"]["thread_columns"]
            tm, tn = bm * columns // tile["threads"], bn // columns
            a, b = direct_sector_bytes(m, n, k, bm, bn, bk, tm, tn, tile["threads"])
            if direct_access is None or direct_access.get(row["id"]) != (a, b, gmem["c"]):
                raise ValueError("direct sectors disagree with register access map")
            allocation, compute = 0, max(1, tile["flops"] // 1024)
            raw = max(1, (a + b) // 1024)
        hidden = min(raw // (ctas * steps), compute // (ctas * steps)) * (steps - 1) * ctas if tma else 0
        local = tile["layout"]["bank_work"] // 32 + gmem["a_issues"] // 32 + smem["b_issues"] // 32
        if direct:
            local = 0
        memory = raw - hidden + gmem["c"] // 1024 + local
        if row["capacity"] != 227 * 1024:
            raise ValueError("unknown joint capacity")
        pressure = (compute + memory) * (allocation + (16 if tma else 0)) // row["capacity"]
        compute += pressure
        derived[row["id"]] = dict(raw_input=raw, hidden_input=hidden, pressure=pressure)
        sync = 2 * ctas * steps + (ctas if tma else 0)
        if direct:
            sync = 0
        launch = 32 * ctas if tma else 0
        legal = (allocation + (16 if tma else 0) <= min(48 * 1024, row["capacity"]))
        resource_legal = legal
        if model >= 3:
            columns = tile["layout"]["thread_columns"]
            tm, tn = bm * columns // tile["threads"], bn // columns
            expected_resource = dict(register_bytes=4 * (tm * tn + (2 if direct else tm + tn)),
                register_capacity=1020, shared_bytes=allocation + (16 if tma else 0),
                shared_limit=min(48 * 1024, row["capacity"]), threads=tile["threads"],
                thread_limit=1024, source_line=row["source_line"])
            if resources is None or resources.get(row["id"]) != expected_resource:
                raise ValueError("movement resources disagree with tile/schedule")
            resource_legal &= (expected_resource["register_bytes"] <= expected_resource["register_capacity"]
                               and 0 < tile["threads"] <= expected_resource["thread_limit"])
            legal = resource_legal
        if tma:
            legal &= (tile["layout"]["a_layout"] == "row_major" and
                      smem["b_layout"] == "row_major" and
                      m % bm == n % bn == k % bk == 0 and n % 4 == k % 4 == 0
                      and tile["shared"] % 128 == 0)
        if model == 4:
            if profile is None:
                raise ValueError("missing readiness profile")
            record, terms = readiness_cost(tile, row["kind"], profile["latency"],
                                            (direct_access or {}).get(row["id"]))
            if (readiness or {}).get(row["id"]) != record:
                raise ValueError("readiness ledger or recurrence mismatch")
            compute, memory, _, sync, launch, _, total = terms
            derived[row["id"]] = dict(delivery=record["delivery"], window=record["window"], wait=record["wait"])
        if (row["id"] != ordinal or row["tile_plan"] != tile["plan"]
                or row["fetch_plan"] != expected_fetch[(row["tile"], row.get("kind", "T2" if tma else "S1"))]
                or row["engine"] != ("tma_like" if row.get("kind") == "T2" else "demand") and model >= 2
                or row["source_line"] != tile["source_line"]
                or row["buffering"] != allocation
                or row["legality"] != ("proven" if legal else "rejected")
                or (legal and row["reason"] != "none")
                or (model >= 3 and not legal and row["reason"] != ("resource" if not resource_legal else "target_mismatch"))
                or (row["compute"], row["memory"], row["communication"], row["sync"],
                    row["launch"], row["runtime"], row["total"]) !=
                   (compute, memory, 0, sync, launch, 0, compute + memory + sync + launch)):
            raise ValueError("joint cost or lowerability disagrees with tile/schedule")
    eligible = [row for row in rows if row["legality"] == "proven"]
    if set(direct_access or {}) != {row["id"] for row in rows if row.get("kind") == "D"}:
        raise ValueError("direct sector coverage mismatch")
    if (model >= 3 and set(resources or {}) != {row["id"] for row in rows}) or (model < 3 and resources):
        raise ValueError("movement resource coverage mismatch")
    if (model == 4 and set(readiness or {}) != {row["id"] for row in rows}) or (model != 4 and (readiness or profile)):
        raise ValueError("readiness coverage mismatch")
    synchronous = [row for row in eligible if row["engine"] == "demand" and row.get("kind") != "D"]
    if (not synchronous or decision["selected"] != min(eligible, key=lambda row: (row["total"], row["id"]))["id"]
            or decision["fallback"] != min(synchronous, key=lambda row: (row["total"], row["id"]))["id"]):
        raise ValueError("joint selection or synchronous fallback disagrees with costs")
    return dict(mode=mode, candidates=rows, selection=decision,
                chosen=rows[decision["selected"] - 1], fallback=rows[decision["fallback"] - 1], derived=derived,
                direct_access=direct_access or {}, resources=resources or {},
                readiness=readiness or {}, profile=profile)


def parse(trace: str) -> tuple[list[dict], dict, dict]:
    candidates: list[dict] = []
    grids: dict[int, dict] = {}
    k_loop: dict | None = None
    whirl_kernel: dict | None = None
    device_binding: dict | None = None
    pipeline_kernel: dict | None = None
    edge_plan: dict | None = None
    selection: dict | None = None
    action: dict | None = None
    fetch_candidates: list[dict] = []
    fetch_selection: dict | None = None
    fetch_action: dict | None = None
    fetch_reason = None
    cost_model: str | None = None
    layouts: dict[int, dict] = {}
    gmem: dict[int, dict] = {}
    smem: dict[int, dict] = {}
    roof_target: dict | None = None
    context: dict | None = None
    phases: dict[int, dict] = {}
    semantic: list[dict] = []
    locality: list[dict] = []
    joint_mode, joint_selection = None, None
    joint_candidates = []
    direct_access = {}
    joint_resources = {}
    joint_readiness = {}
    readiness_profile = None
    trace_version: str | None = None
    for line in trace.splitlines():
        if not line.startswith("AI_OPT "):
            continue
        fields = line.split()
        if len(fields) < 3 or fields[1] not in ("1", "2", "3", "4", "5", "6", "7"):
            raise ValueError(f"unsupported AI_OPT schema: {line}")
        if trace_version is not None and fields[1] != trace_version:
            raise ValueError("mixed AI_OPT schema versions")
        trace_version = fields[1]
        kind = fields[2]
        if kind == "JOINT_MODE" and trace_version == "7" and len(fields) in (10, 11):
            if joint_mode is not None or fields[3] != "joint" or fields[8] != "low" or fields[9] not in ("model=1", "model=2", "model=3", "model=4") or fields[7] != ("cycles" if fields[9] == "model=4" else "relative"):
                raise ValueError("invalid joint mode")
            joint_mode = dict(budget=int(fields[4]), tiles=int(fields[5]), tma=int(fields[6]))
            if fields[9] in ("model=2", "model=3", "model=4"):
                if len(fields) != 11 or fields[10] not in ("direct=0", "direct=1"):
                    raise ValueError("invalid direct search policy")
                joint_mode.update(model=int(fields[9][-1]), direct=int(fields[10][-1]))
        elif kind == "READINESS_PROFILE" and trace_version == "7" and len(fields) == 12:
            if (readiness_profile is not None or fields[3] != "assumed_v1" or
                    fields[5:] != ["4", "32", "128", "4", "8", "2", "one_cta_per_sm"] or
                    not 0 <= int(fields[4]) <= 1000000):
                raise ValueError("invalid readiness profile")
            readiness_profile = dict(name=fields[3], latency=int(fields[4]))
        elif kind == "JOINT_READINESS" and trace_version == "7" and len(fields) == 17:
            ident = int(fields[3])
            if ident in joint_readiness or min(map(int, fields[4:])) < 0:
                raise ValueError("invalid or duplicate readiness row")
            joint_readiness[ident] = dict(zip(("ctas", "steps", "consumer", "input_bytes",
                "output_bytes", "copy", "warps", "delivery", "window", "wait", "waves", "total", "source_line"), map(int, fields[4:])))
        elif kind == "JOINT_RESOURCE" and trace_version == "7" and len(fields) == 11:
            ident = int(fields[3])
            if ident in joint_resources or min(map(int, fields[4:])) < 0:
                raise ValueError("invalid or duplicate movement resource record")
            joint_resources[ident] = dict(zip(("register_bytes", "register_capacity", "shared_bytes",
                "shared_limit", "threads", "thread_limit", "source_line"), map(int, fields[4:])))
        elif kind == "DIRECT_ACCESS" and trace_version == "7" and len(fields) == 8:
            ident, a, b, c = map(int, fields[3:7])
            if ident in direct_access or fields[7] != "cache=unknown" or min(a, b, c) <= 0:
                raise ValueError("invalid direct access record")
            direct_access[ident] = (a, b, c)
        elif kind == "JOINT_CAND" and trace_version == "7" and len(fields) in (20, 21):
            row = dict(zip(("id", "tile", "tile_plan", "fetch_plan"), map(int, fields[3:7])))
            row.update(zip(("engine", "legality", "reason"), fields[7:10]))
            row.update(zip(("compute", "memory", "communication", "sync", "launch", "runtime", "total", "buffering", "capacity"), map(int, fields[10:19])))
            row["source_line"] = int(fields[-1])
            if len(fields) == 21:
                if fields[19] not in ("D", "S1", "T2"):
                    raise ValueError("unknown residency plan")
                row["kind"] = fields[19]
            joint_candidates.append(row)
        elif kind == "JOINT_SELECT" and trace_version == "7" and len(fields) == 7:
            if joint_selection is not None or fields[-1] != "selection_reason=lowest_complete_joint_cost":
                raise ValueError("invalid joint selection")
            joint_selection = dict(zip(("selected", "fallback", "count"), map(int, fields[3:6])))
        elif kind == "CONTEXT" and trace_version in ("6", "7") and len(fields) == 5:
            if context is not None or fields[4] != "matmul_f32_source":
                raise ValueError("duplicate or unknown AI optimization context")
            context = dict(pu=fields[3], scope=fields[4])
        elif kind == "PHASE" and trace_version in ("6", "7") and len(fields) == 11:
            scope, number, status, reason, regions, count, selected, rewrites = fields[3:]
            phase = int(number)
            if (scope != "matmul_f32_source" or phase not in range(12)
                    or phase in phases or status not in PHASE_STATUSES
                    or not reason or any(int(value) < 0 for value in
                                      (regions, count, selected, rewrites))):
                raise ValueError("invalid or duplicate phase coverage")
            phases[phase] = dict(status=status, reason=reason,
                                 regions=int(regions), candidates=int(count),
                                 selected=int(selected), rewrites=int(rewrites))
        elif kind == "SEMANTIC" and trace_version == "7" and len(fields) == 13:
            (fact, value, role, rank, shape, dtype, ownership, reuse,
             uses, source_line) = fields[3:]
            semantic.append(dict(fact=int(fact), value=int(value), role=role,
                                 rank=int(rank), shape=shape, dtype=dtype,
                                 ownership=ownership, reuse=reuse,
                                 uses=int(uses), source_line=int(source_line)))
        elif kind == "LOCALITY" and trace_version == "7" and len(fields) == 19:
            (fact, value, size, lifetime, distance, alias, access, residency,
             critical, object_bytes, reuse_statements, working_set_bytes,
             read_bytes, write_bytes, producer, last_use) = fields[3:]
            locality.append(dict(fact=int(fact), value=int(value), size=size,
                                 lifetime=lifetime, distance=distance,
                                 alias=alias, access=access, residency=residency,
                                 critical=critical, object_bytes=int(object_bytes),
                                 reuse_statements=int(reuse_statements),
                                 working_set_bytes=int(working_set_bytes),
                                 read_bytes=int(read_bytes),
                                 write_bytes=int(write_bytes),
                                 producer=int(producer), last_use=int(last_use)))
        elif kind == "PIPELINE_KERNEL" and trace_version == "7" and len(fields) == 13:
            if pipeline_kernel is not None:
                raise ValueError("duplicate executable pipeline")
            ordinal, owner, tile_plan, fetch_plan, engine, stages, size = fields[3:10]
            if fields[10] not in ("acquire_issue_wait_compute_release", "direct_load_compute_store") or fields[11] != "whirl_order=verified":
                raise ValueError("unproven executable pipeline order")
            pipeline_kernel = dict(ordinal=int(ordinal), owner=owner, tile_plan=int(tile_plan),
                                   fetch_plan=int(fetch_plan), engine=engine, stages=int(stages),
                                   bytes=int(size), order=fields[10], abi=fields[12][4:])
        elif kind == "COST_MODEL" and trace_version == "7" and len(fields) in (6, 7, 9, 10):
            if cost_model is not None or fields[3:] not in (
                    ["2", "valid_predicated_elements", "dynamic_cta_k_barriers"],
                    ["3", "valid_predicated_elements", "dynamic_cta_k_barriers",
                     "bank_conflict_work"],
                    ["4", "valid_predicated_elements", "dynamic_cta_k_barriers",
                     "bank_conflict_work", "source_warp_sectors",
                     "smem_a_load_issues"],
                    ["5", "valid_predicated_elements", "dynamic_cta_k_barriers",
                     "bank_conflict_work", "source_warp_sectors",
                     "smem_all_operand_issues", "smem_address_work"]):
                raise ValueError("unknown or duplicate tile cost model")
            cost_model = "v" + fields[3]
        elif kind == "ROOF_TARGET" and trace_version in ("4", "5", "6", "7") and len(fields) == 9:
            if roof_target is not None:
                raise ValueError("duplicate Roofline target")
            target, name, fp32, hbm, traffic, ceiling = fields[3:]
            if traffic != "traffic=global_requests_as_cold_hbm_proxy" or ceiling != "ceiling=published_peak":
                raise ValueError("unknown Roofline assumptions")
            roof_target = dict(target=int(target), name=name, fp32=int(fp32),
                               hbm=int(hbm))
            if name not in ("h100_sxm_reference", "unavailable"):
                raise ValueError("unknown Roofline target")
            if (name == "unavailable") != (int(fp32) == int(hbm) == 0):
                raise ValueError("inconsistent Roofline target")
            if name == "h100_sxm_reference" and (int(fp32), int(hbm)) != (67000, 3350):
                raise ValueError("reference Roofline ceilings changed")
        elif kind == "TILE_CAND" and len(fields) == (26 if trace_version == "1" else 32 if trace_version in ("4", "5", "6", "7") else 27):
            values = fields[3:]
            roof_values = tuple(map(int, values[-5:])) if trace_version in ("4", "5", "6", "7") else None
            if roof_values is not None:
                del values[-5:]
            source_line = int(values.pop()) if trace_version != "1" else None
            if source_line is not None and source_line < 0:
                raise ValueError("negative source line")
            (site, tile, candidate, plan, family, legality, reason,
             complete, unit, confidence, compute, memory, communication,
             sync, launch, runtime, total, shared, registers, threads,
             cta_m, cta_n, cta_k) = values
            row = dict(site=int(site), tile=int(tile), candidate=int(candidate),
                       plan=int(plan), family=family, legality=legality,
                       reason=reason, complete=int(complete), unit=unit,
                       confidence=confidence, compute=int(compute),
                       memory=int(memory), communication=int(communication),
                       sync=int(sync), launch=int(launch), runtime=int(runtime),
                       total=int(total), shared=int(shared),
                       registers=int(registers), threads=int(threads),
                       cta=(int(cta_m), int(cta_n), int(cta_k)),
                       source_line=source_line)
            if roof_values is not None:
                (row["flops"], row["roof_bytes"], row["roof_compute_ns"],
                 row["roof_memory_ns"], row["roof_bound_ns"]) = roof_values
            if row["complete"] and row["total"] != sum(
                    row[key] for key in ("compute", "memory", "communication",
                                         "sync", "launch", "runtime")):
                raise ValueError("candidate cost components do not sum to total")
            candidates.append(row)
        elif kind == "TILE_GRID" and trace_version == "7" and len(fields) == 13:
            (site, tile, plan, problem_m, problem_n, problem_k,
             grid_m, grid_n, grid_ctas, k_iterations) = map(int, fields[3:])
            if plan in grids:
                raise ValueError("duplicate tile grid")
            grids[plan] = dict(site=site, tile=tile,
                               problem=(problem_m, problem_n, problem_k),
                               grid=(grid_m, grid_n),
                               ctas=grid_ctas, k_iterations=k_iterations)
        elif kind == "TILE_LAYOUT" and trace_version == "7" and len(fields) == 12:
            site, tile, plan = map(int, fields[3:6])
            copy_map, a_layout = fields[6:8]
            stride, bank_work, thread_columns, warps = map(int, fields[8:])
            if (plan in layouts or copy_map != "linear" or a_layout not in
                    ("row_major", "transposed_padded")):
                raise ValueError("invalid or duplicate tile layout")
            layouts[plan] = dict(site=site, tile=tile, copy_map=copy_map,
                                 a_layout=a_layout, a_stride=stride,
                                 bank_work=bank_work,
                                 thread_columns=thread_columns, warps=warps)
        elif kind == "TILE_SMEM" and trace_version == "7" and len(fields) == 17:
            site, tile, plan, banks, word = map(int, fields[3:8])
            b_layout = fields[8]
            keys = ("a_bytes", "b_bytes", "a_copy", "b_copy", "a_read", "b_read", "b_issues", "address")
            if plan in smem or b_layout not in ("row_major", "xor_columns"):
                raise ValueError("invalid or duplicate SMEM record")
            smem[plan] = dict(zip(keys, map(int, fields[9:])), site=site,
                              tile=tile, banks=banks, word=word, b_layout=b_layout)
        elif kind == "TILE_GMEM" and trace_version == "7" and len(fields) == 12:
            site, tile, plan, sector, a, b, c, aligned, a_issues = map(int, fields[3:])
            if (plan in gmem or min(a, b, c, aligned, a_issues) < 0 or
                    sector not in (0, 32)):
                raise ValueError("invalid or duplicate GMEM sector record")
            gmem[plan] = dict(site=site, tile=tile, sector=sector,
                              a=a, b=b, c=c, aligned=aligned, a_issues=a_issues)
        elif kind == "TILE_SELECT" and len(fields) == (11 if trace_version in ("3", "4", "5", "6", "7") else 10):
            if selection is not None:
                raise ValueError("duplicate tile selection")
            reason = fields[-1] if trace_version in ("3", "4", "5", "6", "7") else "not_recorded"
            allowed = ("selection_reason=lowest_complete_relative_cost",
                       "selection_reason=lowest_complete_joint_cost",
                       "selection_reason=roofline_near_cost") if trace_version in ("4", "5", "6", "7") else (
                       "selection_reason=lowest_complete_relative_cost",)
            if trace_version in ("3", "4", "5", "6", "7") and reason not in allowed:
                raise ValueError("unknown tile selection reason")
            if trace_version in ("3", "4", "5", "6", "7"):
                reason = reason.partition("=")[2]
            site, baseline, selected, target, legal, complete, rejected = map(
                int, fields[3:10])
            selection = dict(site=site, baseline=baseline, selected=selected,
                             target=target, legal=legal, complete=complete,
                             rejected=rejected, reason=reason)
        elif kind == "TILE_ACTION" and len(fields) == 9:
            if action is not None:
                raise ValueError("duplicate tile action")
            site, plan, emission, tile_rewrite, lower, count = fields[3:]
            action = dict(site=int(site), plan=int(plan), emission=emission,
                          tile_rewrite=tile_rewrite, lower=lower,
                          lowered_count=int(count))
        elif kind == "EDGE_PLAN" and trace_version == "7" and len(fields) == 10:
            if edge_plan is not None:
                raise ValueError("duplicate device edge plan")
            site, plan, policy, a_load, b_load, c_store, barriers = fields[3:]
            if not (a_load.startswith("a=") and b_load.startswith("b=")
                    and c_store.startswith("c=")):
                raise ValueError("malformed device edge plan")
            edge_plan = dict(site=int(site), plan=int(plan), policy=policy,
                             a_load=a_load[2:], b_load=b_load[2:],
                             c_store=c_store[2:],
                             barriers=barriers)
        elif kind == "K_LOOP" and trace_version == "7" and len(fields) == 10:
            if k_loop is not None:
                raise ValueError("duplicate selected K loop")
            site, plan, tile, start, end, step, scope = fields[3:]
            k_loop = dict(site=int(site), plan=int(plan), tile=int(tile),
                          start=int(start), end=int(end), step=int(step),
                          scope=scope)
        elif kind == "WHIRL_KERNEL" and trace_version == "7" and len(fields) == 7:
            if whirl_kernel is not None:
                raise ValueError("duplicate device WHIRL kernel")
            site, plan, tile, image = fields[3:]
            whirl_kernel = dict(site=int(site), plan=int(plan),
                                tile=int(tile), image=image)
        elif kind == "DEVICE_BIND" and trace_version == "7" and len(fields) == 9:
            if device_binding is not None:
                raise ValueError("duplicate device binding in single-PU report")
            ordinal, owner, plan, tile, entry, image = fields[3:]
            device_binding = dict(ordinal=int(ordinal), owner=owner,
                                  plan=int(plan), tile=int(tile),
                                  entry=entry, image=image)
        elif kind == "FETCH_CAND" and trace_version in ("5", "6", "7") and len(fields) in (15, 16):
            (site, plan, optimization_plan, engine, legality, reason,
             stages, distance, buffering, raw, hidden, unhidden) = fields[3:15]
            row = dict(site=int(site), plan=int(plan),
                       optimization_plan=int(optimization_plan),
                       engine=engine, legality=legality, reason=reason,
                       stages=int(stages), distance=int(distance),
                       buffering=int(buffering), raw=int(raw),
                       hidden=int(hidden), unhidden=int(unhidden))
            if row["raw"] != row["hidden"] + row["unhidden"]:
                raise ValueError("fetch movement cost does not balance")
            fetch_candidates.append(row)
            if len(fields) == 16:
                if fields[15] not in ("D", "S1", "T2"):
                    raise ValueError("unknown fetch residency kind")
                row["kind"] = fields[15]
        elif kind == "FETCH_SELECT" and trace_version in ("5", "6", "7") and len(fields) == 8:
            if fetch_selection is not None:
                raise ValueError("duplicate fetch selection")
            site, baseline, selected, engine, reason = fields[3:]
            if reason not in ("selection_reason=lowest_complete_relative_cost", "selection_reason=lowest_complete_joint_cost"):
                raise ValueError("unknown fetch selection reason")
            fetch_reason = reason.partition("=")[2]
            fetch_selection = dict(site=int(site), baseline=int(baseline),
                                   selected=int(selected), engine=engine)
        elif kind == "FETCH_ACTION" and trace_version in ("5", "6", "7") and len(fields) == 8:
            if fetch_action is not None:
                raise ValueError("duplicate fetch action")
            site, selected, source, movement, rewrite = fields[3:]
            fetch_action = dict(site=int(site), selected=int(selected),
                                source=source, movement=movement,
                                rewrite=rewrite)
        else:
            raise ValueError(f"unknown or malformed AI_OPT record: {line}")
    if trace_version in ("6", "7"):
        if context is None or set(phases) != set(range(12)):
            raise ValueError("missing AI optimization context or phase coverage")
        for phase, row in phases.items():
            if (row["status"] in ("disabled_by_option", "disabled_by_policy",
                                  "not_applicable", "no_eligible_region")
                    and (row["candidates"] or row["selected"] or row["rewrites"])):
                raise ValueError(f"inactive phase P{phase} has decision evidence")
            if row["status"] != "rewritten" and row["rewrites"]:
                raise ValueError(f"phase P{phase} claims unverified rewrite")
        if not candidates and selection is None and action is None:
            if phases[7]["status"] not in ("disabled_by_option", "no_eligible_region"):
                raise ValueError("missing tile decision for active P7")
            if (fetch_candidates or fetch_selection or fetch_action or grids or
                    layouts or gmem or smem or pipeline_kernel or joint_mode or joint_candidates or joint_selection or direct_access or joint_resources):
                raise ValueError("optimization decision without tile selection")
            if semantic or locality:
                raise ValueError("tensor facts without active tile analysis")
            return [], dict(context=context, phases=phases,
                            version=int(trace_version),
                            selected=0), {}
    if not candidates or selection is None or action is None:
        raise ValueError("missing candidate, selection, or action record")
    if trace_version in ("4", "5", "6", "7"):
        if roof_target is None or roof_target["target"] != selection["target"]:
            raise ValueError("missing or mismatched Roofline target")
        selection["roofline_target"] = roof_target
        for row in candidates:
            if roof_target["name"] == "unavailable":
                if any(row[key] for key in ("roof_bytes", "roof_compute_ns",
                                        "roof_memory_ns", "roof_bound_ns")):
                    raise ValueError("Roofline projection without target")
            else:
                compute = (row["flops"] + roof_target["fp32"] - 1) // roof_target["fp32"]
                memory = (row["roof_bytes"] + roof_target["hbm"] - 1) // roof_target["hbm"]
                if (row["roof_bytes"] <= 0 or row["roof_compute_ns"] != compute
                        or row["flops"] <= 0
                        or row["roof_memory_ns"] != memory
                        or row["roof_bound_ns"] != max(compute, memory)):
                    raise ValueError("Roofline projection disagrees with target")
    if len({row["plan"] for row in candidates}) != len(candidates) or len({
            row["candidate"] for row in candidates}) != len(candidates):
        raise ValueError("duplicate tile plan or candidate ID")
    if any(row["site"] != selection["site"] for row in candidates):
        raise ValueError("tile site mismatch")
    by_plan = {row["plan"]: row for row in candidates}
    if grids:
        if set(grids) != set(by_plan):
            raise ValueError("incomplete tile grid coverage")
        for plan, grid in grids.items():
            row = by_plan[plan]
            m, n, k = grid["problem"]
            bm, bn, bk = row["cta"]
            if (grid["site"] != row["site"] or grid["tile"] != row["tile"]
                    or min(m, n, k) <= 0):
                raise ValueError("tile grid disagrees with candidate")
            expected = ((m + bm - 1) // bm, (n + bn - 1) // bn,
                        (k + bk - 1) // bk) if min(bm, bn, bk) > 0 else (0, 0, 0)
            if (grid["grid"] != expected[:2]
                    or grid["ctas"] != expected[0] * expected[1]
                    or grid["k_iterations"] != expected[2]
                    or (row["family"] == "baseline") != (row["cta"] == (0, 0, 0))):
                raise ValueError("tile grid disagrees with candidate")
            row["grid"] = grid
    if selection["baseline"] not in by_plan or selection["selected"] not in by_plan:
        raise ValueError("baseline or selected plan missing from candidates")
    if by_plan[selection["baseline"]]["family"] != "baseline":
        raise ValueError("baseline plan does not name baseline family")
    if cost_model not in ("v4", "v5") and gmem:
        raise ValueError("GMEM sector records without v4 cost model")
    if (cost_model == "v5" and set(smem) != set(by_plan)) or (cost_model != "v5" and smem):
        raise ValueError("inconsistent SMEM record coverage")
    if cost_model in ("v2", "v3", "v4", "v5"):
        if cost_model in ("v3", "v4", "v5") and set(layouts) != set(by_plan):
            raise ValueError("incomplete tile layout coverage")
        if cost_model in ("v4", "v5") and set(gmem) != set(by_plan):
            raise ValueError("incomplete GMEM sector coverage")
        for row in candidates:
            grid = row.get("grid")
            if grid is None:
                raise ValueError("v2 cost model lacks candidate grid")
            m, n, k = grid["problem"]
            if row["family"] == "baseline":
                read_bytes = 2 * m * n * k * 4
                sync = 0
            else:
                grid_m, grid_n = grid["grid"]
                read_bytes = (m * k * grid_n + k * n * grid_m) * 4
                sync = 2 * grid["ctas"] * grid["k_iterations"]
            total_bytes = read_bytes + m * n * 4
            bank_work = 0
            if cost_model in ("v3", "v4", "v5"):
                layout = layouts[row["plan"]]
                bm, bn, bk = row["cta"]
                if (layout["site"] != row["site"] or
                        layout["tile"] != row["tile"] or
                        layout["warps"] != (row["threads"] + 31) // 32):
                    raise ValueError("v3 tile layout disagrees with candidate")
                if row["family"] == "baseline":
                    if (layout["a_stride"] or layout["bank_work"] or
                            layout["thread_columns"]):
                        raise ValueError("v3 baseline has physical tile layout")
                else:
                    cols = layout["thread_columns"]
                    if (cols <= 0 or bn % cols or row["threads"] == 0 or
                            bm * cols % row["threads"]):
                        raise ValueError("v3 invalid thread layout")
                    thread_m = bm * cols // row["threads"]
                    transpose = layout["a_layout"] == "transposed_padded"
                    stride = bm + 1 if transpose else bk
                    if (layout["a_stride"] != stride or
                            (transpose and bk < 32) or
                            row["shared"] != (bk * (bm + 1 if transpose else bm)
                                              + bk * bn) * 4):
                        raise ValueError("v3 invalid SMEM layout")
                    groups = (32 + cols - 1) // cols
                    bank_work = (0 if transpose or cols >= 32 else
                                 grid["ctas"] * grid["k_iterations"] * bk *
                                 layout["warps"] * thread_m * (groups - 1))
                    if cost_model != "v5" and layout["bank_work"] != bank_work:
                        raise ValueError("v3 bank work disagrees with geometry")
                row["layout"] = layout
            if cost_model == "v5":
                record = smem[row["plan"]]
                row["smem"] = record
                baseline = row["family"] == "baseline"
                expected = source_smem_work(row)
                bm, bn, bk = row["cta"]
                a_bytes = bk * (bm + int(row["layout"]["a_layout"] == "transposed_padded")) * 4
                if (record["site"] != row["site"] or record["tile"] != row["tile"]
                        or (record["banks"], record["word"]) != ((0, 0) if baseline else (32, 4))
                        or (record["a_bytes"], record["b_bytes"]) != (a_bytes, bk * bn * 4)
                        or any(record[key] != value for key, value in expected.items())
                        or (record["b_layout"] == "xor_columns" and
                            (bn < 32 or bn > 1024 or bn & (bn - 1)))):
                    raise ValueError("SMEM record disagrees with source access map")
                bank_work = sum(record[key] for key in ("a_copy", "b_copy", "a_read", "b_read"))
                if (row["layout"]["bank_work"] != bank_work or
                        row["compute"] != max(1, row["flops"] // 1024) + record["address"] // 1024):
                    raise ValueError("SMEM work disagrees with candidate cost")
            cost_bytes = total_bytes
            if cost_model in ("v4", "v5"):
                record = gmem[row["plan"]]
                if record["site"] != row["site"] or record["tile"] != row["tile"]:
                    raise ValueError("GMEM sector identity disagrees with candidate")
                expected_gmem = ((read_bytes, 0, m * n * 4, total_bytes)
                                 if row["family"] == "baseline" else
                                 source_warp_sector_bytes(row))
                if row["family"] == "baseline":
                    expected_issues = 0
                else:
                    bm, bn, bk = row["cta"]
                    cols = row["layout"]["thread_columns"]
                    tm = bm * cols // row["threads"]
                    expected_issues = (grid["ctas"] * grid["k_iterations"] *
                                       bk * row["layout"]["warps"] * tm)
                if ((record["a"], record["b"], record["c"], record["aligned"])
                        != expected_gmem or
                        record["a_issues"] != expected_issues or
                        record["sector"] != (0 if row["family"] == "baseline" else 32)
                        or sum(expected_gmem[:3]) < total_bytes):
                    raise ValueError("GMEM sectors disagree with source access map")
                row["gmem"] = record
                cost_bytes = sum(expected_gmem[:3])
            if (row["memory"] != max(1, cost_bytes // 1024) +
                    bank_work // 32 +
                    (row["gmem"]["a_issues"] // 32 if cost_model in ("v4", "v5") else 0) +
                    (row["smem"]["b_issues"] // 32 if cost_model == "v5" else 0) or
                    row["sync"] != sync or
                    (row.get("roof_bytes") and
                     row["roof_bytes"] != total_bytes)):
                raise ValueError(f"{cost_model} tile cost disagrees with traced geometry")
        selection["cost_model"] = cost_model
    eligible = [row for row in candidates if row["legality"] == "proven"
                and row["complete"] and row["unit"] != "unknown"]
    if (selection["legal"] != sum(row["legality"] == "proven"
                                   for row in candidates)
            or selection["rejected"] != sum(row["legality"] != "proven"
                                           for row in candidates)
            or selection["complete"] != len(eligible)):
        raise ValueError("selection counts do not match candidate records")
    lowest = min(eligible, key=lambda row: (row["total"], row["plan"])) if eligible else None
    expected = lowest
    expected_reason = "lowest_complete_relative_cost"
    if trace_version in ("4", "5", "6", "7") and roof_target["name"] != "unavailable" and lowest:
        best_bound = lowest["roof_bound_ns"]
        for row in candidates:
            if (row in eligible and row["total"] <= lowest["total"] + lowest["total"] // 33
                    and row["roof_bound_ns"] < best_bound
                    and row["roof_bound_ns"] <= best_bound - best_bound // 20):
                expected, best_bound = row, row["roof_bound_ns"]
                expected_reason = "roofline_near_cost"
    joint = None
    if (direct_access or joint_resources or joint_readiness or readiness_profile) and joint_mode is None:
        raise ValueError("direct access without joint search")
    if joint_mode is not None or joint_candidates or joint_selection is not None:
        joint = verify_joint(joint_mode, joint_candidates, joint_selection, candidates,
                             direct_access, joint_resources, joint_readiness, readiness_profile)
        expected = by_plan[joint["chosen"]["tile_plan"]]
        expected_reason = "lowest_complete_joint_cost"
        selection["joint"] = joint
    if not eligible or selection["selected"] != expected["plan"]:
        raise ValueError("selection disagrees with comparable cost minimum")
    if trace_version in ("4", "5", "6", "7") and selection["reason"] != expected_reason:
        raise ValueError("selection reason disagrees with Roofline decision")
    if cost_model == "v5":
        chosen = by_plan[selection["selected"]]
        selection["selected_smem"] = dict(chosen["smem"],
                                            a_layout=chosen["layout"]["a_layout"])
    if selection["reason"] == "lowest_complete_relative_cost" and any(
            row["unit"] != "relative" for row in eligible):
        raise ValueError("relative-cost reason disagrees with cost units")
    if (action["site"] != selection["site"] or
            action["plan"] != selection["selected"] or
            action["emission"] not in ("source_sidecar", "device_whirl_sidecar") or
            action["tile_rewrite"] not in ("none", "generated_runtime_call") or
            action["lower"] != "dsl_to_runtime"):
        raise ValueError("action does not match selected tile plan")
    if k_loop is not None:
        chosen_loop_tile = next(
            (row for row in candidates if row["plan"] == selection["selected"]),
            None)
        if (chosen_loop_tile is None or
                (k_loop["site"], k_loop["plan"], k_loop["tile"],
                 k_loop["start"], k_loop["step"], k_loop["scope"]) !=
                (selection["site"], selection["selected"],
                 chosen_loop_tile["tile"], 0, chosen_loop_tile["cta"][2],
                 "detached_plan_only") or k_loop["end"] <= 0 or
                ("grid" in chosen_loop_tile and
                 k_loop["end"] != chosen_loop_tile["grid"]["problem"][2])):
            raise ValueError("selected K loop disagrees with tile plan")
        selection["k_loop"] = k_loop
    if whirl_kernel is None and device_binding is not None and selection is not None:
        whirl_kernel = dict(site=selection["site"],
                            plan=device_binding["plan"],
                            tile=device_binding["tile"],
                            image=device_binding["image"])
    if whirl_kernel is not None:
        chosen_kernel_tile = by_plan[selection["selected"]]
        if ((whirl_kernel["site"], whirl_kernel["plan"],
             whirl_kernel["tile"]) !=
            (selection["site"], selection["selected"],
             chosen_kernel_tile["tile"]) or
                not whirl_kernel["image"].endswith(".B")):
            raise ValueError("device WHIRL kernel disagrees with selected tile")
        selection["whirl_kernel"] = whirl_kernel
    if edge_plan is not None:
        chosen = by_plan[selection["selected"]]
        problem = chosen.get("grid", {}).get("problem")
        guards = tuple(extent % tile != 0 for extent, tile in
                       zip(problem, chosen["cta"])) if problem else ()
        expected = "predicated" if any(guards) else "exact"
        direct = joint is not None and joint["chosen"].get("kind") == "D"
        movement = (("zero_fill" if guards[0] or guards[2] else "direct",
                     "zero_fill" if guards[2] or guards[1] else "direct",
                     "guarded_store" if guards[0] or guards[1] else "direct")
                    if guards else ())
        if direct:
            movement = ("guarded_load", "guarded_load", movement[2])
        if (whirl_kernel is None or problem is None or
                (edge_plan["site"], edge_plan["plan"],
                 edge_plan["policy"], edge_plan["a_load"],
                 edge_plan["b_load"], edge_plan["c_store"],
                 edge_plan["barriers"]) !=
                (selection["site"], selection["selected"], expected,
                 *movement, "barriers=none" if direct else "barriers=uniform")):
            raise ValueError("device edge plan disagrees with selected tile")
        selection["edge_plan"] = edge_plan
    if device_binding is not None:
        ordinal = device_binding["ordinal"]
        expected_entry = ("ai_matmul_tile_f32" if ordinal == 0 else
                          f"ai_matmul_tile_f32_{ordinal}")
        if (whirl_kernel is None or context is None or ordinal < 0 or
                device_binding["owner"] != context["pu"] or
                device_binding["entry"] != expected_entry or
                (device_binding["plan"], device_binding["tile"],
                 device_binding["image"]) !=
                (whirl_kernel["plan"], whirl_kernel["tile"],
                 whirl_kernel["image"])):
            raise ValueError("device binding disagrees with selected kernel")
        selection["device_binding"] = device_binding
    if action["emission"] == "device_whirl_sidecar" and whirl_kernel is None:
        raise ValueError("device WHIRL action has no kernel image")
    applied = action["tile_rewrite"] == "generated_runtime_call"
    if applied and (trace_version != "7" or
                    any(row["engine"] == "tma_like" and row["optimization_plan"] ==
                        fetch_selection["selected"] for row in fetch_candidates)):
        raise ValueError("generated M2 rewrite requires v7 synchronous tile")
    if trace_version in ("5", "6", "7") and fetch_candidates:
        if not fetch_candidates or fetch_selection is None or fetch_action is None:
            raise ValueError("missing P8 fetch decision or action")
        if len({row["optimization_plan"] for row in fetch_candidates}) != len(fetch_candidates):
            raise ValueError("duplicate fetch optimization plan")
        by_fetch_plan = {row["optimization_plan"]: row for row in fetch_candidates}
        if fetch_selection["selected"] not in by_fetch_plan:
            raise ValueError("selected fetch plan missing")
        chosen = by_fetch_plan[fetch_selection["selected"]]
        if fetch_reason != ("lowest_complete_joint_cost" if joint else "lowest_complete_relative_cost"):
            raise ValueError("fetch reason disagrees with joint search mode")
        eligible_fetch = [row for row in fetch_candidates if row["legality"] == "proven"]
        if joint:
            if ((chosen["optimization_plan"], chosen["engine"]) != (joint["chosen"]["fetch_plan"], joint["chosen"]["engine"]) or
                (joint["mode"].get("model", 1) >= 2 and chosen.get("kind") != joint["chosen"].get("kind"))):
                raise ValueError("joint selection disagrees with selected fetch plan")
        elif (not eligible_fetch or chosen != min(eligible_fetch, key=lambda row:
                (row["unhidden"] + int(row["engine"] != "demand"),
                 row["optimization_plan"]))):
            raise ValueError("fetch selection disagrees with movement cost")
        tma = chosen["engine"] == "tma_like"
        direct = chosen.get("kind") == "D"
        device = fetch_action["source"] == "device_whirl_sidecar"
        if (chosen["engine"] not in ("demand", "vector", "tma_like")
                or chosen["stages"] != (2 if tma else 1)
                or chosen["distance"] != int(tma)
                or chosen["buffering"] != (0 if direct else
                    by_plan[selection["selected"]]["shared"] * chosen["stages"])
                or fetch_selection["engine"] != chosen["engine"]
                or fetch_action != dict(site=fetch_selection["site"],
                                        selected=fetch_selection["selected"],
                                        source="device_whirl_sidecar" if device else "source_sidecar",
                                        movement="direct_scalar" if direct else "tma_2d" if tma else "cooperative_scalar",
                                        rewrite="whirl_rewrite=device_pipeline" if device else "whirl_rewrite=none")
                or (not device and not tma)):
            raise ValueError("P8 action disagrees with selected plan")
        if device:
            selected_tile = by_plan[selection["selected"]]
            if (whirl_kernel is None or pipeline_kernel is None or device_binding is None
                    or pipeline_kernel != dict(
                        ordinal=device_binding["ordinal"], owner=device_binding["owner"],
                        tile_plan=selection["selected"], fetch_plan=chosen["optimization_plan"],
                        engine=chosen["engine"], stages=chosen["stages"],
                        bytes=chosen["buffering"] + (16 if tma else 0),
                        order="direct_load_compute_store" if direct else "acquire_issue_wait_compute_release",
                        abi="direct_ptr_v1" if direct else "tma_maps_ptr_v1" if tma else "sync_ptr_v1")
                    or (tma and (selected_tile["layout"]["a_layout"] != "row_major" or
                                 selected_tile["smem"]["b_layout"] != "row_major"))):
                raise ValueError("executable pipeline proof disagrees with selected plan")
            selection["pipeline_kernel"] = pipeline_kernel
            if direct:
                selection["selected_smem"] = dict(a_bytes=0, b_bytes=0, barriers=0, residency="direct_gmem")
                selection["selected_direct"] = dict(zip(("a_sectors_bytes", "b_sectors_bytes", "c_sectors_bytes"),
                    joint["direct_access"][joint["chosen"]["id"]]))
        elif pipeline_kernel is not None:
            raise ValueError("pipeline proof without device WHIRL action")
        selection["fetch"] = dict(candidates=fetch_candidates,
                                  selection=fetch_selection,
                                  action=fetch_action)
    elif trace_version == "5":
        raise ValueError("missing P8 fetch decision or action")
    elif fetch_selection is not None or fetch_action is not None:
        raise ValueError("incomplete P8 fetch decision or action")
    elif pipeline_kernel is not None:
        raise ValueError("pipeline proof without fetch decision")
    if trace_version in ("6", "7"):
        expected_status = "rewritten" if applied else "analyzed_no_rewrite"
        expected_rewrites = 1 if applied else 0
        if (phases[7]["status"] != expected_status
                or phases[7]["candidates"] != len(candidates)
                or phases[7]["selected"] != selection["selected"]
                or phases[7]["rewrites"] != expected_rewrites
                or phases[9]["status"] != expected_status
                or phases[9]["selected"] != selection["selected"]
                or phases[9]["rewrites"] != expected_rewrites):
            raise ValueError("P7/P9 coverage disagrees with selected tile")
        if joint and phases[9]["candidates"] != len(joint_candidates):
            raise ValueError("P9 coverage disagrees with complete joint plans")
        if "fetch" in selection:
            device = "pipeline_kernel" in selection
            if (phases[8]["status"] != ("rewritten" if device else "analyzed_no_rewrite")
                    or phases[8]["rewrites"] != int(device)
                    or phases[8]["candidates"] != len(fetch_candidates)
                    or phases[8]["selected"] != fetch_selection["selected"]):
                raise ValueError("P8 coverage disagrees with selected fetch plan")
        elif phases[8]["status"] != "disabled_by_option":
            raise ValueError("P8 coverage lacks a fetch decision")
        if trace_version == "7":
            semantic_values = [row["value"] for row in semantic]
            locality_values = [row["value"] for row in locality]
            if (len(semantic) < 3 or len(set(semantic_values)) != len(semantic)
                    or set(locality_values) != set(semantic_values)
                    or len(locality) != len(semantic)
                    or any(row["fact"] <= 0 or row["rank"] < 0
                           or row["uses"] < 0 or row["source_line"] < 0
                           for row in semantic)
                    or any(row["fact"] <= 0 or row["read_bytes"] < 0
                           or row["write_bytes"] < 0 for row in locality)):
                raise ValueError("incomplete or inconsistent P0/P1 tensor facts")
            selection.update(semantic=semantic, locality=locality)
        selection.update(context=context, phases=phases,
                         version=int(trace_version))
    return candidates, selection, action


def render_coverage(trace: Path, reports: Path, selection: dict,
                    tile_content: str, fetch_content: str) -> None:
    digest = hashlib.sha256(trace.read_bytes()).hexdigest()
    context = selection["context"]
    target = selection.get("roofline_target", {}).get("name", "unavailable")
    version = selection["version"]
    phase_contents = []
    summary = [
        "# What it summarizes: Cross-phase coverage, selected plans, applied rewrites, measurements, and remaining blockers.",
        f"trace_sha256={digest} schema={version} scope={context['scope']} pu={context['pu']} target={target}",
    ]
    for number, (filename, description) in enumerate(PHASE_LOGS):
        row = selection["phases"][number]
        metadata = (
            f"trace_sha256={digest} schema={version} target={target} "
            f"compilation={digest} pu={context['pu']} "
            f"region={'1' if row['regions'] else 'none'} phase=P{number} "
            f"status={row['status']} reason={row['reason']} "
            f"regions={row['regions']} candidates={row['candidates']} "
            f"selected={row['selected']} rewrites={row['rewrites']}\n"
        )
        details = tile_content if number == 7 else fetch_content if number == 8 else ""
        if details:
            details = "\n".join(details.splitlines()[1:]) + "\n"
            if number in (7, 8) and selection.get("joint", {}).get("resources"):
                joint = selection["joint"]
                details += "# P7 staged resources are comparators only; each P8 movement is gated independently.\n"
                details += "# Selected JOINT_RESOURCE columns: joint_id register_bytes register_capacity active_smem_bytes smem_limit threads thread_limit source_line; provisional, not native allocation.\n"
                details += f"JOINT_RESOURCE {joint['chosen']['id']} " + " ".join(
                    map(str, joint["resources"][joint["chosen"]["id"]].values())) + "\n"
                if joint["readiness"]:
                    details += "# Tile-only relative scores above are diagnostics, not the joint selection cost.\n"
                    details += "# Analytical readiness: assumed_v1 reference cycles, low confidence; total=compute+exposed_movement+sync+transfer_issue.\n"
                    details += f"# Assumed startup={joint['profile']['latency']}; one CTA/SM, four SMs; no native calibration.\n"
                    details += "# JOINT_READINESS columns: id CTAs steps consumer input_bytes output_bytes copy warps delivery window wait waves total source_line.\n"
                    details += "".join(f"JOINT_READINESS {ident} " + " ".join(map(str, record.values())) + "\n"
                                       for ident, record in joint["readiness"].items())
        elif number == 0 and selection.get("semantic"):
            details = "".join(
                f"tensor={row['fact']} value={row['value']} role={row['role']} "
                f"rank={row['rank']} shape={row['shape']} dtype={row['dtype']} "
                f"ownership={row['ownership']} reuse={row['reuse']} "
                f"uses={row['uses']} source_line={row['source_line']}\n"
                for row in selection["semantic"]
            )
        elif number == 1 and selection.get("locality"):
            details = "".join(
                f"tensor={row['fact']} value={row['value']} size={row['size']} "
                f"lifetime={row['lifetime']} distance={row['distance']} "
                f"alias={row['alias']} access={row['access']} "
                f"residency={row['residency']} critical={row['critical']} "
                f"object_bytes={row['object_bytes']} "
                f"reuse_statements={row['reuse_statements']} "
                f"working_set_bytes={row['working_set_bytes']} "
                f"read_bytes={row['read_bytes']} write_bytes={row['write_bytes']} "
                f"producer={row['producer']} last_use={row['last_use']}\n"
                for row in selection["locality"]
            )
        elif number == 9 and selection.get("whirl_kernel"):
            kernel = selection["whirl_kernel"]
            details = (f"device_pu=ai_matmul_tile_f32 plan={kernel['plan']} "
                       f"tile={kernel['tile']} image={kernel['image']} "
                       "state=materialized_sidecar target_lowered=no\n")
            if "joint" in selection:
                joint = selection["joint"]
                if joint["mode"].get("model") == 4:
                    details += ("# Joint cost model 4: assumed_v1 low-confidence reference cycles, not measured GPU timing.\n"
                                "# total=consumer_compute+exposed_movement_and_copy+sync+transfer_issue; wait is diagnostic, not added twice.\n"
                                "# T2: wait next-ready, consume, release, then refill; two-slot prologue and final drain retained.\n"
                                "# One CTA/SM, four assumed SMs; no cache/native occupancy claim.\n")
                    suffix = "delivery window wait"
                else:
                    details += f"# Joint cost model {joint['mode'].get('model', 1)}: low-confidence relative units; not measured latency.\n"
                    details += "# Formula: total=compute_with_pressure+memory_unhidden+communication_unhidden+sync+setup+runtime; memory_unhidden=raw_input-hidden_input+C_store_sectors/1024+local_SMEM_work.\n"
                    details += "# Overlap: min(raw_input/(CTAs*K_tiles),base_compute/(CTAs*K_tiles))*(K_tiles-1)*CTAs for lowerable two-slot TMA; zero synchronously. Pressure=(base_compute+memory_unhidden)*physical_storage/shared_capacity.\n"
                    suffix = "raw_input hidden_input pressure"
                details += "# Columns: id tile tile_plan fetch_plan engine legality reason compute memory_unhidden communication_unhidden sync setup runtime total buffer_bytes capacity_bytes " + ("kind " if joint['mode'].get('model', 1) >= 2 else "") + suffix + " source_line\n"
                details += "".join("joint " + " ".join(str(value) for key, value in candidate.items() if key != "source_line")
                                   + " " + " ".join(str(value) for value in joint["derived"][candidate["id"]].values())
                                   + f" {candidate['source_line']}\n" for candidate in joint["candidates"])
                details += (f"selection joint={joint['chosen']['id']} fallback={joint['fallback']['id']} "
                            f"fallback_tile={joint['fallback']['tile_plan']} fallback_engine=demand "
                            "selection_reason=lowest_complete_joint_cost fallback_kind=compile_time_plan\n")
                if joint["direct_access"]:
                    details += "# DIRECT_ACCESS columns: joint_id A_sector_bytes B_sector_bytes C_sector_bytes; cache=unknown, source warp requests, not measured HBM bytes.\n"
                    details += "".join(f"DIRECT_ACCESS {ident} {a} {b} {c}\n" for ident, (a, b, c) in joint["direct_access"].items())
                if joint["resources"]:
                    details += "# JOINT_RESOURCE columns: joint_id register_bytes register_capacity active_smem_bytes smem_limit threads thread_limit source_line. Provisional value storage, not native registers/occupancy; staged P7 resource filtering is deferred.\n"
                    details += "".join(f"JOINT_RESOURCE {ident} " + " ".join(map(str, resource.values())) + "\n"
                                       for ident, resource in joint["resources"].items())
                if joint["readiness"]:
                    details += f"# READINESS_PROFILE {joint['profile']}\n"
                    details += "# JOINT_READINESS columns: id CTAs steps consumer input_bytes output_bytes copy warps delivery window wait waves total source_line.\n"
                    details += "".join(f"JOINT_READINESS {ident} " + " ".join(map(str, record.values())) + "\n"
                                       for ident, record in joint["readiness"].items())
        else:
            details = "detail=not_recorded\n" if row["status"] == "analyzed_no_rewrite" else ""
        content = f"# What it summarizes: {description}\n{metadata}{details}"
        (reports / "phases" / filename).write_text(content, encoding="utf-8")
        phase_contents.append(f"## P{number} {filename}\n{content}")
        next_step = ("verify_generated_runtime_and_gpu" if number == 7
                     and row["status"] == "rewritten" else
                     "apply_selected_tile_to_executable_whirl" if number == 7
                     and row["selected"] else "native_cuda_validation" if number == 8
                     and row["selected"] else "none")
        summary.append(
            f"P{number} status={row['status']} reason={row['reason']} "
            f"regions={row['regions']} candidates={row['candidates']} "
            f"selected={row['selected']} rewrites={row['rewrites']} "
            + (f"selected_grid_ctas={selection['selected_grid_ctas']} "
               f"grid_parallelism_cost={'assumed_wave_model' if selection.get('joint', {}).get('mode', {}).get('model') == 4 else 'unmodeled'} "
               if number == 7 and 'selected_grid_ctas' in selection else "")
            + f"next={next_step}"
        )
    if selection.get("whirl_kernel"):
        kernel = selection["whirl_kernel"]
        summary.append(
            f"device_pu=ai_matmul_tile_f32 plan={kernel['plan']} "
            f"tile={kernel['tile']} image={kernel['image']} "
            "state=materialized_sidecar target_lowered=no")
    if selection.get("device_binding"):
        binding = selection["device_binding"]
        summary.append(
            f"device_binding owner={binding['owner']} "
            f"entry={binding['entry']} ordinal={binding['ordinal']} "
            f"plan={binding['plan']} image={binding['image']}")
    if selection.get("edge_plan"):
        edge = selection["edge_plan"]
        summary.append(
            f"edge_plan={edge['policy']} a_load={edge['a_load']} "
            f"b_load={edge['b_load']} c_store={edge['c_store']} "
            f"{edge['barriers']}")
    if selection.get("cost_model"):
        summary.append(f"cost_model={selection['cost_model']} "
                       "reads=valid_predicated_elements "
                       "sync=dynamic_cta_k_barriers "
                       + ("gmem=source_warp_sectors "
                          if selection["cost_model"] in ("v4", "v5") else "")
                       + "confidence=low")
    if "selected_smem" in selection:
        summary.append("selected_smem " + " ".join(
            f"{key}={value}" for key, value in selection["selected_smem"].items()
            if key not in ("site", "tile")))
    if "pipeline_kernel" in selection:
        summary.append("executable_pipeline " + " ".join(
            f"{key}={value}" for key, value in selection["pipeline_kernel"].items()))
    if "selected_direct" in selection:
        summary.append("direct_gmem " + " ".join(f"{key}={value}" for key, value in selection["selected_direct"].items())
                       + " cache=unknown tile_roofline=staged_comparator_only")
    if "joint" in selection:
        joint = selection["joint"]
        summary.append(f"gemm_plan_search=joint model={joint['mode'].get('model', 1)} candidates={len(joint['candidates'])} "
                       f"selected={joint['chosen']['id']} tile_plan={joint['chosen']['tile_plan']} "
                       f"engine={joint['chosen']['engine']} total={joint['chosen']['total']} "
                       f"kind={joint['chosen'].get('kind', 'not_recorded')} "
                       f"fallback={joint['fallback']['id']} fallback_tile={joint['fallback']['tile_plan']} "
                       "fallback_kind=compile_time_plan selection_reason=lowest_complete_joint_cost")
        if joint["resources"]:
            resource = joint["resources"][joint["chosen"]["id"]]
            summary.append("movement_resources model=logical_v1 " + " ".join(f"{key}={value}" for key, value in resource.items())
                           + " native_registers=unknown occupancy=unknown p7_resource_gate=deferred")
        if joint["readiness"]:
            record = joint["readiness"][joint["chosen"]["id"]]
            summary.append(f"readiness profile=assumed_v1 latency={joint['profile']['latency']} "
                           f"delivery={record['delivery']} window={record['window']} "
                           f"wait={record['wait']} waves={record['waves']} unit=reference_cycles "
                           "confidence=low calibration=none selection_model=analytical_schedule")
    (reports / "optimization.log").write_text(
        "# What it summarizes: Complete P0-P11 optimization process, including every phase decision and non-execution reason.\n"
        + "".join(phase_contents), encoding="utf-8")
    (reports / "optimization-summary.log").write_text(
        "\n".join(summary) + "\n", encoding="utf-8")


def render(trace: Path, reports: Path) -> tuple[dict, dict]:
    candidates, selection, action = parse(trace.read_text(encoding="utf-8"))
    applied = action.get("tile_rewrite") == "generated_runtime_call"
    if not candidates:
        (reports / "phases").mkdir(parents=True, exist_ok=True)
        render_coverage(trace, reports, selection, "", "")
        return selection, action
    tile_lines = [
        "# What it summarizes: AIO-9 tile candidates, costs, selection, and actual source/WHIRL actions.",
        "# Cost model: low-confidence relative score, not cycles, latency, or measured performance.",
        "# Symbols: M,N,K=problem extents; E=element bytes; BM,BN,BK=CTA tile extents; ceil=ceiling division.",
        "# compute=max(1,floor(2*M*N*K/1024)); one multiply-add counts as two operations.",
        ("# memory_unhidden=max(1,floor((A+B+C worst sector bytes)/1024))+floor(bank_conflict_work/32)+floor(smem_a_load_issues/32)."
         if selection.get("cost_model") == "v4" else
         "# memory_unhidden=max(1,floor((read_bytes+write_bytes)/1024)); write_bytes=M*N*E."),
        *( ["# v3/v4 memory_unhidden adds floor(bank_conflict_work/32); this is a low-confidence bank-serialization proxy, not time."]
           if selection.get("cost_model") in ("v3", "v4") else [] ),
        *( ["# v4 memory_unhidden also adds floor(smem_a_load_issues/32), a low-confidence issue-work proxy.",
            "# v4 memory_unhidden uses worst 4-byte-aligned A/B/C source-warp 32-byte sector requests, not valid payload bytes.",
            "# TILE_GMEM: A/B loads and C stores; aligned_bytes assumes 32-byte-aligned operand bases, cost uses independent worst-case bases.",
            "# Sector counts model source-level scalar warp issues, not PTX vectorization, cache hits, or HBM transactions."]
           if selection.get("cost_model") == "v4" else [] ),
        "# read_bytes baseline=2*M*N*K*E.",
        ("# read_bytes tiled=(M*K*ceil(N/BN)+K*N*ceil(M/BM))*E; invalid edge loads are zero-filled without GMEM requests."
         if selection.get("cost_model") in ("v2", "v3", "v4", "v5") else
         "# read_bytes tiled=ceil(M/BM)*ceil(N/BN)*ceil(K/BK)*(BM*BK+BK*BN)*E."),
        "# total=compute+memory_unhidden+communication_unhidden+sync+launch+runtime_selection.",
        ("# sync tiled=2*ceil(M/BM)*ceil(N/BN)*ceil(K/BK), baseline=0; this counts issued CTA barriers, not exposed latency."
         if selection.get("cost_model") in ("v2", "v3", "v4", "v5") else
         "# sync=static barrier_count (0 baseline, 2 tiled); communication_unhidden, launch, runtime_selection=0 here."),
        ("# Caveat: source-warp sectors are not observed cache/HBM bytes; bandwidth, cache effects, overlap, and SM parallelism remain unmodeled."
         if selection.get("cost_model") in ("v4", "v5") else
         "# Caveat: valid element bytes are not sector transactions or HBM bytes; bandwidth, cache effects, overlap, and SM parallelism remain unmodeled."
         if selection.get("cost_model") in ("v2", "v3") else
         "# Caveat: memory_unhidden is a traffic proxy; bandwidth, overlap, cache effects, and dynamic barrier frequency are not modeled."),
        "# Selection: lowest complete relative cost among proven plans; a modeled Roofline bound can resolve near-cost alternatives where available.",
        f"site={selection['site']} target={selection['target']} baseline={selection['baseline']} selected={selection['selected']}",
    ]
    if "joint" in selection:
        tile_lines.append("# Joint mode: TILE_CAND costs and the tile-only selection description above are diagnostics; physical-plan.log contains the authoritative complete costs and selection.")
    if selection.get("cost_model") == "v5":
        tile_lines[3:5] = [
            "# compute=max(1,floor(2*M*N*K/1024))+floor(smem_address_work/1024).",
            "# memory_unhidden=max(1,floor(worst_sector_bytes/1024))+floor(bank_conflict_work/32)+floor(A_read_issues/32)+floor(B_read_issues/32).",
            "# bank_conflict_work=sum of excess rounds for A/B staging stores and compute loads; identical-word loads are broadcasts.",
            "# smem_address_work=2*(B staging warp issues+B compute warp issues) for XOR columns; otherwise zero.",
            "# A/B allocation bytes describe separate static __shared__ arrays, single stage; accumulator bytes remain per thread.",
            "# b_smem_layout=xor_columns maps col to col^(col/32) on both fills and reads; global layout is unchanged.",
            "# v5 uses scalar source warp issues, not PTX/SASS transactions or measured latency; bandwidth, caches, occupancy and overlap remain unmodeled.",
        ]
        tile_lines = [line for line in tile_lines if not line.startswith("# sync=")]
    if any("grid" in row for row in candidates):
        tile_lines.insert(-2, "# Grid: CTA counts and K iterations are traced, but SM count, occupancy, grid waves, and underfilled-device throughput are not priced.")
    if candidates[0].get("roof_bound_ns") is not None:
        target = selection["roofline_target"]
        tile_lines.insert(-1, f"# Roofline target: {target['name']} FP32={target['fp32']} GFLOP/s HBM={target['hbm']} GB/s (published peak, uncalibrated).")
        tile_lines.insert(-1, "# Roofline: scalar-FMA FP32 published peak where available; global requests stand in for cold HBM bytes, not observed HBM traffic.")
        tile_lines.insert(-1, "# roof_compute_ns=ceil(FLOPs/FP32_GFLOP_s); roof_memory_ns=ceil(global_bytes/HBM_GB_s); bound=max(compute,memory).")
        tile_lines.insert(-1, "# intensity=FLOPs/global_bytes; required_HBM_GB_s=ceil(FP32_GFLOP_s/intensity); roof_limit is the larger bound term.")
        tile_lines.insert(-1, "# Decision aid: within floor(best_relative_cost/33), prefer a bound at least floor(current_bound/20) lower; this is an optimistic heuristic, not predicted latency.")
    for row in candidates:
        decision = ("selected" if row["plan"] == selection["selected"] else
                    "baseline_reference" if row["plan"] == selection["baseline"] else
                    "rejected_legality" if row["legality"] != "proven" else
                    "rejected_incomplete_cost" if not row["complete"] else
                    "not_selected_cost")
        tile_lines.append(
            f"candidate={row['candidate']} plan={row['plan']} tile={row['tile']} "
            f"family={row['family']} decision={decision} "
            f"legality={row['legality']} reason={row['reason']} "
            f"cost={row['total']} unit={row['unit']} confidence={row['confidence']} "
            f"compute={row['compute']} memory_unhidden={row['memory']} "
            f"communication_unhidden={row['communication']} sync={row['sync']} "
            f"launch={row['launch']} runtime_selection={row['runtime']} "
            f"shared_bytes={row['shared']} register_bytes_per_thread={row['registers']} "
            f"threads={row['threads']} cta={','.join(map(str, row['cta']))} "
            f"source_line={row['source_line'] if row['source_line'] is not None else 'unknown'}")
        if "grid" in row:
            grid = row["grid"]
            tile_lines[-1] += (f" problem={','.join(map(str, grid['problem']))}"
                               f" grid={','.join(map(str, grid['grid']))}"
                               f" grid_ctas={grid['ctas']} k_iterations={grid['k_iterations']}")
            if selection.get("cost_model") in ("v2", "v3", "v4", "v5") and row["family"] != "baseline":
                m, n, k = grid["problem"]
                grid_m, grid_n = grid["grid"]
                tile_lines[-1] += (f" valid_a_load_bytes={m * k * grid_n * 4}"
                                   f" valid_b_load_bytes={k * n * grid_m * 4}")
            if "layout" in row:
                layout = row["layout"]
                tile_lines[-1] += (f" gmem_copy_map={layout['copy_map']}"
                                   f" a_smem_layout={layout['a_layout']}"
                                   f" a_stride={layout['a_stride']}"
                                   f" bank_conflict_work={layout['bank_work']}")
            if "gmem" in row:
                traffic = row["gmem"]
                tile_lines[-1] += (f" gmem_sector_bytes={traffic['sector']}"
                                   f" a_sector_bytes={traffic['a']}"
                                   f" b_sector_bytes={traffic['b']}"
                                   f" c_sector_bytes={traffic['c']}"
                                   f" aligned_sector_bytes={traffic['aligned']}"
                                   f" smem_a_load_issues={traffic['a_issues']}"
                                   f" cost_basis=worst_base_alignment")
            if "smem" in row:
                record = row["smem"]
                tile_lines[-1] += " " + " ".join(
                    f"smem_{key}={value}" for key, value in record.items()
                    if key not in ("site", "tile"))
        if "roof_bound_ns" in row:
            tile_lines[-1] += (f" flops={row['flops']} global_bytes={row['roof_bytes']} "
                               f"roof_compute_ns={row['roof_compute_ns']} "
                               f"roof_memory_ns={row['roof_memory_ns']} "
                               f"roof_bound_ns={row['roof_bound_ns']}")
            target = selection["roofline_target"]
            if target["name"] != "unavailable":
                milli_intensity = row["flops"] * 1000 // row["roof_bytes"]
                required_hbm = (target["fp32"] * row["roof_bytes"] + row["flops"] - 1) // row["flops"]
                limit = "hbm" if row["roof_memory_ns"] > row["roof_compute_ns"] else "compute"
                tile_lines[-1] += (f" intensity_flop_per_byte={milli_intensity // 1000}.{milli_intensity % 1000:03d}"
                                   f" required_hbm_gbps={required_hbm} roof_limit={limit}")
    tile_lines.append(
        f"selection baseline={selection['baseline']} chosen={selection['selected']} "
        f"legal={selection['legal']} complete_cost={selection['complete']} "
        f"rejected={selection['rejected']} selection_reason={selection['reason']}")
    if "k_loop" in selection:
        loop = selection["k_loop"]
        tile_lines.append(
            f"whirl_k_loop=DO_LOOP scope={loop['scope']} "
            f"plan={loop['plan']} tile={loop['tile']} "
            f"start={loop['start']} end={loop['end']} step={loop['step']} "
            + ("device_pu=separate_selected_image"
               if selection.get("whirl_kernel") else
               "device_pu=not_materialized"))
    if "whirl_kernel" in selection:
        kernel = selection["whirl_kernel"]
        tile_lines.append(
            f"device_pu=ai_matmul_tile_f32 plan={kernel['plan']} "
            f"tile={kernel['tile']} image={kernel['image']} "
            "state=materialized_sidecar target_lowered=no")
    if "edge_plan" in selection:
        edge = selection["edge_plan"]
        tile_lines.append(
            f"edge_plan={edge['policy']} a_load={edge['a_load']} "
            f"b_load={edge['b_load']} c_store={edge['c_store']} "
            f"{edge['barriers']}")
    chosen = next(row for row in candidates if row["plan"] == selection["selected"])
    if "grid" in chosen:
        selection["selected_grid_ctas"] = chosen["grid"]["ctas"]
        tile_lines.append(
            f"selection_grid_ctas={chosen['grid']['ctas']} "
            f"grid_parallelism_cost={'assumed_wave_model' if selection.get('joint', {}).get('mode', {}).get('model') == 4 else 'unmodeled'} "
            "selection_confidence=requires_native_calibration")
    tile_lines.append(
        f"action source={action['emission']} tile_whirl_rewrite={action['tile_rewrite']} "
        f"whirl_lowering={action['lower']} lowered_nodes={action['lowered_count']}")
    tile_content = "\n".join(tile_lines) + "\n"
    fetch_content = ""
    if "fetch" in selection:
        fetch = selection["fetch"]
        fetch_lines = [
            "# What it summarizes: P8 movement candidates, selected pipeline, stage allocation, modeled overlap, and emitted source action.",
            "# Cost model: low-confidence relative movement score; hidden movement is a planning estimate, not measured latency.",
            "# Selection: lowest complete relative movement cost among proven plans; TMA source is emitted only for a selected multidimensional-async plan.",
        ]
        if "joint" in selection:
            fetch_lines.append("# Joint mode: raw/hidden/unhidden below are legacy per-stage diagnostics, not the selection score. Whole-K costs, bounded overlap, resource pressure, setup, and retained synchronous fallback are in physical-plan.log.")
        for row in fetch["candidates"]:
            fetch_lines.append(
                f"candidate={row['plan']} optimization_plan={row['optimization_plan']} "
                f"engine={row['engine']} legality={row['legality']} reason={row['reason']} "
                f"kind={row.get('kind', 'not_recorded')} "
                f"stages={row['stages']} distance={row['distance']} "
                f"buffer_bytes={row['buffering']} raw={row['raw']} "
                f"hidden={row['hidden']} unhidden={row['unhidden']}")
        fetch_lines.append(
            f"selection baseline={fetch['selection']['baseline']} "
            f"chosen={fetch['selection']['selected']} "
            f"engine={fetch['selection']['engine']} "
            f"selection_reason={'lowest_complete_joint_cost' if 'joint' in selection else 'lowest_complete_relative_cost'}")
        fetch_lines.append(
            f"action source={fetch['action']['source']} "
            f"movement={fetch['action']['movement']} "
            f"{fetch['action']['rewrite']}")
        if "pipeline_kernel" in selection:
            pipeline = selection["pipeline_kernel"]
            if pipeline["abi"] == "direct_ptr_v1":
                fetch_lines.extend((
                    "# Direct order: guarded GMEM A load -> register reuse across j -> guarded B load and accumulation -> guarded C store.",
                    "# No A/B SMEM allocation, staged buffer slot, async movement, overlap credit or CTA barrier exists; stages=1 denotes the logical demand step only.",
                    "# Proof is structural WHIRL/source review plus host numerical simulation; native GPU correctness remains unverified.",
                    "executable " + " ".join(f"{key}={value}" for key, value in pipeline.items()),
                ))
            else:
                fetch_lines.extend((
                "# Order: initially free slot -> acquire/arm bytes -> issue A/B -> acquire-wait -> compute -> uniform CTA release -> guarded refill; drain before invalidation.",
                "# Two-slot identity: slot=kt%2, phase=(kt//2)%2; refill tile=kt+2 only after release. Non-TMA uses one synchronous slot and CTA publication/release barriers.",
                "# Proof is structural WHIRL plus slot induction; native GPU correctness and measured overlap remain unverified.",
                "executable " + " ".join(f"{key}={value}" for key, value in pipeline.items()),
                ))
        fetch_content = "\n".join(fetch_lines) + "\n"
    summary = (
        "# What it summarizes: Observed optimization-phase outcomes in this trace; untraced phases are not certified.\n"
        f"P7 tile {'rewritten' if applied else 'analyzed_no_rewrite'} candidates={len(candidates)} "
        f"selected={selection['selected']} selection_reason={selection['reason']} "
        + (f"roofline_bound_ns={next(row['roof_bound_ns'] for row in candidates if row['plan'] == selection['selected'])} "
           f"roofline_target={selection['roofline_target']['name']} "
           if 'roofline_target' in selection else "")
        + f"source={action['emission']} "
        f"tile_whirl_rewrite={action['tile_rewrite']} "
        + ("next=verify_generated_runtime_and_gpu\n" if applied else
           "next=apply_selected_tile_to_executable_whirl\n"))
    if "fetch" in selection:
        fetch = selection["fetch"]
        summary += (f"P8 fetch source_sidecar selected={fetch['selection']['selected']} "
                    f"engine={fetch['selection']['engine']} "
                    f"stages={next(row['stages'] for row in fetch['candidates'] if row['optimization_plan'] == fetch['selection']['selected'])} "
                    "whirl_rewrite=none runtime_verified=no\n")
    (reports / "phases").mkdir(parents=True, exist_ok=True)
    if selection.get("version") in (6, 7):
        render_coverage(trace, reports, selection, tile_content, fetch_content)
        return selection, action
    (reports / "phases/tile.log").write_text(tile_content, encoding="utf-8")
    if fetch_content:
        (reports / "phases/fetch-pipeline.log").write_text(fetch_content,
                                                             encoding="utf-8")
    (reports / "optimization.log").write_text(
        "# What it summarizes: Complete content of traced optimization phase logs in phase order.\n"
        + tile_content + fetch_content, encoding="utf-8")
    (reports / "optimization-summary.log").write_text(summary, encoding="utf-8")
    return selection, action


def render_program(trace: Path, reports: Path) -> None:
    """Render a multi-PU trace without conflating PU-local plan IDs."""
    lines = trace.read_text(encoding="utf-8").splitlines()
    bindings = [line.split() for line in lines
                if line.startswith("AI_OPT 7 DEVICE_BIND ")]
    pipelines = [line.split() for line in lines
                 if line.startswith("AI_OPT 7 PIPELINE_KERNEL ")]
    segments: list[list[str]] = []
    segment: list[str] = []
    for line in lines:
        if line.startswith(("AI_OPT 7 DEVICE_BIND ", "AI_OPT 7 PIPELINE_KERNEL ")):
            continue
        segment.append(line)
        if line.startswith("AI_OPT 7 PHASE matmul_f32_source 11 "):
            segments.append(segment)
            segment = []
    if (segment and any(line.startswith("AI_OPT ") for line in segment)) or \
            len(segments) < 2 or len(bindings) != len(segments):
        raise ValueError("incomplete multi-PU AI trace or device bindings")
    reports.mkdir(parents=True, exist_ok=True)
    digest = hashlib.sha256(trace.read_bytes()).hexdigest()
    summary = [
        "# What it summarizes: Per-PU phase outcomes, selected costs, host rewrites, and device entry bindings.",
        f"trace_sha256={digest} pu_count={len(segments)} schema=7",
    ]
    full = ["# What it summarizes: Complete optimization phase reports for every PU in program order.\n"]
    phase_content = [[] for _ in PHASE_LOGS]
    for ordinal, (segment_lines, fields) in enumerate(zip(segments, bindings)):
        context_lines = [line.split() for line in segment_lines
                         if line.startswith("AI_OPT 7 CONTEXT ")]
        if (len(fields) != 9 or fields[3] != str(ordinal) or
                len(context_lines) != 1 or fields[4] != context_lines[0][3]):
            raise ValueError("multi-PU trace binding order or owner mismatch")
        owner, plan, tile, entry, image = fields[4:]
        segment_lines.append(" ".join(fields))
        matching = [row for row in pipelines if row[3] == str(ordinal) and row[4] == owner]
        if pipelines and len(matching) != 1:
            raise ValueError("multi-PU pipeline proof owner mismatch")
        segment_lines.extend(" ".join(row) for row in matching)
        pu_trace = reports / f"pu-{ordinal}.t"
        pu_reports = reports / f"pu-{ordinal}"
        pu_trace.write_text("\n".join(segment_lines) + "\n", encoding="utf-8")
        selection, action = render(pu_trace, pu_reports)
        candidates, _, _ = parse(pu_trace.read_text(encoding="utf-8"))
        selected = next(row for row in candidates
                        if row["plan"] == selection["selected"])
        selected_cost = selection["joint"]["chosen"]["total"] if "joint" in selection else selected["total"]
        summary.append(
            f"pu={owner} ordinal={ordinal} plan={plan} tile={tile} "
            f"entry={entry} image={image} selected_cost={selected_cost} "
            f"cost_unit={selected['unit']} "
            f"selection_reason={selection['reason']} "
            f"transform={action['tile_rewrite']}")
        summary.extend(f"pu={owner} {line}" for line in
                       (pu_reports / "optimization-summary.log").read_text(
                           encoding="utf-8").splitlines()
                       if line and not line.startswith("#"))
        full.append(f"## PU {ordinal} {owner}\n" +
                    (pu_reports / "optimization.log").read_text(
                        encoding="utf-8"))
        for phase, (filename, _) in enumerate(PHASE_LOGS):
            phase_content[phase].append(
                f"pu={owner} ordinal={ordinal}\n" +
                (pu_reports / "phases" / filename).read_text(
                    encoding="utf-8"))
    (reports / "phases").mkdir(exist_ok=True)
    for phase, (filename, description) in enumerate(PHASE_LOGS):
        (reports / "phases" / filename).write_text(
            f"# What it summarizes: {description}\n" +
            "".join(phase_content[phase]), encoding="utf-8")
    (reports / "optimization.log").write_text(
        "".join(full), encoding="utf-8")
    (reports / "optimization-summary.log").write_text(
        "\n".join(summary) + "\n", encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace_file", nargs="?", type=Path)
    parser.add_argument("--trace", type=Path)
    parser.add_argument("--reports", type=Path)
    parser.add_argument("--phase", choices=[f"P{i}" for i in range(12)])
    args = parser.parse_args()
    trace = args.trace or args.trace_file
    if trace is None or (args.trace is not None and args.trace_file is not None):
        parser.error("provide exactly one trace file")
    reports = args.reports or trace.parent / "reports"
    if sum(line.startswith("AI_OPT 7 CONTEXT ") for line in
           trace.read_text(encoding="utf-8").splitlines()) > 1:
        render_program(trace, reports)
    else:
        render(trace, reports)
    if args.phase is not None:
        filename = PHASE_LOGS[int(args.phase[1:])][0]
        print((reports / "phases" / filename).read_text(encoding="utf-8"), end="")
    else:
        print((reports / "optimization-summary.log").read_text(encoding="utf-8"), end="")


if __name__ == "__main__":
    main()
