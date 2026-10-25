"""Validate AI optimization trace records and derive readable phase reports."""

from __future__ import annotations

import argparse
import hashlib
from pathlib import Path


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


def parse(trace: str) -> tuple[list[dict], dict, dict]:
    candidates: list[dict] = []
    grids: dict[int, dict] = {}
    k_loop: dict | None = None
    whirl_kernel: dict | None = None
    device_binding: dict | None = None
    selection: dict | None = None
    action: dict | None = None
    fetch_candidates: list[dict] = []
    fetch_selection: dict | None = None
    fetch_action: dict | None = None
    roof_target: dict | None = None
    context: dict | None = None
    phases: dict[int, dict] = {}
    semantic: list[dict] = []
    locality: list[dict] = []
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
        if kind == "CONTEXT" and trace_version in ("6", "7") and len(fields) == 5:
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
        elif kind == "TILE_SELECT" and len(fields) == (11 if trace_version in ("3", "4", "5", "6", "7") else 10):
            if selection is not None:
                raise ValueError("duplicate tile selection")
            reason = fields[-1] if trace_version in ("3", "4", "5", "6", "7") else "not_recorded"
            allowed = ("selection_reason=lowest_complete_relative_cost",
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
        elif kind == "FETCH_CAND" and trace_version in ("5", "6", "7") and len(fields) == 15:
            (site, plan, optimization_plan, engine, legality, reason,
             stages, distance, buffering, raw, hidden, unhidden) = fields[3:]
            row = dict(site=int(site), plan=int(plan),
                       optimization_plan=int(optimization_plan),
                       engine=engine, legality=legality, reason=reason,
                       stages=int(stages), distance=int(distance),
                       buffering=int(buffering), raw=int(raw),
                       hidden=int(hidden), unhidden=int(unhidden))
            if row["raw"] != row["hidden"] + row["unhidden"]:
                raise ValueError("fetch movement cost does not balance")
            fetch_candidates.append(row)
        elif kind == "FETCH_SELECT" and trace_version in ("5", "6", "7") and len(fields) == 8:
            if fetch_selection is not None:
                raise ValueError("duplicate fetch selection")
            site, baseline, selected, engine, reason = fields[3:]
            if reason != "selection_reason=lowest_complete_relative_cost":
                raise ValueError("unknown fetch selection reason")
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
            if fetch_candidates or fetch_selection or fetch_action or grids:
                raise ValueError("fetch decision without tile selection")
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
    if not eligible or selection["selected"] != expected["plan"]:
        raise ValueError("selection disagrees with comparable cost minimum")
    if trace_version in ("4", "5", "6", "7") and selection["reason"] != expected_reason:
        raise ValueError("selection reason disagrees with Roofline decision")
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
                not whirl_kernel["image"].endswith(".B") or
                fetch_candidates):
            raise ValueError("device WHIRL kernel disagrees with selected tile")
        selection["whirl_kernel"] = whirl_kernel
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
    if applied and (trace_version != "7" or fetch_candidates):
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
        eligible_fetch = [row for row in fetch_candidates if row["legality"] == "proven"]
        if (not eligible_fetch or chosen != min(eligible_fetch, key=lambda row:
                (row["unhidden"] + int(row["engine"] != "demand"),
                 row["optimization_plan"]))):
            raise ValueError("fetch selection disagrees with movement cost")
        if (chosen["engine"] != "tma_like" or chosen["stages"] != 2
                or chosen["distance"] != 1
                or chosen["buffering"] !=
                    by_plan[selection["selected"]]["shared"] * chosen["stages"]
                or fetch_selection["engine"] != chosen["engine"]
                or fetch_action != dict(site=fetch_selection["site"],
                                        selected=fetch_selection["selected"],
                                        source="source_sidecar", movement="tma_2d",
                                        rewrite="whirl_rewrite=none")):
            raise ValueError("P8 TMA action disagrees with selected plan")
        selection["fetch"] = dict(candidates=fetch_candidates,
                                  selection=fetch_selection,
                                  action=fetch_action)
    elif trace_version == "5":
        raise ValueError("missing P8 fetch decision or action")
    elif fetch_selection is not None or fetch_action is not None:
        raise ValueError("incomplete P8 fetch decision or action")
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
        if "fetch" in selection:
            if (phases[8]["status"] != "analyzed_no_rewrite"
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
               "grid_parallelism_cost=unmodeled "
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
        "# memory_unhidden=max(1,floor((read_bytes+write_bytes)/1024)); write_bytes=M*N*E.",
        "# read_bytes baseline=2*M*N*K*E.",
        "# read_bytes tiled=ceil(M/BM)*ceil(N/BN)*ceil(K/BK)*(BM*BK+BK*BN)*E.",
        "# total=compute+memory_unhidden+communication_unhidden+sync+launch+runtime_selection.",
        "# sync=static barrier_count (0 baseline, 2 tiled); communication_unhidden, launch, runtime_selection=0 here.",
        "# Caveat: memory_unhidden is a traffic proxy; bandwidth, overlap, cache effects, and dynamic barrier frequency are not modeled.",
        "# Selection: lowest complete relative cost among proven plans; a modeled Roofline bound can resolve near-cost alternatives where available.",
        f"site={selection['site']} target={selection['target']} baseline={selection['baseline']} selected={selection['selected']}",
    ]
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
    chosen = next(row for row in candidates if row["plan"] == selection["selected"])
    if "grid" in chosen:
        selection["selected_grid_ctas"] = chosen["grid"]["ctas"]
        tile_lines.append(
            f"selection_grid_ctas={chosen['grid']['ctas']} "
            "grid_parallelism_cost=unmodeled "
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
        for row in fetch["candidates"]:
            fetch_lines.append(
                f"candidate={row['plan']} optimization_plan={row['optimization_plan']} "
                f"engine={row['engine']} legality={row['legality']} reason={row['reason']} "
                f"stages={row['stages']} distance={row['distance']} "
                f"buffer_bytes={row['buffering']} raw={row['raw']} "
                f"hidden={row['hidden']} unhidden={row['unhidden']}")
        fetch_lines.append(
            f"selection baseline={fetch['selection']['baseline']} "
            f"chosen={fetch['selection']['selected']} "
            f"engine={fetch['selection']['engine']} "
            "selection_reason=lowest_complete_relative_cost")
        fetch_lines.append(
            f"action source={fetch['action']['source']} "
            f"movement={fetch['action']['movement']} "
            f"{fetch['action']['rewrite']}")
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
    segments: list[list[str]] = []
    segment: list[str] = []
    for line in lines:
        if line.startswith("AI_OPT 7 DEVICE_BIND "):
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
        pu_trace = reports / f"pu-{ordinal}.t"
        pu_reports = reports / f"pu-{ordinal}"
        pu_trace.write_text("\n".join(segment_lines) + "\n", encoding="utf-8")
        selection, action = render(pu_trace, pu_reports)
        candidates, _, _ = parse(pu_trace.read_text(encoding="utf-8"))
        selected = next(row for row in candidates
                        if row["plan"] == selection["selected"])
        summary.append(
            f"pu={owner} ordinal={ordinal} plan={plan} tile={tile} "
            f"entry={entry} image={image} selected_cost={selected['total']} "
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
