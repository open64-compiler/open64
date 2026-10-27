#!/usr/bin/env python3
"""Host research oracles and a serial analytical ledger, not compiler lowering.

Owning design: doc/AI-COMPILER-COMMON-EXECUTION-PLANNING.md and sibling README.
All timing/resource assumptions come from the synthetic JSON fixture. This
module neither reads production traces nor changes WHIRL or production policy.
"""

import argparse
from dataclasses import asdict, dataclass
import hashlib
import json
import math
from pathlib import Path


def ceil_div(n, d):
    """Round a nonnegative quantity upward; reject invalid denominators."""
    if n < 0 or d <= 0:
        raise ValueError("invalid nonnegative division")
    return (n + d - 1) // d


@dataclass(frozen=True)
class Phase:
    name: str
    read: int = 0
    write: int = 0
    alu: int = 0
    exp: int = 0
    div: int = 0
    rsqrt: int = 0
    shared_traffic: int = 0
    barriers: int = 0


@dataclass(frozen=True)
class Kernel:
    name: str
    ctas: int
    threads: int
    registers: int
    shared: int
    phases: tuple


@dataclass(frozen=True)
class Plan:
    case: str
    name: str
    kernels: tuple
    workspace: int = 0
    reassociates: bool = False


def plans():
    """Construct reviewed domain ledgers; the evaluator is operation-agnostic."""
    result = []
    for tile in (16, 32):
        area = tile * tile
        reads = 4 * (64 // 16) * (tile * 16 + 16 * tile)
        phase = Phase("k_loop_and_store", read=reads, write=4 * area,
                      alu=2 * area * 64, shared_traffic=reads + 8 * area * 64,
                      barriers=2 * (64 // 16))
        kernel = Kernel("gemm", (64 // tile) ** 2, 64,
                        20 + ceil_div(area, 64), 4 * (tile * 16 + 16 * tile),
                        (phase,))
        result.append(Plan("gemm", "tile%d" % tile, (kernel,)))
    n, threads = 8192, 128
    scratch = ceil_div(threads, 32) * 4
    def reduction_kernel(name, ctas, length):
        return Kernel(name, ctas, threads, 24, scratch,
                      (Phase("sum", read=4 * length, write=4, alu=length,
                             shared_traffic=2 * scratch, barriers=2),))
    result.append(Plan("reduction", "one_cta", (reduction_kernel("sum", 1, n),)))
    result.append(Plan("reduction", "split16",
                       (reduction_kernel("partials", 16, n // 16),
                        reduction_kernel("merge", 1, 16)), 16 * 4, True))
    n, rows = 256, 4
    whole = Phase("retained_row", read=4 * n, write=4 * n, alu=4 * n,
                  exp=n, div=1, shared_traffic=4 * scratch, barriers=4)
    stream = (Phase("max", read=4 * n, alu=n,
                    shared_traffic=2 * scratch, barriers=2),
              Phase("sum_exp", read=4 * n, alu=2 * n, exp=n,
                    shared_traffic=2 * scratch, barriers=2),
              Phase("normalize", read=4 * n, write=4 * n,
                    alu=2 * n, exp=n, div=1))
    result.append(Plan("softmax", "retain_row",
                       (Kernel("softmax", rows, threads,
                               16 + 2 * ceil_div(n, threads), scratch, (whole,)),)))
    result.append(Plan("softmax", "reread_row",
                       (Kernel("softmax", rows, threads, 18, scratch, stream),)))
    n = 2048
    centered = (Phase("mean", read=4 * n, alu=n, div=1,
                      shared_traffic=2 * scratch, barriers=2),
                Phase("centered_m2", read=4 * n, alu=3 * n, div=1,
                      shared_traffic=2 * scratch, barriers=2),
                Phase("normalize", read=4 * n, write=4 * n,
                      alu=2 * n, rsqrt=1))
    merges = threads - 1
    stable = (Phase("welford_partials_merge", read=4 * n,
                    alu=5 * n + 10 * merges, div=n + 2 * merges + 1,
                    shared_traffic=6 * scratch, barriers=2),
              Phase("normalize", read=4 * n, write=4 * n,
                    alu=2 * n, rsqrt=1))
    result.append(Plan("normalization", "centered",
                       (Kernel("norm", rows, threads, 24, scratch, centered),)))
    result.append(Plan("normalization", "welford",
                       (Kernel("norm", rows, threads, 32, 3 * scratch, stable),),
                       reassociates=True))
    return result


def evaluate(plan, profile, allow_algebra=True):
    """Evaluate fixed-reservation, serial residency rounds; no overlap credit."""
    for key, value in profile.items():
        if isinstance(value, int) and value <= 0:
            raise ValueError("profile quantities must be positive: " + key)
    if plan.reassociates and not allow_algebra:
        return {"legal": False, "reason": "numerical_permission_required"}
    totals = dict(compute=0, memory=0, synchronization=0, launch=0)
    kernels = []
    for kernel in plan.kernels:
        if min(kernel.ctas, kernel.threads, kernel.registers) <= 0 or kernel.shared < 0:
            raise ValueError("invalid kernel geometry")
        if kernel.registers > profile["registers_per_thread_limit"]:
            return {"legal": False, "reason": "register_limit"}
        qreg = profile["register_quantum_per_cta"]
        regs = ceil_div(kernel.threads * kernel.registers, qreg) * qreg
        qsmem = profile["shared_quantum"]
        shared = ceil_div(kernel.shared, qsmem) * qsmem
        limits = {
            "blocks": profile["ctas_per_sm"],
            "threads": profile["threads_per_sm"] // kernel.threads,
            "warps": profile["warps_per_sm"] // ceil_div(kernel.threads, profile["warp_lanes"]),
            "registers": profile["registers_per_sm"] // regs,
            "shared": profile["shared_bytes_per_sm"] // shared if shared else profile["ctas_per_sm"],
        }
        resident = min(limits.values())
        if resident == 0:
            return {"legal": False, "reason": "zero_residency"}
        rounds = []
        slots = profile["sms"] * resident
        for start in range(0, kernel.ctas, slots):
            active = min(slots, kernel.ctas - start)
            active_sms = min(active, profile["sms"])
            q = ceil_div(active, active_sms)
            bandwidth = min(profile["global_bytes_per_tick"],
                            active_sms * profile["global_bytes_per_sm_tick"])
            parts = dict(compute=0, memory=0, synchronization=0)
            for phase in kernel.phases:
                if any(value < 0 for key, value in asdict(phase).items() if key != "name"):
                    raise ValueError("negative phase demand")
                for field, rate in (("alu", "alu_ops_per_sm_tick"),
                                    ("exp", "exp_ops_per_sm_tick"),
                                    ("div", "div_ops_per_sm_tick"),
                                    ("rsqrt", "rsqrt_ops_per_sm_tick")):
                    parts["compute"] += ceil_div(q * getattr(phase, field), profile[rate])
                for amount in (phase.read, phase.write):
                    if amount:
                        parts["memory"] += profile["global_startup_ticks"] + ceil_div(active * amount, bandwidth)
                parts["memory"] += ceil_div(q * phase.shared_traffic, profile["shared_bytes_per_sm_tick"])
                parts["synchronization"] += q * phase.barriers * profile["barrier_ticks"]
            rounds.append(dict(active=active, q=q, active_sms=active_sms, **parts))
            for key, value in parts.items():
                totals[key] += value
        totals["launch"] += profile["launch_ticks"]
        kernels.append(dict(name=kernel.name, resident=resident, limits=limits,
                            registers=regs, shared=shared, rounds=rounds))
    return dict(legal=True, reason="research_contract", components=totals,
                total=sum(totals.values()), kernels=kernels,
                workspace_bytes=plan.workspace)


def reachable(edges, source, target):
    """Check a completion-to-next-use proof; never infer it from timestamps."""
    seen, pending = set(), list(edges.get(source, ()))
    while pending:
        node = pending.pop()
        if node == target:
            return True
        if node not in seen:
            seen.add(node)
            pending.extend(edges.get(node, ()))
    return False


def reuse_allowed(edges, completions, next_write):
    """Require every prior access-completion obligation, not just one consumer."""
    nodes = set(edges) | {node for values in edges.values() for node in values}
    active, done = set(), set()
    def visit(node):
        if node in active:
            raise ValueError("cyclic event dependency")
        if node not in done:
            active.add(node)
            for successor in edges.get(node, ()):
                visit(successor)
            active.remove(node)
            done.add(node)
    for node in sorted(nodes):
        visit(node)
    if next_write not in nodes or any(end not in nodes for end in completions):
        return False
    return bool(completions) and all(reachable(edges, end, next_write) for end in completions)


def stable_softmax(row, retain):
    """Two lawful finite-input realizations; retained vs recomputed exponentials."""
    maximum = max(row)
    terms = [math.exp(x - maximum) for x in row] if retain else None
    denominator = sum(terms) if retain else sum(math.exp(x - maximum) for x in row)
    return [(terms[i] if retain else math.exp(x - maximum)) / denominator
            for i, x in enumerate(row)]


def merge_statistics(left, right):
    """Merge disjoint (count, mean, M2) states, handling empty partitions."""
    na, ma, sa = left
    nb, mb, sb = right
    if not na:
        return right
    if not nb:
        return left
    count = na + nb
    delta = mb - ma
    return count, ma + delta * nb / count, sa + sb + delta * delta * na * nb / count


def normalize(row, use_welford):
    """Compare population variance plus epsilon, with no gamma/beta epilogue."""
    if use_welford:
        states = []
        for lane in range(128):
            mean, m2, count = 0.0, 0.0, 0
            for x in row[lane::128]:
                count += 1
                delta = x - mean
                mean += delta / count
                m2 += delta * (x - mean)
            states.append((count, mean, m2))
        while len(states) > 1:
            states = [merge_statistics(states[i], states[i + 1])
                      for i in range(0, len(states), 2)]
        _, mean, m2 = states[0]
    else:
        mean = math.fsum(row) / len(row)
        m2 = math.fsum((x - mean) ** 2 for x in row)
    inverse = 1.0 / math.sqrt(m2 / len(row) + 1e-5)
    return [(x - mean) * inverse for x in row]


def numerical_checks():
    """Execute FP64 host examples; no native FP32, WHIRL or timing certificate."""
    a = [[((i * 3 + k) % 17 - 8) / 8 for k in range(64)] for i in range(64)]
    b = [[((k + j * 5) % 19 - 9) / 8 for j in range(64)] for k in range(64)]
    reference = [[math.fsum(a[i][k] * b[k][j] for k in range(64))
                  for j in range(64)] for i in range(64)]
    errors = {}
    for tile in (16, 32):
        out = [[0.0] * 64 for _ in range(64)]
        for bi in range(0, 64, tile):
            for bj in range(0, 64, tile):
                for bk in range(0, 64, 16):
                    for i in range(bi, bi + tile):
                        for j in range(bj, bj + tile):
                            for k in range(bk, bk + 16):
                                out[i][j] += a[i][k] * b[k][j]
        errors["gemm_tile%d" % tile] = max(abs(out[i][j] - reference[i][j])
                                          for i in range(64) for j in range(64))
    row = [(i % 31 - 15) / 8 for i in range(8192)]
    errors["reduction_one_cta"] = abs(sum(row) - math.fsum(row))
    errors["reduction_split16"] = abs(sum(sum(row[i:i + 512]) for i in range(0, 8192, 512)) - math.fsum(row))
    for label, row in (("shifted", [10000.0 + (i % 23 - 11) / 8 for i in range(256)]),
                       ("constant", [3.0] * 256)):
        x, y = stable_softmax(row, True), stable_softmax(row, False)
        errors["softmax_" + label] = max(abs(u - v) for u, v in zip(x, y))
        if abs(math.fsum(x) - 1.0) > 1e-12 or any(not math.isfinite(v) for v in x + y):
            raise AssertionError("softmax reference failed")
    for label, row in (("ordinary", [(i % 29 - 14) / 8 for i in range(2048)]),
                       ("offset", [1e6 + (i % 17) * 1e-3 for i in range(2048)]),
                       ("constant", [3.0] * 2048)):
        x, y = normalize(row, False), normalize(row, True)
        errors["normalization_" + label] = max(abs(u - v) for u, v in zip(x, y))
    if any(not math.isfinite(v) or v > 1e-5 for v in errors.values()):
        raise AssertionError(errors)
    return errors


def run(profile):
    """Evaluate every fixture candidate in every declared synthetic scenario."""
    records, decisions = [], []
    for scenario, overrides in profile["scenarios"].items():
        resolved = dict(profile, **overrides)
        for plan in plans():
            records.append(dict(scenario=scenario, case=plan.case, plan=plan.name,
                                input=asdict(plan), result=evaluate(plan, resolved)))
        for case in sorted({p.case for p in plans()}):
            candidates = [r for r in records if r["scenario"] == scenario and
                          r["case"] == case and r["result"]["legal"]]
            winner = min(candidates, key=lambda r: (r["result"]["total"], r["plan"]))
            decisions.append(dict(scenario=scenario, case=case, winner=winner["plan"],
                                  total=winner["result"]["total"], reason="lowest_complete_serial_model_cost"))
    return dict(schema=1, evidence="synthetic_analytical_not_GPU", unit=profile["unit"],
                profile=profile, candidates=records, decisions=decisions,
                numerical_errors=numerical_checks())


def main():
    """Publish research-only reports to a fresh directory; never overwrite."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    profile_path = Path(__file__).with_name("synthetic_profile.json")
    result = run(json.loads(profile_path.read_text()))
    args.output.mkdir(parents=True, exist_ok=False)
    (args.output / "results.json").write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
    lines = ["# What it summarizes: four-case synthetic complete serial-model costs; not compiler traces or GPU timings.",
             "# scenario case plan compute memory sync launch total shared_bytes workspace_bytes"]
    for row in result["candidates"]:
        cost = row["result"]
        c = cost["components"]
        lines.append("%s %s %s %d %d %d %d %d %d %d" % (
            row["scenario"], row["case"], row["plan"], c["compute"], c["memory"],
            c["synchronization"], c["launch"], cost["total"],
            max(k["shared"] for k in cost["kernels"]), cost["workspace_bytes"]))
    lines.append("# decisions: scenario case winner total reason")
    lines.extend("{scenario} {case} {winner} {total} {reason}".format(**d) for d in result["decisions"])
    (args.output / "comparison.log").write_text("\n".join(lines) + "\n")
    hashes = {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
              for p in (Path(__file__), profile_path)}
    (args.output / "inputs.json").write_text(json.dumps(hashes, indent=2, sort_keys=True) + "\n")
    print("PASS: %d candidates, %d decisions, %d numerical checks" % (
        len(result["candidates"]), len(result["decisions"]), len(result["numerical_errors"])))


if __name__ == "__main__":
    main()
