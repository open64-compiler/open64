"""Read-only source-warp GEMM timing experiment; never select or rewrite code.

Design: doc/AI-COMPILER-OPTIMIZATION-BUBBLE-MODEL.md, implemented replay slice.
Consumes the independently audited model-3 census. Timing, scalar instruction
expansion, no-spill register storage and one CTA/SM are explicit assumptions,
not a native NVIDIA simulator. No compiler/backend dependency is introduced.
"""

from __future__ import annotations

import argparse
from collections import Counter, defaultdict
from dataclasses import asdict, dataclass
import gzip
import hashlib
import heapq
import json
from pathlib import Path

from ai_opt_report import parse, render


MODEL = "source_warp_v1"
LIMIT = (1 << 63) - 1


class Unknown(ValueError):
    """Incomplete modeling evidence; no partial candidate ranking is valid."""


@dataclass(frozen=True)
class Profile:
    """Synthetic reference-cycle assumptions, not a calibrated GPU profile."""

    name: str = "synthetic-memory-100"
    provenance: str = "synthetic"
    revision: int = 1
    sms: int = 4
    schedulers: int = 4
    memory_latency: int = 100
    memory_bytes_per_cycle: int = 32
    memory_queue: int = 16
    shared_latency: int = 4
    arithmetic_latency: int = 4
    barrier_latency: int = 8
    address_ops: int = 2
    base_residue_words: int = 0

    def validate(self):
        """Reject missing, noninteger or misleading profile inputs."""
        if self.provenance != "synthetic" or not self.name.startswith("synthetic-"):
            raise ValueError("only explicitly synthetic profiles are supported")
        for key, value in asdict(self).items():
            if key in ("name", "provenance"):
                continue
            if type(value) is not int or not 0 <= value <= LIMIT:
                raise ValueError("invalid profile field: " + key)
        if not (0 < self.sms <= 256 and 0 < self.schedulers <= 32 and
                0 < self.memory_queue <= 1024 and self.revision == 1 and
                self.memory_bytes_per_cycle and self.arithmetic_latency and
                0 <= self.base_residue_words < 8):
            raise ValueError("invalid profile capacity or revision")


@dataclass
class Event:
    """Dependencies distinguish issue ordering from result completion."""

    actor: str
    op: str
    deps: tuple
    demands: tuple
    latency: int
    sm: int


class Graph:
    """Bounded append-only event DAG; IDs are stable within one candidate."""

    def __init__(self, budget=1000000):
        self.events = []
        self.last = {}
        self.budget = budget

    def add(self, actor, op, deps=(), demands=(), latency=0, sm=-1):
        """Append an operation, preserving actor issue order, not false data edges."""
        if len(self.events) >= self.budget:
            raise Unknown("event_budget")
        edges = set(deps)
        if actor in self.last:
            edges.add((self.last[actor], "issue"))
        ident = len(self.events)
        if any(type(i) is not int or not 0 <= i < ident or phase not in ("issue", "done")
               for i, phase in edges):
            raise ValueError("invalid event dependency")
        if type(latency) is not int or not 0 <= latency <= LIMIT:
            raise ValueError("invalid latency")
        if any(type(duration) is not int or not 0 < duration <= LIMIT or
               type(capacity) is not int or not 0 < capacity <= 1024
               for _, duration, capacity in demands):
            raise ValueError("invalid resource demand")
        if len({name for name, _, _ in demands}) != len(demands):
            raise ValueError("duplicate resource demand")
        self.events.append(Event(actor, op, tuple(sorted(edges)), tuple(demands), latency, sm))
        self.last[actor] = ident
        return ident


def simulate(graph):
    """Schedule oldest-ready events with finite resource calendars and queues.

    Input actor order is in-order issue with independent results in flight.
    Resource service is reserved once; queue entries live until completion.
    The deterministic policy is a hypothesis, not NVIDIA scheduler emulation.
    """
    events = graph.events
    successors = defaultdict(list)
    remaining = []
    ready = []
    notifications = []
    pools = {}
    stamps = {}
    timeline = []
    compute_busy = defaultdict(int)
    windows = {}
    waits = Counter()
    actor_previous = {}
    for ident, event in enumerate(events):
        remaining.append(len(event.deps))
        for dep in event.deps:
            successors[dep].append(ident)
        if not event.deps:
            heapq.heappush(ready, (0, ident))
        for name, _, capacity in event.demands:
            if name in pools and len(pools[name]) != capacity:
                raise ValueError("inconsistent resource capacity")
            pools.setdefault(name, [0] * capacity)
    while ready or notifications:
        t = min(ready[0][0] if ready else LIMIT,
                notifications[0][0] if notifications else LIMIT)
        while notifications and notifications[0][0] <= t:
            when, ident, phase = heapq.heappop(notifications)
            stamps[(ident, phase)] = when
            for child in successors[(ident, phase)]:
                remaining[child] -= 1
                if remaining[child] == 0:
                    heapq.heappush(ready, (when, child))
        while ready and ready[0][0] <= t:
            _, ident = heapq.heappop(ready)
            event = events[ident]
            earliest = max([t] + [pools[name][0] for name, _, _ in event.demands])
            if earliest > t:
                heapq.heappush(ready, (earliest, ident))
                continue
            finish = t + event.latency
            if finish > LIMIT or t + 1 > LIMIT:
                raise Unknown("cycle_overflow")
            for name, duration, _ in event.demands:
                if t + duration > LIMIT:
                    raise Unknown("cycle_overflow")
                heapq.heapreplace(pools[name], t + duration)
                if name.startswith("f32:"):
                    compute_busy[event.sm] += duration
            # Actors do not wait before admission/activation. In particular,
            # a virtual output join is not a resident warp stalled on stores.
            previous = actor_previous.get(event.actor, t)
            data_ready = max([previous] + [stamps[d] for d in event.deps])
            if data_ready > previous:
                blocking = max(event.deps, key=lambda d: (stamps[d], d))
                waits[events[blocking[0]].op] += data_ready - previous
            waits["issue_resource"] += max(0, t - data_ready)
            actor_previous[event.actor] = t + 1
            timeline.append((ident, t, finish))
            heapq.heappush(notifications, (t + 1, ident, "issue"))
            heapq.heappush(notifications, (finish, ident, "done"))
            if event.sm >= 0:
                first, last = windows.get(event.sm, (t, 0))
                windows[event.sm] = (min(first, t), max(last, finish))
    if len(timeline) != len(events):
        raise Unknown("deadlock")
    completion = max((end for _, _, end in timeline), default=0)
    idle = sum(end - start - compute_busy[sm] for sm, (start, end) in windows.items())
    return dict(completion_cycles=completion, compute_issue_idle_cycles=idle,
                compute_issue_busy_cycles=sum(compute_busy.values()),
                actor_wait_cycles=sum(waits.values()), wait_causes=dict(sorted(waits.items())),
                timeline=timeline, event_count=len(events))


def audit_timeline(graph, result):
    """Independently check token precedence and interval capacities after scheduling."""
    timeline = result["timeline"]
    if sorted(i for i, _, _ in timeline) != list(range(len(graph.events))):
        raise ValueError("timeline coverage")
    starts = {i: start for i, start, _ in timeline}
    ends = {i: end for i, _, end in timeline}
    use = defaultdict(list)
    capacities = {}
    busy = 0
    windows = {}
    for i, event in enumerate(graph.events):
        start, end = starts[i], ends[i]
        if start < 0 or end != start + event.latency:
            raise ValueError("timeline latency")
        for pred, phase in event.deps:
            if not 0 <= pred < i or phase not in ("issue", "done"):
                raise ValueError("timeline dependency identity")
            if start < (starts[pred] + 1 if phase == "issue" else ends[pred]):
                raise ValueError("timeline dependency")
        for name, duration, capacity in event.demands:
            if duration <= 0 or capacity <= 0 or (name in capacities and capacities[name] != capacity):
                raise ValueError("timeline resource specification")
            capacities[name] = capacity
            use[name].extend(((start, 1), (start + duration, -1)))
            if name.startswith("f32:"):
                busy += duration
        if event.sm >= 0:
            first, last = windows.get(event.sm, (start, 0))
            windows[event.sm] = (min(first, start), max(last, end))
    for name, changes in use.items():
        level = 0
        for _, delta in sorted(changes):
            level += delta
            if not 0 <= level <= capacities[name]:
                raise ValueError("timeline resource capacity")
    if (result["completion_cycles"] != max(ends.values(), default=0) or
            result["compute_issue_busy_cycles"] != busy or
            result["compute_issue_idle_cycles"] != sum(b - a for a, b in windows.values()) - busy):
        raise ValueError("timeline totals")
    waits = Counter()
    previous = {}
    for i, start, _ in sorted(timeline, key=lambda row: (row[1], row[0])):
        event = graph.events[i]
        prior = previous.get(event.actor, start)
        readiness = max([prior] + [starts[d] + 1 if phase == "issue" else ends[d]
                                   for d, phase in event.deps])
        if readiness > prior:
            blocker = max(event.deps, key=lambda dep: (
                starts[dep[0]] + 1 if dep[1] == "issue" else ends[dep[0]], dep))
            waits[graph.events[blocker[0]].op] += readiness - prior
        waits["issue_resource"] += max(0, start - readiness)
        previous[event.actor] = start + 1
    if result["wait_causes"] != dict(sorted(waits.items())) or result["actor_wait_cycles"] != sum(waits.values()):
        raise ValueError("timeline actor waits")


class Recipe:
    """Expand the reviewed scalar loop structure, without emitting or mutating WHIRL."""

    def __init__(self, tile, row, profile, budget):
        self.tile, self.row, self.p = tile, row, profile
        self.g = Graph(budget)
        self.cta_end = {}

    def op(self, actor, sm, kind, deps=(), words=(), bank_words=None):
        """Translate source work into the profile's explicit scalar issue hypothesis."""
        p = self.p
        scheduler = ("scheduler:%d:%d" % (sm, int(actor.split(":")[-1]) % p.schedulers), 1, 1)
        demands = [scheduler]
        latency = 1
        if kind in ("load", "store", "tma"):
            size = len({(w + p.base_residue_words) // 8 for w in words}) * 32
            if kind == "tma":
                size = len(words) * 4
            service = max(1, (size + p.memory_bytes_per_cycle - 1) // p.memory_bytes_per_cycle)
            latency = p.memory_latency + service
            demands += [("memory_service", service, 1), ("memory_queue", latency, p.memory_queue)]
        elif kind in ("shared_load", "shared_store"):
            banks = defaultdict(set)
            for word in bank_words or ():
                banks[word % 32].add(word)
            service = max(map(len, banks.values()), default=1)
            latency = p.shared_latency + service - 1
            demands += [("smem:%d" % sm, service, 1)]
        elif kind in ("mul", "add"):
            latency = p.arithmetic_latency
            demands += [("f32:%d" % sm, 1, 1)]
        elif kind == "barrier":
            latency = p.barrier_latency
        if kind in ("load", "store", "tma", "shared_load", "shared_store"):
            for _ in range(p.address_ops):
                self.g.add(actor, "address", demands=(scheduler,), latency=1, sm=sm)
        return self.g.add(actor, kind, deps, demands, latency, sm)

    def build(self):
        """Preserve real staging barriers, T2 two-slot preload and post-consume refill."""
        t, row, p = self.tile, self.row, self.p
        m, n, k = t["grid"]["problem"]
        bm, bn, bk = t["cta"]
        threads, columns = t["threads"], t["layout"]["thread_columns"]
        tm, tn = bm * columns // threads, bn // columns
        warps = threads // 32
        kind = row["kind"]
        if kind not in ("D", "S1", "T2"):
            raise Unknown("unsupported_movement")
        if threads % 32 or min(tm, tn, bk) <= 0:
            raise Unknown("unsupported_geometry")
        for cta in range(t["grid"]["ctas"]):
            sm = cta % p.sms
            cm, cn = divmod(cta, t["grid"]["grid"][1])
            r0, c0 = cm * bm, cn * bn
            actors = ["%d:%d" % (cta, w) for w in range(warps)]
            start_deps = [(self.cta_end[sm], "done")] if sm in self.cta_end else []
            starts = [self.op(a, sm, "control", start_deps) for a in actors]
            acc = {}
            gates = starts
            slots = {}

            def barrier(deps):
                """All warp arrivals precede each warp's modeled barrier completion."""
                return [self.op(a, sm, "barrier", [(x, "done") for x in deps]) for a in actors]

            def tma_load(step, release):
                """One issuing thread's warp launches A then B; no producer warp invented."""
                base = step * bk
                a = [((r0 + r) * k + base + q) for r in range(bm) for q in range(bk)]
                b = [((base + q) * n + c0 + c) for q in range(bk) for c in range(bn)]
                acquire = self.op(actors[0], sm, "control", [(x, "done") for x in release])
                ea = self.op(actors[0], sm, "tma", [(acquire, "done")], a)
                eb = self.op(actors[0], sm, "tma", [(acquire, "done")], b)
                fills = []
                for transfer, words in ((ea, a), (eb, b)):
                    service = max(1, (len(words) + 31) // 32)
                    fills.append(self.g.add("tma-fill-%d" % cta, "tma_fill",
                        [(transfer, "done")], (("smem:%d" % sm, service, 1),), service, sm))
                return fills

            if kind == "T2":
                gates = barrier(starts)
                for step in range(min(2, t["grid"]["k_iterations"])):
                    slots[step] = tma_load(step, gates)
            for step in range(t["grid"]["k_iterations"]):
                base = step * bk
                if kind == "S1":
                    copies = []
                    for w, actor in enumerate(actors):
                        for operand, count, width in ((0, bm * bk, bk), (1, bk * bn, bn)):
                            for offset in range(0, count, threads):
                                physical, global_words = [], []
                                for tid in range(w * 32, (w + 1) * 32):
                                    index = offset + tid
                                    if index >= count:
                                        continue
                                    rr, cc = divmod(index, width)
                                    valid = (r0 + rr < m and base + cc < k) if operand == 0 else (base + rr < k and c0 + cc < n)
                                    if valid:
                                        global_words.append((r0 + rr) * k + base + cc if operand == 0 else (base + rr) * n + c0 + cc)
                                    physical.append(self.shared_word(operand, rr, cc))
                                deps = [(gates[w], "done")]
                                if global_words:
                                    load = self.op(actor, sm, "load", deps, global_words)
                                    deps = [(load, "done")]
                                copies.append(self.op(actor, sm, "shared_store", deps, bank_words=physical))
                    gates = barrier(copies)
                elif kind == "T2":
                    gates = [self.op(a, sm, "wait", [(x, "done") for x in slots[step]]) for a in actors]
                lasts = []
                for w, actor in enumerate(actors):
                    lanes = [((tid // columns) * tm, (tid % columns) * tn) for tid in range(w * 32, (w + 1) * 32)]
                    for q in range(bk if kind != "D" else min(bk, k - base)):
                        self.op(actor, sm, "control", [(gates[w], "done")])
                        for i in range(tm):
                            a_words = [(r0 + r + i) * k + base + q for r, _ in lanes if r0 + r + i < m]
                            if kind == "D" and not a_words:
                                continue
                            av = self.op(actor, sm, "load" if kind == "D" else "shared_load", [(gates[w], "done")], a_words,
                                         [self.shared_word(0, r + i, q) for r, _ in lanes])
                            for j in range(tn):
                                b_words = [(base + q) * n + c0 + c + j for r, c in lanes if r0 + r + i < m and c0 + c + j < n]
                                if kind == "D" and not b_words:
                                    continue
                                bv = self.op(actor, sm, "load" if kind == "D" else "shared_load", words=b_words,
                                             bank_words=[self.shared_word(1, q, c + j) for _, c in lanes])
                                mul = self.op(actor, sm, "mul", [(av, "done"), (bv, "done")])
                                deps = [(mul, "done")]
                                if (w, i, j) in acc:
                                    deps.append((acc[w, i, j], "done"))
                                acc[w, i, j] = self.op(actor, sm, "add", deps)
                    # Final accumulator versions transitively cover every earlier
                    # update; preserve all accumulators, not just the final issue.
                    lasts.extend([value for (owner, _, _), value in acc.items()
                                  if owner == w] or [gates[w]])
                if kind != "D":
                    gates = barrier(lasts)
                    if kind == "T2" and step + 2 < t["grid"]["k_iterations"]:
                        slots[step + 2] = tma_load(step + 2, gates)
                else:
                    gates = [self.g.last[a] for a in actors]
            outputs = []
            for w, actor in enumerate(actors):
                for i in range(tm):
                    for j in range(tn):
                        words = [(r0 + (tid // columns) * tm + i) * n + c0 + (tid % columns) * tn + j
                                 for tid in range(w * 32, (w + 1) * 32)
                                 if r0 + (tid // columns) * tm + i < m and c0 + (tid % columns) * tn + j < n]
                        if words:
                            deps = [(gates[w], "done")]
                            if (w, i, j) in acc:
                                deps.append((acc[w, i, j], "done"))
                            outputs.append(self.op(actor, sm, "store", deps, words))
            self.cta_end[sm] = self.g.add("cta-end-%d" % cta, "completion",
                [(x, "done") for x in outputs + [self.g.last[a] for a in actors]])
        return self.g

    def shared_word(self, operand, r, c):
        """Use source-consistent padded A and XOR-column B addresses."""
        bm, bn, bk = self.tile["cta"]
        if operand == 0:
            return c * (bm + 1) + r if self.tile["layout"]["a_layout"] == "transposed_padded" else r * bk + c
        return r * bn + (c ^ (c // 32) if self.tile["smem"]["b_layout"] == "xor_columns" else c)


def compare(trace, profiles, budget=1000000, retain=None):
    """Audit the complete input census, then compare hypothetical cycle rankings."""
    candidates, selection, _ = parse(trace)
    joint = selection.get("joint")
    if not joint or joint["mode"].get("model") != 3:
        raise ValueError("bubble replay requires audited joint model 3")
    if len({p.name for p in profiles}) != len(profiles) or not profiles:
        raise ValueError("duplicate or empty profile set")
    tiles = {t["tile"]: t for t in candidates}
    results, comparisons = [], []
    for p in profiles:
        p.validate()
        rows = []
        for row in joint["candidates"]:
            result = dict(candidate=row["id"], kind=row["kind"], profile=p.name,
                          source_line=row["source_line"], old_score=row["total"])
            if row["legality"] != "proven":
                result.update(status="rejected", reason=row["reason"])
            else:
                try:
                    graph = Recipe(tiles[row["tile"]], row, p, budget).build()
                    timing = simulate(graph)
                    audit_timeline(graph, timing)
                    if retain:
                        retain(p.name, row["id"], graph, timing)
                    timing.pop("timeline")
                    result.update(status="modeled", reason="synthetic_source_schedule", **timing)
                except Unknown as error:
                    result.update(status="unknown", reason=str(error))
            rows.append(result)
        old = joint["selection"]["selected"]
        complete = not any(r["status"] == "unknown" for r in rows)
        eligible = [r for r in rows if r["status"] == "modeled"]
        winner = min(eligible, key=lambda r: (r["completion_cycles"], r["candidate"])) if complete and eligible else None
        incumbent = next(r for r in rows if r["candidate"] == old)
        comparisons.append(dict(profile=p.name, old=old, emitted=old,
            shadow=winner["candidate"] if winner else None,
            outcome="incomplete" if not winner else "changed" if winner["candidate"] != old else "unchanged",
            incumbent_cycles=incumbent.get("completion_cycles"),
            shadow_cycles=winner["completion_cycles"] if winner else None,
            source_line=joint["chosen"]["source_line"]))
        results.extend(rows)
    return dict(model=MODEL, evidence="synthetic_source_schedule_not_native",
                profiles=[asdict(p) for p in profiles], candidates=results, comparisons=comparisons,
                assumptions=["one_CTA_per_SM", "in_order_warp_issue", "no_spills",
                    "separate_scalar_mul_add", "fixed_address_ops_per_memory_instruction",
                    "one_shared_bank_service_port_per_SM", "single_request_service_path",
                    "no_cache_inference", "no_native_scheduler_or_instruction_fetch_model",
                    "actor_waits_are_not_compute_bubbles", "kernel_boundary_excludes_host_setup"])


def main():
    """Publish a fresh replay directory; never overwrite raw trace or emitted sources."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--profiles", type=Path, required=True)
    parser.add_argument("--event-budget", type=int, default=1000000)
    parser.add_argument("--retain-events", action="store_true")
    args = parser.parse_args()
    if args.event_budget <= 0:
        parser.error("event budget must be positive")
    raw = args.trace.read_bytes()
    parse(raw.decode())  # Reject malformed/multi-PU inputs before creating output.
    profiles_raw = args.profiles.read_bytes()
    profiles = [Profile(**row) for row in json.loads(profiles_raw)]
    for profile in profiles:
        profile.validate()
    args.output.mkdir(parents=True, exist_ok=False)
    (args.output / "input.t").write_bytes(raw)
    (args.output / "profiles.json").write_bytes(profiles_raw)
    render(args.output / "input.t", args.output / "reports")

    def retain(name, ident, graph, timing):
        """Keep replayable event evidence, not redundant compiler JSONL traces."""
        if args.retain_events:
            path = args.output / "events" / name
            path.mkdir(parents=True, exist_ok=True)
            payload = json.dumps(dict(graph=[asdict(e) for e in graph.events],
                                      result=timing), separators=(",", ":")).encode()
            (path / (str(ident) + ".json.gz")).write_bytes(gzip.compress(payload, compresslevel=1, mtime=0))

    result = compare(raw.decode(), profiles, args.event_budget, retain)
    result["input_sha256"] = hashlib.sha256(raw).hexdigest()
    result["profile_sha256"] = hashlib.sha256(profiles_raw).hexdigest()
    (args.output / "comparison.json").write_text(json.dumps(result, indent=2) + "\n")
    lines = ["# What it summarizes: Synthetic source-warp completion, compute issue idle and actor waits; hypothetical versus actual winners.",
             "# columns: profile candidate kind status cycles compute_issue_idle actor_waits reason source_line",
             "# evidence=synthetic_source_schedule_not_native; compute issue idle includes startup/drain/control, causes not fully attributed."]
    for row in result["candidates"]:
        lines.append("BUBBLE_COST " + " ".join(str(row.get(k, "NA")) for k in (
            "profile", "candidate", "kind", "status", "completion_cycles", "compute_issue_idle_cycles",
            "actor_wait_cycles", "reason", "source_line")))
    for row in result["comparisons"]:
        lines.append("BUBBLE_COMPARE " + " ".join(str(row[k]) for k in (
            "profile", "old", "shadow", "emitted", "incumbent_cycles", "shadow_cycles", "outcome", "source_line")))
    report = "\n".join(lines) + "\n"
    reports = args.output / "reports"
    (reports / "phases" / "bubble.log").write_text(report)
    for filename in ("optimization.log", "optimization-summary.log"):
        with (reports / filename).open("a") as out:
            out.write("\n" + report)
    for filename in ("tile.log", "fetch-pipeline.log", "physical-plan.log"):
        with (reports / "phases" / filename).open("a") as out:
            out.write("\n# Bubble shadow evidence only; see bubble.log. Actual compiler selection is unchanged.\n")
    print("\n".join(lines[-len(result["comparisons"]):]))


if __name__ == "__main__":
    main()
