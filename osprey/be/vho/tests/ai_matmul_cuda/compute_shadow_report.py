"""Independent MMA2 dependency-graph oracle and real-source trace inspection.

Only synthetic fixture inputs have numerical costs. No CUDA timing database,
instruction emitter, executable selector or native-performance claims.
"""

import argparse
from pathlib import Path


def source_summary(text, selected):
    rows = [line.split()[2:] for line in text.splitlines()
            if line.startswith("AI_MMA_SHADOW 1 SOURCE ")]
    if not rows:
        return ""
    if len(rows) != 4 or any(len(row) != 11 for row in rows) or {int(row[1]) for row in rows} != {1, 2, 3, 4}:
        raise ValueError("missing/duplicate MMA source tuple")
    for row in rows:
        if (len(row) != 11 or row[4] not in ("exact_target_required", "numerical_contract_required")
                or row[5] != "unknown" or row[7] != "unknown" or row[9] != "none"
                or int(row[6]) != selected or int(row[8]) != selected or int(row[3]) != 1):
            raise ValueError("MMA source changed F32/SIMT execution or invented timing")
        expected = "numerical_contract_required" if int(row[2]) == (2 if int(row[1]) <= 2 else 4) else "exact_target_required"
        if row[4] != expected:
            raise ValueError("MMA exact-target screening disagrees with profile")
    return ("# MMA2 analytical shadow: four exact tuples screened; source=F32; "
            "timing=unknown shadow_winner=unknown emitted=SIMT action=none native=unverified\n"
            + "\n".join(" ".join(row) for row in rows) + "\n")


def ceil_div(amount, rate):
    return (amount + rate - 1) // rate


class Graph:
    """Build explicit precedence edges, then solve longest paths separately."""

    def __init__(self):
        self.nodes = [(0, ())]

    def event(self, duration, *dependencies):
        self.nodes.append((duration, dependencies))
        return len(self.nodes) - 1

    def solve(self):
        ends = []
        for duration, predecessors in self.nodes:
            ends.append(duration + max((ends[index] for index in predecessors), default=0))
        return ends


def oracle_round(geometry, model, rates, active):
    _, _, total_k, bm, bn, bk, _, _, _ = geometry
    profile, family, sms, queue = model[1], model[2], model[8], model[10]
    tmem = family == 3
    simt = family == 1
    graph = Graph()
    calendars = {}
    busy = {}

    def resource(kind, sm, duration, *predecessors):
        key = kind, sm
        result = graph.event(duration, calendars.get(key, 0), *predecessors)
        calendars[key] = result
        busy[key] = busy.get(key, 0) + duration
        return result

    actors = [resource("scalar", cta % sms, rates[11] + (rates[12] if tmem else 0))
              for cta in range(active)]
    ready = [0] * active
    slots = [[0] * queue for _ in range(sms)]
    issued = [0] * sms
    atom_m = 64 if profile <= 2 else 128
    input_bytes = 2 * (bm + bn) * bk
    operand_bytes = 2 * (atom_m + 64) * 16
    output_bytes = 4 * bm * bn
    compute = ceil_div(2 * atom_m * 64 * 16, rates[3 if simt else 2])
    for _ in range(total_k // bk):
        for cta in range(active):
            sm = cta % sms
            transferred = resource("device", 0, ceil_div(input_bytes, rates[0]), actors[cta])
            loaded = graph.event(rates[7], transferred)
            staged = resource("shared", sm, ceil_div(input_bytes, rates[1]), loaded)
            ready[cta] = resource("scalar", sm, rates[10] +
                                  (ceil_div(input_bytes // 2, rates[6]) if simt else 0), staged)
        for _ in range(bk // 16):
            for cta in range(active):
                sm = cta % sms
                slot = issued[sm] % queue
                operand = resource("shared", sm, ceil_div(operand_bytes, rates[1]),
                                   ready[cta], actors[cta], slots[sm][slot])
                issue_start = graph.event(0, operand, calendars.get(("scalar", sm), 0),
                                          0 if simt else calendars.get(("tensor", sm), 0))
                issued_event = resource("scalar", sm, rates[8], issue_start)
                if simt:
                    serviced = resource("scalar", sm, compute, issued_event)
                else:
                    serviced = resource("tensor", sm, compute, issued_event)
                latency = graph.event(rates[9], issue_start)
                actors[cta] = graph.event(0, serviced, latency)
                slots[sm][slot] = actors[cta]
                issued[sm] += 1
        for cta in range(active):
            actors[cta] = resource("scalar", cta % sms, rates[10], actors[cta])
    finished = []
    for cta in range(active):
        sm = cta % sms
        last = actors[cta]
        if tmem:
            last = resource("tmem", sm, ceil_div(output_bytes, rates[5]), last)
        last = resource("scalar", sm, ceil_div(output_bytes // 4, rates[4]), last)
        last = resource("device", 0, ceil_div(output_bytes, rates[0]), last)
        last = graph.event(rates[7], last)
        if tmem:
            last = resource("scalar", sm, rates[13], last)
        finished.append(last)
    ends = graph.solve()
    return max(ends[index] for index in finished), max(busy.values())


def check_case(case):
    model = list(map(int, case["MODEL"][0]))
    geometry = list(map(int, case["GEOMETRY"][0]))
    profile, family = model[1:3]
    problem_m, problem_n, total_k, bm, bn, bk, register_bytes, shared_bytes, _ = geometry
    atom_m = 64 if profile <= 2 else 128
    ctas = (problem_m // bm) * (problem_n // bn)
    assert model[0] == profile and bm == atom_m and bn == 64 and total_k % bk == 0
    expected_work = [2 * problem_m * problem_n * total_k] * 2
    input_bytes = ctas * (total_k // bk) * 2 * (bm + bn) * bk
    output_bytes = ctas * bm * bn * 4
    expected_work += [ctas * (total_k // 16), ctas, total_k // bk, input_bytes,
                      output_bytes, input_bytes * 2, output_bytes if family == 3 else 0]
    assert list(map(int, case["WORK"][0][:-1])) == expected_work
    metrics = case["METRIC"]
    assert len(metrics) == 16 and [int(row[0]) for row in metrics] == list(range(16))
    if model[4] == 0:
        assert model[5:13] == [0] * 8
        assert all(row[1:] == ["unknown"] * 4 for row in metrics)
        assert all(row[1] == "unknown" for row in case["COST"])
        return False
    assert model[4] == 1 and model[5] == 2 and model[6] > 0 and model[7] == 1
    assert all(row[4] == "synthetic" for row in metrics)
    resource = list(map(int, case["CAPACITY"][0]))
    blocks, warps, threads, block_threads, registers, thread_regs, quantum, warp_quantum, _, shared_budget, shared_quantum, reserved = resource
    assert block_threads >= 128
    if family == 1:
        register_bytes = max(register_bytes, bm * bn * 4 // 128 + 8)
    per_thread = ceil_div(register_bytes, 4)
    rounded_reg = ceil_div(per_thread * 32, quantum) * quantum * ceil_div(4, warp_quantum) * warp_quantum
    rounded_shared = ceil_div(shared_bytes + reserved, shared_quantum) * shared_quantum
    limits = [blocks, warps // 4, threads // 128, registers // rounded_reg if per_thread <= thread_regs else 0,
              shared_budget // rounded_shared, model[9]]
    if family == 3:
        assert model[11] == 1
        limits.append(model[12])
    resident = min(limits)
    limiting = sum(1 << index for index, limit in enumerate(limits) if limit == resident)
    capacity = resident * model[8]
    full, tail = divmod(ctas, capacity)
    assert list(map(int, case["RESIDENCY"][0][:-2])) == [rounded_reg, rounded_shared, resident, limiting, full, tail, capacity - tail if tail else 0]
    totals = []
    for corner, row in enumerate(case["COST"]):
        data = list(map(int, row))
        assert data[0] == corner
        rates = [int(metric[(3 - corner if corner != 1 else 2) if index < 7 else corner + 1])
                 for index, metric in enumerate(metrics)]
        kernel = floor = 0
        for count, repeats in ((capacity, full), (tail, int(tail != 0))):
            if repeats:
                time, minimum = oracle_round(geometry, model, rates, count)
                kernel += time * repeats
                floor += minimum * repeats
        assert data[1:4] == [kernel, kernel + rates[14] + rates[15], floor], (data[1:4], kernel, floor)
        assert sum(data[4:12]) == data[2] and data[3] <= data[1]
        assert all(value >= 0 for value in data)
        totals.append(data[2])
    assert totals == sorted(totals)
    return True


def parse_models(text):
    cases = []
    for line in text.splitlines():
        if not line.startswith("AI_MMA_SHADOW 1 "):
            continue
        _, _, kind, *values = line.split()
        if kind == "MODEL":
            cases.append({})
        cases[-1].setdefault(kind, []).append(values)
    return cases


def check_stream(text):
    cases = parse_models(text)
    complete = sum(check_case(case) for case in cases)
    if complete != 496 or len(cases) != 504:
        raise ValueError("incomplete synthetic/unknown coverage")
    return complete


def render_models(text, root):
    """Expose fixture-only inputs, full costs and paired conditional rankings."""
    check_stream(text)
    root.mkdir(parents=True, exist_ok=False)
    headings = {
        "tile.log": "Exact synthetic MMA1 tuples and useful/executed work; matrix tails reject.",
        "residency.log": "Rounded RF/SMEM and explicit TMEM caps; logical estimates, not native allocation.",
        "fetch-pipeline.log": "Synchronous SS delivery, shared queues, accumulator completion and nonadditive waits.",
        "physical-plan.log": "Synthetic scenario costs/envelopes and same-contract comparisons; no executable action.",
    }
    contents = {name: f"# What it summarizes: {heading}\n# Synthetic reference cycles only; real timing=unknown native=unverified typed_fallback=unimplemented.\n"
                for name, heading in headings.items()}
    pairs = {}
    for index, case in enumerate(parse_models(text)):
        model = list(map(int, case["MODEL"][0]))
        identity = f"case={index} profile={model[1]} family={model[2]} scenario={model[6]} "
        contents["tile.log"] += identity + "geometry=" + " ".join(case["GEOMETRY"][0]) + " work=" + " ".join(case["WORK"][0]) + "\n"
        contents["residency.log"] += identity + "reservations=" + " ".join(case["RESIDENCY"][0]) + f" tmem_cap={model[12] if model[11] else 'unknown'}\n"
        for row in case["COST"]:
            contents["physical-plan.log"] += identity + "cost=" + " ".join(row) + "\n"
            contents["fetch-pipeline.log"] += identity + f"queue={model[10] or 'unknown'} corner={row[0]} waits=" + " ".join(row[12:16] if row[1] != "unknown" else ["unknown"]) + "\n"
        if model[4] == 0:
            continue
        key = (model[1], tuple(case["GEOMETRY"][0]), model[10], tuple(tuple(row) for row in case["METRIC"]))
        pairs.setdefault(key, {})[model[2]] = case
    for key, pair in pairs.items():
        simt = pair[1]
        mma_family = 2 if key[0] <= 2 else 3
        mma = pair[mma_family]
        scalar_cost = [int(row[2]) for row in simt["COST"]]
        mma_cost = [int(row[2]) for row in mma["COST"]]
        nominal = mma_family if mma_cost[1] < scalar_cost[1] else 1
        bounded = mma_family if mma_cost[2] < scalar_cost[0] else 1 if scalar_cost[2] < mma_cost[0] else "unknown"
        contents["physical-plan.log"] += (f"profile={key[0]} geometry={' '.join(key[1])} queue={key[2]} "
            f"hypothetical_simt={scalar_cost} mma={mma_cost} nominal_family={nominal} bounded_family={bounded} "
            "emitted_authority=existing_SIMT action=none native=unverified\n")
    for name, content in contents.items():
        (root / name).write_text(content)
    (root / "optimization.log").write_text("# What it summarizes: Complete MMA2 synthetic fixture phase review, not a production kernel trace.\n" + "".join(contents.values()))
    (root / "optimization-summary.log").write_text(
        "# What it summarizes: Conditional model/oracle coverage and executable/native boundaries.\n"
        f"synthetic_schedules=496 corners=1488 paired_comparisons={len(pairs)} unknown_profiles=8 "
        "real_timing=unknown executable_selection=unchanged typed_image=not_generated native=unverified\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace", type=Path)
    parser.add_argument("--reports", type=Path)
    args = parser.parse_args()
    count = check_stream(args.trace.read_text())
    if args.reports:
        render_models(args.trace.read_text(), args.reports)
    print(f"Independent event-DAG oracle passed: {count} synthetic cases x 3 corners; 8 unknown profiles")


if __name__ == "__main__":
    main()
