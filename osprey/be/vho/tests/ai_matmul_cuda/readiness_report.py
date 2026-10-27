"""Independent analytical adapter/recurrence audit, never a selector.

Model 5 consumes HW Description IR facts from the trace. The old constants
below are solely for reopening historical model-4 receipts, never new output.
"""

HW_PERFORMANCE_COLUMNS = ("sms", "warp_lanes", "memory_bytes_per_cycle",
    "scalar_ops_per_cycle", "local_issues_per_cycle", "copy_elements_per_cycle",
    "latency_hiding_warps", "demand_latency", "staged_latency", "tma_latency",
    "store_latency", "publication_cycles", "wait_cycles", "release_cycles",
    "operand_issue_cycles")

HW_RESOURCE_COLUMNS = ("max_blocks", "max_warps", "max_threads", "max_threads_per_cta",
    "registers_per_sm", "max_registers_per_thread", "register_quantum", "register_warp_quantum",
    "shared_capacity", "shared_budget", "shared_quantum", "shared_reserved")
OCCUPANCY_COLUMNS = ("registers", "shared", "block_limit", "warp_limit", "thread_limit",
    "register_limit", "shared_limit", "resident", "warps", "limiting", "full", "tail",
    "unused", "tail_sms", "legacy_total", "source_line")


def occupancy_record(resource, hw, demand, ctas, legacy_total):
    """Independent integer capacity oracle; unknown or impossible models fail closed."""
    def roundup(value, quantum):
        return ((value + quantum - 1) // quantum) * quantum
    if (any(resource[key] <= 0 for key in HW_RESOURCE_COLUMNS[:-1]) or
            resource["shared_reserved"] < 0 or resource["shared_budget"] > resource["shared_capacity"]):
        raise ValueError("invalid occupancy hardware resources")
    threads = demand["threads"]
    regs = (demand["register_bytes"] + 3) // 4
    if not threads > 0 or not regs > 0:
        raise ValueError("invalid occupancy demand")
    warps = (threads + hw["warp_lanes"] - 1) // hw["warp_lanes"]
    registers = roundup(regs * hw["warp_lanes"], resource["register_quantum"]) * roundup(warps, resource["register_warp_quantum"])
    shared = roundup(demand["shared_bytes"] + resource["shared_reserved"], resource["shared_quantum"])
    limits = [resource["max_blocks"], resource["max_warps"] // warps,
              resource["max_threads"] // threads if threads <= resource["max_threads_per_cta"] else 0,
              resource["registers_per_sm"] // registers if regs <= resource["max_registers_per_thread"] else 0,
              resource["shared_budget"] // shared if shared else resource["max_blocks"]]
    resident = min(limits)
    if max(registers, shared, resident * hw["sms"]) >= 2 ** 64 - 1:
        raise ValueError("unknown or overflowing residency")
    full, tail = divmod(ctas, resident * hw["sms"]) if resident else (0, 0)
    return dict(zip(OCCUPANCY_COLUMNS, (registers, shared, *limits, resident, resident * warps,
        sum(1 << i for i, value in enumerate(limits) if value == resident), full, tail,
        resident * hw["sms"] - tail if tail else 0, min(tail, hw["sms"]), legacy_total, demand["source_line"])))


def legacy_performance(latency):
    return dict(zip(HW_PERFORMANCE_COLUMNS,
        (4, 32, 32, 128, 4, 128, 16, latency, latency, latency, latency, 8, 8, 8, 1)))


def readiness_cost(tile, kind, latency, direct_bytes=None, performance=None, occupancy=None):
    def ceil(a, b):
        return (a + b - 1) // b

    hw = performance if performance is not None else legacy_performance(latency)
    latency = hw[{"D": "demand_latency", "S1": "staged_latency", "T2": "tma_latency"}[kind]]
    barriers = hw["wait_cycles"] + hw["release_cycles"]
    pair_issue = 2 * hw["operand_issue_cycles"]
    ctas = tile["grid"]["ctas"]
    bm, bn, bk = tile["cta"]
    m, n, k = tile["grid"]["problem"]
    steps = k if kind == "D" else tile["grid"]["k_iterations"]
    pairs = ctas * steps
    warps = ceil(tile["threads"], hw["warp_lanes"])
    gmem, smem = tile["gmem"], tile["smem"]
    raw = (sum(direct_bytes[:2]) if kind == "D" else
           4 * (m * k * tile["grid"]["grid"][1] + k * n * tile["grid"]["grid"][0])
           if kind == "T2" else gmem["a"] + gmem["b"])
    input_bytes, output_bytes = ceil(raw, pairs), ceil(gmem["c"], ctas)
    consumer = ceil(2 * bm * bn * (1 if kind == "D" else bk), hw["scalar_ops_per_cycle"])
    copy = 0
    if kind != "D":
        consumer += ceil(gmem["a_issues"] + smem["b_issues"] +
                         tile["layout"]["bank_work"] + smem["address"], pairs * hw["local_issues_per_cycle"])
        if kind == "S1":
            copy = ceil((bm + bn) * bk, hw["copy_elements_per_cycle"])

    def wave(active):
        q = ceil(active, hw["sms"]) if occupancy is not None else 1
        work, local_copy = consumer * q, copy * q
        sync_pair, issue_pair = barriers * q, pair_issue * q
        service = ceil(input_bytes * active, hw["memory_bytes_per_cycle"])
        delivery = service + latency
        compute = work * steps
        output = ceil(output_bytes * active, hw["memory_bytes_per_cycle"]) + hw["store_latency"]
        if kind != "T2":
            wait = ceil(latency, min(warps * q, hw["latency_hiding_warps"])) * steps
            memory = service * steps + wait + local_copy * steps
            sync = sync_pair * steps if kind == "S1" else 0
            issue, window = 0, 0
        else:
            count = min(2, steps)
            ready, service_end = [], 0
            for j in range(count):
                service_end = max(service_end, issue_pair * (j + 1)) + service
                ready.append(service_end + latency)
            clock = issue_pair * count + hw["publication_cycles"] * q
            if steps > 2:
                # Independently construct symbolic max-plus boundary updates.
                d = work + sync_pair + issue_pair
                matrix = [[d, d, None, None], [None, None, 0, None],
                          [d + delivery, d + delivery, None, delivery],
                          [d + service, d + service, None, service]]
                state = [clock, *ready, service_end]
                exponent = steps - 2
                while exponent:
                    if exponent & 1:
                        state = [max(edge + x for edge, x in zip(row, state) if edge is not None)
                                 for row in matrix]
                    exponent //= 2
                    if exponent:
                        square = []
                        for row in matrix:
                            output_row = []
                            for col in zip(*matrix):
                                values = [a + b for a, b in zip(row, col) if a is not None and b is not None]
                                output_row.append(max(values) if values else None)
                            square.append(output_row)
                        matrix = square
                clock, *rest = state
                ready = rest[:2]
            for value in ready:
                clock = max(clock, value) + work + sync_pair
            sync, issue = sync_pair * steps + hw["publication_cycles"] * q, issue_pair * steps
            memory = wait = clock - compute - sync - issue
            window = work + sync_pair if steps > 1 else 0
        return dict(compute=compute, memory=memory + output, sync=sync, launch=issue,
                    wait=wait, delivery=delivery, window=window)

    totals = dict.fromkeys(("compute", "memory", "sync", "launch", "wait"), 0)
    delivery = window = 0
    sms = hw["sms"] * (occupancy["resident"] if occupancy is not None and occupancy["resident"] else 1)
    for active, count in ((sms, ctas // sms), (ctas % sms, int(ctas % sms != 0))):
        if count:
            current = wave(active)
            for key in totals:
                totals[key] += current[key] * count
            delivery = max(delivery, current["delivery"])
            window = max(window, current["window"])
    total = sum(totals[key] for key in ("compute", "memory", "sync", "launch"))
    record = dict(ctas=ctas, steps=steps, consumer=consumer, input_bytes=input_bytes,
                  output_bytes=output_bytes, copy=copy, warps=warps, delivery=delivery,
                  window=window, wait=totals["wait"], waves=ceil(ctas, sms), total=total,
                  source_line=tile["source_line"])
    terms = (totals["compute"], totals["memory"], 0, totals["sync"], totals["launch"], 0, total)
    if max(*record.values(), *terms) >= 2 ** 64 - 1:
        raise ValueError("readiness arithmetic overflow")
    return record, terms
