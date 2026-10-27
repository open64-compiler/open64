"""Independent analytical adapter/recurrence audit, never a selector.

Model 5 consumes HW Description IR facts from the trace. The old constants
below are solely for reopening historical model-4 receipts, never new output.
"""

HW_PERFORMANCE_COLUMNS = ("sms", "warp_lanes", "memory_bytes_per_cycle",
    "scalar_ops_per_cycle", "local_issues_per_cycle", "copy_elements_per_cycle",
    "latency_hiding_warps", "demand_latency", "staged_latency", "tma_latency",
    "store_latency", "publication_cycles", "wait_cycles", "release_cycles",
    "operand_issue_cycles")


def legacy_performance(latency):
    return dict(zip(HW_PERFORMANCE_COLUMNS,
        (4, 32, 32, 128, 4, 128, 16, latency, latency, latency, latency, 8, 8, 8, 1)))


def readiness_cost(tile, kind, latency, direct_bytes=None, performance=None):
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
        service = ceil(input_bytes * active, hw["memory_bytes_per_cycle"])
        delivery = service + latency
        compute = consumer * steps
        output = ceil(output_bytes * active, hw["memory_bytes_per_cycle"]) + hw["store_latency"]
        if kind != "T2":
            wait = ceil(latency, min(warps, hw["latency_hiding_warps"])) * steps
            memory = service * steps + wait + copy * steps
            sync = barriers * steps if kind == "S1" else 0
            issue, window = 0, 0
        else:
            count = min(2, steps)
            ready, service_end = [], 0
            for j in range(count):
                service_end = max(service_end, pair_issue * (j + 1)) + service
                ready.append(service_end + latency)
            clock = pair_issue * count + hw["publication_cycles"]
            if steps > 2:
                # Independently construct symbolic max-plus boundary updates.
                d = consumer + barriers + pair_issue
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
                clock = max(clock, value) + consumer + barriers
            sync, issue = barriers * steps + hw["publication_cycles"], pair_issue * steps
            memory = wait = clock - compute - sync - issue
            window = consumer + barriers if steps > 1 else 0
        return dict(compute=compute, memory=memory + output, sync=sync, launch=issue,
                    wait=wait, delivery=delivery, window=window)

    totals = dict.fromkeys(("compute", "memory", "sync", "launch", "wait"), 0)
    delivery = window = 0
    sms = hw["sms"]
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
    return record, terms
