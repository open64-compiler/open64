"""Independent analytical model-4 adapter/recurrence audit, never a selector."""


def readiness_cost(tile, kind, latency, direct_bytes=None):
    def ceil(a, b):
        return (a + b - 1) // b

    ctas = tile["grid"]["ctas"]
    bm, bn, bk = tile["cta"]
    m, n, k = tile["grid"]["problem"]
    steps = k if kind == "D" else tile["grid"]["k_iterations"]
    pairs = ctas * steps
    warps = ceil(tile["threads"], 32)
    gmem, smem = tile["gmem"], tile["smem"]
    raw = (sum(direct_bytes[:2]) if kind == "D" else
           4 * (m * k * tile["grid"]["grid"][1] + k * n * tile["grid"]["grid"][0])
           if kind == "T2" else gmem["a"] + gmem["b"])
    input_bytes, output_bytes = ceil(raw, pairs), ceil(gmem["c"], ctas)
    consumer = ceil(2 * bm * bn * (1 if kind == "D" else bk), 128)
    copy = 0
    if kind != "D":
        consumer += ceil(gmem["a_issues"] + smem["b_issues"] +
                         tile["layout"]["bank_work"] + smem["address"], pairs * 4)
        if kind == "S1":
            copy = ceil((bm + bn) * bk, 128)

    def wave(active):
        service = ceil(input_bytes * active, 32)
        delivery = service + latency
        compute = consumer * steps
        output = ceil(output_bytes * active, 32) + latency
        if kind != "T2":
            wait = ceil(latency, min(warps, 16)) * steps
            memory = service * steps + wait + copy * steps
            sync = 16 * steps if kind == "S1" else 0
            issue, window = 0, 0
        else:
            count = min(2, steps)
            ready, service_end = [], 0
            for j in range(count):
                service_end = max(service_end, 2 * (j + 1)) + service
                ready.append(service_end + latency)
            clock = 2 * count + 8
            if steps > 2:
                # Independently construct symbolic max-plus boundary updates.
                d = consumer + 18
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
                clock = max(clock, value) + consumer + 16
            sync, issue = 16 * steps + 8, 2 * steps
            memory = wait = clock - compute - sync - issue
            window = consumer + 16 if steps > 1 else 0
        return dict(compute=compute, memory=memory + output, sync=sync, launch=issue,
                    wait=wait, delivery=delivery, window=window)

    totals = dict.fromkeys(("compute", "memory", "sync", "launch", "wait"), 0)
    delivery = window = 0
    for active, count in ((4, ctas // 4), (ctas % 4, int(ctas % 4 != 0))):
        if count:
            current = wave(active)
            for key in totals:
                totals[key] += current[key] * count
            delivery = max(delivery, current["delivery"])
            window = max(window, current["window"])
    total = sum(totals[key] for key in ("compute", "memory", "sync", "launch"))
    record = dict(ctas=ctas, steps=steps, consumer=consumer, input_bytes=input_bytes,
                  output_bytes=output_bytes, copy=copy, warps=warps, delivery=delivery,
                  window=window, wait=totals["wait"], waves=ceil(ctas, 4), total=total,
                  source_line=tile["source_line"])
    terms = (totals["compute"], totals["memory"], 0, totals["sync"], totals["launch"], 0, total)
    return record, terms
