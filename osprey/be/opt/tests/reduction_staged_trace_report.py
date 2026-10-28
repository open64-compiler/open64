"""Audit staged binding and exact slice plans; never certify executed GPU copies.

Design: doc/AI-COMPILER-REDUCTION-STAGED-BINDING-v0.1.md.
"""
import json
from pathlib import Path
from reduction_binding_numerics import equivalent, host_add, oracle

COLUMNS = ('node input result rows width version base_status arch chunk_request '
           'slots scratch_object space offset stride allocation row_budget cta_budget other rows_cta ctas threads '
           'copy visibility tail e0 e1 e2 e3 e4 n0 n1 n2 n3 n4 chunk chunks last_count last_active '
           'row_bytes scratch_end reserved hw_budget headroom quantum max_threads obligations status selected source_line').split()
HEADER = 'AI_REDUCE_STAGE 1 HEADER ' + ' '.join(COLUMNS)
SLICE_COLUMNS = 'node row generation base count input_offset scratch_offset output_offset source_line'.split()
SLICE_HEADER = 'AI_REDUCE_SLICE 1 HEADER ' + ' '.join(SLICE_COLUMNS)
MAX = (1 << 64) - 1
REASONS = ('bound_runtime_obligations', 'base_contract_refused', 'invalid_chunk',
           'unsupported_copy_visibility', 'invalid_scratch_storage', 'invalid_row_ownership',
           'scratch_capacity_or_budget', 'invalid_visibility_order', 'unsafe_slot_reuse', 'unsupported_tail')
BASE_REASONS = ('bound_runtime_obligations', 'source_binding_missing', 'stale_source',
                'unknown_architecture', 'unsupported_instruction', 'unsafe_fp_options',
                'unsupported_layout', 'invalid_range', 'runtime_lease_missing',
                'overlapping_ranges', 'invalid_owner_mapping', 'invalid_event_order')


def events_valid(edges):
    """Independent DAG closure proves all adjacent happens-before relations."""
    reach = list(edges)
    if any(edge < 0 or edge >= 32 for edge in reach):
        return False
    for k in range(5):
        for i in range(5):
            if reach[i] & (1 << k):
                reach[i] |= reach[k]
    return not any(reach[i] & (1 << i) for i in range(5)) and all(
        reach[i] & (1 << (i + 1)) for i in range(4))


def expected_status(r):
    """Recompute gates with unbounded Python integers before applying u64 limits."""
    if r['base_status']:
        assert r['base_status'] < len(BASE_REASONS)
        return 1
    assert r['version'] == 2 and 1 <= r['arch'] <= 4
    c = min(r['chunk_request'], r['width'])
    if not c:
        return 2
    if r['copy'] != 1 or r['visibility'] != 1:
        return 3
    if (r['slots'] != 1 or r['space'] != 2 or not r['scratch_object'] or
            r['offset'] % 4 or r['stride'] % 4 or r['stride'] < 4 * c):
        return 4
    assert r['hw_budget'] > 0 and r['quantum'] > 0 and r['max_threads'] > 0
    b = r['rows_cta']
    if (not b or b > r['max_threads'] or r['ctas'] != (r['rows'] + b - 1) // b or
            b * r['ctas'] > MAX or b * r['ctas'] != r['threads']):
        return 5
    end = r['offset'] + (b - 1) * r['stride'] + 4 * c
    total = r['allocation'] + r['other']
    reserved = (total + r['quantum'] - 1) // r['quantum'] * r['quantum'] + r['headroom']
    if (4 * c > r['row_budget'] or end > MAX or end > r['allocation'] or
            not r['cta_budget'] or r['cta_budget'] > r['hw_budget'] or
            total + r['quantum'] - 1 > MAX or reserved > MAX or reserved > r['cta_budget']):
        return 6
    if not events_valid([r['e%d' % i] for i in range(5)]):
        return 7
    if [r['n%d' % i] for i in range(5)] != [0, 0, 0, 0, 1]:
        return 8
    return 0 if r['tail'] == 1 else 9


def audit(text):
    """Require explicit schemas and exact fields, derived extents and coverage."""
    rows, slices = [], []
    seen = slice_seen = False
    for line in text.splitlines():
        if line == HEADER:
            seen = True
        elif line.startswith('AI_REDUCE_STAGE '):
            fields = line.split()
            assert seen and fields[:3] == ['AI_REDUCE_STAGE', '1', 'BIND'] and len(fields) == len(COLUMNS) + 3
            row = dict(zip(COLUMNS, map(int, fields[3:])))
            assert all(0 <= value <= MAX for value in row.values())
            assert row['rows'] and row['width'] and row['selected'] == 0
            status = expected_status(row)
            assert row['status'] == status and row['obligations'] == (0 if status else 255)
            if not status:
                c = min(row['chunk_request'], row['width'])
                assert row['chunk'] == c and row['row_bytes'] == 4 * c
                assert row['chunks'] == (row['width'] + c - 1) // c
                assert row['last_count'] == row['width'] - (row['chunks'] - 1) * c
                assert row['last_active'] == row['rows'] - (row['ctas'] - 1) * row['rows_cta']
                assert row['scratch_end'] == row['offset'] + (row['rows_cta'] - 1) * row['stride'] + 4 * c
                assert row['reserved'] == ((row['allocation'] + row['other'] + row['quantum'] - 1) //
                                           row['quantum']) * row['quantum'] + row['headroom']
            rows.append(row)
        elif line == SLICE_HEADER:
            slice_seen = True
        elif line.startswith('AI_REDUCE_SLICE '):
            fields = line.split()
            assert slice_seen and fields[:3] == ['AI_REDUCE_SLICE', '1', 'PLAN'] and len(fields) == len(SLICE_COLUMNS) + 3
            slices.append(dict(zip(SLICE_COLUMNS, map(int, fields[3:]))))
    assert rows
    first = rows[0]
    assert all([row[key] for key in ('node', 'input', 'result', 'rows', 'width', 'version', 'source_line')] ==
               [first[key] for key in ('node', 'input', 'result', 'rows', 'width', 'version', 'source_line')]
               for row in rows)
    if first['status']:
        assert not slices
    else:
        keys = set()
        next_generation = {}
        for s in slices:
            row, g = s['row'], s['generation']
            assert 0 <= row < first['rows'] and 0 <= g < first['chunks'] and (row, g) not in keys
            keys.add((row, g))
            assert g == next_generation.get(row, 0)
            next_generation[row] = g + 1
            assert s['node'] == first['node'] and s['source_line'] == first['source_line']
            assert s['base'] == g * first['chunk']
            assert s['count'] == min(first['chunk'], first['width'] - s['base'])
            assert s['scratch_offset'] == first['offset'] + row % first['rows_cta'] * first['stride']
        assert len(keys) == first['rows'] * first['chunks']
    return rows, slices


def slice_witness(first, slices):
    """Execute only a host contract witness: poison slots, copy bits, never read
    beyond the native-derived count, and retain one accumulator across reuse."""
    patterns = ([0x60ad78ec, 0x3f800000, 0xe0ad78ec, 0x3f800000, 1, 0x80000000],
                [0x7f7fffff, 0x7f7fffff, 0xff7fffff, 0x7f800001],
                [0x80000000, 1, 0x80000001, 0, 0x007fffff])
    state, words, read, slots = {}, {}, {}, {}
    for row in range(first['rows']):
        pattern = patterns[row % len(patterns)]
        words[row] = [pattern[k % len(pattern)] for k in range(first['width'])]
        state[row] = 0
        read[row] = []
    for s in slices:
        row, start, count = s['row'], s['base'], s['count']
        key = (row // first['rows_cta'], s['scratch_offset'])
        # Release of the preceding generation is required before this acquire.
        assert key not in slots or slots[key]['released']
        slot = [0x7fc0dead] * first['chunk']
        source = words[row][start:start + count]
        slot[:count] = source
        assert slot[:count] == source  # Integer copy: no FP conversion/FTZ.
        for j in range(count):
            state[row] = host_add(state[row], slot[j])
            read[row].append(start + j)
        slots[key] = dict(released=True)
    results = []
    for row in range(first['rows']):
        assert read[row] == list(range(first['width']))
        assert equivalent(state[row], oracle(words[row]))
        results.append(dict(row=row, input_bits=['%08x' % word for word in words[row]],
                            result_bits='%08x' % state[row], expected_bits='%08x' % oracle(words[row])))
    return results


def write_reports(trace, preopt, output):
    """Join actual PREOPT and summarize contracts separately from host witnesses."""
    from reduction_binding_trace_report import audit_preopt
    from reduction_direct_trace_report import audit as audit_direct
    trace, preopt, output = map(Path, (trace, preopt, output))
    text = trace.read_text()
    rows, slices = audit(text)
    first = rows[0]
    source = audit_preopt(preopt.read_text())
    base = audit_direct(text)[0]
    assert base['status'] == BASE_REASONS[first['base_status']]
    assert first['arch'] == base['arch'] and first['threads'] == base['threads']
    for key in ('node', 'input', 'result', 'rows', 'width', 'version', 'source_line'):
        assert str(first[key]) == source[key]
    assert all(first[k] == base[k] for k in ('node', 'input', 'result', 'rows', 'width', 'source_line'))
    for s in slices:
        assert s['input_offset'] == base['in_offset'] + 4 * (s['row'] * first['width'] + s['base'])
        assert s['output_offset'] == base['out_offset'] + 4 * s['row']
        assert s['input_offset'] + 4 * s['count'] <= base['in_offset'] + base['in_bytes']
    output.mkdir(parents=True, exist_ok=False)
    numerical = slice_witness(first, slices) if slices else []
    (output / 'slice-witness.json').write_text(json.dumps(dict(rows=numerical, host_only=True, gpu=False), indent=2) + '\n')
    reports = {
        'alternatives.log': ('Staged binding choices, explicit refusal reasons and absence of profitability selection.',
            ''.join('query=%d status=%s obligations=%d selected=0 source_line=%d\n' %
                    (i, REASONS[r['status']], r['obligations'], r['source_line']) for i, r in enumerate(rows))),
        'residency.log': ('CTA-local scratch identity, row-slot partitioning, padding, capacity and target-envelope reservation.',
            'scratch_object=%d rows_per_cta=%d stride=%d offset=%d scratch_end=%d allocation=%d reserved=%d budget=%d\n'
            'resource_provenance=existing_estimated_planning_envelope actual_launch_limit=unverified\n' %
            tuple(first[k] for k in ('scratch_object', 'rows_cta', 'stride', 'offset', 'scratch_end', 'allocation', 'reserved', 'cta_budget'))),
        'fetch-pipeline.log': ('Planned bit-copy visibility, last-read slot release, generation reuse and exact tail/owner predicates.',
            'copy=sync_bits32 visibility=same_owner_program_order slots=1 carry=release_g_before_acquire_next\n'
            'chunk=%d chunks=%d last_count=%d last_active_rows=%d planned_slices=%d source_line=%d\n'
            'copy_execution=not_run async=unsupported cross_thread_consumers=unsupported\n' %
            tuple([first[k] for k in ('chunk', 'chunks', 'last_count', 'last_active')] + [len(slices), first['source_line']])),
        'numerical.log': ('Host slice witnesses versus the independent integer binary32 oracle; no source permission or GPU proof.',
            'host_witness_rows=%d adds_per_row=%d state=one_positive_zero_initialized_accumulator\n'
            'order=ascending_global_k partial_sums=forbidden acceptance=exact_bits_except_nan_class gpu=not_run\n' %
            (len(numerical), first['width']))}
    for name, (summary, body) in reports.items():
        (output / name).write_text('# What it summarizes: ' + summary + '\n' + body)
    summary = ('queries=%d bound=%d refused=%d planned_slices=%d host_witness_rows=%d\n'
               'runtime_obligations=outstanding staged_runtime_adapter=not_implemented\n'
               'candidate_selection=not_run profitability=not_run transformation=none native_copy_execution=not_run gpu=not_run\n' %
               (len(rows), sum(r['status'] == 0 for r in rows), sum(r['status'] != 0 for r in rows), len(slices), len(numerical)))
    (output / 'optimization-summary.log').write_text('# What it summarizes: completed staged-binding checks and remaining execution gates.\n' + summary)
    (output / 'optimization.log').write_text('# What it summarizes: full collection of staged-binding phase reports and recorded refusals.\n' +
        ''.join((output / name).read_text() for name in reports) + (output / 'optimization-summary.log').read_text())
    return dict(queries=len(rows), bound=sum(r['status'] == 0 for r in rows), slices=len(slices), host_rows=len(numerical))
