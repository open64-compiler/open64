"""Audit compact runtime enforcement evidence, not device certification.

Design: doc/AI-COMPILER-REDUCTION-RUNTIME-GUARDS-v0.1.md.
"""
from pathlib import Path

EVENTS = ('ready', 'launch', 'complete', 'consume', 'input_release', 'output_release')
HEADER = 'AI_REDUCE_RUN 1 HEADER seq ticket action status done leases source_line'
BIND_HEADER = ('AI_REDUCE_RUN 1 BIND_HEADER ticket contract arch in_root out_root '
               'in_offset in_bytes out_offset out_bytes e0 e1 e2 e3 e4 e5 source_line')


def predecessors(edges):
    """Independently check the DAG and retain every extra ordering constraint."""
    reach = list(edges)
    assert len(reach) == 6 and all(0 <= edge < 64 for edge in reach)
    for k in range(6):
        for i in range(6):
            if reach[i] & (1 << k):
                reach[i] |= reach[k]
    assert all(not (reach[i] & (1 << i)) for i in range(6))
    assert all(reach[a] & (1 << b) for a, b in ((0, 1), (1, 2), (2, 3), (2, 4), (3, 5)))
    return [sum(1 << i for i in range(6) if reach[i] & (1 << j)) for j in range(6)]


def audit(text):
    """Replay successful and failed transitions without trusting printed masks."""
    seen = False
    bind_seen = False
    seq = 0
    bindings, tickets = {}, {}
    counts = dict(admitted=0, refused=0, backend_failures=0, closed=0, events=0)
    for line in text.splitlines():
        if not line.startswith('AI_REDUCE_RUN '):
            continue
        if line == HEADER:
            assert not seen
            seen = True
            continue
        if line == BIND_HEADER:
            assert seen and not bind_seen
            bind_seen = True
            continue
        fields = line.split()
        assert seen and fields[:2] == ['AI_REDUCE_RUN', '1']
        if fields[2] == 'BIND':
            assert bind_seen and len(fields) == 19
            values = list(map(int, fields[3:]))
            ticket, contract, arch, a, b, ao, ab, bo, bb = values[:9]
            assert ticket > 0 and ticket not in bindings and contract > 0 and 1 <= arch <= 4
            assert a > 0 and b > 0 and ao >= 0 and bo >= 0 and ab > 0 and bb > 0
            if a == b:
                assert ao + ab <= bo or bo + bb <= ao
            bindings[ticket] = (predecessors(values[9:15]), values[-1])
            continue
        assert fields[2] == 'EVENT' and len(fields) == 10
        current, ticket = map(int, fields[3:5])
        action = fields[5]
        assert action in EVENTS + ('admit', 'register', 'retire', 'abort', 'close')
        status, done, leases, source_line = map(int, fields[6:])
        assert current == seq + 1 and 0 <= status <= 7 and 0 <= done < 64 and 0 <= leases <= 3
        seq = current
        counts['events'] += 1
        if action == 'admit':
            if status:
                assert ticket == 0 and done == leases == 0
                counts['refused'] += 1
            else:
                assert ticket in bindings and ticket not in tickets and done == 0 and leases == 3
                assert source_line == bindings[ticket][1]
                tickets[ticket] = dict(done=0, leases=3, attempted=False, failed=False,
                                       consumer=False, aborted=False)
                counts['admitted'] += 1
            continue
        if action in ('register', 'retire') and ticket == 0:
            assert done == leases == source_line == 0
            continue
        if ticket not in tickets:
            assert status == 1 and done == leases == source_line == 0
            continue
        state = tickets[ticket]
        assert source_line == bindings[ticket][1]
        if action in EVENTS:
            event = EVENTS.index(action)
            if status == 5:
                assert event < 4
                counts['backend_failures'] += 1
            if status in (0, 5):
                assert not state['aborted']
                assert not state['failed'] or event == 2
                if not state['done'] & (1 << event):
                    available = state['done'] | (2 if state['failed'] and event == 2 else 0)
                    required = bindings[ticket][0][event]
                    assert available & required == required
                    if event == 1:
                        assert not state['attempted']
                        state['attempted'] = True
                        state['failed'] = status == 5
                    if event == 3:
                        state['consumer'] = True
                    if status == 0:
                        state['done'] |= 1 << event
                        if event >= 4:
                            state['leases'] &= ~(1 if event == 4 else 2)
                else:
                    assert status == 0
        elif action == 'abort' and status == 0:
            assert not state['attempted'] or state['done'] & 4
            assert not state['consumer'] or state['done'] & 8
            state['aborted'] = True
            state['leases'] = 0
        elif action == 'close' and status == 0:
            assert not state['leases']
            counts['closed'] += 1
        else:
            assert action in ('abort', 'close', 'retire') and status != 0
        assert (done, leases) == (state['done'], state['leases'])
        if action == 'close' and status == 0:
            del tickets[ticket]
    assert seen and bind_seen and seq and counts['admitted'] and not tickets
    assert counts['admitted'] == counts['closed'] == len(bindings)
    return counts


def write_reports(runtime_trace, direct_trace, output):
    """Join source-bound export identity and summarize only observed host proof."""
    from reduction_direct_trace_report import audit as audit_direct
    runtime_trace, direct_trace, output = map(Path, (runtime_trace, direct_trace, output))
    native_text = direct_trace.read_text()
    direct = audit_direct(native_text)[0]
    exports = [line.split()[3:] for line in native_text.splitlines()
               if line.startswith('AI_REDUCE_GUARD 1 EXPORT ')]
    expected = [direct[key] for key in ('node', 'input', 'result', 'contract', 'arch', 'rows', 'width', 'obligations')]
    expected += [0, direct['source_line']]
    assert len(exports) == 1 and list(map(int, exports[0])) == expected
    counts = audit(runtime_trace.read_text())
    for line in runtime_trace.read_text().splitlines():
        if line.startswith('AI_REDUCE_RUN 1 BIND '):
            values = list(map(int, line.split()[3:]))
            assert values[1:3] == [direct['contract'], direct['arch']]
            assert [values[6], values[8], values[-1]] == [direct['in_bytes'], direct['out_bytes'], direct['source_line']]
    output.mkdir(parents=True, exist_ok=False)
    body = ' '.join('%s=%s' % pair for pair in counts.items()) + '\n'
    (output / 'runtime-guard.log').write_text(
        '# What it summarizes: compiler-exported reduction guards, live range leases, event joins, refusals and recovery.\n' +
        body + 'evidence=host_mock source_line=%d selected=0 transformation=none gpu=not_run\n' % direct['source_line'])
    (output / 'optimization-summary.log').write_text(
        '# What it summarizes: completed runtime-enforcement boundary and remaining reduction execution gates.\n' +
        body + 'guard_export=native_whirl runtime_enforcement=host_mock_pass\n'
        'profitability=not_run lowering=not_implemented cuda_adapter=not_implemented native_certification=not_run\n')
    (output / 'optimization.log').write_text(
        '# What it summarizes: successful compiler binding and runtime guard reports, including recorded refusals.\n' +
        (output / 'runtime-guard.log').read_text() + (output / 'optimization-summary.log').read_text())
    return counts
