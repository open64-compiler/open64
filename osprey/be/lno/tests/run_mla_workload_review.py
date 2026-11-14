"""Q2 semantic workload inference against retained native Q1 attention graphs.

The adapter supplies explicit invocation witnesses. JSON is review input and
never engine authority. No candidate search, compilation or profiling occurs.
Design: doc/AI-COMPILER-MLA-Q2-WORKLOAD-v1.0.md.
"""
import argparse
import copy
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import torch
from run_quantized_high_review import execute


def review(args):
    """Prove positive/unknown/refused facts and source-compatible masked work."""
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    sys.path.insert(0, str(args.source / 'osprey/torch2whirl/python/tests'))
    from models.mla_quantized_paged import CASES, make_fixture
    from models.mla_quantized_reference import reference
    commands, checks, sources = [], [], {}
    digests = dict((name, sha) for sha, name in
                   (line.split('  ', 1) for line in (args.previous / 'SHA256SUMS').read_text().splitlines()))

    def run(name, case, witness=None, mode=None, reason=None):
        """Separate native processes reopen the genuine retained High image."""
        image = args.previous / 'production' / (case + '-O3') / 'q1.high.after.B'
        assert hashlib.sha256(image.read_bytes()).hexdigest() == digests[str(image.relative_to(args.previous))]
        target = out / (name + '.json')
        argv = [args.lowerer, 'q2-workload', image, target]
        if witness is not None:
            path = out / (name + '.invocation.json')
            path.write_text(json.dumps(witness, indent=2) + '\n')
            argv.append(path)
        elif mode:
            argv.append('-')
        if mode:
            argv.append(mode)
        p = subprocess.run(list(map(str, argv)), capture_output=True, text=True, timeout=180)
        (out / (name + '.stdout')).write_text(p.stdout)
        (out / (name + '.stderr')).write_text(p.stderr)
        commands.append(dict(label=name, argv=list(map(str, argv)), returncode=p.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        if reason:
            assert p.returncode != 0 and reason in p.stderr and not target.exists(), (name, p.stderr)
            checks.append(dict(name=name, result='REFUSED', reason=reason))
            return None
        assert p.returncode == 0, (name, p.stderr)
        f = json.loads(target.read_text())
        assert f['native_state_unchanged'] and f['deterministic'] and not f['search_invoked']
        assert not f['dispatch_eligible'] and not f['engine_buffer_binding']
        checks.append(dict(name=name, result='PASS', phase=f['phase'], workload=f['workload']))
        return f

    def witness(case, facts, phase=None):
        """Witness values come from the source fixture, not its filename alone."""
        model, _, meta = make_fixture(case)
        payload = torch.load(args.previous / 'parent/inputs/review' / case / 'inputs-and-source.pt', weights_only=True)
        inputs = payload['inputs']
        allowed = inputs[9 if meta['storage'] == 'paged' else 6]
        g = facts['geometry']
        requests = []
        for b in range(meta['batch']):
            q, l, a = meta['queries'], meta['length'], meta['append']
            requests.append(dict(request=b, query_begin=b*q, queries=q, history=l-a, valid=l,
                append_tokens=a, window_left=0, phase=phase or meta['phase'], mask_policy='causal',
                update='append' if a else 'construction' if g['source_effect'] == 'construction' else 'none',
                query_positions=list(range(l-q, l)), key_positions=list(range(l)),
                update_positions=list(range(l-a, l)), allowed=allowed[b].flatten().tolist()))
        return dict(schema='open64.mla-workload-invocation.v1', source_key=facts['source_key'],
                    invocation_id='review-' + case, evidence='bound-serving-invocation-v1', requests=requests)

    for case in CASES:
        facts = run(case + '-source', case)
        _, _, meta = make_fixture(case)
        assert facts['geometry']['known']
        assert facts['geometry']['query_capacity'] == meta['queries'] and facts['geometry']['kv_extent'] == meta['length']
        assert facts['phase'] == 'unknown' and facts['has_unknown']
        assert all(s['queries'] == s['valid'] == s['history'] == -1 for s in facts['segments'])
        sources[case] = facts
        bound = witness(case, facts)
        inferred = run(case + '-bound', case, bound)
        assert inferred['phase'] == meta['phase'] and not inferred['has_unknown']
        untrusted = copy.deepcopy(bound)
        untrusted['evidence'] = 'fixture-intent-not-inferred-from-graph'
        assert run(case + '-intent', case, untrusted)['phase'] == 'unknown'
    # Same actual graph and values, two consistent external serving intents.
    case = 'dense_s8_decode'
    assert run('same-graph-prefill-tail', case, witness(case, sources[case], 'prefill'))['phase'] == 'prefill'
    renamed = run('renamed-source', case, mode='rename')
    assert renamed['phase'] == 'unknown' and renamed['geometry'] == sources[case]['geometry']
    unrecognized = run('unrecognized-attention', case, mode='unrecognized')
    assert not unrecognized['geometry']['known'] and unrecognized['phase'] == 'unknown'

    case = 'dense_s8_prefill'
    facts = sources[case]
    baseline = witness(case, facts)
    b, h, q, l = (facts['geometry'][key] for key in ('batch', 'heads', 'query_capacity', 'kv_extent'))

    def resize(w, counts, lengths, phases):
        """Dense padded source remains unchanged; unused query rows are masked."""
        total = 0
        for i, r in enumerate(w['requests']):
            n, valid = counts[i], lengths[i]
            r.update(query_begin=total, queries=n, history=valid, valid=valid, phase=phases[i],
                     query_positions=list(range(valid-n, valid)), key_positions=list(range(valid)))
            mask = torch.zeros(h, q, l, dtype=torch.bool)
            for j in range(n):
                mask[:, j, :r['query_positions'][j]+1] = True
            mask[-1] = False
            r['allowed'] = mask.flatten().tolist()
            total += n
        return w

    scenarios = {
        'mixed-prefill-decode': resize(copy.deepcopy(baseline), [1, 3], [l, l], ['decode', 'prefill']),
        'ragged-prefix-chunks': resize(copy.deepcopy(baseline), [1, 3], [5, l], ['prefill', 'prefill']),
        'zero-segment': resize(copy.deepcopy(baseline), [0, 3], [l, l], ['unknown', 'prefill']),
        'zero-all': resize(copy.deepcopy(baseline), [0, 0], [l, l], ['unknown', 'unknown']),
        'zero-logical-length': resize(copy.deepcopy(baseline), [0, 0], [0, 0], ['unknown', 'unknown']),
        'short-history-decode': resize(copy.deepcopy(baseline), [1, 1], [3, 3], ['decode', 'decode']),
        'longer-history-decode': resize(copy.deepcopy(baseline), [1, 1], [l, l], ['decode', 'decode']),
    }
    results = {name: run(name, case, w) for name, w in scenarios.items()}
    assert results['mixed-prefill-decode']['phase'] == 'mixed' and results['mixed-prefill-decode']['ragged']
    assert results['ragged-prefix-chunks']['phase'] == 'prefill' and results['ragged-prefix-chunks']['ragged']
    assert results['zero-segment']['phase'] == 'prefill' and results['zero-segment']['segments'][0]['phase'] == 'unknown'
    assert results['zero-segment']['ragged']
    assert results['zero-all']['zero_active_queries'] and results['zero-all']['phase'] == 'unknown'
    assert results['zero-logical-length']['zero_active_queries'] and results['zero-logical-length']['segments'][0]['valid'] == 0
    assert results['short-history-decode']['phase'] == results['longer-history-decode']['phase'] == 'decode'
    partial = copy.deepcopy(scenarios['mixed-prefill-decode'])
    partial['requests'][1]['phase'] = 'unknown'
    assert run('partial-phase-unknown', case, partial)['phase'] == 'unknown'
    speculation = copy.deepcopy(baseline)
    for r in speculation['requests']:
        r['phase'] = 'speculative-verify'
    assert run('multi-query-verification', case, speculation)['phase'] == 'speculative-verify'
    opaque = copy.deepcopy(baseline)
    for r in opaque['requests']:
        r.update(phase='unknown', mask_policy='opaque')
    assert run('opaque-mask-unknown', case, opaque)['phase'] == 'unknown'
    window = copy.deepcopy(baseline)
    for r in window['requests']:
        r.update(mask_policy='causal-window', window_left=4)
        mask = torch.tensor(r['allowed']).reshape(h, q, l)
        for j, pos in enumerate(r['query_positions']):
            mask[:, j, :max(0, pos-3)] = False
        r['allowed'] = mask.flatten().tolist()
    assert run('causal-window', case, window)['phase'] == 'prefill'

    negatives = {}
    def bad(name, reason, mutate):
        w = copy.deepcopy(baseline)
        mutate(w)
        negatives[name] = (w, reason)
    bad('stale-source', 'q2_stale_source_binding', lambda w: w.update(source_key='stale'))
    bad('unknown-version', 'Q2 schema version', lambda w: w.update(schema='open64.mla-workload-invocation.v2'))
    bad('decode-multi-query', 'q2_decode_query_count', lambda w: w['requests'][0].update(phase='decode'))
    bad('boolean-length', 'Q2 bounded integer required', lambda w: w['requests'][0].update(valid=True))
    bad('negative-length', 'Q2 bounded integer required', lambda w: w['requests'][0].update(history=-1))
    bad('capacity-overflow', 'q2_length_domain', lambda w: w['requests'][0].update(valid=l+1))
    bad('reordered-positions', 'q2_position_order', lambda w: w['requests'][0]['query_positions'].reverse())
    bad('segment-overlap', 'q2_query_segment', lambda w: w['requests'][1].update(query_begin=0))
    bad('missing-request', 'q2_request_extent', lambda w: w['requests'].pop())
    bad('missing-mask', 'q2_mask_extent', lambda w: w['requests'][0]['allowed'].pop())
    bad('integer-mask', 'Q2 mask boolean required', lambda w: w['requests'][0]['allowed'].__setitem__(0, 1))
    bad('future-visible-key', 'q2_mask_future_key', lambda w: w['requests'][0]['allowed'].__setitem__(l-1, True))
    bad('opaque-phase', 'q2_phase_without_visibility_contract', lambda w: w['requests'][0].update(mask_policy='opaque'))
    bad('unexpected-update', 'q2_unexpected_append', lambda w: w['requests'][0].update(append_tokens=1, update_positions=[l-1]))
    bad('unknown-field', 'Q2 field census', lambda w: w.update(assume_decode=True))
    bad('history-after-valid', 'q2_length_domain', lambda w: w['requests'][0].update(history=l+1))
    bad('queries-over-capacity', 'q2_query_segment', lambda w: w['requests'][0].update(queries=q+1))
    bad('query-outside-cache-positions', 'q2_query_outside_key_positions', lambda w: w['requests'][0]['query_positions'].__setitem__(-1, l+1))
    for name, (w, reason) in negatives.items():
        run(name, case, w, reason=reason)
    for name, mutate, reason in (
            ('append-history', lambda w: w['requests'][0].update(history=0), 'q2_append_length'),
            ('append-order', lambda w: w['requests'][0].update(update_positions=[0, 1, 2]), 'q2_append_order'),
            ('append-unknown-phase', lambda w: w['requests'][0].update(update='indexed-unknown-order'), 'q2_phase_unknown_update_order')):
        c = 'append_u4_verify'
        w = witness(c, sources[c])
        mutate(w)
        run(name, c, w, reason=reason)
    c = 'append_u4_verify'
    w = witness(c, sources[c])
    for r in w['requests']:
        r.update(update='indexed-unknown-order', phase='unknown')
    assert run('indexed-update-unknown', c, w)['phase'] == 'unknown'

    # Inference must not cause masked/no-work requests to skip source effects.
    # Execute the retained, unchanged source-compatible Q1 implementation on
    # these masks; active/ragged labels grant no new materializer permission.
    prior = args.previous / 'production' / (case + '-O3')
    manifest = json.loads((prior / 'q1.high.after.manifest.json').read_text())
    model, inputs, meta = make_fixture(case)
    for name, w in scenarios.items():
        modified = [x.clone() for x in inputs]
        modified[6] = torch.stack([torch.tensor(r['allowed']).reshape(h, q, l) for r in w['requests']])
        actual = execute(prior / 'q1.high.after.so', manifest, modified)
        with torch.no_grad():
            expected = model(*modified)
        oracle = reference([x.tolist() for x in modified], meta)
        for i, (a, e, ref) in enumerate(zip(actual, expected, oracle)):
            torch.testing.assert_close(a, e, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
            o = torch.tensor(ref, dtype=torch.float64 if i < 2 else a.dtype)
            torch.testing.assert_close(a.double() if i < 2 else a, o, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
    report = dict(status='PASS', source_cases=len(CASES), checks=checks, masked_execution_cases=len(scenarios),
                  masked_outputs=6*len(scenarios), native_state_unchanged=True, search_invoked=False,
                  engine_adapter=False, gpu_execution=False)
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print('PASS Q2 checks=' + str(len(checks)) + '; masked execution cases=' + str(len(scenarios)))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('source', 'previous', 'lowerer', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    review(parser.parse_args())
