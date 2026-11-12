"""M3 native production controls, global budgets and convergence acceptance.

Design: doc/AI-COMPILER-SEARCH-M3-CONTROLS-v1.0.md. Uses retained native input
images and the same physical-state guard/dependency audit as M2c. No candidates
are transformed or compiled, including when a global budget has no incumbent.
"""
import argparse
import copy
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess

from high_observer_image_parity import compare_images
from run_high_observer_review import audit_objects, binding, ledger


def controls():
    """Explicit default v2 envelope; no host deadline or economic inference."""
    return dict(schema='open64.mla-search-controls.v2', space='mla-family-width-space-v1', mode='estimate',
                global_proposal_limit=36, global_unique_limit=12, global_time_limit_us=0,
                runs=[dict(id=name, strategy=name, seed=17, proposal_limit=12) for name in ('local', 'random', 'exhaustive')],
                stopping=['exhausted', 'budget'], patience=12, minimum_gain_cycles=0, target_cycles=0, gap_ppm=10000,
                remaining_invocations=0, evaluation_cost_cycles=0, economic_conversion=None)


def fields(text):
    """Parse fixed scalar trace fields; complete framed keys stay opaque."""
    return dict(re.findall(r'(\w+)=(\S+)', text))


def audit_result(text, control):
    """Recompute counters, cache charging, trajectories, union winner and hard
    limits from the actual native ledger, rather than trusting a PASS marker."""
    summary = fields(re.search(r'AI_HIGH_SEARCH 2 OBSERVED (.*)', text)[1])
    events = [fields(line) for line in re.findall(r'AI_HIGH_SEARCH 2 EVENT (.*)', text)]
    runs = [fields(line) for line in re.findall(r'AI_HIGH_SEARCH 2 RUN (.*)', text)]
    candidates = {int(row[0]): row for row in ledger(text)}
    assert len(candidates) == 12 and len(runs) == len(control['runs'])
    assert int(summary['proposals']) == len(events) <= control['global_proposal_limit']
    assert int(summary['estimates']) <= control['global_unique_limit']
    seen, visits, run_best, global_best, global_winner = set(), [set() for _ in runs], [0] * len(runs), 0, None
    hits = 0
    for index, event in enumerate(events):
        run, candidate = int(event['run']), int(event['candidate'])
        row = candidates[candidate]; eligible, cycles = row[4] == 'true', int(row[5])
        assert int(event['proposal']) == index + 1 and candidate not in visits[run]
        visits[run].add(candidate)
        hit = candidate in seen; hits += hit; seen.add(candidate)
        assert event['cache_hit'] == str(hit).lower() and int(event['unique']) == len(seen)
        assert event['eligible'] == str(eligible).lower()
        if eligible:
            run_best[run] = min(run_best[run] or cycles, cycles)
            if not global_best or (cycles, candidate) < (global_best, global_winner):
                global_best, global_winner = cycles, candidate
        assert int(event['run_best']) == run_best[run] and int(event['global_best']) == global_best
    assert len(seen) == int(summary['estimates']) and hits == int(summary['cache_hits'])
    assert sum(row[3] == 'true' for row in candidates.values()) == len(seen)
    assert summary['complete'] == str(len(seen) == 12).lower()
    assert summary['winner'] == ('none' if global_winner is None else str(global_winner))
    for index, run in enumerate(runs):
        assert int(run['proposals']) == len(visits[index]) <= control['runs'][index]['proposal_limit']
        assert int(run['best']) == run_best[index]
    assert summary['applied'] == 'false' and summary['schedule_legality'] == 'not_proved'
    assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in text
    return dict(summary=summary, events=events, runs=runs,
                predicates=re.findall(r'AI_HIGH_SEARCH 2 PREDICATE (\S+)', text))


def review(args):
    """Exercise production flags separately from the legacy environment path."""
    out = args.output.resolve(); out.mkdir(parents=True, exist_ok=False); (out / 'logs').mkdir()
    env = dict(os.environ); env.pop('OPEN64_HIGH_SEARCH_BINDINGS', None)
    commands, report = [], dict(status='PASS', contexts={}, cases={}, refused={}, parity={}, gpu_execution=False)
    abi = None

    def run(label, argv, binding_path=None, pinned=False):
        """Retain exact input/control path, dependencies and diagnostics."""
        local = dict(env); tools = args.pinned_tools if pinned else args.tools
        local['LD_LIBRARY_PATH'] = ':'.join(map(str, (tools / 'be', tools / 'wopt',
            (tools / 'lno') if pinned else args.lno.parent)))
        if binding_path is not None: local['OPEN64_HIGH_SEARCH_BINDINGS'] = str(binding_path)
        result = subprocess.run(list(map(str, argv)), env=local, cwd=out, capture_output=True, text=True, timeout=600)
        for stream in ('stdout', 'stderr'):
            (out / 'logs' / (label + '.' + stream)).write_text(getattr(result, stream))
        commands.append(dict(label=label, argv=list(map(str, argv)), environment_binding=str(binding_path) if binding_path else None,
                             pinned_backend=pinned, returncode=result.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert result.returncode == 0, label + ': ' + result.stderr[-4000:]
        assert 'ambiguous in group' not in result.stderr, result.stderr

    def backend(label, image, path=None, mode='on', partition=None, environment=None, lno=None, pinned=False, level=3, tracing='on', extra=()):
        """Exactly one normal driver per observation; only normal output files."""
        before = set(out.iterdir()); tools = args.pinned_tools if pinned else args.tools
        library = tools / 'lno/lno.so' if pinned else (lno or args.lno)
        base = out / label
        argv = [tools / 'be/be', '-fB,' + str(image), '-fo,' + str(base) + '.o', '-ft,' + str(base) + '.phase.t',
                '-PHASE:l=on:w=off:c=off:lpath=' + str(library.parent) + ':wpath=' + str(tools / 'wopt'),
                '-O' + str(level), '-DSL:ai_opt_trace=' + tracing, '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=2']
        if mode is not None: argv.append('-LNO:high_search=' + mode)
        if path is not None: argv.append('-LNO:high_search_bindings=' + str(path))
        if partition is not None: argv.append('-DSL:partition_axis=' + partition)
        run(label, [*argv, *extra, args.source], environment, pinned)
        assert {p.name for p in set(out.iterdir()) - before} <= {label + '.N', label + '.phase.t'}
        return base.with_suffix('.N'), base.with_suffix('.phase.t').read_text()

    def parity(first, second):
        """Retain every raw padding difference; meaningful bytes must be exact."""
        result = compare_images(first, second, abi); report['parity'][first.name + ':' + second.name] = result

    def reopen(label, image):
        """Inspect actual mapped images, with their own stems and source."""
        run(label, [args.b2a, '-st', '-src', image, image.with_suffix('.T')])

    def write_binding(name, value):
        """Publish explicit bounded test input; no automatic native selection."""
        path = out / (name + '.json'); path.write_text(json.dumps(value, indent=2) + '\n'); return path

    report['object_isolation'] = audit_objects(args.objects, out / 'logs')
    run('pure-control-tests', [args.unit, args.previous / 'observer/short.binding.json', args.previous / 'observer/long.binding.json'])
    assert 'controlled_search_checks=37 PASS' in (out / 'logs/pure-control-tests.stdout').read_text()
    cases = {}
    for context in ('short', 'long'):
        image = out / (context + '.input.B'); shutil.copy2(args.previous / 'observer' / (context + '.after.B'), image); reopen(context + '-input', image)
        probe, text = backend(context + '-probe', image, mode=None, partition='on')
        original = json.loads((args.previous / 'observer' / (context + '.binding.json')).read_text())
        manifest = json.loads((args.previous / 'observer' / (context + '.high.json')).read_text())
        legacy = binding(manifest, text, original['gpu'], original['model'])
        legacy_path = write_binding(context + '.v1', legacy)
        value = copy.deepcopy(legacy); value['schema'] = 'open64.lno-high-search.v2'; value['search_controls'] = controls()
        path = write_binding(context + '.v2', value)
        native, trace = backend(context + '-controlled', image, path)
        parsed = audit_result(trace, value['search_controls'])
        assert parsed['summary']['complete'] == 'true' and parsed['summary']['termination'] == 'declared_space_complete'
        assert [event['run'] for event in parsed['events'][:3]] == ['0', '1', '2']
        abi = {key: int(val) for key, val in re.findall(r'(\w+)=(\d+)', re.search(r'AI_HIGH_SEARCH 1 IMAGE_ABI (.*)', trace)[1])}
        parity(native, probe)
        repeated, repeat_trace = backend(context + '-repeat', image, path)
        repeat = audit_result(repeat_trace, value['search_controls'])
        assert parsed['events'] == repeat['events'] and parsed['predicates'] == repeat['predicates']
        parity(repeated, native)
        old, old_trace = backend(context + '-legacy', image, mode=None, partition='on', environment=legacy_path)
        assert 'OBSERVED estimates=12 cache_hits=24 runs=3 complete=true' in old_trace
        assert ledger(trace) == ledger(old_trace); parity(old, native)
        baseline, baseline_trace = backend(context + '-baseline', image, mode=None, lno=args.tools / 'lno/lno.so')
        assert 'AI_HIGH_SEARCH 2' not in baseline_trace; parity(baseline, native)
        pinned, _ = backend(context + '-pinned', image, mode=None, pinned=True)
        parity(pinned, native)
        quiet, quiet_trace = backend(context + '-quiet', image, path, tracing='off')
        assert 'AI_HIGH_SEARCH' not in quiet_trace; parity(quiet, native)
        reopen(context + '-probe-view', probe); reopen(context + '-controlled-view', native)
        difference = ''.join(difflib.unified_diff(probe.with_suffix('.T').read_text().splitlines(True),
                                                native.with_suffix('.T').read_text().splitlines(True)))
        (out / (context + '.control-before-after.diff')).write_text(difference); assert not difference
        report['contexts'][context] = parsed; cases[context] = (image, value, native, legacy_path)

    def observe_case(name, context, mutate):
        """Audit each supported stopping/cap outcome against its exact controls."""
        image, value, normal, _ = cases[context]; value = copy.deepcopy(value); mutate(value)
        path = write_binding(name, value); result, text = backend(name, image, path)
        parsed = audit_result(text, value['search_controls']); parity(result, normal)
        report['cases'][name] = parsed; return parsed

    observe_case('proposal-cap', 'short', lambda x: x['search_controls'].update(global_proposal_limit=3))
    observe_case('unique-cap', 'long', lambda x: x['search_controls'].update(global_unique_limit=2))
    observe_case('deadline', 'short', lambda x: x['search_controls'].update(global_time_limit_us=1))
    assert 'global_time_budget' in report['cases']['deadline']['predicates']
    for strategy in ('exhaustive', 'random', 'local'):
        observe_case(strategy + '-only', 'short', lambda x, strategy=strategy: x['search_controls'].update(
            runs=[dict(id=strategy, strategy=strategy, seed=29, proposal_limit=12)]))
    observe_case('run-caps', 'long', lambda x: [run.update(proposal_limit=1) for run in x['search_controls']['runs']])
    target = observe_case('target-and-budget', 'short', lambda x: x['search_controls'].update(
        runs=[dict(id='one', strategy='exhaustive', seed=17, proposal_limit=12)], stopping=['target'],
        target_cycles=10**18, global_proposal_limit=1))
    assert {'global_proposal_budget', 'estimated_target_reached'} <= set(target['predicates'])
    plateau = observe_case('plateau', 'short', lambda x: x['search_controls'].update(
        runs=[dict(id='plateau', strategy='exhaustive', seed=17, proposal_limit=12)], stopping=['plateau'], patience=1,
        minimum_gain_cycles=10**9))
    assert plateau['runs'][0]['termination'] == 'empirical_plateau'
    empty = observe_case('no-incumbent', 'long', lambda x: x['search_controls'].update(
        runs=[dict(id='empty', strategy='exhaustive', seed=17, proposal_limit=12)], global_proposal_limit=1))
    assert empty['summary']['winner'] == 'none'

    def composite(value):
        """Compatible model units and declared synthetic conversion evidence."""
        value['model']['parameters']['launch_cycles'] = 10**9
        value['search_controls'].update(runs=[dict(id='composite', strategy='exhaustive', seed=17, proposal_limit=12)],
            stopping=['target', 'relative-gap', 'cost-aware'], target_cycles=10**18, gap_ppm=10**6,
            remaining_invocations=1, evaluation_cost_cycles=10**18, global_proposal_limit=1,
            economic_conversion=dict(schema='open64.search-cost-conversion.v1', host_microseconds=10**9,
                                     reference_cycles_per_microsecond=10**9, provenance='synthetic'))
    composite_result = observe_case('composite', 'short', composite)
    assert {'estimated_target_reached', 'estimator_gap_certificate', 'estimator_benefit_bound'} <= set(composite_result['predicates'])
    cached = observe_case('cache-caps', 'short', lambda x: x['search_controls'].update(
        runs=[dict(id=str(i), strategy='exhaustive', seed=17, proposal_limit=12) for i in range(2)], global_unique_limit=2))
    assert cached['summary']['proposals'] == '3' and cached['summary']['cache_hits'] == '1'

    image, original, normal, legacy_path = cases['short']
    mutations = {
        'zero-proposals': lambda x: x['search_controls'].update(global_proposal_limit=0),
        'zero-unique': lambda x: x['search_controls'].update(global_unique_limit=0),
        'huge-time': lambda x: x['search_controls'].update(global_time_limit_us=2**64-1),
        'no-runs': lambda x: x['search_controls'].update(runs=[]),
        'duplicate-id': lambda x: x['search_controls']['runs'][1].update(id='local'),
        'unknown-provider': lambda x: x['search_controls']['runs'][0].update(strategy='bayesian'),
        'unknown-space': lambda x: x['search_controls'].update(space='other-space'),
        'apply-mode': lambda x: x['search_controls'].update(mode='apply'),
        'confidence-stop': lambda x: x['search_controls'].update(stopping=['probabilistic-confidence']),
        'duplicate-stop': lambda x: x['search_controls'].update(stopping=['budget', 'budget']),
        'missing-target': lambda x: x['search_controls'].update(stopping=['target'], target_cycles=0),
        'economic-no-conversion': lambda x: x['search_controls'].update(stopping=['cost-aware'], remaining_invocations=1, evaluation_cost_cycles=10),
        'coerced-budget': lambda x: x['search_controls'].update(global_unique_limit='2'),
        'unknown-control-field': lambda x: x['search_controls'].update(lower_winner=True),
        'missing-global-field': lambda x: x['search_controls'].pop('global_proposal_limit'),
    }
    for name, mutate in mutations.items():
        value = copy.deepcopy(original); mutate(value); result, text = backend(name, image, write_binding(name, value))
        assert 'AI_HIGH_SEARCH 2 OBSERVED' not in text and 'estimates=0' in text, name
        parity(result, normal); report['refused'][name] = re.search(r'SKIP reason=(\S+)', text)[1]
    for name, kwargs, expected in (
            ('explicit-off', dict(path=out / 'absent.json', mode='off', partition='on', environment=out / 'short.v2.json'), 'search_explicitly_disabled'),
            ('bad-mode', dict(path=out / 'short.v2.json', mode='apply'), 'invalid_search_mode'),
            ('production-v1', dict(path=legacy_path), 'production_controls_required'),
            ('missing-path', dict(), 'no_binding_document')):
        result, text = backend(name, image, **kwargs)
        assert 'SKIP reason=' + expected in text and 'AI_HIGH_SEARCH 2 OBSERVED' not in text
        parity(result, normal); report['refused'][name] = expected
    inert, text = backend('path-alone', image, path=out / 'short.v2.json', mode=None)
    assert 'AI_HIGH_SEARCH' not in text; parity(inert, normal)
    for level in (0, 1, 2):
        on, text = backend('O' + str(level) + '-on', image, out / 'short.v2.json', level=level)
        off, _ = backend('O' + str(level) + '-off', image, mode=None, level=level)
        assert 'AI_HIGH_SEARCH' not in text; parity(on, off)
    disabled, text = backend('lno-disabled', image, out / 'short.v2.json', extra=['-LNO:opt=0'])
    disabled_off, _ = backend('lno-disabled-off', image, mode=None, extra=['-LNO:opt=0'])
    assert 'AI_HIGH_SEARCH' not in text; parity(disabled, disabled_off)
    report.update(pure_service_checks=37, native_artifacts_created_by_search=0, fallback_meaningful_bytes_equal=True,
                  v1_compatibility=True, own_production_gate=True, source_pinned_baseline=True, scopes_preserved=True)
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    (out / 'SHA256SUMS').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest() + '  ' + str(p.relative_to(out)) + '\n'
        for p in sorted(out.rglob('*')) if p.is_file()))
    print(json.dumps(dict(status='PASS', contexts=2, production_cases=len(report['cases']), refused=len(report['refused']),
                          pure_checks=37, native_state='unchanged', GPU=False), indent=2))


def main():
    """Require exact native artifacts, baseline/current tools and fresh output."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('tools', 'pinned-tools', 'lno', 'b2a', 'unit', 'objects', 'previous', 'source', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    review(parser.parse_args())


if __name__ == '__main__':
    main()
