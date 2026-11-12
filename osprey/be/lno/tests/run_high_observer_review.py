"""Native M2c binding/refusal, detached exploration and isolation evidence.

Design: doc/AI-COMPILER-SEARCH-M2C-OBSERVER-v1.0.md.
Uses existing estimator/providers and ordinary backend scope. No candidate is
materialized, compiled, emitted or reference-tested by the observer.
"""
import argparse
import copy
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess

from high_observer_image_parity import compare_images, dependence_padding


def semantic_algorithm(recipe):
    """Adapt the existing source manifest, retaining every semantic field."""
    result = {key: recipe[key] for key in ('source_pu', 'source_file', 'opt_level', 'threads', 'values', 'operations')}
    result = copy.deepcopy(result)
    result['schema'] = 'open64.high-algorithm.v1'
    result['operations'] = [{key: op[key] for key in ('name', 'version', 'source_node', 'source_line',
                                                    'inputs', 'output', 'attributes')} for op in recipe['operations']]
    return result


def binding(manifest, trace, gpu, model):
    """Join a producer manifest to complete current native source/fact keys."""
    lines = trace.splitlines()
    source = next(line.split('SOURCE ', 1)[1] for line in lines if line.startswith('AI_HIGH_SNAPSHOT SOURCE '))
    facts = next(line.split('FACTS ', 1)[1] for line in lines if line.startswith('AI_HIGH_SNAPSHOT FACTS '))
    numeric, target = re.search(r'AI_HIGH_SNAPSHOT BINDINGS numeric=(\S+) target=(\S+)', trace).groups()
    return dict(schema='open64.lno-high-search.v1', unit=manifest['recipe']['source_file'], function=manifest['function'],
                source_key=source, facts_key=facts, numeric=numeric, host_target=target, allow_reassociate=True,
                algorithm=semantic_algorithm(manifest['recipe']), buffers=manifest['buffers'], gpu=gpu, model=model)


def ledger(trace):
    """Ignore wall-clock accounting while retaining every candidate estimate."""
    return re.findall(r'AI_HIGH_SEARCH 1 CANDIDATE ordinal=(\d+) policy=(\d+) width=(\d+) evaluated=(\w+) eligible=(\w+) cycles=(\d+) reason=([^\n]+)', trace)


def audit_objects(objects, output):
    """Audit the entire detached closure, including the JSON adapter. Disallow
    native allocation/context/emission/process entry points, including losers."""
    names = ('dsl_high_observer', 'dsl_high_observer_json', 'tensor_schedule',
             'tensor_attention_chunk_select', 'tensor_attention_search')
    forbidden = re.compile(r'\b(WN_|New_ST|New_TY|ST_tensor|TY_tensor|Current_|MEM_POOL|Scope_tab|Ty_Table|St_Table|PU_Info|'
                           r'DSL_Builder|DSL_IR|DSL_WN|Save_Str|New_PREG|Common_Option_Groups|Get_Command_Line_Group|VHO_mla_search_test_clock_us|LNO_DSL_observe_high|LNO_DSL_capture|LNO_DSL_revalidate|LNO_DSL_high_bindings|VHO_tensor_build_selected_high|LNO_DSL_apply_selected_high|VHO_tensor_materialize|VHO_tensor_prepare|VHO_tensor_annotate|Write_Global|Write_PU|system\(|fork\(|execv)')
    for name in names:
        result = subprocess.run(['nm', '-uC', str(objects / (name + '.o'))], capture_output=True, text=True, check=True)
        (output / (name + '.undefined.txt')).write_text(result.stdout)
        assert not forbidden.search(result.stdout), name + ': forbidden native dependency'
    # Prove the checker detects a native constructor, not just a zero-row report.
    assert forbidden.search('U WN_Create(OPERATOR, unsigned char, unsigned char, short)')
    return dict(objects=list(names), native_allocation_context_emission_references=0,
                transient_native_creation='excluded_by_detached_object_dependency_closure')


def review(args):
    """Exercise actual High producer -> PREOPT/LNO observer with raw parity."""
    out = args.output.resolve(); out.mkdir(parents=True, exist_ok=False); (out / 'logs').mkdir()
    commands, report = [], dict(status='PASS', contexts={}, negative_cases={}, image_comparisons={}, gpu_execution=False)
    abi = None

    def parity(first, second):
        """Require exact meaningful bytes; record any known CG padding churn."""
        result = compare_images(first, second, abi)
        report['image_comparisons'][first.name + ':' + second.name] = result
        return result['meaningful_bytes_identical']

    base_env = dict(os.environ)
    base_env.pop('OPEN64_HIGH_SEARCH_BINDINGS', None)
    base_env['LD_LIBRARY_PATH'] = ':'.join(map(str, (args.tools / 'be', args.tools / 'wopt', args.lno.parent)))

    def run(label, argv, binding_path=None):
        """Retain exact invocation and diagnostics for each one-PU driver run."""
        env = dict(base_env)
        if binding_path is not None: env['OPEN64_HIGH_SEARCH_BINDINGS'] = str(binding_path)
        result = subprocess.run(list(map(str, argv)), cwd=out, env=env, capture_output=True, text=True, timeout=600)
        for stream in ('stdout', 'stderr'):
            (out / 'logs' / (label + '.' + stream)).write_text(getattr(result, stream))
        commands.append(dict(label=label, argv=list(map(str, argv)), binding=str(binding_path) if binding_path else None,
                             returncode=result.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert result.returncode == 0, label + ': ' + result.stderr[-4000:]

    def backend(label, image, binding_path=None, lno=None, trace='on', enabled='on', level=3, roundoff=2, extra=()):
        """No per-candidate backend calls. Only normal LNO output files may appear."""
        before = set(out.iterdir()); base = out / label; library = lno or args.lno
        argv = [args.tools / 'be/be', '-fB,' + str(image), '-fo,' + str(base) + '.o', '-ft,' + str(base) + '.phase.t',
                '-PHASE:l=on:w=off:c=off:lpath=' + str(library.parent) + ':wpath=' + str(args.tools / 'wopt'),
                '-O' + str(level), '-DSL:ai_opt_trace=' + trace, '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=' + str(roundoff)]
        if enabled is not None: argv.append('-DSL:partition_axis=' + enabled)
        run(label, [*argv, *extra, args.source], binding_path)
        created = {path.name for path in set(out.iterdir()) - before}
        assert created <= {label + '.N', label + '.phase.t'}, (label, created)
        return base.with_suffix('.N'), base.with_suffix('.phase.t').read_text()

    def reopen(label, image):
        """Reopen actual mapped images with both symbol and source references."""
        run(label, [args.b2a, '-st', '-src', image, image.with_suffix('.T')])

    report['object_isolation'] = audit_objects(args.objects, out / 'logs')
    gpu = json.loads(args.gpu.read_text()); model = json.loads(args.model.read_text())
    cases = {}
    for context in ('short', 'long'):
        recipe, before, image, manifest_path = [out / (context + suffix) for suffix in ('.recipe.json', '.before.B', '.after.B', '.high.json')]
        run(context + '-prepare', [args.lowerer, 'prepare', args.previous / 'selected' / context / 'mla.B', recipe, '--verify', '3'])
        run(context + '-high', [args.lowerer, 'high', recipe, before, image, manifest_path])
        reopen(context + '-before', before); reopen(context + '-after', image)
        manifest = json.loads(manifest_path.read_text())
        probe, text = backend(context + '-probe', image)
        assert 'SKIP reason=no_binding_document' in text
        value = binding(manifest, text, gpu, model); path = out / (context + '.binding.json')
        path.write_text(json.dumps(value, indent=2) + '\n')
        observed, trace = backend(context + '-observed', image, path)
        assert 'OBSERVED estimates=12 cache_hits=24 runs=3 complete=true' in trace, trace[-2000:]
        assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in trace
        assert 'applied=false schedule_legality=not_proved' in trace
        abi = {key: int(value) for key, value in re.findall(r'(\w+)=(\d+)', re.search(r'AI_HIGH_SEARCH 1 IMAGE_ABI (.*)', trace)[1])}
        assert len(ledger(trace)) == 12 and parity(observed, probe)
        if context == 'short':
            # Retain a deliberately invalid side artifact; never alter a valid
            # image or hide a change to a meaningful DEP_STRUCT field.
            data = bytearray(observed.read_bytes())
            pad = min(dependence_padding(data, abi)); data[pad - 1] ^= 1
            tamper = out / 'semantic-tamper.bin'; tamper.write_bytes(data)
            try:
                compare_images(observed, tamper, abi)
            except AssertionError:
                report['image_auditor_rejects_semantic_change'] = True
            else:
                raise AssertionError('image auditor accepted a changed Is_Must field')
        repeated, repeat_trace = backend(context + '-repeat', image, path)
        assert ledger(trace) == ledger(repeat_trace) and parity(repeated, observed)
        for suffix, options in (('baseline', dict(lno=args.tools / 'lno/lno.so')), ('off', dict(enabled='off')),
                                ('default', dict(enabled=None)), ('trace-off', dict(trace='off'))):
            other, other_trace = backend(context + '-' + suffix, image, path, **options)
            assert parity(other, observed), suffix
            assert 'AI_HIGH_SEARCH ' not in other_trace
        reopen(context + '-probe-reopen', probe); reopen(context + '-observed-reopen', observed)
        difference = ''.join(difflib.unified_diff(probe.with_suffix('.T').read_text().splitlines(True),
                                                observed.with_suffix('.T').read_text().splitlines(True)))
        (out / (context + '.observer-before-after.diff')).write_text(difference); assert not difference
        # Independent standalone entry to the same reusable estimator. These
        # preparation commands are outside the compiler observer's search loop.
        fused, cooperative, attention, register = [out / (context + '.' + name + '.json') for name in ('fused', 'cooperative', 'attention', 'register')]
        run(context + '-fuse', [args.lowerer, 'optimize', recipe, fused, '--fuse', '--split-width', '128', '--allow-reassociate'])
        run(context + '-cooperate', [args.lowerer, 'cooperate', fused, cooperative, '--allow-reassociate'])
        run(context + '-attention', [args.lowerer, 'fuse-attention', cooperative, attention, '--allow-reassociate'])
        run(context + '-register', [args.lowerer, 'stream-attention', attention, register, '--split-width', '128', '--allow-reassociate'])
        census = out / (context + '.standalone.json')
        run(context + '-standalone', [args.lowerer, 'explore-attention', register, census, '--gpu-profile', args.gpu,
                                     '--cost-profile', args.model, '--allow-reassociate'])
        standalone = json.loads(census.read_text())
        expected = [(str(c['ordinal']), str(c['policy']), str(c['width']), str(c['evaluated']).lower(),
                     str(c['eligible']).lower(), str(c['estimated_cycles']), c['reason']) for c in standalone['candidates']]
        assert ledger(trace) == expected
        assert int(re.search(r'OBSERVED .* winner=(\d+)', trace)[1]) == standalone['winner']
        cases[context] = (image, value, observed, trace)
        report['contexts'][context] = dict(estimates=12, native_state_equal=True, fallback_meaningful_bytes_identical=True,
                                           repeatable=True, standalone_estimator_equal=True, candidate_artifacts=0,
                                           winner=standalone['winner'])

    image, original, normal, trace = cases['short']
    mutations = {
        'stale-source': lambda x: x.update(source_key=x['source_key'] + 'changed'),
        'stale-facts': lambda x: x.update(facts_key=x['facts_key'] + 'changed'),
        'wrong-unit': lambda x: x.update(unit='other-unit'),
        'wrong-numerical-binding': lambda x: x.update(numeric='ieee=3/roundoff=0'),
        'wrong-host-target': lambda x: x.update(host_target='other-abi'),
        'no-manifest-permission': lambda x: x.update(allow_reassociate=False),
        'wrong-function': lambda x: x.update(function='other-pu'),
        'wrong-storage-order': lambda x: x['buffers'].reverse(),
        'wrong-recipe-witness': lambda x: x['algorithm']['values'][0].update(name='different-logical-value'),
        'bad-shape': lambda x: x['algorithm']['values'][0]['shape'].__setitem__(0, 0),
        'gpu-zero-sms': lambda x: x['gpu'].update(sms=0),
        'gpu-bad-quantum': lambda x: x['gpu']['resources'].update(shared_quantum=0),
        'gpu-unsupported-arch': lambda x: x['gpu'].update(architecture='sm_unknown'),
        'model-arch-mismatch': lambda x: x['model'].update(architecture='sm_90a'),
        'model-zero-timing': lambda x: x['model']['parameters'].update(exp_warp_cycles=0),
        'model-missing-timing': lambda x: x['model']['parameters'].pop('exp_warp_cycles'),
        'model-wrong-unit': lambda x: x['model'].update(unit='nanoseconds'),
        'model-unknown-version': lambda x: x['model'].update(schema='open64.mla-cost-profile.v2'),
        'unknown-field': lambda x: x.update(apply_winner=True),
        'numeric-coercion': lambda x: x['gpu'].update(sms='132'),
    }
    for name, mutate in mutations.items():
        value = copy.deepcopy(original); mutate(value); path = out / (name + '.binding.json')
        path.write_text(json.dumps(value) + '\n')
        other, text = backend(name, image, path)
        assert parity(other, normal) and 'AI_HIGH_SEARCH 1 OBSERVED' not in text, name
        reason = re.search(r'AI_HIGH_SEARCH 1 SKIP reason=(\S+)', text)[1]
        assert 'exploration_us=0 estimates=0' in text, name
        report['negative_cases'][name] = reason
    for name, content in (('duplicate-field', json.dumps(original)[:-1] + ',"allow_reassociate":true}'),
                          ('embedded-NUL', json.dumps(original) + '\0'), ('extra-root', json.dumps(original) + '{}'),
                          ('deep-json', '[' * 200 + '0' + ']' * 200)):
        path = out / (name + '.binding.json'); path.write_text(content)
        other, text = backend(name, image, path)
        assert parity(other, normal) and 'SKIP reason=binding_document_invalid' in text
        report['negative_cases'][name] = 'binding_document_invalid'
    other, text = backend('unavailable-document', image, out / 'absent.json')
    assert parity(other, normal) and 'SKIP reason=binding_document_unavailable' in text
    report['negative_cases']['unavailable-document'] = 'binding_document_unavailable'
    other, text = backend('cross-image', image, out / 'long.binding.json')
    assert parity(other, normal) and 'SKIP reason=source_or_context_mismatch' in text
    report['negative_cases']['cross-image'] = 'source_or_context_mismatch'
    old, text = backend('no-producer-witness', args.old_high / 'short.after.B', out / 'short.binding.json')
    assert 'SKIP reason=native_producer_manifest_mismatch' in text
    baseline, _ = backend('old-high-baseline', args.old_high / 'short.after.B', lno=args.tools / 'lno/lno.so')
    assert parity(old, baseline)
    report['negative_cases']['no-producer-witness'] = 'native_producer_manifest_mismatch'
    for level in (0, 1, 2):
        on, text = backend('O' + str(level) + '-on', image, out / 'short.binding.json', level=level)
        off, _ = backend('O' + str(level) + '-off', image, level=level, enabled='off')
        assert 'AI_HIGH_SEARCH ' not in text and parity(on, off)
    strict, text = backend('strict-numerics', image, out / 'short.binding.json', roundoff=0)
    strict_baseline, _ = backend('strict-baseline', image, lno=args.tools / 'lno/lno.so', roundoff=0)
    assert parity(strict, strict_baseline) and 'SKIP reason=epoch_or_numerical_permission' in text
    report['negative_cases']['strict-numerics'] = 'epoch_or_numerical_permission'
    disabled, text = backend('lno-disabled', image, out / 'short.binding.json', extra=['-LNO:opt=0'])
    disabled_baseline, _ = backend('lno-disabled-baseline', image, lno=args.tools / 'lno/lno.so', extra=['-LNO:opt=0'])
    assert 'AI_HIGH_SEARCH ' not in text and parity(disabled, disabled_baseline)

    for name, context, mutate, prefix in (
            ('model-only', 'short', lambda x: x['model']['parameters'].update(launch_cycles=2000), 'MODEL_KEY'),
            ('resource-only', 'long', lambda x: x['gpu'].update(sms=80), 'RESOURCE_KEY')):
        image, value, normal, original_trace = cases[context]; value = copy.deepcopy(value); mutate(value)
        path = out / (name + '.binding.json'); path.write_text(json.dumps(value) + '\n')
        other, text = backend(name, image, path)
        assert 'OBSERVED estimates=12' in text and parity(other, normal)
        assert re.search(r'AI_HIGH_SEARCH 1 ' + prefix + r' (.*)', text)[1] != re.search(r'AI_HIGH_SEARCH 1 ' + prefix + r' (.*)', original_trace)[1]
        assert ledger(text) != ledger(original_trace), name + ' did not affect estimates'
    report.update(level_scope=True, disabled_lno=True, explicit_numerical_permission=True, model_and_resource_updates_rerank_without_IR=True,
                  no_candidate_artifacts=True, lowering_executed_by_observer=False)
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    (out / 'SHA256SUMS').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest() + '  ' + str(p.relative_to(out)) + '\n'
        for p in sorted(out.rglob('*')) if p.is_file()))
    print(json.dumps(dict(status='PASS', contexts=2, estimates_per_context=12,
                          negative_cases=len(report['negative_cases']), native_state='unchanged', GPU=False), indent=2))


def main():
    """Require explicit retained inputs/tools and a fresh evidence directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('tools', 'lno', 'lowerer', 'b2a', 'previous', 'old-high', 'objects', 'source', 'gpu', 'model', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    review(parser.parse_args())


if __name__ == '__main__':
    main()
