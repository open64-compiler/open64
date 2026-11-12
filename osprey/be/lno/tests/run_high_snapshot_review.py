"""Audit real High snapshot identity/lifetime/invalidation and unchanged LNO.

Design: doc/AI-COMPILER-SEARCH-M2B-OWNED-SNAPSHOT-v1.0.md.
The test LNO component is separately built with LNO_DSL_HIGH_SNAPSHOT_TEST.
No candidate search, schedule application, CUDA generation or GPU claims.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess


def review(args):
    """Use normal backend scope; retain exact tools, commands and mapped views."""
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    (out / 'logs').mkdir()
    commands = []

    def run(label, argv, lno=None):
        """Record command and diagnostics before checking the process result."""
        env = dict(os.environ)
        env['LD_LIBRARY_PATH'] = ':'.join(map(str, (args.tools / 'be', args.tools / 'wopt',
                                                   (lno or args.lno).parent)))
        result = subprocess.run(list(map(str, argv)), cwd=out, env=env,
                                capture_output=True, text=True, timeout=600)
        for stream in ('stdout', 'stderr'):
            (out / 'logs' / (label + '.' + stream)).write_text(getattr(result, stream))
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=result.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert result.returncode == 0, label + ': ' + result.stderr[-4000:]
        return result

    def reopen(label, image):
        """Independently reopen each mapped image with symbol/source evidence."""
        run(label, [args.b2a, '-st', '-src', image, image.with_suffix('.T')])

    def backend(label, image, source, lno=None, tracing='on', enabled='on'):
        """Exercise production or test component under the same normal driver."""
        lno = lno or args.lno
        base = out / label
        argv = [args.tools / 'be/be', '-fB,' + str(image), '-fo,' + str(base) + '.o',
                '-ft,' + str(base) + '.phase.t', '-PHASE:l=on:w=off:c=off:lpath=' + str(lno.parent) +
                ':wpath=' + str(args.tools / 'wopt'), '-O3', '-DSL:ai_opt_trace=' + tracing,
                '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=0']
        if enabled is not None:
            argv.append('-DSL:partition_axis=' + enabled)
        run(label, [*argv, source], lno)
        return base.with_suffix('.N'), base.with_suffix('.phase.t').read_text()

    def facts(text):
        """Exclude activation tokens; compare full owned source and fact keys."""
        return [line for line in text.splitlines() if line.startswith((
            'AI_HIGH_SNAPSHOT SOURCE ', 'AI_HIGH_SNAPSHOT FACTS ', 'AI_HIGH_SNAPSHOT BINDINGS '))]

    run('epoch-build', ['g++', '-std=c++98', '-Wall', '-Wextra', '-Werror',
                       Path(__file__).with_name('dsl_high_epoch_test.cxx'),
                       Path(__file__).parents[1] / 'dsl_high_snapshot.cxx', '-o', out / 'epoch-test'])
    run('epoch-run', [out / 'epoch-test'])
    report = dict(status='PASS', scope='M2b owned High snapshot', cases={}, gpu_execution=False)
    required = {'numeric_type_codes_are_text', 'unchanged_live_facts', 'repeat_identity_and_content', 'live_test_coverage',
                'unannounced_loop_bound_edit', 'unannounced_affine_access_edit',
                'unannounced_DU_completeness_edit', 'unannounced_alias_context_edit',
                'unannounced_alias_answer_edit', 'effective_numerical_control_edit',
                'target_rebind_before_native_access', 'manifest_rebind_before_native_access',
                'model_rebind_before_native_access', 'source_position_does_not_renumber_identity',
                'same_owner_new_activation_rejected', 'cross_PU_rejected',
                'mutation_epoch_before_native_access', 'closed_epoch_before_native_access',
                'refusal_clears_previous_output', 'all_live_edits_restored'}
    for context in ('short', 'long'):
        image = out / (context + '.high.B')
        shutil.copyfile(args.high / (context + '.after.B'), image)
        reopen(context + '-input', image)
        normal, trace = backend(context + '-normal', image, args.source)
        repeat, repeated = backend(context + '-repeat', image, args.source)
        assert len(facts(trace)) == 3 and facts(trace) == facts(repeated), context
        assert 'AI_HIGH_SNAPSHOT 1 REFUSED' not in trace
        assert 'snapshot_current=false reason=legacy_lno_mutation' in trace
        for stage in ('CAPTURE', 'EXPIRE_BEFORE_HOIST', 'OWNED_AFTER_CLEANUP'):
            assert len(re.findall(r'AI_HIGH_SNAPSHOT 1 ' + stage + r' ', trace)) == 1
        assert normal.read_bytes() == repeat.read_bytes()
        tested, tests = backend(context + '-invalidation', image, args.source, args.test_lno)
        passed = set(re.findall(r'AI_HIGH_SNAPSHOT_TEST PASS (\w+)', tests))
        assert required <= passed, (context, required - passed)
        assert normal.read_bytes() == tested.read_bytes(), 'test perturbations did not restore source/analyses'
        reopen(context + '-before', normal)
        reopen(context + '-after', tested)
        diff = ''.join(difflib.unified_diff(normal.with_suffix('.T').read_text().splitlines(True),
                                          tested.with_suffix('.T').read_text().splitlines(True)))
        (out / (context + '.observer-before-after.diff')).write_text(diff)
        assert not diff
        report['cases'][context] = dict(native_tests=sorted(passed), full_content_stable=True,
                                       test_build_output_byte_identical=True, owned_after_cleanup=True,
                                       capture=re.search(r'AI_HIGH_SNAPSHOT 1 CAPTURE .*', trace)[0])

    fixture = Path(__file__).with_name('dsl_high_snapshot.c')
    image = out / 'dsl_high_snapshot.B'
    if args.fixture_image:
        shutil.copyfile(args.fixture_image, image)
    else:
        run('frontend', [args.cc, '-std=c99', '-O0', '-keep', '-c', '-PHASE:c=off',
                         '-OPT:IEEE_arith=3:roundoff=0', fixture])
    reopen('native-fixture', image)
    normal, trace = backend('forms-normal', image, fixture)
    _, repeated = backend('forms-repeat', image, fixture)
    assert facts(trace) == facts(repeated)
    assert len(facts(trace)) == 6, 'two admitted PUs, one refused PU expected'
    assert 'ADMISSION snapshot_dynamic refused' in trace
    assert 'ADMISSION snapshot_extended_constant admitted' in trace
    assert 'AI_HIGH_SNAPSHOT 1 REFUSED reason=unsupported_constant fallback=ordinary_lno' in trace
    identities = re.findall(r'AI_HIGH_SNAPSHOT 1 CAPTURE id=(\S+) epoch=(\S+)', trace)
    assert len(identities) == 2 and len(set(i[0] for i in identities)) == 2
    assert len(set(i[1] for i in identities)) == 2, 'PU activations reused'
    assert any(int(n) > 0 for n in re.findall(r'AI_HIGH_SNAPSHOT 1 CAPTURE .*? edges=(\d+)', trace))
    tested, tests = backend('forms-invalidation', image, fixture, args.test_lno)
    assert 'AI_HIGH_SNAPSHOT_TEST PASS unannounced_dependence_edit' in tests
    assert normal.read_bytes() == tested.read_bytes()
    for label, lno, tracing, enabled in (
            ('baseline', args.tools / 'lno/lno.so', 'on', 'on'),
            ('off', args.lno, 'on', 'off'), ('default', args.lno, 'on', None),
            ('trace-off', args.lno, 'off', 'on')):
        other, text = backend('forms-' + label, image, fixture, lno, tracing, enabled)
        assert normal.read_bytes() == other.read_bytes(), label
        if label != 'baseline':
            assert 'AI_HIGH_SNAPSHOT ' not in text
    report['cases']['ordinary_C'] = dict(snapshot_admitted=2, snapshot_refused=1, form_refused=1, distinct_PU_identities=True,
                                       dependence_invalidation=True, fallback_byte_identical=True,
                                       native_tests=sorted(set(re.findall(r'AI_HIGH_SNAPSHOT_TEST PASS (\w+)', tests))))
    report['native_test_kinds'] = sorted(required | {'unannounced_dependence_edit'})
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    (out / 'SHA256SUMS').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest() + '  ' +
        str(p.relative_to(out)) + '\n' for p in sorted(out.rglob('*')) if p.is_file()))
    print(json.dumps(dict(status='PASS', native_test_kinds=len(report['native_test_kinds']),
                          contexts=2, ordinary_PUs=4, gpu_execution=False), indent=2))


def main():
    """Require explicit production/test tools and immutable input fixtures."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('tools', 'lno', 'test-lno', 'b2a', 'high', 'cc', 'source', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--fixture-image', type=Path)
    review(parser.parse_args())


if __name__ == '__main__':
    main()
