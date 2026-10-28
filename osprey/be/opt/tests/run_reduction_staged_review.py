"""Retain native staged binding, PREOPT preservation and slice-plan evidence.

Design: doc/AI-COMPILER-REDUCTION-STAGED-BINDING-v0.1.md. No selected lowering.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess
from reduction_binding_trace_report import audit_preopt
from reduction_staged_trace_report import audit, write_reports


def main():
    """Create a fresh eight-case receipt, never replace previous evidence."""
    parser = argparse.ArgumentParser()
    parser.add_argument('--build-dir', required=True, type=Path)
    parser.add_argument('--output-dir', required=True, type=Path)
    args = parser.parse_args()
    output, build = args.output_dir.resolve(), args.build_dir.resolve()
    output.mkdir(parents=True, exist_ok=False)
    (output / 'logs').mkdir()
    commands, cases = [], []
    env = dict(os.environ, LD_LIBRARY_PATH=str(build / 'be') + ':' + str(build / 'wopt'))

    def run(label, command, success=True):
        """Record exact commands and required downstream missing-lowering rejects."""
        command = list(map(str, command))
        result = subprocess.run(command, env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        (output / 'logs' / (label + '.stdout')).write_text(result.stdout)
        (output / 'logs' / (label + '.stderr')).write_text(result.stderr)
        commands.append(dict(label=label, argv=command, returncode=result.returncode, expected_success=success))
        (output / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        if (result.returncode == 0) != success:
            raise RuntimeError(label + ': ' + result.stderr[-2000:] + result.stdout[-1000:])
        return result

    variants = [('hopper-tail', 'sm_90', 2, 3, 17, 8, 'row_major'),
                ('hopper-a-exact', 'sm_90a', 2, 3, 16, 8, 'row_major'),
                ('blackwell-cta-tail', 'sm_100', 2, 33, 17, 8, 'row_major'),
                ('blackwell-a-wide', 'sm_100a', 2, 3, 65, 16, 'row_major'),
                ('unit-clipped', 'sm_90', 2, 1, 1, (1 << 64) - 1, 'row_major'),
                ('single-element-chunks', 'sm_100', 2, 3, 17, 1, 'row_major'),
                ('unbound-v1', 'sm_90', 1, 3, 17, 8, 'row_major'),
                ('unsupported-layout', 'sm_90', 2, 3, 17, 8, 'column_major')]
    source = Path(__file__).with_name('dsl_reduction_staged_test.cxx').resolve()
    for label, arch, version, owners, width, chunk, layout in variants:
        case = output / label
        (case / 'trace').mkdir(parents=True); (case / 'ascii').mkdir()
        result = run(label + '-produce', [build / 'ir_tools/dsl_reduction_staged_test',
            case / 'reduction.B', case / 'trace/staged.t', arch, version, owners, width, chunk, layout])
        assert 'FAIL' not in result.stdout
        assertions = result.stdout.count(' PASS')
        rows, slices = audit((case / 'trace/staged.t').read_text())
        supported = version == 2 and layout == 'row_major'
        assert len(rows) == (42 if supported else 1)
        run(label + '-reopen', [build / 'ir_tools/ir_b2a', '-st', '-src', case / 'reduction.B', case / 'ascii/reduction.T'])
        modes = [('default', 'on', None)]
        if label == 'hopper-tail':
            modes += [('trace-on', 'on', 'on'), ('trace-off', 'on', 'off'), ('preopt-off', 'off', None)]
        for mode, preopt, trace in modes:
            command = [build / 'be/be', '-fB,' + str(case / 'reduction.B'),
                '-fo,' + str(case / (mode + '.obj')), '-ft,' + str(case / 'trace' / (mode + '.t')),
                '-PHASE:w=on:c=off:wpath=' + str(build / 'wopt'), '-O2', '-DSL:wopt=' + preopt,
                '-WOPT:cr_simp=on', '-tr25']
            if trace:
                command.append('-DSL:ai_opt_trace=' + trace)
            result = run(label + '-' + mode, command + [source], success=False)
            assert 'OPR_DSLREDUCESUM.v%d has no executable VHO lowering route' % version in result.stderr
            assert not (case / (mode + '.obj')).exists()
            partial = case / (mode + '.O')
            if partial.exists():
                partial.rename(case / (mode + '.incomplete-mapped-output'))
            raw = (case / 'trace' / (mode + '.t')).read_text()
            if mode in ('trace-off', 'preopt-off'):
                assert 'AI_REDUCE_NUM ' not in raw
            else:
                fact = audit_preopt(raw)
                assert fact['rows'] == str(owners) and fact['width'] == str(width)
        report = write_reports(case / 'trace/staged.t', case / 'trace/default.t', case / 'reports')
        cases.append(dict(case=label, architecture=arch, version=version, rows=owners, width=width,
            chunk=chunk, layout=layout, assertions=assertions, **report))
    for name in ('optimization.log', 'optimization-summary.log'):
        (output / name).write_text('# What it summarizes: all staged-reduction binding review cases.\n' +
            ''.join('case=' + c['case'] + '\n' + (output / c['case'] / 'reports' / name).read_text() for c in cases))
    (output / 'receipt.json').write_text(json.dumps(dict(schema=1, cases=cases, selected=False,
        transformation='none', staged_runtime=False, native_copy_execution=False, gpu=False, mma1_changed=False), indent=2) + '\n')
    print('staged reduction review passed: eight native cases, no selected lowering or GPU claim')


if __name__ == '__main__':
    main()
