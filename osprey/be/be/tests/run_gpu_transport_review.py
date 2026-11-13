"""H1d real helper transport, mismatch/fallback and native-boundary evidence.

Reuses authenticated H1c native inputs and profiles; no profiling or GPU timing.
Design: doc/AI-COMPILER-H1D-DEVICE-HOST-CONTRACTS-v1.0.md.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess


def main():
    """Run actual driver/helper consumers against exact retained native input."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('build', 'previous', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    previous = args.previous.resolve()
    for row in (previous / 'SHA256SUMS').read_text().splitlines():
        digest, name = row.split('  ', 1)
        assert hashlib.sha256((previous / name).read_bytes()).hexdigest() == digest, name
    old = previous / 'backend-qualified'
    tools = args.build / 'osprey/targdir'
    helper = tools / 'ir_tools/tensor_selected_bridge'
    env = dict(os.environ, LD_LIBRARY_PATH=':'.join(str(tools / p) for p in ('be', 'wopt', 'lno', 'whirl2c')))
    env.pop('OPEN64_HIGH_SEARCH_BINDINGS', None)
    commands, report = [], dict(status='RUNNING', production={}, refusals={}, fallback={}, GPU=False)

    def run(label, argv, success=True):
        """Keep complete diagnostics before checking the expected outcome."""
        p = subprocess.run(list(map(str, argv)), cwd=out, env=env, capture_output=True, text=True, timeout=300)
        (out / (label + '.stdout')).write_text(p.stdout)
        (out / (label + '.stderr')).write_text(p.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=p.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert (p.returncode == 0) == success, (label, p.stderr[-4000:])
        return p

    def reopen(path):
        """Independent readers use source-interleaved symbol traces and exact stems."""
        trace = path.with_suffix('.T')
        run(path.stem + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', path, trace])
        return trace

    def backend(label, device=None, mode=None, adapter=helper, profile_mode='base'):
        """Only be_driver owns capture/helper invocation; selected input is fixed."""
        base, destination = out / label, out / (label + '-device')
        argv = [tools / 'be/be', '-fB,' + str(old / 'mla.after.B'), '-fo,' + str(base) + '.o',
                '-ft,' + str(base) + '.phase.t', '-O3',
                '-PHASE:l=on:w=off:c=off:lpath=' + str(tools / 'lno') + ':wpath=' + str(tools / 'wopt'),
                '-DSL:partition_axis=on:ai_opt_trace=on', '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=2',
                '-LNO:high_search=on:high_apply=on:high_search_bindings=' + str(old / 'mla.binding.json')]
        if device:
            argv += ['-DSL:gpu_device=' + device]
            if mode != 'identity':
                profile = old / (device + '-' + profile_mode + '.json')
                argv += ['-DSL:gpu_profile=' + str(profile),
                         '-DSL:gpu_profile_sha256=' + hashlib.sha256(profile.read_bytes()).hexdigest(),
                         '-DSL:gpu_profile_toolchain=fixture-toolchain-v1']
        if mode is not None:
            argv += ['-LNO:high_device_output=' + str(destination) + ':high_device_tool=' + str(adapter)]
        p = run(label, argv + ['/src/osprey/torch2whirl/python/tests/models/absorbed_mla.py'])
        return reopen(base.with_suffix('.N')), destination, p

    def compare(label, a, b):
        """Retain a raw complete native diff, including empty no-op evidence."""
        diff = ''.join(difflib.unified_diff(a.read_text().splitlines(True), b.read_text().splitlines(True), fromfile=str(a), tofile=str(b)))
        (out / (label + '.diff')).write_text(diff)
        assert not diff, label

    for device in ('h20_96gb', 'h20_141gb', None):
        name = device or 'architecture-only'
        baseline, _, _ = backend(name + '-host', device)
        trace, destination, p = backend(name + '-production', device, 'production')
        assert 'AI_HIGH_DEVICE 1 PUBLISHED' in p.stderr
        compare(name + '-host-parity', baseline, trace)
        case = destination / 'pu-0'
        complete = json.loads((case / 'selected.complete.json').read_text())
        assert complete['search_invocations'] == 0 and complete['status'] == 'PASS'
        launch = json.loads((case / 'selected.launch.json').read_text())
        if device:
            requirements = complete['gpu_requirements']
            assert requirements == launch['gpu_requirements']
            assert requirements['device'] == device and requirements['imported']
            assert not requirements['performance_qualified'] and requirements['runtime_budget_reservation_required']
            assert (case / 'device.profile.json').read_bytes() == (old / (device + '-base.json')).read_bytes()
        else:
            assert 'gpu_requirements' not in complete and 'gpu_requirements' not in launch
        for level in ('high', 'mid', 'low'):
            image = case / ('selected.' + level + '.B')
            text = reopen(image).read_text()
            run(name + '-' + level + '-audit', [helper, 'bridge-audit', image, level])
            assert 'Selected High MLA' in text
            assert ('tensor.bridge.device' in text) == bool(device)
            if device:
                run(name + '-' + level + '-device-seal-refusal', [tools / 'ir_tools/tensor_selected_bridge_test',
                    'bridge-audit', image, level, '--test-fault', 'device'], success=False)
        for before, after in (('source', 'selected.high'), ('selected.high', 'selected.mid'), ('selected.mid', 'selected.low')):
            a, b = case / (before + '.B'), case / (after + '.B')
            at, bt = reopen(a), reopen(b)
            (case / (before + '-' + after + '.diff')).write_text(''.join(difflib.unified_diff(at.read_text().splitlines(True), bt.read_text().splitlines(True), fromfile=str(at), tofile=str(bt))))
        ptx = case / 'reopened.ptx'
        run(name + '-ptx-reemit', [helper, 'bridge-ptx', case / 'selected.low.B', ptx])
        assert ptx.read_bytes() == (case / 'selected.ptx').read_bytes()
        report['production'][name] = dict(host_trace_equal=True, native_boundaries=3, search_invocations=0, comments=True, ptx_reemit_equal=True)
        if not device:
            continue
        profile = old / (device + '-base.json')
        digest = hashlib.sha256(profile.read_bytes()).hexdigest()
        for fault, context in [
                ('stripped', []),
                ('device', ['--gpu-context', 'h20_141gb' if device == 'h20_96gb' else 'h20_96gb', profile, digest, 'fixture-toolchain-v1']),
                ('pin', ['--gpu-context', device, profile, '0' * 64, 'fixture-toolchain-v1']),
                ('toolchain', ['--gpu-context', device, profile, digest, 'other-toolchain']),
                *[(kind, ['--gpu-context', device, old / (device + '-' + kind + '.json'),
                           hashlib.sha256((old / (device + '-' + kind + '.json')).read_bytes()).hexdigest(), 'fixture-toolchain-v1'])
                  for kind in ('calibration', 'tight', 'unknown')]]:
            prefix = out / (name + '-refuse-' + fault)
            run(prefix.name, [helper, 'bridge-production', case / 'source.B', old / 'mla.binding.json', prefix, *context], success=False)
            assert not prefix.with_suffix('.high.B').exists() and not prefix.with_suffix('.complete.json').exists()
            report['refusals'][name + '-' + fault] = True
        # A helper that corrupts only its fresh private transport copy must fail
        # its pin and cannot affect the parent/ordinary host compilation.
        wrapper = out / (name + '-tamper.py')
        wrapper.write_text('#!/usr/bin/python3\nimport os,sys\nfrom pathlib import Path\na=sys.argv[1:]\nPath(a[6]).write_text("{}")\nos.execv(' + repr(str(helper)) + ',[' + repr(str(helper)) + ']+a)\n')
        wrapper.chmod(0o700)
        for mode, adapter in [('tamper', wrapper), ('failed', Path('/bin/false')), ('incomplete', Path('/bin/true')), ('missing', out / 'missing-helper')]:
            failed_trace, failed_dir, result = backend(name + '-' + mode, device, 'production', adapter)
            assert 'AI_HIGH_DEVICE 1 REFUSED' in result.stderr
            compare(name + '-' + mode + '-host-parity', baseline, failed_trace)
            assert not list(failed_dir.glob('pu-*/selected.complete.json'))
            report['fallback'][name + '-' + mode] = True
        # Valid unknown imports cannot grant an optimized/device plan.
        _, unknown_dir, result = backend(name + '-unknown', device, 'production', profile_mode='unknown')
        assert 'AI_HIGH_DEVICE 1 SKIP' in result.stderr and not unknown_dir.exists()
        report['fallback'][name + '-unknown'] = True
        _, identity_dir, result = backend(name + '-identity', device, 'identity')
        assert 'AI_HIGH_DEVICE 1 PUBLISHED' in result.stderr
        identity = json.loads((identity_dir / 'pu-0/selected.complete.json').read_text())['gpu_requirements']
        assert not identity['imported'] and not identity['performance_qualified']
        assert not (identity_dir / 'pu-0/device.profile.json').exists()
    report.update(status='PASS', refusal_cases=len(report['refusals']), fallback_cases=len(report['fallback']),
                  input_receipt_sha256=hashlib.sha256((previous / 'SHA256SUMS').read_bytes()).hexdigest())
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
