"""Retain H1b importer/backend parity and early CLI refusals on synthetic inputs.

Requires the configured native builder on PYTHONPATH. Runs no GPU, profiling or
candidate compilation. H1c resource-dependent planning is tested separately;
these import-only cases use O0 so unknown profiles remain valid imports. Design:
doc/AI-COMPILER-H1B-GPU-PROFILE-IMPORT-v1.0.md.
"""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(REPO / 'osprey/be/com/tests'))
from gpu_profile_fixtures import profile, TOOLCHAIN


def run(command, case, success=True):
    """Retain exact actual backend/reopen commands and their expected status."""
    result = subprocess.run(list(map(str, command)), cwd=case, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=240)
    with (case / 'commands.jsonl').open('a') as out:
        out.write(json.dumps(dict(argv=list(map(str, command)), returncode=result.returncode)) + '\n')
    with (case / 'validation.log').open('a') as out:
        out.write(result.stdout)
    if (result.returncode == 0) != success:
        raise RuntimeError(str(case) + ': ' + result.stdout[-2000:])
    return result.stdout


def main():
    """Verify successful imports retain generic output and bad inputs publish nothing."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-dir', type=Path, required=True)
    parser.add_argument('--output-dir', type=Path, required=True)
    args = parser.parse_args()
    build, root = args.build_dir.resolve(), args.output_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    run([sys.executable, REPO / 'osprey/be/com/tests/run_gpu_profile_contract.py',
         '--output-dir', root / 'native-contract'], root)
    fixture = REPO / 'osprey/be/vho/tests/ai_matmul_cuda/edge_matmul_fixture.py'
    run([sys.executable, fixture, root / 'input.B', '3', '5', '7'], root)
    driver, reader = build / 'osprey/targdir/be/be', build / 'osprey/targdir/ir_tools/ir_b2a'
    base = [driver, '-fB,input.B', '-fo,result.o', '-ft,phase.t',
            '-PHASE:w=on:c=off:wpath=' + str(build / 'osprey/targdir/wopt')]
    accepted = []
    baselines = {}
    cases = [('generic-o3', None, None, 3), ('generic-o0', None, None, 0)]
    for device in ['h20_96gb', 'h20_141gb']:
        cases += [(device + '-' + mode, device, mode, 0) for mode in [
            'estimated', 'resource-only', 'partial', 'unknown', 'mig', 'declared-measured']]
        cases.append((device + '-o0', device, 'estimated', 0))
    for label, device, mode, level in cases:
        case = root / label
        case.mkdir()
        shutil.copyfile(root / 'input.B', case / 'input.B')
        options = []
        if device:
            data = profile(device, mode)
            raw = json.dumps(data, sort_keys=True).encode()
            path = case / 'profile.json'
            path.write_bytes(raw)
            digest = hashlib.sha256(raw).hexdigest()
            options = [f'-DSL:gpu_device={device}', '-DSL:gpu_profile=' + str(path),
                       '-DSL:gpu_profile_sha256=' + digest, '-DSL:gpu_profile_toolchain=' + TOOLCHAIN]
        command = base + ['-O' + str(level)] + options
        if level == 3:
            command += ['-DSL:cuda_legacy_emit=OFF', '-DSL:gemm_plan_search=joint',
                        '-DSL:cuda_whirl_output=selected.B']
        text = run(command + [fixture], case)
        lines = [line for line in text.splitlines() if line.startswith('DSL_GPU_PROFILE 1 ')]
        if device:
            if len(lines) != 1:
                raise RuntimeError('profile was not resolved once: ' + label)
            identity = dict(item.split('=', 1) for item in lines[0].split()[2:])
            assert identity['device'] == device and identity['arch'] == 'sm_90'
            assert identity['input_sha256'] == digest and identity['toolchain'] == TOOLCHAIN
            assert identity['planning'] == 'generic-architecture-fallback'
            assert identity['consumer_bindings'] == 'h1c'
            assert int(identity['resource_known']) == (0 if mode == 'unknown' else 13 if mode == 'partial' else 14)
            assert int(identity['execution_known']) == (0 if mode in ['resource-only', 'unknown'] else 14)
        elif lines or 'DSL_GPU_DEVICE 1 ' in text:
            raise RuntimeError('architecture-only reporting changed')
        image = case / ('selected.B' if level == 3 else 'fallback.B')
        if level == 0:
            shutil.copyfile(case / 'result.O', image)
        run([reader, '-st', '-src', image.name, image.with_suffix('.T').name], case)
        current = (image.read_bytes(), image.with_suffix('.T').read_bytes())
        if device is None:
            baselines[level] = current
        else:
            assert baselines[level] == current, 'import changed generic code or COMMENT/metadata'
        accepted.append(label)
    # Real malformed artifacts and missing/mismatched controls must reject before
    # even opening absent input.B or the output destination.
    negative_labels = ['bad-schema', 'bad-resource-device', 'bad-execution-resource-revision',
        'bad-execution-partition', 'wrong-byte-unit', 'cycle-unit', 'over-capacity-budget',
        'duplicate-root-member', 'duplicate-fact-member', 'measured-missing-condition',
        'file-too-large', 'raw-nul', 'invalid-utf8', 'budget-add-overflow']
    refusals = []
    for label in negative_labels + ['wrong-sha', 'wrong-toolchain', 'missing-sha',
        'missing-toolchain', 'no-device', 'orphan-sha', 'orphan-toolchain', 'missing-file']:
        case = root / ('reject-' + label)
        case.mkdir()
        source = root / 'native-contract' / (label if label in negative_labels else 'h20_96gb-estimated') / 'profile.json'
        raw = source.read_bytes()
        path = case / 'profile.json'
        path.write_bytes(raw)
        digest = hashlib.sha256(raw).hexdigest()
        options = ['-DSL:gpu_device=h20_96gb', '-DSL:gpu_profile=' + str(path),
                   '-DSL:gpu_profile_sha256=' + digest, '-DSL:gpu_profile_toolchain=' + TOOLCHAIN]
        if label == 'wrong-sha':
            options[2] = '-DSL:gpu_profile_sha256=' + '0' * 64
        elif label == 'wrong-toolchain':
            options[3] = '-DSL:gpu_profile_toolchain=foreign-toolchain'
        elif label == 'missing-sha':
            del options[2]
        elif label == 'missing-toolchain':
            del options[3]
        elif label == 'no-device':
            del options[0]
        elif label == 'orphan-sha':
            options = [options[0], options[2]]
        elif label == 'orphan-toolchain':
            options = [options[0], options[3]]
        elif label == 'missing-file':
            options[1] = '-DSL:gpu_profile=' + str(case / 'absent.json')
        previous = (root / 'input.B').read_bytes() if label == 'bad-schema' else None
        if previous is not None:
            (case / 'forbidden.B').write_bytes(previous)
        text = run(base + ['-O3', '-DSL:cuda_whirl_output=forbidden.B'] + options + ['absent-source.c'], case, False)
        assert 'DSL_GPU_PROFILE-001:' in text and 'No such file' not in text
        assert not (case / 'phase.t').exists(), 'refusal was after source/phase initialization'
        if previous is None:
            assert not (case / 'forbidden.B').exists()
        else:
            assert (case / 'forbidden.B').read_bytes() == previous
        refusals.append(label)
    report = dict(schema='open64.h1b-backend-review.v1', status='PASS',
        accepted=accepted, early_refusals=refusals, generic_O0_O3_B_T_byte_identical=True,
        existing_checkpoint_preserved=True, consumer_bindings='H1c; import-only O0 cases', GPU=False)
    (root / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    (root / 'SHA256SUMS').write_text(''.join(
        f'{hashlib.sha256(p.read_bytes()).hexdigest()}  {p.relative_to(root)}\n'
        for p in sorted(root.rglob('*')) if p.is_file() and p != root / 'SHA256SUMS'))
    print(json.dumps({key: len(value) if isinstance(value, list) else value for key, value in report.items()}, indent=2))


if __name__ == '__main__':
    main()
