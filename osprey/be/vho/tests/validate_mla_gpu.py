"""Run retained WHIRL-derived CUDA launchers against portable FP64 oracles.

Requires Python's standard library, Linux x86_64 launchers, an sm_90-compatible
GPU and CUDA runtime. No PyTorch, model download or kernel recompilation.
Design: doc/AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.1.md.
"""
import argparse
import json
import math
from pathlib import Path
import struct
import subprocess


def check_output(reference, binary):
    """Check all F32 outputs and exact infinity sign against the retained FP64 oracle."""
    oracle = json.loads(Path(reference).read_text())
    if oracle['schema'] != 'open64.mla-output-oracle.v1':
        raise ValueError('unknown MLA oracle schema')
    groups = oracle['values']
    size = sum(map(len, groups))
    raw = Path(binary).read_bytes()
    if len(raw) != size * 4:
        raise ValueError('wrong output byte count')
    values = iter(struct.unpack('<' + str(size) + 'f', raw))
    errors = []
    for group in groups:
        maximum = 0.
        for ref in group:
            got = next(values)
            if isinstance(ref, str):
                if ref not in ('-inf', '+inf') or got != float(ref):
                    raise ValueError('incorrect infinity class/sign')
            else:
                if not math.isfinite(got) or not math.isclose(got, ref, rel_tol=oracle['rtol'], abs_tol=oracle['atol']):
                    raise ValueError('CUDA result differs from FP64 reference')
                maximum = max(maximum, abs(got - ref))
        errors.append(maximum)
    return dict(status='PASS', max_absolute_errors=errors)


def main():
    """Publish a fresh GPU-only report; unavailable devices exit 77, never PASS."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-dir', type=Path, required=True)
    parser.add_argument('--output-dir', type=Path, required=True)
    args = parser.parse_args()
    root, out = args.artifact_dir.resolve(), args.output_dir.resolve()
    out.mkdir(parents=True, exist_ok=False)
    report = dict(status='RUNNING', cases={})
    code = 0
    try:
        for context in ('short', 'long'):
            for level in (0, 3):
                runner = root / context / ('O' + str(level)) / 'gpu_runner'
                label = context + '-O' + str(level)
                probe = subprocess.run([str(runner), '--probe'], capture_output=True, text=True)
                (out / (label + '.probe.stdout')).write_text(probe.stdout)
                (out / (label + '.probe.stderr')).write_text(probe.stderr)
                if probe.returncode == 77:
                    report.update(status='GPU_UNAVAILABLE', reason=probe.stdout.strip())
                    code = 77
                    break
                if probe.returncode:
                    raise RuntimeError('CUDA probe failed: ' + probe.stderr)
                checks = {}
                for variant in ('original', 'one-masked-head', 'all-masked'):
                    path = out / (label + '-' + variant + '.bin')
                    result = subprocess.run([str(runner), str(root / context / (variant + '.inputs.bin')), str(path)],
                                            capture_output=True, text=True)
                    (out / (label + '-' + variant + '.stdout')).write_text(result.stdout)
                    (out / (label + '-' + variant + '.stderr')).write_text(result.stderr)
                    if result.returncode:
                        raise RuntimeError('CUDA launch/execution failed: ' + result.stderr)
                    checks[variant] = check_output(root / context / (variant + '.reference.json'), path)
                report['cases'][label] = checks
            if code == 77:
                break
        if not code:
            report['status'] = 'PASS'
    except Exception as error:
        report.update(status='FAIL', reason=str(error))
        code = 1
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))
    return code


if __name__ == '__main__':
    raise SystemExit(main())
