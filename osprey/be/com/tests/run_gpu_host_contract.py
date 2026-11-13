"""H1d C++98 owned transport/runtime requirements test with fatal sanitizers.

Synthetic profiles only; no GPU performance or native mutation claims.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
from gpu_profile_fixtures import profile

REPO = Path(__file__).resolve().parents[4]


def main():
    """Build the production importer/contract and exercise both H20 identities."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', type=Path, required=True)
    parser.add_argument('--sanitize', action='store_true')
    args = parser.parse_args()
    out = args.output_dir.resolve()
    out.mkdir(parents=True, exist_ok=False)
    commands = []

    def run(label, argv):
        """Retain diagnostics before asserting, treating sanitizer text as failure."""
        p = subprocess.run(list(map(str, argv)), capture_output=True, text=True, timeout=120,
                           env=dict(os.environ, UBSAN_OPTIONS='halt_on_error=1', ASAN_OPTIONS='halt_on_error=1'))
        text = p.stdout + p.stderr
        (out / (label + '.log')).write_text(text)
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=p.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        if p.returncode or 'runtime error:' in text or 'ERROR: AddressSanitizer' in text:
            raise RuntimeError(label + ': ' + text[-3000:])

    includes = ['osprey', 'osprey/linux/include', 'osprey/common/com', 'osprey/common/util',
                'osprey/common/targ_info/access', 'osprey/be/com', 'osprey/be/vho']
    sources = ['osprey/be/com/tests/dsl_gpu_host_contract.cxx', 'osprey/be/com/dsl_gpu_profile.cxx',
               'osprey/be/vho/fhe_sha256.cxx', 'osprey/common/targ_info/access/dsl_memory_hierarchy.cxx']
    binary = out / 'contract'
    flags = ['-fsanitize=address,undefined', '-g'] if args.sanitize else ['-O2']
    run('build', [os.environ.get('CXX', 'c++'), '-std=c++98', *flags,
                  *[arg for path in includes for arg in ['-I', REPO / path]], *[REPO / path for path in sources], '-o', binary])
    for device in ['h20_96gb', 'h20_141gb']:
        raw = json.dumps(profile(device), sort_keys=True).encode()
        path = out / (device + '.json')
        path.write_bytes(raw)
        run(device, [binary, device, path, hashlib.sha256(raw).hexdigest()])
    report = dict(schema='open64.h1d-host-contract.v1', status='PASS', devices=2, mismatch_cases=24,
                  calibration_compatibility=True, architecture_only_qualified=False, sanitizer=args.sanitize, GPU=False)
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
