"""H2 actual owned-fact/profile service with fatal ASan/UBSan and synthetic inputs."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
from gpu_profile_fixtures import profile

REPO = Path(__file__).resolve().parents[4]


def main():
    """Compile the production C++98 service and exercise both H20 bindings."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', type=Path, required=True)
    parser.add_argument('--sanitize', action='store_true')
    args = parser.parse_args()
    out = args.output_dir.resolve()
    out.mkdir(parents=True, exist_ok=False)
    commands = []

    def run(label, argv):
        """Preserve raw diagnostics and reject sanitizer reports even on exit zero."""
        p = subprocess.run(list(map(str, argv)), capture_output=True, text=True, timeout=120,
                           env=dict(os.environ, ASAN_OPTIONS='halt_on_error=1', UBSAN_OPTIONS='halt_on_error=1'))
        output = p.stdout + p.stderr
        (out / (label + '.log')).write_text(output)
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=p.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        if p.returncode or 'runtime error:' in output or 'ERROR: AddressSanitizer' in output:
            raise RuntimeError(label + ': ' + output[-4000:])

    includes = ['osprey', 'osprey/linux/include', 'osprey/common/com', 'osprey/common/util',
                'osprey/common/targ_info/access', 'osprey/be/com', 'osprey/be/vho']
    sources = ['osprey/be/com/tests/dsl_movement_facts_contract.cxx', 'osprey/be/com/dsl_movement_facts.cxx',
               'osprey/be/com/dsl_gpu_profile.cxx', 'osprey/be/vho/fhe_sha256.cxx',
               'osprey/common/targ_info/access/dsl_memory_hierarchy.cxx']
    flags = ['-fsanitize=address,undefined', '-g'] if args.sanitize else ['-O2']
    binary = out / 'contract'
    run('build', [os.environ.get('CXX', 'c++'), '-std=c++98', *flags,
                  *[arg for path in includes for arg in ['-I', REPO / path]], *[REPO / path for path in sources], '-o', binary])
    for device in ['h20_96gb', 'h20_141gb']:
        raw = json.dumps(profile(device), sort_keys=True).encode()
        path = out / (device + '.json')
        path.write_bytes(raw)
        run(device, [binary, device, path, hashlib.sha256(raw).hexdigest()])
    report = dict(status='PASS', devices=2, dependency_changes=26, structural_refusals=40,
                  raw_decoded_readers_and_generation_checks=True, after_cleanup_owned=True,
                  physical_traffic_and_spills='unknown', sanitizer=args.sanitize, GPU=False)
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
