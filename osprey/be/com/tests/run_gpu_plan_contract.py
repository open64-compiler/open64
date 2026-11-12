"""Exercise H1c production owned queries, key invalidation and LNO bindings.

All fixtures are synthetic. No GPU or native candidate transformation occurs.
Design: doc/AI-COMPILER-H1C-PLANNER-BINDINGS-v1.0.md.
"""
import argparse
import copy
import hashlib
import json
import os
from pathlib import Path
import subprocess
from gpu_profile_fixtures import profile, fact

REPO = Path(__file__).resolve().parents[4]


def main():
    """Compile actual importer/snapshot bindings and retain both variant oracles."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', type=Path, required=True)
    parser.add_argument('--sanitize', action='store_true')
    args = parser.parse_args()
    out = args.output_dir.resolve()
    out.mkdir(parents=True, exist_ok=False)
    commands = []

    def run(label, argv):
        """Retain exact commands and complete diagnostics; no silent skips."""
        p = subprocess.run(list(map(str, argv)), capture_output=True, text=True, timeout=120,
                           env=dict(os.environ, UBSAN_OPTIONS="halt_on_error=1", ASAN_OPTIONS="halt_on_error=1"))
        (out / (label + '.log')).write_text(p.stdout + p.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=p.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        if p.returncode or "runtime error:" in p.stdout + p.stderr or "ERROR: AddressSanitizer" in p.stdout + p.stderr:
            raise RuntimeError(label + ': ' + p.stderr[-3000:])

    binary = out / 'contract'
    includes = ['osprey', 'osprey/linux/include', 'osprey/common/com',
                'osprey/common/util', 'osprey/common/targ_info/access', 'osprey/be/com', 'osprey/be/vho']
    sources = ['osprey/be/com/tests/dsl_gpu_plan_contract.cxx', 'osprey/be/com/dsl_gpu_profile.cxx',
               'osprey/be/lno/dsl_high_snapshot.cxx', 'osprey/be/vho/fhe_sha256.cxx',
               'osprey/common/targ_info/access/dsl_memory_hierarchy.cxx']
    flags = ['-fsanitize=address,undefined', '-g'] if args.sanitize else ['-O2']
    run('build', [os.environ.get('CXX', 'c++'), '-std=c++98', *flags,
                  *[arg for path in includes for arg in ['-I', REPO / path]],
                  *[REPO / path for path in sources], '-o', binary])
    for device in ['h20_96gb', 'h20_141gb']:
        base = profile(device)
        variants = [base]
        changed = copy.deepcopy(base)
        changed['execution']['values']['hbm_mixed_rate'] = fact(2e10, 'bytes/second', 'estimated')
        variants.append(changed)
        changed = copy.deepcopy(base)
        changed['budget']['usable_bytes'] = fact(4096, 'bytes')
        variants.append(changed)
        changed = copy.deepcopy(base)
        changed['resources']['values']['max_threads_per_cta'] = fact(64, 'count')
        variants.append(changed)
        variants.append(profile(device, 'unknown'))
        changed = copy.deepcopy(base)
        changed['resources']['revision'] = 2
        changed['execution']['resource_revision'] = changed['budget']['resource_revision'] = 2
        variants.append(changed)
        changed = copy.deepcopy(base)
        changed['resources']['partition_id'] = changed['execution']['partition_id'] = changed['budget']['partition_id'] = 'fixture-other-partition'
        variants.append(changed)
        changed = copy.deepcopy(base)
        changed['resources']['values']['register_quantum'].update(lower=128, upper=512, provenance='estimated')
        variants.append(changed)
        changed = copy.deepcopy(base)
        changed['resources']['values']['registers_per_sm'] = fact(4352, 'registers')
        variants.append(changed)
        argv = [binary, device]
        for i, data in enumerate(variants):
            path = out / (device + '-' + str(i) + '.json')
            raw = json.dumps(data, sort_keys=True).encode()
            path.write_bytes(raw)
            argv += [path, hashlib.sha256(raw).hexdigest()]
        run(device, argv)
    report = dict(schema='open64.h1c-plan-contract.v1', status='PASS', variants=2,
                  profiles=18, physical_units='nanoseconds; no reference_cycles conversion',
                  checks=['owned-lifetime', 'unknown-refusal', 'budget/resource/partition/calibration-invalidation',
                          'LNO-binding-equality', 'physical-rate-effect', 'architecture-only', 'overflow'],
                  sanitizer=args.sanitize, GPU=False)
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
