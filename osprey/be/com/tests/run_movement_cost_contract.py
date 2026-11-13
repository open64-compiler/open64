"""H3 actual physical model/cache/importer with synthetic profiles and sanitizers."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
from gpu_profile_fixtures import profile

REPO = Path(__file__).resolve().parents[4]


def main():
    """Compile real C++98 production services; retain exact commands and failures."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', type=Path, required=True)
    parser.add_argument('--sanitize', action='store_true')
    args = parser.parse_args()
    out = args.output_dir.resolve()
    out.mkdir(parents=True, exist_ok=False)
    commands = []

    def run(label, argv):
        """Reject fatal sanitizer errors even when an external tool returns zero."""
        p = subprocess.run(list(map(str, argv)), capture_output=True, text=True, timeout=120,
                           env=dict(os.environ, ASAN_OPTIONS='halt_on_error=1', UBSAN_OPTIONS='halt_on_error=1'))
        text = p.stdout + p.stderr
        (out / (label + '.log')).write_text(text)
        commands.append(dict(argv=list(map(str, argv)), returncode=p.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert not p.returncode and 'runtime error:' not in text and 'ERROR: AddressSanitizer' not in text, (label, text[-5000:])
        return text

    includes = ['osprey', 'osprey/linux/include', 'osprey/common/com', 'osprey/common/util',
                'osprey/common/targ_info/access', 'osprey/be/com', 'osprey/be/vho']
    sources = ['osprey/be/com/tests/dsl_movement_cost_contract.cxx', 'osprey/be/vho/dsl_movement_cost.cxx',
               'osprey/be/vho/dsl_movement_cost_import.cxx', 'osprey/be/com/dsl_movement_facts.cxx',
               'osprey/be/com/dsl_gpu_profile.cxx', 'osprey/be/vho/fhe_sha256.cxx',
               'osprey/common/targ_info/access/dsl_memory_hierarchy.cxx']
    flags = ['-fsanitize=address,undefined', '-g'] if args.sanitize else ['-O2']
    binary = out / 'contract'
    run('build', [os.environ.get('CXX', 'c++'), '-std=c++98', *flags,
                  *[arg for path in includes for arg in ['-I', REPO / path]], *[REPO / path for path in sources], '-o', binary])
    checks = 0
    for device in ['h20_96gb', 'h20_141gb']:
        data = json.dumps(profile(device), sort_keys=True).encode()
        path = out / (device + '.gpu.json')
        path.write_bytes(data)
        prefix = [binary, device, path, hashlib.sha256(data).hexdigest()]
        run(device, prefix + ['test'])
        binding = run(device + '-binding', prefix + ['binding']).split('BINDING ')[1].strip()
        model = dict(schema='open64.movement-cost-model.v1', id='synthetic-h3-fixture', device_binding=binding,
                     provenance='synthetic', objective='worst', transaction_bytes=32, queue_per_sm=4,
                     warm_hit_ppm=500000, contention_ppm=2000000, issue_per_sm=1e10, barrier_ns=10,
                     shuffle_ns=1, fence_ns=2, rsqrt_ns=3, spill_request_bytes_per_cta=0, site_limit=128, request_limit=1024,
                     memory_limit=67108864, deadline_us=60000000)
        for mutation in range(14):
            changed = model.copy()
            if mutation == 1:
                changed['device_binding'] += 'other'
            if mutation == 2:
                changed['site_limit'] = 129
            if mutation == 3:
                changed['request_limit'] = 1025
            if mutation == 4:
                changed['memory_limit'] = 67108865
            if mutation == 5:
                changed['issue_per_sm'] = None
            if mutation == 6:
                changed['objective'] = 'global-optimum'
            if mutation == 7:
                changed['unknown'] = 1
            if mutation == 8:
                del changed['barrier_ns']
            if mutation == 9:
                changed['queue_per_sm'] = -1
            if mutation == 10:
                changed['schema'] = 'open64.movement-cost-model.v0'
            if mutation == 12:
                changed['spill_request_bytes_per_cta'] = None
            if mutation == 13:
                changed['deadline_us'] = 0
            raw = json.dumps(changed, sort_keys=True).encode()
            if mutation == 11:
                raw = raw[:-1] + b',"id":"duplicate"}'
            config = out / (device + '-' + str(mutation) + '.model.json')
            config.write_bytes(raw)
            digest = hashlib.sha256(raw).hexdigest()
            run(device + '-import-' + str(mutation), prefix + ['import', config, digest, int(mutation == 0)])
            checks += 1
        config = out / (device + '-0.model.json')
        run(device + '-bad-pin', prefix + ['import', config, '0' * 64, 0])
        checks += 1
    report = dict(status='PASS', variants=2, importer_cases=checks, independent_traffic_and_ns_oracles=True,
                  capacity_queue_depth_counterexamples=True, finite_budgets_and_shared_cache=True,
                  source_facts_unchanged=True, sanitizer=args.sanitize, GPU=False)
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
