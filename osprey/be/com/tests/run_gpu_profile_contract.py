"""Compile and exercise the production H1b importer with retained schema cases.

This does not initialize WHIRL or use a GPU. All positive data are deliberately
synthetic fixtures, including the declared-measured metadata test. Design:
doc/AI-COMPILER-H1B-GPU-PROFILE-IMPORT-v1.0.md.
"""
import argparse
import copy
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
from gpu_profile_fixtures import profile, fact, TOOLCHAIN

REPO = Path(__file__).resolve().parents[4]


def run(command, case):
    """Retain complete diagnostics; recoverable sanitizer errors also fail."""
    result = subprocess.run(list(map(str, command)), cwd=case, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=90,
                            env=dict(os.environ, UBSAN_OPTIONS="halt_on_error=1", ASAN_OPTIONS="halt_on_error=1"))
    with (case / 'commands.log').open('a') as out:
        out.write(json.dumps(dict(argv=list(map(str, command)), returncode=result.returncode)) + '\n')
    with (case / 'validation.log').open('a') as out:
        out.write(result.stdout)
    if result.returncode or "runtime error:" in result.stdout or "ERROR: AddressSanitizer" in result.stdout:
        raise RuntimeError(str(case) + ': ' + result.stdout[-2000:])
    return result.stdout


def main():
    """Test exact schema, quantities, bindings, atomic ownership and section keys."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', type=Path, required=True)
    parser.add_argument('--sanitize', action='store_true')
    args = parser.parse_args()
    root = args.output_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    binary = root / 'contract'
    flags = ['-fsanitize=address,undefined', '-g'] if args.sanitize else ['-O2']
    run([os.environ.get('CXX', 'c++'), '-std=c++11'] + flags + [
        '-I', REPO / 'osprey', '-I', REPO / 'osprey/linux/include',
        '-I', REPO / 'osprey/common/com', '-I', REPO / 'osprey/common/targ_info/access',
        '-I', REPO / 'osprey/be/com', '-I', REPO / 'osprey/be/vho',
        REPO / 'osprey/be/com/tests/dsl_gpu_profile_contract.cxx',
        REPO / 'osprey/be/com/dsl_gpu_profile.cxx',
        REPO / 'osprey/common/targ_info/access/dsl_memory_hierarchy.cxx',
        REPO / 'osprey/be/vho/fhe_sha256.cxx', '-o', binary], root)
    rows = []

    def exercise(label, data=None, raw=None, device='h20_96gb', expected='pass',
                 digest=None, toolchain=TOOLCHAIN, mutate=False):
        """Pin each case's exact bytes and validate the native transactional oracle."""
        case = root / label
        case.mkdir()
        payload = raw if raw is not None else json.dumps(profile(device) if data is None else data, sort_keys=True).encode()
        (case / 'profile.json').write_bytes(payload)
        pinned = hashlib.sha256(payload).hexdigest() if digest is None else digest
        command = [binary, case / 'profile.json', pinned, device, toolchain, expected]
        if mutate:
            command.append('mutate')
        text = run(command, case)
        marker = 'PROFILE_ACCEPT ' if expected == 'pass' else 'PROFILE_REFUSED '
        lines = [line for line in text.splitlines() if line.startswith(marker)]
        if len(lines) != 1:
            raise RuntimeError('missing native result: ' + label)
        result = dict(item.split('=', 1) for item in lines[0].split()[1:])
        rows.append(dict(label=label, expected=expected, result=result))
        return result

    for device in ['h20_96gb', 'h20_141gb']:
        for mode in ['estimated', 'resource-only', 'partial', 'unknown', 'mig', 'declared-measured']:
            data = profile(device, mode)
            result = exercise(device + '-' + mode, data, device=device)
            expected_resources = sum(x['value'] is not None for x in data['resources']['values'].values())
            expected_execution = 0 if data['execution'] is None else sum(
                x['value'] is not None for x in data['execution']['values'].values())
            assert int(result['resource_known']) == expected_resources
            assert int(result['execution_known']) == expected_execution
            assert int(result['budget_proved']) == int(mode != 'unknown')
        base = profile(device)
        initial = exercise(device + '-key-baseline', base, device=device)
        changed = copy.deepcopy(base)
        changed['execution']['values']['hbm_read_rate'] = fact(2e10, 'bytes/second', 'estimated')
        calibrated = exercise(device + '-calibration-only-change', changed, device=device)
        assert initial['resource_sha256'] == calibrated['resource_sha256']
        assert initial['budget_sha256'] == calibrated['budget_sha256']
        assert initial['execution_sha256'] != calibrated['execution_sha256']
        assert initial['context_sha256'] != calibrated['context_sha256']
        changed = copy.deepcopy(base)
        changed['budget']['reserved_bytes'] = fact(8192, 'bytes')
        changed['budget']['usable_bytes'] = fact(changed['budget']['limit_bytes']['value'] - 8192, 'bytes')
        budget = exercise(device + '-budget-only-change', changed, device=device)
        assert initial['resource_sha256'] == budget['resource_sha256']
        assert initial['execution_sha256'] == budget['execution_sha256']
        assert initial['budget_sha256'] != budget['budget_sha256']
        exercise(device + '-snapshot-survives-file-mutation', profile(device), device=device, mutate=True)
        other = 'h20_141gb' if device == 'h20_96gb' else 'h20_96gb'
        exercise(device + '-wrong-variant', profile(other), device=device, expected='fail')
        foreign = profile('h200_141gb')
        exercise(device + '-wrong-H200', foreign, device=device, expected='fail')
    # Independent malformed and cross-binding cases. Every refusal retains a prior context.
    mutations = []

    def change(label, path, value=None, delete=False):
        """Make one controlled contract violation without a duplicate parser model."""
        data = profile()
        current = data
        for key in path[:-1]:
            current = current[key]
        if delete:
            del current[path[-1]]
        else:
            current[path[-1]] = value
        mutations.append((label, data))

    change('bad-schema', ['schema'], 'open64.gpu-device-profile.v99')
    change('bad-contract', ['contract'], 'foreign-contract')
    change('bad-root-arch', ['architecture'], 'sm_90a')
    change('bad-device-revision', ['device_revision'], 2)
    change('boolean-device-revision', ['device_revision'], True)
    change('bad-toolchain', ['toolchain'], 'foreign-toolchain')
    change('unknown-root-key', ['extra'], 1)
    change('missing-root-key', ['toolchain'], delete=True)
    change('null-resource-object', ['resources'], None)
    change('bad-resource-device', ['resources', 'device'], 'h200_141gb')
    change('bad-resource-arch', ['resources', 'architecture'], 'sm_100')
    change('bad-partition-kind', ['resources', 'partition_kind'], 'mystery')
    change('empty-partition', ['resources', 'partition_id'], '')
    change('nul-inventory', ['resources', 'inventory_id'], 'fixture\x00hidden')
    change('bad-observation', ['resources', 'observed_at'], '2026-13-50T80:00:00Z')
    change('zero-resource-revision', ['resources', 'revision'], 0)
    change('overflow-resource-revision', ['resources', 'revision'], 1 << 32)
    change('unknown-resource-field', ['resources', 'values', 'invented'], fact(1, 'count'))
    change('missing-resource-field', ['resources', 'values', 'visible_sms'], delete=True)
    change('wrong-byte-unit', ['resources', 'values', 'visible_hbm_bytes', 'unit'], 'GiB')
    change('float-resource', ['resources', 'values', 'visible_sms', 'value'], 2.0)
    change('bool-resource', ['resources', 'values', 'visible_sms', 'value'], True)
    change('overflow-resource', ['resources', 'values', 'visible_sms', 'value'], 1 << 64)
    change('zero-resource', ['resources', 'values', 'visible_sms'], fact(0, 'count'))
    change('unordered-resource', ['resources', 'values', 'visible_sms', 'upper'], 1)
    change('false-unknown', ['resources', 'values', 'visible_sms', 'provenance'], 'unknown')
    change('false-measured-resource', ['resources', 'values', 'visible_sms', 'provenance'], 'measured')
    change('missing-evidence', ['resources', 'values', 'visible_sms', 'evidence'], '')
    change('too-long-evidence', ['resources', 'values', 'visible_sms', 'evidence'], 'x' * 257)
    change('bad-shared-bound', ['resources', 'values', 'shared_bytes_per_cta_optin'], fact(70000, 'bytes'))
    change('bad-thread-bound', ['resources', 'values', 'max_threads_per_cta'], fact(2048, 'count'))
    change('bad-visible-capacity', ['resources', 'values', 'visible_hbm_bytes'], fact(1 << 40, 'bytes'))
    change('execution-as-array', ['execution'], [])
    change('bad-execution-device', ['execution', 'device'], 'h200_141gb')
    change('bad-execution-arch', ['execution', 'architecture'], 'sm_90a')
    change('bad-execution-resource-id', ['execution', 'resource_id'], 'other-resource')
    change('bad-execution-resource-revision', ['execution', 'resource_revision'], 2)
    change('bad-execution-partition', ['execution', 'partition_id'], 'other-partition')
    change('bad-execution-toolchain', ['execution', 'toolchain'], 'foreign-toolchain')
    change('bad-model', ['execution', 'model'], '')
    change('unknown-condition', ['execution', 'conditions', 'other'], 'x')
    change('bad-condition-type', ['execution', 'conditions', 'ecc'], True)
    change('cycle-unit', ['execution', 'values', 'memory_startup_ns', 'unit'], 'reference_cycles')
    change('wrong-rate-scope', ['execution', 'values', 'shared_rate_per_sm', 'unit'], 'bytes/second')
    change('zero-rate', ['execution', 'values', 'hbm_read_rate'], fact(0, 'bytes/second', 'estimated'))
    change('negative-latency', ['execution', 'values', 'memory_startup_ns'], fact(-1, 'nanoseconds', 'estimated'))
    change('bad-rate-bounds', ['execution', 'values', 'hbm_read_rate', 'lower'], 2e10)
    change('resource-provenance-on-rate', ['execution', 'values', 'hbm_read_rate', 'provenance'], 'queried')
    change('bad-budget-id-join', ['budget', 'resource_id'], 'other')
    change('bad-budget-revision-join', ['budget', 'resource_revision'], 2)
    change('bad-budget-partition-join', ['budget', 'partition_id'], 'other')
    change('over-capacity-budget', ['budget', 'limit_bytes'], fact(1 << 40, 'bytes'))
    change('over-reserved-budget', ['budget', 'reserved_bytes'], fact(1 << 30, 'bytes'))
    change('bad-budget-type', ['budget', 'usable_bytes', 'value'], False)
    for label, data in mutations:
        exercise(label, data, expected='fail')
    data = profile(mode='declared-measured')
    data['execution']['conditions']['clock'] = None
    exercise('measured-missing-condition', data, expected='fail')
    data = profile(mode='declared-measured')
    data['execution']['observed_at'] = None
    exercise('measured-missing-observation', data, expected='fail')
    data = profile()
    data['resources']['values']['visible_sms']['provenance'] = 'queried'
    data['resources']['observed_at'] = None
    exercise('queried-missing-observation', data, expected='fail')
    data = profile()
    data['resources']['values']['visible_sms']['provenance'] = 'queried'
    data['resources']['values']['visible_sms']['upper'] = 3
    exercise('queried-not-exact', data, expected='fail')
    data = profile()
    data['resources']['values']['visible_hbm_bytes'] = fact(None, 'bytes')
    data['budget']['limit_bytes'] = fact(None, 'bytes')
    data['budget']['reserved_bytes'] = fact((1 << 64) - 1, 'bytes')
    data['budget']['usable_bytes'] = fact(1, 'bytes')
    exercise('budget-add-overflow', data, expected='fail')
    data = profile()
    data['execution']['values']['memory_startup_ns'] = fact(0, 'nanoseconds', 'estimated')
    exercise('known-zero-latency', data)
    data = profile()
    data['budget']['reserved_bytes'] = fact(0, 'bytes')
    data['budget']['usable_bytes'] = fact(None, 'bytes')
    partial_budget = exercise('unknown-budget-join', data)
    assert partial_budget['budget_proved'] == '0'
    exercise('empty-root-object', {}, expected='fail')
    original = json.dumps(profile(), sort_keys=True).encode()
    exercise('duplicate-root-member', raw=original.replace(b'"contract":', b'"toolchain":', 1), expected='fail')
    exercise('duplicate-fact-member', raw=original.replace(b'"lower":', b'"value":', 1), expected='fail')
    exercise('trailing-document', raw=original + b' {}', expected='fail')
    exercise('raw-nul', raw=original + b'\0', expected='fail')
    exercise('invalid-utf8', raw=original.replace(b'fixture-inventory-only', b'\xff'), expected='fail')
    exercise('file-too-large', raw=b' ' * (256 * 1024) + original, expected='fail')
    exercise('infinite-json', raw=original.replace(b'10000000000.0', b'1e400', 1), expected='fail')
    exercise('wrong-sha256', digest='0' * 64, expected='fail')
    exercise('uppercase-sha256', digest='A' * 64, expected='fail')
    exercise('short-sha256', digest='bad', expected='fail')
    exercise('empty-toolchain', toolchain='', expected='fail')
    report = dict(schema='open64.h1b-native-import-review.v1', status='PASS',
                  cases=rows, accepted=sum(x['expected'] == 'pass' for x in rows),
                  refused=sum(x['expected'] == 'fail' for x in rows),
                  sanitizer=args.sanitize, GPU=False, fixtures='synthetic-only; no hardware facts established')
    (root / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({key: value for key, value in report.items() if key != 'cases'}, indent=2))


if __name__ == '__main__':
    main()
