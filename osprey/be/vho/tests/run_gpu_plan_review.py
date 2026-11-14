"""H1c real backend and detached MLA profile-consumer acceptance evidence.

Reuses authenticated H1b synthetic source/algorithm fixtures. Profiling, GPU
execution and per-candidate compilation remain outside compiler exploration.
Design: doc/AI-COMPILER-H1C-PLANNER-BINDINGS-v1.0.md.
"""
import argparse
import copy
import difflib
from datetime import datetime, timezone
import time
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(REPO / 'osprey/be/com/tests'))
sys.path.insert(0, str(REPO / 'osprey/be/lno/tests'))
from gpu_profile_fixtures import profile, fact, TOOLCHAIN
from run_high_observer_review import binding, ledger, audit_objects
from run_high_search_control_review import controls
from run_common_high_review import execute, check_output


def main():
    """Exercise actual owners, current native scope, isolation and refused outputs."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-dir', type=Path, required=True)
    parser.add_argument('--previous-h1b', type=Path, required=True)
    parser.add_argument('--output-dir', type=Path, required=True)
    args = parser.parse_args()
    build, previous, out = args.build_dir.resolve(), args.previous_h1b.resolve(), args.output_dir.resolve()
    out.mkdir(parents=True, exist_ok=False)
    manifest = {}
    for row in (previous / 'SHA256SUMS').read_text().splitlines():
        expected, name = row.split('  ', 1)
        if hashlib.sha256((previous / name).read_bytes()).hexdigest() != expected:
            raise RuntimeError('retained H1b hash mismatch: ' + name)
        manifest[name] = expected
    fixture = next((previous / 'search-regressions').glob('test_portfolio*/baseline.json')).parent
    recipe = json.loads((fixture / 'baseline.json').read_text())
    gpu = json.loads((fixture / 'search.gpu.json').read_text())
    model = json.loads((fixture / 'search.cost.json').read_text())
    source = REPO / 'osprey/torch2whirl/python/tests/models/absorbed_mla.py'
    commands = []
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE='1', LD_LIBRARY_PATH=':'.join(str(build / 'osprey/targdir' / p) for p in ['be', 'wopt', 'lno', 'whirl2c']))
    tools = build / 'osprey/targdir'

    def run(label, argv, case=out, success=True):
        """Retain complete process evidence, including deliberate user refusals."""
        started = datetime.now(timezone.utc).isoformat()
        begin = time.monotonic()
        result = subprocess.run(list(map(str, argv)), cwd=case, env=env, capture_output=True, text=True, timeout=240)
        ended = datetime.now(timezone.utc).isoformat()
        (case / (label + '.stdout')).write_text(result.stdout)
        (case / (label + '.stderr')).write_text(result.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), cwd=str(case), returncode=result.returncode, expected_success=success,
                             started_utc=started, ended_utc=ended, host_process_elapsed_s=time.monotonic() - begin))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        if (result.returncode == 0) != success:
            raise RuntimeError(label + ': ' + result.stderr[-3000:] + result.stdout[-1000:])
        return result

    def pinned(label, data):
        """Write new synthetic evidence; never mutate retained source profiles."""
        path = out / (label + '.json')
        raw = json.dumps(data, sort_keys=True).encode()
        path.write_bytes(raw)
        return path, hashlib.sha256(raw).hexdigest()

    def options(device, pair):
        """Use the actual shared backend DSL option group, not a test-only route."""
        path, digest = pair
        return ['-DSL:gpu_device=' + device, '-DSL:gpu_profile=' + str(path),
                '-DSL:gpu_profile_sha256=' + digest, '-DSL:gpu_profile_toolchain=' + TOOLCHAIN]

    profiles = {}
    for device in ['h20_96gb', 'h20_141gb']:
        data = profile(device)
        resources = data['resources']['values']
        fields = {'visible_sms': gpu['sms'], 'max_blocks_per_sm': gpu['resources']['max_blocks'],
                  'max_threads_per_sm': gpu['resources']['max_threads'],
                  'max_threads_per_cta': gpu['resources']['max_threads_per_cta'],
                  'registers_per_sm': gpu['resources']['registers_per_sm'],
                  'register_quantum': gpu['resources']['register_quantum'],
                  'shared_bytes_per_sm': gpu['resources']['shared_capacity'],
                  'shared_bytes_per_cta_default': gpu['resources']['shared_budget'],
                  'shared_bytes_per_cta_optin': gpu['resources']['shared_budget'],
                  'shared_quantum': gpu['resources']['shared_quantum']}
        for key, value in fields.items():
            resources[key] = fact(value, resources[key]['unit'])
        base = pinned(device + '-base', data)
        changed = copy.deepcopy(data)
        changed['execution']['values']['hbm_mixed_rate'] = fact(2e10, 'bytes/second', 'estimated')
        calibrated = pinned(device + '-calibration', changed)
        changed = copy.deepcopy(data)
        changed['budget']['usable_bytes'] = fact(1, 'bytes')
        tight = pinned(device + '-tight', changed)
        unknown = pinned(device + '-unknown', profile(device, 'unknown'))
        profiles[device] = [base, calibrated, tight, unknown]

    # Actual WOPT source owners bind imported facts and reject stale handoffs.
    wopt_cases = []
    for device, pairs in profiles.items():
        base_data = json.loads(pairs[0][0].read_text())
        partial_data = copy.deepcopy(base_data)
        partial_data['resources']['values']['visible_sms'] = fact(None, 'count')
        partial = pinned(device + '-partial', partial_data)
        for mode, pair, expected in [('base', pairs[0], 'pass'), ('tight', pairs[2], 'refuse'),
                                     ('unknown', pairs[3], 'refuse'), ('partial', partial, 'partial')]:
            case = out / ('wopt-' + device + '-' + mode)
            case.mkdir()
            argv = [tools / 'ir_tools/dsl_gpu_owner_contract', case / 'source.B', case / 'owner.phase.t',
                    device, *pair, TOOLCHAIN, expected]
            if expected == 'pass':
                argv.extend(pairs[1])
            run('owner', argv, case)
            for path in [case / 'source.B', case / 'source.B.preserved.B']:
                run('reopen-' + path.stem, [tools / 'ir_tools/ir_b2a', '-st', '-src', path, path.with_suffix('.T')], case)
            wopt_cases.append(dict(device=device, mode=mode))

    # Actual mapped GEMM input and output, including no-device parity.
    native_fixture = REPO / 'osprey/be/vho/tests/ai_matmul_cuda/edge_matmul_fixture.py'
    shutil.copyfile(previous / 'backend-review-final/input.B', out / 'input.B')
    accepted, refused = [], []
    for label, device, pair, success in [('generic', None, None, True)] + [
        (device + '-' + mode, device, pairs[i], i < 2)
        for device, pairs in profiles.items() for i, mode in enumerate(['base', 'calibration', 'tight', 'unknown'])]:
        case = out / ('gemm-' + label)
        case.mkdir()
        shutil.copyfile(out / 'input.B', case / 'input.B')
        before = (out / 'input.B').read_bytes()
        if not success:
            (case / 'selected.B').write_bytes(before)
        argv = [tools / 'be/be', '-fB,input.B', '-fo,result.o', '-ft,phase.t',
                '-PHASE:w=on:c=off:wpath=' + str(tools / 'wopt'), '-O3',
                '-DSL:cuda_legacy_emit=OFF', '-DSL:gemm_plan_search=joint', '-DSL:cuda_whirl_output=selected.B']
        result = run('driver', argv + (options(device, pair) if device else []) + [native_fixture], case, success)
        if success:
            run('reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', 'selected.B', 'selected.T'], case)
            if device:
                assert 'DSL_GPU_PLAN 1 family=GEMM-payload' in result.stderr
                assert 'device_performance_qualified=0' in result.stderr
            accepted.append(label)
        else:
            assert (case / 'selected.B').read_bytes() == before
            assert 'DSL_GPU_PLAN' in result.stderr and 'Internal compiler error' not in result.stderr
            refused.append(label)
    for suffix in ['B', 'T']:
        assert (out / 'gemm-generic' / ('selected.' + suffix)).read_bytes() == (
            previous / 'backend-review-final/generic-o3' / ('selected.' + suffix)).read_bytes(), 'legacy generic parity'

    # Fixed tile/fetch reconstruction: generic staging wins, imported SMEM makes
    # only direct demand compatible. Rebuild must preserve the admitted pair.
    pair_root = out / 'gemm-pair'
    pair_root.mkdir()
    run('pair-input', [sys.executable, native_fixture, pair_root / 'input.B', '128', '128', '144'])
    pair_kinds = []
    for label, limit in [('generic', None), ('constrained', 4096)]:
        case = pair_root / label
        case.mkdir()
        shutil.copyfile(pair_root / 'input.B', case / 'input.B')
        extra = []
        if limit:
            data = json.loads(profiles['h20_96gb'][0][0].read_text())
            data['resources']['values']['shared_bytes_per_cta_default'] = fact(limit, 'bytes')
            extra = options('h20_96gb', pinned('pair-constrained', data))
        run('pair-backend', [tools / 'be/be', '-fB,input.B', '-fo,result.o', '-ft,phase.t',
            '-PHASE:w=on:c=off:wpath=' + str(tools / 'wopt'), '-O3', '-tr25', '-tt25:1',
            '-DSL:ai_opt_trace=ON:cuda_legacy_emit=OFF:gemm_plan_search=joint:gemm_cost_model=relative:cuda_tma_prefetch=ON:cuda_smem_layout_search=OFF:cuda_whirl_output=selected.B',
            *extra, native_fixture], case)
        run('pair-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', 'selected.B', 'selected.T'], case)
        trace = (case / 'phase.t').read_text()
        rows = [line.split() for line in trace.splitlines() if line.startswith('AI_OPT 7 JOINT_CAND ')]
        choice = int(next(line.split()[3] for line in trace.splitlines() if line.startswith('AI_OPT 7 JOINT_SELECT ')))
        winner = next(row for row in rows if int(row[3]) == choice)
        pair_kinds.append(winner[-2])
        if limit:
            assert any(row[4] == winner[4] and row[-2] == 'S1' and
                       int(row[16]) < int(winner[16]) and int(row[17]) > limit for row in rows)
    assert pair_kinds == ['S1', 'D'], 'admitted joint fetch was not preserved'
    pair_case = pair_root / 'constrained'
    for stem, original in [('before', pair_root / 'input.B'), ('after', pair_case / 'selected.B')]:
        shutil.copyfile(original, pair_case / (stem + '.B'))
        run('pair-' + stem + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', stem + '.B', stem + '.T'], pair_case)
    (pair_case / 'before-after.diff').write_text(''.join(difflib.unified_diff(
        (pair_case / 'before.T').read_text().splitlines(True), (pair_case / 'after.T').read_text().splitlines(True),
        fromfile='semantic-source.before.T', tofile='created-device.after.T')))


    # Produce genuine common High, then bind its observed native facts.
    recipe_path = out / 'mla.recipe.json'
    recipe_path.write_text(json.dumps(recipe, indent=2) + '\n')
    run('mla-high', [tools / 'ir_tools/tensor_kernel_lower', 'high', recipe_path,
                    out / 'mla.before.B', out / 'mla.after.B', out / 'mla.high.json'])
    for stem in ['mla.before', 'mla.after']:
        run(stem + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', out / (stem + '.B'), out / (stem + '.T')])
    high = json.loads((out / 'mla.high.json').read_text())

    def backend(label, extra):
        """The observer alone explores candidates; driver calls are test cases."""
        argv = [tools / 'be/be', '-fB,' + str(out / 'mla.after.B'), '-fo,' + str(out / (label + '.o')),
                '-ft,' + str(out / (label + '.phase.t')),
                '-PHASE:l=on:w=off:c=off:lpath=' + str(tools / 'lno') + ':wpath=' + str(tools / 'wopt'),
                '-O3', '-DSL:partition_axis=on:ai_opt_trace=on', '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=2']
        run(label, argv + extra + [source])
        return (out / (label + '.phase.t')).read_text()

    probe = backend('mla-probe', [])
    document = binding(high, probe, gpu, model)
    document["schema"] = "open64.lno-high-search.v2"
    document["search_controls"] = controls()
    document_path = out / 'mla.binding.json'
    document_path.write_text(json.dumps(document, indent=2) + '\n')
    for device, pairs in profiles.items():
        argv = [tools / 'lno/dsl_high_device_test', document_path, device]
        for pair in pairs:
            argv.extend(pair)
        long_data = json.loads(pairs[0][0].read_text())
        live = 0
        for i, value in enumerate(recipe['values']):
            if value['storage'] == i:
                size = 1 if value['dtype'] == 'bool' else 4
                for extent in value['shape']:
                    size *= 8193 if extent == 513 else 64 if extent == 5 else extent
                live += size
        long_data['budget']['usable_bytes'] = fact(live, 'bytes')
        argv.extend(pinned(device + '-long-exact-backing', long_data))
        run(device + '-detached', argv)
    generic = backend('mla-generic', ['-LNO:high_search=on:high_search_bindings=' + str(document_path)])
    assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in generic
    reference_results = {}
    for device, pairs in profiles.items():
        trace = backend('mla-' + device, options(device, pairs[0]) + ['-LNO:high_search=on:high_search_bindings=' + str(document_path)])
        assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in trace
        assert ledger(trace) == ledger(generic)
        assert 'AI_HIGH_DEVICE_COST 1' in trace and 'complete_cost=false' in trace
        unknown = backend('mla-' + device + '-unknown', options(device, pairs[3]) + ['-LNO:high_search=on:high_search_bindings=' + str(document_path)])
        assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in unknown
        assert 'SKIP reason=gpu_resources_invalid fallback=ordinary_lno' in unknown
        selected = 'mla-' + device + '-selected'
        selected_trace = backend(selected, options(device, pairs[0]) + [
            '-LNO:high_search=on:high_apply=on:high_search_bindings=' + str(document_path),
            '-LNO:high_apply_checkpoint=' + str(out / selected)])
        assert 'applied=true' in selected_trace
        for boundary in ['before', 'after']:
            path = out / (selected + '.' + boundary + '.B')
            run(selected + '-' + boundary + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', path, path.with_suffix('.T')])
        before_text = (out / (selected + '.before.T')).read_text()
        after_text = (out / (selected + '.after.T')).read_text()
        assert 'tensor.high.selected.device' in after_text
        (out / (selected + '.before-after.diff')).write_text(''.join(difflib.unified_diff(
            before_text.splitlines(True), after_text.splitlines(True), fromfile=selected + '.before.T', tofile=selected + '.after.T')))
        c = out / (selected + '.cu')
        run(selected + '-whirl2c', [tools / 'whirl2c/whirl2c', '-TARG:abi=n64', '-O0',
            '-CLIST:dotc_file=' + str(c), '-CLIST:doth_file=' + str(c.with_suffix('.h')),
            '-ft,' + str(out / (selected + '.w2c.phase.t')), out / (selected + '.after.B')])
        library = out / (selected + '.so')
        run(selected + '-host-compile', ['gcc', '-x', 'c', '-std=c99', '-O2', '-shared', '-fPIC',
            '-ffp-contract=off', '-fno-fast-math', '-I' + str(REPO / 'osprey/include'),
            '-include', REPO / 'osprey/be/lno/tests/common_high_host.h', c, '-lm', '-o', library])
        binary = out / (selected + '.outputs.bin')
        binary.write_bytes(execute(library, high, (fixture / 'native/input.bin').read_bytes()))
        reference_results[device] = check_output(fixture / 'native/reference.json', binary)
    isolated = audit_objects(tools / 'lno', out)
    report = dict(schema='open64.h1c-backend-review.v1', status='PASS', GEMM_accepted=accepted,
                  GEMM_refused=refused, GEMM_pair_preservation=pair_kinds, WOPT_cases=wopt_cases, legacy_B_T_parity=True, MLA_native_variants=2,
                  MLA_detached_variants=2, native_isolation=True, object_isolation=isolated,
                  selected_reference_checks=reference_results,
                  physical_service='separate partial ns component; legacy cycle scenario ranking',
                  device_helper_transport='guarded host fallback; H1d pending', GPU=False)
    report['producer_sha256'] = {name: hashlib.sha256((tools / name).read_bytes()).hexdigest() for name in [
        'be/be', 'be/be.so', 'wopt/wopt.so', 'lno/lno.so', 'ir_tools/ir_b2a', 'ir_tools/tensor_kernel_lower',
        'lno/dsl_high_device_test', 'ir_tools/dsl_gpu_owner_contract', 'whirl2c/whirl2c']}
    report['started_utc'] = commands[0]['started_utc']
    report['ended_utc'] = commands[-1]['ended_utc']
    report['reused_input_receipt'] = hashlib.sha256((previous / 'report.json').read_bytes()).hexdigest()
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
