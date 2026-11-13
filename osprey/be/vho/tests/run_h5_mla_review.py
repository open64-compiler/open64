"""Qualify selected H5 WHIRL against authenticated MLA reference inputs.

Design: doc/AI-COMPILER-H5-MLA-HIERARCHY-v1.0.md. Profiles are explicitly
synthetic fixtures. Search, selected construction and host correctness are
separate operations; this runner never claims GPU correctness or performance.
"""
import argparse
import copy
import difflib
import hashlib
import json
import math
import os
from pathlib import Path
import re
import shutil
import struct
import subprocess
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / 'lno/tests'))
from run_common_high_review import execute
from run_high_movement_review import hierarchy
from run_high_observer_review import binding
from mla_kernel_review import runner
from validate_mla_gpu import check_output


def sha(path):
    """Authenticate exact retained bytes before reusing an input."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def version4(value):
    """Keep archived bindings intact; opt in through a separately sealed schema."""
    value = copy.deepcopy(value)
    value.update(schema='open64.lno-high-search.v4', hierarchy=hierarchy())
    value['hierarchy']['capabilities'] = 'mla-h5-v1:G1=verified-v8;G2,G4=panel16-two-sweep'
    control = value['search_controls']
    control.update(schema='open64.mla-search-controls.v4', space='mla-h5-space-v1-22',
                   global_proposal_limit=66, global_unique_limit=22, patience=22)
    for run in control['runs']:
        run['proposal_limit'] = 22
    return value


def check_trace(trace, batch, heads, length, rank, rope):
    """Independent integer oracle for global cache requests and padded resources.

    Count per real head/token class, without copying the production estimator's
    implementation. Full raw trace is retained; this audit makes no selection.
    """
    rows = []
    for line in trace.splitlines():
        if line.startswith('AI_HIGH_MOVEMENT 1 CANDIDATE '):
            fields, reason = line.rsplit(' reason=', 1)
            row = dict(x.split('=', 1) for x in fields.split()[3:])
            row['reason'] = reason
            rows.append(row)
    assert len(rows) == 22
    stages = []
    for line in trace.splitlines():
        if line.startswith('AI_HIGH_MOVEMENT 1 STAGE '):
            stages.append(dict(x.split('=', 1) for x in line.split()[3:]))
    checked = 0
    for row in rows:
        if row['modeled'] != 'true' or int(row['G']) == 1:
            continue
        group = int(row['G'])
        width = int(row['W']) or length
        assert int(row['cache_requests']) == 4 * batch * ((heads + group - 1) // group) * length * (2 * rank + rope)
        for stage in stages:
            if stage['candidate'] != row['ordinal'] or stage['op'] != '33':
                continue
            assert int(stage['threads']) == 32 * group
            assert int(stage['shared']) == 2048 + 4 * group * ((width + 31) // 32) * 32
            # Enumerate every published panel slot, including geometric zero fill.
            writes = 0
            for dimension in (rank, rope, rank):
                for _token in range(0, width, 32):
                    for _column in range(0, dimension, 16):
                        writes += 512 * 4
            w, h = int(stage['tokens']), int(stage['heads'])
            shared_reads = 4 * w * h * (2 * rank + rope)
            score_slots = 4 * group * ((width + 31) // 32) * 32 * 5
            weight_reads = 4 * w * h * ((rank + 15) // 16)
            assert int(stage['shared_transfer_per_cta']) == writes + shared_reads + score_slots + weight_reads
            checked += 1
    assert checked
    return checked


def main():
    """Run real driver/bridge/reopen/reference boundaries with fresh outputs."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('build', 'previous', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    args = parser.parse_args()
    prior, out = args.previous, args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    for line in (prior / 'SHA256SUMS').read_text().splitlines():
        digest, name = line.split('  ', 1)
        assert sha(prior / name) == digest, name
    source = Path(__file__).resolve().parents[4]
    tools = args.build / 'osprey/targdir'
    env = dict(os.environ, LD_LIBRARY_PATH=':'.join(str(tools / x) for x in ('be', 'wopt', 'lno', 'whirl2c')))
    for name in ('OPEN64_HIGH_SEARCH_BINDINGS', 'OPEN64_HIGH_APPLY_TEST', 'OPEN64_HIGH_REUSE_TEST'):
        env.pop(name, None)
    commands, records = [], []

    def run(label, argv, case, success=True):
        """Preserve each process's exact inputs and diagnostics before checking it."""
        argv = list(map(str, argv))
        result = subprocess.run(argv, cwd=case, env=env, capture_output=True, text=True, timeout=300)
        (case / (label + '.stdout')).write_text(result.stdout)
        (case / (label + '.stderr')).write_text(result.stderr)
        commands.append(dict(label=label, argv=argv, cwd=str(case), returncode=result.returncode, expected_success=success))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert (result.returncode == 0) == success, (label, result.stderr[-3000:])
        return result

    def save(path, value):
        """Publish only fresh test inputs; never alter a retained receipt."""
        path.write_text(json.dumps(value, indent=2) + '\n')
        return path

    def reopen(image, case):
        """Independent mapped readers preserve exact stems and source cross references."""
        run(image.name + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', image, image.with_suffix('.T')], case)
        return image.with_suffix('.T').read_text()

    def backend(case, image, bp, device, model, apply=True, level=3, enabled=True, use_model=True, make_device=False):
        """One bound source search, followed by at most one selected application."""
        case.mkdir(exist_ok=False)
        profile = prior / ('inputs/H1c/' + device + '-base.json')
        args = [tools / 'be/be', '-fB,' + str(image), '-fo,result.o', '-ft,phase.t', '-O' + str(level),
                '-PHASE:l=on:w=off:c=off:lpath=' + str(tools / 'lno') + ':wpath=' + str(tools / 'wopt'),
                '-DSL:partition_axis=on:ai_opt_trace=on', '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=2',
                '-LNO:high_search=' + ('on' if enabled else 'off') + ':high_apply=' + ('on' if apply else 'off') +
                ':high_search_bindings=' + str(bp) + ':high_apply_checkpoint=' + str(case / 'selected'),
                '-DSL:gpu_device=' + device, '-DSL:gpu_profile=' + str(profile),
                '-DSL:gpu_profile_sha256=' + sha(profile), '-DSL:gpu_profile_toolchain=fixture-toolchain-v1']
        if use_model:
            args += ['-DSL:h3_model=' + str(model), '-DSL:h3_model_sha256=' + sha(model)]
        if make_device:
            args += ['-LNO:high_device_output=' + str(case / 'device') + ':high_device_tool=' + str(tools / 'ir_tools/tensor_selected_bridge')]
        run('driver', [*args, source / 'osprey/torch2whirl/python/tests/models/absorbed_mla.py'], case)
        return (case / 'phase.t').read_text()

    for device in ('h20_96gb', 'h20_141gb'):
        model = prior / ('native-corrected/' + device + '.h3.json')
        for context in ('short', 'long'):
            image = prior / ('inputs/MR2/' + context + '.high.B')
            original = json.loads((prior / ('inputs/MR2/' + context + '.v3.json')).read_text())
            bp = save(out / (device + '-' + context + '.v4.json'), version4(original))
            case = out / (device + '-' + context)
            trace = backend(case, image, bp, device, model, make_device=True)
            assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in trace
            assert 'AI_HIGH_APPLY 1 COMMITTED' in trace and 'candidates_materialized=1' in trace
            winner = int(re.search(r'AI_HIGH_SEARCH 2 OBSERVED .*?winner=(\d+)', trace)[1])
            assert winner >= 12, (device, context, winner)
            values = original['algorithm']['values']
            b, length, rank = values[2]['shape']
            heads, rope = values[0]['shape'][1], values[3]['shape'][2]
            count = check_trace(trace, b, heads, length, rank, rope)
            for before in case.rglob('*.before.B'):
                after = before.with_name(before.name.replace('.before.B', '.after.B'))
                a, z = reopen(before, case), reopen(after, case)
                before.with_name(before.name.replace('.before.B', '.before-after.diff')).write_text(''.join(difflib.unified_diff(a.splitlines(True), z.splitlines(True), fromfile=str(before), tofile=str(after))))
            product = case / 'device/pu-0'
            for product_image in product.glob('selected.*.B'):
                reopen(product_image, case)
            for product_image in [case / 'result.N', product / 'source.B']:
                if product_image.exists():
                    reopen(product_image, case)
            launch = json.loads((product / 'selected.launch.json').read_text())
            run('whirl2c', [tools / 'whirl2c/whirl2c', '-cuda', '-TARG:abi=n64', '-O0',
                '-ft,' + str(case / 'whirl2c.phase.t'), '-CLIST:dotc_file=' + str(case / 'kernels.cu'),
                '-CLIST:doth_file=' + str(case / 'kernels.h'), product / 'selected.low.B'], case)
            assert 'H5 CROSS-HEAD PANEL REUSE' in (case / 'kernels.cu').read_text()
            shutil.copyfile(source / 'osprey/be/vho/tests/tensor_collective_stub/cuda_runtime.h', case / 'cuda_runtime.h')
            checks = {}
            for order, reverse in [('host', False), ('reverse', True)]:
                cpp = case / (order + '.cpp')
                cpp.write_text(runner(launch, False, reverse=reverse))
                run(order + '-compile', ['g++', '-std=c++17', '-O1', '-pthread', '-ffp-contract=off', '-fno-fast-math',
                    '-I' + str(case), cpp, '-o', case / order], case)
                for reference in sorted((prior / ('inputs/MR2/' + context)).glob('*.reference.json')):
                    variant = reference.name[:-len('.reference.json')]
                    binary = case / (variant + '.' + order + '.bin')
                    run(variant + '-' + order, [case / order, reference.with_name(variant + '.inputs.bin'), binary], case)
                    checks[variant + '-' + order] = check_output(reference, binary)
                    if reverse:
                        assert binary.read_bytes() == (case / (variant + '.host.bin')).read_bytes()
            assert len(checks) == 14
            records.append(dict(device=device, context=context, winner=winner, reference_checks=checks,
                production_count_oracle_stages=count, native_isolation=True, COMMENT=True, selected_only=True, GPU=False))
            print('PASS', device, context, 'winner', winner, 'references', len(checks), flush=True)
            if device == 'h20_96gb' and context == 'short':
                baseline = out / 'fallback-disabled'
                backend(baseline, image, bp, device, model, apply=False, enabled=False)
                base_text = reopen(baseline / 'result.N', baseline)
                for name, opts in [('no-model', dict(use_model=False)), ('observe', dict(apply=False)),
                                   ('O0', dict(level=0)), ('O1', dict(level=1)), ('O2', dict(level=2))]:
                    lane = out / ('fallback-' + name)
                    text = backend(lane, image, bp, device, model, **opts)
                    assert 'AI_HIGH_APPLY 1 COMMITTED' not in text
                    if name in ('no-model', 'observe'):
                        assert reopen(lane / 'result.N', lane) == base_text
                    if name.startswith('O'):
                        assert 'AI_HIGH_MOVEMENT ' not in text
                limited = version4(original)
                limited['search_controls'].update(global_proposal_limit=1, global_unique_limit=1)
                lane = out / 'one-budget'
                text = backend(lane, image, save(out / 'one-budget.json', limited), device, model)
                assert 'proposals=1 estimates=1' in text
                assert 'winner=0' in text and 'H5 selected G=1' in reopen(lane / 'selected.after.B', lane)
                bad = version4(original)
                bad['hierarchy']['capabilities'] = 'G8=verified'
                lane = out / 'bad-capability'
                text = backend(lane, image, save(out / 'bad-capability.json', bad), device, model)
                assert 'AI_HIGH_APPLY 1 COMMITTED' not in text
                assert reopen(lane / 'result.N', lane) == base_text
    report = dict(status='PASS', cases=records, reference_executions=sum(len(x['reference_checks']) for x in records),
                  finite_catalog=22, compatibility='v1-v3 preserved; separate follow-up lane', GPU=False)
    save(out / 'report.json', report)


if __name__ == '__main__':
    main()
