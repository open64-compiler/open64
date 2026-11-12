"""H2 actual GEMM/LNO captures with authenticated inputs and no-op native parity.

Profiling/timing and reference executions remain outside candidate exploration.
Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md.
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
    """Exercise real source owners, profile bindings and retained-source parity."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['build', 'previous_h1c', 'previous_h1d', 'output']:
        parser.add_argument('--' + name.replace('_', '-'), type=Path, required=True)
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    old = args.previous_h1c.resolve()
    prior = args.previous_h1d.resolve()
    for receipt in [old, prior]:
        for row in (receipt / 'SHA256SUMS').read_text().splitlines():
            expected, name = row.split('  ', 1)
            assert hashlib.sha256((receipt / name).read_bytes()).hexdigest() == expected, name
    source = Path('/src')
    tools = args.build / 'osprey/targdir'
    root = old / 'backend-qualified'
    env = dict(os.environ, LD_LIBRARY_PATH=':'.join(str(tools / p) for p in ['be', 'wopt', 'lno', 'whirl2c']))
    env.pop('OPEN64_HIGH_SEARCH_BINDINGS', None)
    commands, cases = [], []

    def run(label, argv, case, success=True):
        """Retain exact commands and raw diagnostics, including expected refusals."""
        p = subprocess.run(list(map(str, argv)), env=env, cwd=case, capture_output=True, text=True, timeout=300)
        (case / (label + '.stdout')).write_text(p.stdout)
        (case / (label + '.stderr')).write_text(p.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), cwd=str(case), returncode=p.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert (p.returncode == 0) == success, (label, p.stderr[-3000:])
        return p

    def options(device, mode='base'):
        """Use actual driver options; profiles stay synthetic and immutable."""
        if not device:
            return []
        path = root / (device + '-' + mode + '.json')
        return ['-DSL:gpu_device=' + device, '-DSL:gpu_profile=' + str(path),
                '-DSL:gpu_profile_sha256=' + hashlib.sha256(path.read_bytes()).hexdigest(),
                '-DSL:gpu_profile_toolchain=fixture-toolchain-v1']

    def reopen(image, case):
        """Independent native readers retain exact image stems and source interleave."""
        target = image.with_suffix('.T')
        run(image.stem + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', image, target], case)
        return target

    def no_op_pair(label, current, retained, case):
        """Capture complete current native bytes and a raw retained-boundary diff."""
        before, after = case / (label + '.before.B'), case / (label + '.after.B')
        shutil.copyfile(retained, before)
        shutil.copyfile(current, after)
        a, b = reopen(before, case), reopen(after, case)
        diff = ''.join(difflib.unified_diff(a.read_text().splitlines(True), b.read_text().splitlines(True), fromfile=str(a), tofile=str(b)))
        (case / (label + '.before-after.diff')).write_text(diff)
        assert not diff, (label, 'facts changed native output')

    for device in ['h20_96gb', 'h20_141gb', None]:
        name = device or 'architecture-only'
        case = out / ('gemm-' + name)
        case.mkdir()
        image = root / 'gemm-pair/input.B'
        run('driver', [tools / 'be/be', '-fB,' + str(image), '-fo,result.o', '-ft,phase.t',
            '-PHASE:w=on:c=off:wpath=' + str(tools / 'wopt'), '-O3', '-tr25', '-tt25:1',
            '-DSL:ai_opt_trace=ON:cuda_legacy_emit=OFF:gemm_plan_search=joint:gemm_cost_model=relative:cuda_tma_prefetch=ON:cuda_smem_layout_search=OFF:cuda_whirl_output=selected.B',
            *options(device), source / 'osprey/be/vho/tests/ai_matmul_cuda/edge_matmul_fixture.py'], case)
        phase = (case / 'phase.t').read_text()
        assert 'AI_MOVEMENT_FACTS 1 family=GEMM stage=BEFORE_SEARCH' in phase
        assert 'term=HBM known=0' in phase
        # Imported base limits admit the same fixed pair as the retained generic.
        no_op_pair('selected', case / 'selected.B', root / 'gemm-pair/generic/selected.B', case)
        cases.append(dict(family='GEMM', device=name, owned_before_search=True, native_full_T_equal=True))

        case = out / ('mla-' + name)
        case.mkdir()
        run('driver', [tools / 'be/be', '-fB,' + str(root / 'mla.after.B'), '-fo,result.o', '-ft,phase.t',
            '-PHASE:l=on:w=off:c=off:lpath=' + str(tools / 'lno') + ':wpath=' + str(tools / 'wopt'), '-O3',
            '-DSL:partition_axis=on:ai_opt_trace=on', '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=2',
            '-LNO:high_search=on:high_apply=on:high_search_bindings=' + str(root / 'mla.binding.json'),
            *options(device), source / 'osprey/torch2whirl/python/tests/models/absorbed_mla.py'], case)
        phase = (case / 'phase.t').read_text()
        assert 'AI_MOVEMENT_FACTS 1 family=MLA stage=BEFORE_SEARCH' in phase
        assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in phase
        assert 'AI_HIGH_APPLY 1 COMMITTED' in phase
        no_op_pair('selected', case / 'result.N', prior / 'transport-qualified' / (name + '-host.N'), case)
        cases.append(dict(family='MLA', device=name, owned_before_search=True, isolation=True, selected_only=True, native_full_T_equal=True))

    # Incomplete profiles preserve the existing ordinary-LNO path.
    for device in ['h20_96gb', 'h20_141gb']:
        case = out / ('unknown-' + device)
        case.mkdir()
        p = run('driver', [tools / 'be/be', '-fB,' + str(root / 'mla.after.B'), '-fo,result.o', '-ft,phase.t',
            '-PHASE:l=on:w=off:c=off:lpath=' + str(tools / 'lno') + ':wpath=' + str(tools / 'wopt'), '-O3',
            '-DSL:partition_axis=on:ai_opt_trace=on', '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=2',
            '-LNO:high_search=on:high_search_bindings=' + str(root / 'mla.binding.json'),
            *options(device, 'unknown'), source / 'osprey/torch2whirl/python/tests/models/absorbed_mla.py'], case)
        phase = (case / 'phase.t').read_text()
        assert 'fallback=ordinary_lno' in phase and 'AI_HIGH_APPLY 1 COMMITTED' not in phase
        cases.append(dict(family='MLA', device=device, unknown_profile_fallback=True))
    report = dict(status='PASS', cases=cases, native_no_op_pairs=6, GPU=False,
                  original_receipts=[hashlib.sha256((p / 'SHA256SUMS').read_bytes()).hexdigest() for p in [old, prior]])
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
