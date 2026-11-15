"""H4 actual GEMM owner/native/reference qualification with pinned H3 inputs.

Candidate exploration is detached. Only selected kernels are emitted/executed
by this harness. No GPU performance or hardware race result is claimed.
Design: doc/AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess


def main():
    """Retain genuine native boundaries, independent oracle and fallback evidence."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['build', 'previous', 'output']:
        parser.add_argument('--' + name, type=Path, required=True)
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    prior = args.previous.resolve()
    for line in (prior / 'SHA256SUMS').read_text().splitlines():
        digest, name = line.split('  ', 1)
        assert hashlib.sha256((prior / name).read_bytes()).hexdigest() == digest, name
    source = Path(__file__).resolve().parents[4]
    helper = source / 'osprey/be/vho/tests/ai_matmul_cuda'
    tools = args.build / 'osprey/targdir'
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE='1', PYTHONPATH='/build/publication-python:' + str(source / 'osprey/torch2whirl/python'),
               LD_LIBRARY_PATH=':'.join(str(tools / p) for p in ['be', 'wopt', 'lno', 'whirl2c']))
    env.pop('OPEN64_GEMM_H4_TEST', None)
    commands, cases = [], []

    def sha(path):
        """Authenticate exact file bytes rather than an ID or mutable pathname."""
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def run(label, argv, case, success=True):
        """Save exact commands and diagnostics before asserting process outcomes."""
        p = subprocess.run(list(map(str, argv)), env=env, cwd=case, capture_output=True, text=True, timeout=300)
        (case / (label + '.stdout')).write_text(p.stdout)
        (case / (label + '.stderr')).write_text(p.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), cwd=str(case), returncode=p.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert (p.returncode == 0) == success, (label, p.stderr[-4000:])
        return p

    def pin(name, value):
        """Synthetic model changes are new inputs, never edits to prior evidence."""
        path = out / (name + '.json')
        path.write_text(json.dumps(value, sort_keys=True) + '\n')
        return path

    def options(device, profile, model):
        """Use actual production device/profile and H3 importer controls."""
        return ['-DSL:gpu_device=' + device, '-DSL:gpu_profile=' + str(profile), '-DSL:gpu_profile_sha256=' + sha(profile),
                '-DSL:gpu_profile_toolchain=fixture-toolchain-v1', '-DSL:h3_model=' + str(model), '-DSL:h3_model_sha256=' + sha(model)]

    def reopen(image, case):
        """Exact-stem native views preserve source interleave and full tables."""
        target = image.with_suffix('.T')
        run(image.stem + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', image, target], case)
        assert target.stat().st_size
        return target

    def backend(case, image, opts, enabled=True, tma=True, level=3, checkpoint=True, output='selected.B'):
        """One native invocation selects one plan; no fixture profiling is used."""
        case.mkdir()
        flags = '-DSL:ai_opt_trace=on:cuda_legacy_emit=off:gemm_plan_search=joint:gemm_cost_model=relative:cuda_smem_layout_search=off'
        flags += ':cuda_tma_prefetch=' + ('on' if tma else 'off') + ':cuda_whirl_output=' + output
        flags += ':gemm_hierarchy=' + ('on' if enabled else 'off')
        if checkpoint:
            flags += ':gemm_hierarchy_checkpoint=' + str(case / 'retention')
        run('driver', [tools / 'be/be', '-fB,' + str(image), '-fo,result.o', '-ft,phase.t', '-O' + str(level),
            '-PHASE:w=on:c=off:wpath=' + str(tools / 'wopt'), '-tr25', '-tt25:1', flags, *opts,
            helper / 'hierarchy_matmul_fixture.py'], case)
        return (case / 'phase.t').read_text()

    def qualify(case, trace):
        """Compile actual WHIRL-derived CUDA into the existing delayed-copy host model."""
        selected = case / 'selected.B'
        text = reopen(selected, case).read_text()
        assert 'H4 GEMM:' in text
        c = case / 'selected.cu'
        run('whirl2c', [tools / 'whirl2c/whirl2c', '-cuda', '-TARG:abi=n64', '-O0', '-ft,' + str(case / 'whirl2c.phase.t'),
            '-CLIST:dotc_file=' + str(c), '-CLIST:doth_file=' + str(c.with_suffix('.h')), selected], case)
        assert 'H4 GEMM:' in c.read_text() and selected.with_suffix('.T').read_text() == text
        retained = 'then reuse across' in c.read_text()
        if retained:
            before, after = [reopen(case / ('retention.pu-0.' + boundary + '.B'), case) for boundary in ['before', 'after']]
            diff = ''.join(difflib.unified_diff(before.read_text().splitlines(True), after.read_text().splitlines(True), fromfile=str(before), tofile=str(after)))
            (case / 'retention.before-after.diff').write_text(diff)
            assert diff and 'b_retained_0' in diff
            assert re.search(r'b_retained_0\s*=.*bs', c.read_text())
            assert not re.search(r'b_retained_\d+\s*\[', c.read_text())
        chosen = int(re.search(r'AI_H3_SELECT 1 family=GEMM tile=(\d+)', trace)[1])
        tile = next(line.split() for line in trace.splitlines() if line.startswith('AI_OPT 7 TILE_CAND ') and int(line.split()[4]) == chosen)
        grid = next(line.split() for line in trace.splitlines() if line.startswith('AI_OPT 7 TILE_GRID ') and int(line.split()[4]) == chosen)
        tma = 'abi=tma_maps_ptr_v1' in trace
        run('host-compile', ['g++', '-x', 'c++', '-std=c++11', '-O0', '-pthread', '-ffp-contract=off', '-fno-fast-math',
            '-I', helper / 'cuda_stub', '-c', c, '-o', case / 'kernel.o'], case)
        flags = ['-DAI_TEST_M=' + grid[6], '-DAI_TEST_N=' + grid[7], '-DAI_TEST_K=' + grid[8], '-DAI_TEST_THREADS=' + tile[22],
                 '-DAI_TEST_TMA=' + str(int(tma)), '-DAI_TEST_BM=' + tile[23], '-DAI_TEST_BN=' + tile[24], '-DAI_TEST_BK=' + tile[25]]
        run('host-link', ['g++', '-std=c++11', '-O0', '-pthread', '-ffp-contract=off', '-fno-fast-math', '-I', helper / 'cuda_stub',
            *flags, helper / 'hierarchy_reference.cpp', case / 'kernel.o', '-o', case / 'reference'], case)
        result = run('reference', [case / 'reference'], case)
        assert 'reference_max_error=0' in result.stdout
        return dict(retained=retained, tma=tma, reference=result.stdout.strip(), selected_native=True)

    for device in ['h20_96gb', 'h20_141gb']:
        profile = prior / 'inputs/H1c' / (device + '-base.json')
        base = json.loads((prior / 'native-corrected' / (device + '.h3.json')).read_text())
        base['deadline_us'] = 600000000
        model = pin(device + '.model', base)
        opts = options(device, profile, model)
        images = {}
        for label, shape in [('aligned', (128, 128, 144)), ('ragged', (67, 69, 35)), ('one-slot', (64, 64, 16)), ('generations', (64, 64, 160))]:
            fixture = out / (device + '-' + label + '.B')
            run(label + '-fixture', ['python3', '-B', helper / 'hierarchy_matmul_fixture.py', fixture, *shape], out)
            images[label] = fixture
        for label, tma in [('aligned', True), ('aligned-sync', False), ('ragged', False), ('one-slot', True), ('generations', True)]:
            image = images['aligned' if label == 'aligned-sync' else label]
            case = out / (device + '-' + label)
            trace = backend(case, image, opts, tma=tma)
            assert 'AI_H4_SEARCH 1 ' in trace and 'native_ST=unchanged native_TY=unchanged' in trace
            assert 'reuse_proved=1' in trace and 'term=spills known=0' in trace
            record = qualify(case, trace)
            cases.append(dict(device=device, case=label, **record))
            print(device, label, record['retained'], 'PASS reference', flush=True)
        # A one-estimate budget must retain the historical winner, never revive
        # retained siblings via legacy cycle ranking.
        m = dict(base, site_limit=1, request_limit=1)
        limited = pin(device + '.limited', m)
        a, b = out / (device + '-budget'), out / (device + '-legacy')
        trace = backend(a, images['aligned'], options(device, profile, limited))
        backend(b, images['aligned'], options(device, profile, limited), enabled=False)
        assert 'fallback=existing_plan reason=incomplete_or_budget' in trace
        assert reopen(a / 'selected.B', a).read_bytes() == reopen(b / 'selected.B', b).read_bytes()
        # Unknown layout facts remain unavailable; old authenticated input is
        # reused for negative admission rather than assigning it invented proof.
        case = out / (device + '-unknown-layout')
        trace = backend(case, prior / 'inputs/H1c/gemm-pair/input.B', opts)
        assert 'AI_H4_ADMISSION 1 enabled=0' in trace and 'AI_H4_WORK ' not in trace
    (out / 'report.json').write_text(json.dumps(dict(status='PASS', cases=cases, budget_native_parity_pairs=2,
        missing_layout_fallbacks=2, prior_manifest_sha256=sha(prior / 'SHA256SUMS'), GPU=False), indent=2) + '\n')


if __name__ == '__main__':
    main()
