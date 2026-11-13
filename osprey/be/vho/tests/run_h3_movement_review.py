"""H3 native owner/search/selected-reference checks with authenticated H2 inputs.

No profiling or candidate execution occurs inside exploration. Retain native
before/after images, exact commands and raw source-interleaved full diffs.
Design: doc/AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md.
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
import sys


def main():
    """Exercise real native integrations and preserve old no-model behavior."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['build', 'previous_h1b', 'previous_h1c', 'previous_h2', 'output']:
        parser.add_argument('--' + name.replace('_', '-'), type=Path, required=True)
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    for prior in [args.previous_h1b, args.previous_h1c, args.previous_h2]:
        for line in (prior / 'SHA256SUMS').read_text().splitlines():
            digest, name = line.split('  ', 1)
            assert hashlib.sha256((prior / name).read_bytes()).hexdigest() == digest, name
    tools = args.build / 'osprey/targdir'
    root = args.previous_h1c / 'backend-qualified'
    source = Path(__file__).resolve().parents[4]
    sys.path.insert(0, str(source / 'osprey/be/lno/tests'))
    from run_common_high_review import execute, check_output
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE='1', LD_LIBRARY_PATH=':'.join(str(tools / p) for p in ['be', 'wopt', 'lno', 'whirl2c']))
    env.pop('OPEN64_HIGH_SEARCH_BINDINGS', None)
    commands, cases = [], []

    def run(label, argv, case, success=True, input_text=None):
        """Retain process evidence before asserting accepted or refused output."""
        p = subprocess.run(list(map(str, argv)), env=env, cwd=case, capture_output=True, text=True, timeout=300, input=input_text)
        (case / (label + '.stdout')).write_text(p.stdout)
        (case / (label + '.stderr')).write_text(p.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), cwd=str(case), returncode=p.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert (p.returncode == 0) == success, (label, p.stderr[-5000:])
        return p

    def pin(path, value):
        """New supplemental fixtures are synthetic, never revised old evidence."""
        path.write_text(json.dumps(value, sort_keys=True) + '\n')
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def options(device, model, digest, mode='base'):
        """Use production option import with exact native dependency bindings."""
        profile = root / (device + '-' + mode + '.json')
        return ['-DSL:gpu_device=' + device, '-DSL:gpu_profile=' + str(profile),
                '-DSL:gpu_profile_sha256=' + hashlib.sha256(profile.read_bytes()).hexdigest(),
                '-DSL:gpu_profile_toolchain=fixture-toolchain-v1',
                '-DSL:h3_model=' + str(model), '-DSL:h3_model_sha256=' + digest]

    def reopen(image, case):
        """Exact native stems and independent readers preserve raw evidence."""
        target = image.with_suffix('.T')
        run(image.stem + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', image, target], case)
        return target

    def difference(before, after, target):
        """A normalized excerpt never substitutes for the complete native diff."""
        target.write_text(''.join(difflib.unified_diff(before.read_text().splitlines(True), after.read_text().splitlines(True), fromfile=str(before), tofile=str(after))))

    fixture = next((args.previous_h1b / 'search-regressions').glob('test_portfolio*/native/input.bin')).parent
    manifest = json.loads((root / 'mla.high.json').read_text())
    for device in ['h20_96gb', 'h20_141gb']:
        old_trace = (args.previous_h2 / 'capture-review' / ('gemm-' + device) / 'phase.t').read_text()
        binding = re.search(r'AI_MOVEMENT_FACTS 1 family=GEMM .*?device=(\S+)', old_trace)[1]
        model = dict(schema='open64.movement-cost-model.v1', id='synthetic-H3-native', device_binding=binding,
                     provenance='synthetic', objective='worst', transaction_bytes=32, queue_per_sm=16,
                     warm_hit_ppm=500000, contention_ppm=1500000, issue_per_sm=1e10,
                     barrier_ns=10, shuffle_ns=1, fence_ns=2, rsqrt_ns=3, spill_request_bytes_per_cta=0,
                     site_limit=128, request_limit=1024, memory_limit=67108864, deadline_us=120000000)
        path = out / (device + '.h3.json')
        digest = pin(path, model)
        opts = options(device, path, digest)
        case = out / ('gemm-' + device)
        case.mkdir()
        run('driver', [tools / 'be/be', '-fB,' + str(root / 'gemm-pair/input.B'), '-fo,result.o', '-ft,phase.t',
            '-PHASE:w=on:c=off:wpath=' + str(tools / 'wopt'), '-O3', '-tr25', '-tt25:1',
            '-DSL:ai_opt_trace=ON:cuda_legacy_emit=OFF:gemm_plan_search=joint:gemm_cost_model=relative:cuda_tma_prefetch=ON:cuda_smem_layout_search=OFF:cuda_whirl_output=selected.B',
            *opts, source / 'osprey/be/vho/tests/ai_matmul_cuda/edge_matmul_fixture.py'], case)
        phase = (case / 'phase.t').read_text()
        assert 'AI_H3_COST 1 family=GEMM' in phase and 'AI_H3_SELECT 1 family=GEMM tile=' in phase
        shutil.copyfile(root / 'gemm-pair/input.B', case / 'source.before.B')
        shutil.copyfile(case / 'selected.B', case / 'source.after.B')
        before, after = reopen(case / 'source.before.B', case), reopen(case / 'source.after.B', case)
        difference(before, after, case / 'source.before-after.diff')
        run('whirl2c', [tools / 'whirl2c/whirl2c', '-cuda', '-TARG:abi=n64', '-O0',
            '-ft,' + str(case / 'whirl2c.phase.t'), '-CLIST:dotc_file=' + str(case / 'selected.native.cu'), '-CLIST:doth_file=' + str(case / 'selected.native.h'), case / 'selected.B'], case)
        chosen = int(re.search(r'AI_H3_SELECT 1 family=GEMM tile=(\d+)', phase)[1])
        tile = next(line.split() for line in phase.splitlines() if line.startswith('AI_OPT 7 TILE_CAND ') and int(line.split()[4]) == chosen)
        grid = next(line.split() for line in phase.splitlines() if line.startswith('AI_OPT 7 TILE_GRID ') and int(line.split()[4]) == chosen)
        tma = 'abi=tma_maps_ptr_v1' in phase
        helper = source / 'osprey/be/vho/tests/ai_matmul_cuda'
        run('host-model-compile', ['g++', '-x', 'c++', '-std=c++11', '-O0', '-pthread', '-I', helper / 'cuda_stub',
            '-c', case / 'selected.native.cu', '-o', case / 'selected.host.o'], case)
        flags = ['-DAI_TEST_M=' + grid[6], '-DAI_TEST_N=' + grid[7], '-DAI_TEST_K=' + grid[8],
                 '-DAI_TEST_THREADS=' + tile[22], '-DAI_TEST_TMA=' + str(int(tma)),
                 '-DAI_TEST_BM=' + tile[23], '-DAI_TEST_BN=' + tile[24], '-DAI_TEST_BK=' + tile[25]]
        run('host-model-link', ['g++', '-std=c++11', '-O0', '-pthread', '-I', helper / 'cuda_stub', *flags,
            helper / 'executable_pipeline_sim.cpp', case / 'selected.host.o', '-o', case / 'selected.host'], case)
        simulation = run('host-reference', [case / 'selected.host'], case)
        assert 'reference_max_error=' in simulation.stdout
        cases.append(dict(family='GEMM', device=device, complete_physical_selection=True, selected_native=True,
                          reference=simulation.stdout.strip(), GPU=False))

        case = out / ('mla-' + device)
        case.mkdir()
        run('driver', [tools / 'be/be', '-fB,' + str(root / 'mla.after.B'), '-fo,result.o', '-ft,phase.t', '-O3',
            '-PHASE:l=on:w=off:c=off:lpath=' + str(tools / 'lno') + ':wpath=' + str(tools / 'wopt'),
            '-DSL:partition_axis=on:ai_opt_trace=on', '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=2',
            '-LNO:high_search=on:high_apply=on:high_search_bindings=' + str(root / 'mla.binding.json') + ':high_apply_checkpoint=' + str(case / 'selected'),
            *opts, source / 'osprey/torch2whirl/python/tests/models/absorbed_mla.py'], case)
        phase = (case / 'phase.t').read_text()
        assert 'AI_H3_COST 1 family=MLA' in phase
        assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in phase
        assert 'AI_HIGH_APPLY 1 COMMITTED' in phase
        before, after = reopen(case / 'selected.before.B', case), reopen(case / 'selected.after.B', case)
        difference(before, after, case / 'selected.before-after.diff')
        assert 'H3 chose this existing executable recipe' in after.read_text()
        c = case / 'selected.reference.cu'
        run('whirl2c', [tools / 'whirl2c/whirl2c', '-TARG:abi=n64', '-O0', '-ft,' + str(case / 'whirl2c.phase.t'), '-CLIST:dotc_file=' + str(c),
            '-CLIST:doth_file=' + str(c.with_suffix('.h')), case / 'selected.after.B'], case)
        assert 'H3 chose this existing executable recipe' in c.read_text()
        assert after.stat().st_size and 'H3 chose this existing executable recipe' in after.read_text()
        library = case / 'selected.reference.so'
        run('compile', ['gcc', '-x', 'c', '-std=c99', '-O2', '-shared', '-fPIC', '-ffp-contract=off', '-fno-fast-math',
            '-I' + str(source / 'osprey/include'), '-include', source / 'osprey/be/lno/tests/common_high_host.h', c, '-lm', '-o', library], case)
        binary = case / 'reference.outputs.bin'
        binary.write_bytes(execute(library, manifest, (fixture / 'input.bin').read_bytes()))
        reference = check_output(fixture / 'reference.json', binary)
        cases.append(dict(family='MLA', device=device, complete_physical_selection=True, isolation=True, reference=reference))

        case = out / ('wopt-' + device)
        case.mkdir()
        profile, changed = root / (device + '-base.json'), root / (device + '-calibration.json')
        run('owner', [tools / 'ir_tools/dsl_gpu_owner_contract', case / 'source.B', case / 'owner.phase.t', device,
            profile, hashlib.sha256(profile.read_bytes()).hexdigest(), 'fixture-toolchain-v1', 'pass',
            changed, hashlib.sha256(changed.read_bytes()).hexdigest(), path, digest], case)
        phase = (case / 'owner.phase.t').read_text()
        assert 'AI_H3_COST 1 family=LayerNorm' in phase and 'AI_H3_COST 1 family=Reduction' in phase
        assert 'AI_H3_SELECT 1 family=Reduction' in phase
        for family in ['normalization', 'reduction']:
            a, b = [reopen(case / ('source.B.H2.' + family + '.' + boundary + '.B'), case) for boundary in ['before', 'after']]
            difference(a, b, case / (family + '.before-after.diff'))
            assert not (case / (family + '.before-after.diff')).read_bytes()
        # One selected fixed LN plan enters the existing fresh-device driver;
        # this is outside candidate exploration and after read-only checks.
        norm = case / 'normalization'
        run('norm-device', [tools / 'ir_tools/dsl_normalization_lower_test', 'device-gpu', norm,
            case / 'source.B.H2.h3-normalization.plan', case / 'owner.phase.t', device, profile,
            hashlib.sha256(profile.read_bytes()).hexdigest(), 'fixture-toolchain-v1'], case)
        a, b = [reopen(case / ('normalization.device.' + boundary + '.B'), case) for boundary in ['before', 'after']]
        difference(a, b, case / 'normalization.device.before-after.diff')
        cuda = case / 'normalization.cu'
        run('norm-whirl2c', [tools / 'whirl2c/whirl2c', '-cuda', '-TARG:abi=n64', '-O0',
            '-ft,' + str(case / 'normalization.whirl2c.phase.t'), '-CLIST:dotc_file=' + str(cuda), '-CLIST:doth_file=' + str(cuda.with_suffix('.h')),
            case / 'normalization.device.after.B'], case)
        assert b.stat().st_size and b.read_text()
        sys.path.insert(0, str(source / 'osprey/be/opt/tests'))
        from normalization_lower_trace_report import audit
        from normalization_lower_host import adapter, datasets, protocol, audit_output
        record = audit((case / 'owner.phase.t').read_text())
        host = case / 'normalization.host.cxx'
        host.write_text(adapter(cuda.read_text(), record))
        run('norm-host-compile', ['g++', '-std=c++11', '-O2', '-ffp-contract=off', '-fno-fast-math',
            host, '-o', case / 'normalization.host'], case)
        sets = datasets(record)
        wire = protocol(record, sets)
        (case / 'normalization.inputs.txt').write_text(wire)
        actual = run('norm-reference', [case / 'normalization.host'], case, input_text=wire)
        numerical = audit_output(record, sets, actual.stdout)
        (case / 'normalization.reference.json').write_text(json.dumps(numerical, indent=2) + '\n')
        cases.append(dict(family='LayerNorm+Reduction', device=device, native_read_only=True, bounded_costing=True,
                          selected_LN_reference=numerical, reduction_executable=False))
        print(device, 'PASS H3 native families', flush=True)

    (out / 'report.json').write_text(json.dumps(dict(status='PASS', cases=cases, GPU=False,
        input_manifests={str(p): hashlib.sha256((p / 'SHA256SUMS').read_bytes()).hexdigest()
                         for p in [args.previous_h1b, args.previous_h1c, args.previous_h2]}), indent=2) + '\n')


if __name__ == '__main__':
    main()
