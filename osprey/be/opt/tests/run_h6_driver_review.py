"""H6 LN4 CLI option, post-PREOPT selection and compatibility gates.

The native H6 APIs also qualify Reduction, whose new cooperative launch has
no new generic serving/runtime ABI. This runner exercises the existing real
LayerNorm backend checkpoint and its admitted fallback, not a fake driver.
Design: doc/AI-COMPILER-H6-NORM-REDUCTION-v1.0.md.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess


def main():
    """Record real production options and selected launch/resource evidence."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build', type=Path, required=True)
    parser.add_argument('--previous-h3', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    tools = args.build / 'osprey/targdir'
    source = Path(__file__).resolve().parents[4]
    env = dict(os.environ, LD_LIBRARY_PATH=':'.join(str(tools / x) for x in ['be', 'wopt', 'lno', 'whirl2c']))
    commands, cases = [], []
    manifest = dict((name, digest) for digest, name in (line.split('  ', 1) for line in (args.previous_h3 / 'SHA256SUMS').read_text().splitlines()))

    def sha(path):
        """Pin exact source/model/profile bytes before invoking production import."""
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def run(label, argv, case, success=True):
        """Persist expected refusal or successful process results in separate logs."""
        p = subprocess.run(list(map(str, argv)), cwd=case, env=env, capture_output=True, text=True, timeout=300)
        (case / (label + '.stdout')).write_text(p.stdout)
        (case / (label + '.stderr')).write_text(p.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), cwd=str(case), returncode=p.returncode, expected_success=success))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert (p.returncode == 0) == success, (label, p.stderr[-4000:])
        return p

    for device in ['h20_96gb', 'h20_141gb']:
        profile = args.previous_h3 / 'inputs/H1c' / (device + '-base.json')
        model = args.previous_h3 / 'native-corrected' / (device + '.h3.json')
        assert all(sha(p) == manifest[str(p.relative_to(args.previous_h3))] for p in [profile, model])
        seed = out / (device + '-source')
        seed.mkdir()
        run('producer', [tools / 'ir_tools/dsl_normalization_request_test', seed / 'source.B', seed / 'request.json', 'sm_90', 'wide', 1], seed)
        for mode in ['enabled', 'disabled', 'missing-model', 'budget', 'standalone']:
            case = out / (device + '-' + mode)
            case.mkdir()
            binding_source = seed
            if mode == 'disabled':
                # Existing H3's legacy cycle graph admits this smaller source;
                # the wide fixture already exceeds its historical work cap.
                run('legacy-producer', [tools / 'ir_tools/dsl_normalization_request_test', case / 'source.B', case / 'request.json', 'sm_90', 'default', 1], case)
                binding_source = case
            output = case / 'selected.after.B'
            argv = [tools / 'be/be', '-fB,' + str(binding_source / 'source.B'), '-ft,' + str(case / 'phase.t'), '-O3', '-TARG:abi=n64',
                    '-OPT:roundoff=0:IEEE_arith=1:fast_math=off', '-PHASE:wpath=' + str(tools / 'wopt'),
                    '-DSL:gpu_device=' + device, '-DSL:gpu_profile=' + str(profile), '-DSL:gpu_profile_sha256=' + sha(profile),
                    '-DSL:gpu_profile_toolchain=fixture-toolchain-v1', '-DSL:normalization_request=' + str(binding_source / 'request.json'),
                    '-DSL:normalization_output=' + str(output), '-DSL:ai_opt_trace=on', '-tr25', '-tt25:1']
            if mode != 'disabled':
                argv += ['-DSL:h6_memory_hierarchy=on']
            if mode != 'missing-model':
                pinned = model
                if mode == 'budget':
                    value = json.loads(model.read_text())
                    value['site_limit'] = 1
                    pinned = case / 'model.json'
                    pinned.write_text(json.dumps(value) + '\n')
                argv += ['-DSL:h3_model=' + str(pinned), '-DSL:h3_model_sha256=' + sha(pinned)]
            if mode == 'standalone':
                argv += ['-DSL:normalization_mode=standalone']
            argv += [source / 'osprey/be/opt/tests/dsl_normalization_ref_test.cxx']
            run('backend', argv, case, mode != 'budget')
            trace = (case / 'phase.t').read_text()
            h6 = 'AI_H6_APPLY 1' in trace
            assert h6 == (mode in ['enabled', 'standalone'])
            if mode == 'budget':
                assert not output.exists()
            else:
                review = json.loads((case / 'selected.after.B.manifest.json').read_text())
                if h6:
                    assert review['selected_kind'] == 3
                    assert review['selected_launch'] == dict(grid_x=7, block_x=32, shared_bytes=384)
                    assert review['h6']['estimated'] and review['h6']['rank_ns'] > 0
                    assert 'selected_only=true native_search_mutation=none' in trace
                    assert bool(review['host_dispatch']) == (mode == 'standalone')
                else:
                    assert 'selected_launch' not in review and 'h6' not in review
                for image in [case / 'selected.before.B', output]:
                    run(image.stem + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', image, image.with_suffix('.T')], case)
                a, b = [case / ('selected.' + point + '.T') for point in ['before', 'after']]
                (case / 'selected.before-after.diff').write_text(''.join(difflib.unified_diff(a.read_text().splitlines(True), b.read_text().splitlines(True), fromfile=a.name, tofile=b.name)))
                assert 'COMMENT' in b.read_text()
                if h6:
                    assert 'H6 selected this fixed' in b.read_text()
            cases.append(dict(device=device, mode=mode, selected_h6=h6, published=output.exists(), GPU=False))
            print('PASS production', device, mode, flush=True)
    (out / 'report.json').write_text(json.dumps(dict(status='PASS', cases=cases, GPU=False), indent=2) + '\n')


if __name__ == '__main__':
    main()
