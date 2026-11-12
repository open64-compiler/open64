"""Exercise Q1 native-source routing and real PREOPT rebinding in be.

Design: doc/AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md. Compile and execute only
fixed fixtures; this validation is separate from estimation-only exploration.
"""
import argparse
import difflib
import hashlib
import json
import os
import shutil
from pathlib import Path
import subprocess
import sys
import torch
from run_quantized_high_review import execute


def review(args):
    """Retain native boundaries, exact commands, comments and numerical checks."""
    root = args.output.resolve()
    root.mkdir(parents=True, exist_ok=False)
    sys.path.insert(0, str(args.source / 'osprey/torch2whirl/python/tests'))
    from models.mla_quantized_paged import CASES, make_fixture
    from models.mla_quantized_reference import reference
    commands, cases = [], {}
    digests = dict((name, sha) for sha, name in
                   (line.split('  ', 1) for line in (args.previous / 'SHA256SUMS').read_text().splitlines()))
    tools = args.build / 'osprey/targdir'
    backend = tools / 'be/be'
    lowerer = tools / 'ir_tools/tensor_kernel_lower'
    b2a = tools / 'ir_tools/ir_b2a'
    w2c = tools / 'whirl2c/whirl2c'

    def run(label, argv, cwd, success=True):
        """Keep failures and exact argv visible in the durable review record."""
        result = subprocess.run(list(map(str, argv)), capture_output=True, text=True,
                                cwd=cwd, timeout=240)
        (root / (label + '.stdout')).write_text(result.stdout)
        (root / (label + '.stderr')).write_text(result.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=result.returncode))
        (root / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert (result.returncode == 0) == success, (label, result.returncode, result.stderr[-4000:])
        return result

    def arguments(case, name, level, route='buffer-v1', strict=True, trace=True, preopt_only=False):
        """Use the normal backend entry on the original native source image."""
        return [backend, '-fB,' + str(args.previous / 'inputs/review' / name / 'source.B'),
                '-fo,' + str(case / 'native.o'), '-ft,' + str(case / 'backend.phase.t'),
                '-PHASE:l=' + ('off:p=on' if preopt_only else 'on') + ':w=off:c=off:lpath=' +
                str(tools / 'lno') + ':wpath=' + str(tools / 'wopt'), '-O' + str(level),
                '-DSL:q1_high=' + route + ':q1_checkpoint=' + str(case / 'q1') +
                ':ai_opt_trace=' + ('on' if trace else 'off'),
                # Existing whirl2c cannot emit x86 SIMD REPLICATE as host C.
                # Keep this executable oracle scalar; the default SIMD route
                # is also compiled separately and leaves Q1 High unchanged.
                '-LNO:simd=0', '-tr26', '-tt26:1', '-OPT:IEEE_arith=' + ('1' if strict else '3') + ':roundoff=0',
                args.source / 'osprey/torch2whirl/python/tests/models/mla_quantized_paged.py']

    selected = list(CASES) if args.case is None else [args.case]
    matrix = [(name, 3, True, False) for name in selected]
    if args.case is None:
        matrix += [('dense_s8_decode', level, True, False) for level in (0, 1, 2)]
        matrix += [('dense_s8_decode', 3, False, False), ('dense_s8_decode', 2, True, True)]
    for name, level, trace, preopt_only in matrix:
        label = name + '-O' + str(level) + ('' if trace else '-trace-off') + ('-preopt-only' if preopt_only else '')
        case = root / label
        case.mkdir()
        prior = args.previous / 'inputs/review' / name
        for filename in ('source.B', 'inputs-and-source.pt'):
            p = prior / filename
            assert hashlib.sha256(p.read_bytes()).hexdigest() == digests[str(p.relative_to(args.previous))]
        result = run(label + '-be', arguments(case, name, level, trace=trace, preopt_only=preopt_only), case)
        assert 'AI_Q1_ROUTE 1 APPLIED' in result.stderr
        phase_trace = (case / 'backend.phase.t').read_text()
        preopt = (case / 'q1.preopt-0.after.B').exists()
        if preopt and trace:
            assert 'REBIND PASS' in phase_trace and 'BEFORE_NEXT_PHASE live=false' in phase_trace
            assert 'old_facts_expired=true native_tables_unchanged=true search=false' in phase_trace
        if not trace:
            assert 'AI_Q1' not in phase_trace
        boundaries = [case / 'q1.source.before.B', case / 'q1.high.after.B']
        if preopt:
            boundaries += [case / 'q1.preopt-0.before.B', case / 'q1.preopt-0.after.B']
        final = case / ('native.P' if preopt_only else 'native.N')
        assert final.exists()
        boundaries.append(final)
        for image in boundaries:
            run(label + '-' + image.stem + '-dump', [b2a, '-st', '-src', image, image.with_suffix('.T')], case)
            assert image.with_suffix('.T').stat().st_size > 0
        for before, after in ((boundaries[0], boundaries[1]),) + (
                ((case / 'q1.preopt-0.before.B', case / 'q1.preopt-0.after.B'),) if preopt else ()):
            (case / (before.stem + '--' + after.stem + '.diff')).write_text(''.join(difflib.unified_diff(
                before.with_suffix('.T').read_text().splitlines(True), after.with_suffix('.T').read_text().splitlines(True),
                fromfile=before.with_suffix('.T').name, tofile=after.with_suffix('.T').name)))
        payload = torch.load(prior / 'inputs-and-source.pt', weights_only=True)
        model, _, meta = make_fixture(name)
        oracle = reference([x.tolist() for x in payload['inputs']], meta)
        outputs_checked = 0
        images = [case / 'q1.high.after.B'] + ([case / 'q1.preopt-0.after.B'] if preopt else []) + [final]
        for image in images:
            manifest_path = case / (image.stem + '.manifest.json')
            run(label + '-' + image.stem + '-manifest', [lowerer, 'q1-production-manifest', image, manifest_path], case)
            manifest = json.loads(manifest_path.read_text())
            c = case / (image.stem + '.c')
            emission_image = image
            if image.suffix == '.P':
                # The existing whirl2c executable recognizes B/N input suffixes.
                emission_image = image.with_suffix('.B')
                shutil.copyfile(image, emission_image)
            run(label + '-' + image.stem + '-w2c', [w2c, '-TARG:abi=n64', '-O0',
                '-ft,' + str(case / (image.stem + '.w2c.phase.t')), '-CLIST:dotc_file=' + str(c),
                '-CLIST:doth_file=' + str(c.with_suffix('.h')), emission_image], case)
            library = c.with_suffix('.so')
            run(label + '-' + image.stem + '-cc', ['gcc', '-std=c99', '-O2', '-shared', '-fPIC',
                '-ffp-contract=off', '-fno-fast-math', '-I' + str(args.source / 'osprey/include'),
                '-include', args.source / 'osprey/be/lno/tests/common_high_host.h', c, '-lm', '-o', library], case)
            actual = execute(library, manifest, payload['inputs'])
            for i, (a, b, ref) in enumerate(zip(actual, payload['source'], oracle)):
                torch.testing.assert_close(a, b, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
                expected = torch.tensor(ref, dtype=torch.float64 if i < 2 else a.dtype)
                torch.testing.assert_close(a.double() if i < 2 else a, expected,
                                           rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
                outputs_checked += 1
            if (prior / 'decoder-boundary.pt').exists():
                boundary = torch.load(prior / 'decoder-boundary.pt', weights_only=True)
                actual = execute(library, manifest, boundary['inputs'])
                for i, (a, b) in enumerate(zip(actual, boundary['source'])):
                    torch.testing.assert_close(a, b, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
            if name.startswith(('dense', 'fake')):
                boundary = [x.clone() for x in payload['inputs']]
                maximum = 7 if 'u4' in name else 127
                for index in (2, 3):
                    flat = boundary[index].reshape(-1, boundary[index].shape[-1])
                    for j in range(flat.shape[1] - 1):
                        flat[:, j] = [0.5, -0.5, 1.5, -1.5, 2.5, -2.5][j % 6]
                    flat[:, -1] = maximum
                with torch.no_grad():
                    expected = model(*boundary)
                actual = execute(library, manifest, boundary)
                for i, (a, b) in enumerate(zip(actual, expected)):
                    torch.testing.assert_close(a, b, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
                for value in (float('nan'), float('inf')):
                    invalid = [x.clone() for x in payload['inputs']]
                    invalid[2].reshape(-1)[0] = value
                    execute(library, manifest, invalid, 1)
            if name.startswith(('paged', 'append')):
                for value in (999, 2**63 - 1, -1):
                    invalid = [x.clone() for x in payload['inputs']]
                    invalid[6][0, 0] = value
                    execute(library, manifest, invalid, 1)
            if name.startswith('append'):
                invalid = [x.clone() for x in payload['inputs']]
                invalid[12][1, 0] = invalid[12][0, 0]
                execute(library, manifest, invalid, 1)
            text = image.with_suffix('.T').read_text()
            assert len(text) > 1000 and 'COMMENT' in text, 'lost independently reopened review trace'
            if preopt and image.stem in ('q1.preopt-0.after', 'native'):
                assert 'Q1 PREOPT operation' in c.read_text(), 'lost native COMMENT explanation in whirl2c'
        cases[label] = dict(outputs_checked=outputs_checked, preopt=preopt, reference=True,
                            canaries=True, immutable_inputs=True, trace=trace)
        print('PASS ' + label, flush=True)
    if args.case is None:
        # The explicit entry ABI is never silently applied to ordinary callers.
        for route, strict in (('off', True), ('invalid', True), ('buffer-v1', False)):
            case = root / ('fallback-' + route + ('-unsafe' if not strict else ''))
            case.mkdir()
            result = run(case.name, arguments(case, 'dense_s8_decode', 0, route, strict), case, success=False)
            assert 'AI_Q1_ROUTE 1 APPLIED' not in result.stderr and not list(case.glob('q1.*.B'))
            if route != 'off':
                assert 'AI_Q1_ROUTE 1 REFUSED' in result.stderr
        # Invalid route and default/off reach exactly the existing source-only
        # runtime lowering refusal; Q1 does not invent a successful fallback.
    report = dict(schema='open64.q1-production-validation.v1', status='PASS', cases=cases,
                  search_invoked=False, quantized_candidates=False, gpu_execution=False)
    (root / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('source', 'previous', 'build', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--case')
    review(parser.parse_args())
