"""Exercise actual absorbed MLA High WHIRL, PREOPT/LNO admission and fallback.

Design: doc/AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md.
Requires explicit Linux tools, saved MLA fixtures and a fresh mounted output.
No GPU, candidate search or selected-schedule transformation is exercised.
"""
import argparse
import ctypes
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / 'vho/tests'))
from validate_mla_gpu import check_output


def execute(library, manifest, raw):
    """Call the real whirl2c output using guarded disjoint storage buffers."""
    values = manifest['recipe']['values']
    inputs, offset = {}, 0
    for value in values:
        if value['role'] != 1:
            continue
        size = (1 if value['dtype'] == 'bool' else 4)
        for extent in value['shape']:
            size *= extent
        inputs[value['storage']] = raw[offset:offset + size]
        assert len(inputs[value['storage']]) == size
        offset += size
    assert offset == len(raw)
    buffers, originals = [], []
    for storage in manifest['buffers']:
        value = values[storage]
        size = (1 if value['dtype'] == 'bool' else 4)
        for extent in value['shape']:
            size *= extent
        data = b'\xcd' * 16 + inputs.get(storage, b'\xcd' * size) + b'\xcd' * 16
        buffer = ctypes.create_string_buffer(data, len(data))
        buffers.append(buffer)
        originals.append(data)
    function = getattr(ctypes.CDLL(str(library)), manifest['function'])
    function.argtypes = [ctypes.c_void_p] * len(buffers)
    function.restype = None
    function(*(ctypes.addressof(buffer) + 16 for buffer in buffers))
    by_storage = {}
    for storage, buffer, original in zip(manifest['buffers'], buffers, originals):
        assert buffer.raw[:16] == original[:16] and buffer.raw[-16:] == original[-16:]
        if storage in inputs:
            assert buffer.raw == original, 'input mutation'
        by_storage[storage] = buffer.raw[16:-16]
    return b''.join(by_storage[v['storage']] for v in values if v['role'] == 2)


def review(args):
    """Retain every command and native artifact, comparing actual execution paths."""
    out = args.output.resolve()
    out.mkdir(exist_ok=False, parents=True)
    (out / 'logs').mkdir()
    env = dict(os.environ)
    env['LD_LIBRARY_PATH'] = ':'.join(map(str, (args.tools / 'be', args.tools / 'wopt', args.lno.parent)))
    commands = []

    def run(label, argv, code=0, cwd=None):
        """Save exact arguments and diagnostics before asserting success/failure."""
        result = subprocess.run(list(map(str, argv)), cwd=cwd or out, env=env,
                                capture_output=True, text=True, timeout=600)
        for stream in ('stdout', 'stderr'):
            (out / 'logs' / (label + '.' + stream)).write_text(getattr(result, stream))
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=result.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        if code is not None:
            assert (result.returncode == 0) == (code == 0), label + ': ' + result.stderr[-4000:]
        return result

    def reopen(label, image):
        """Reopen a real mapped image with its stem and source cross-reference."""
        run(label, [args.b2a, '-st', '-src', image, image.with_suffix('.T')])

    def backend(label, image, enabled, trace='on', level=3, lno=None, extra=()):
        """Use normal per-PU driver/PREOPT/LNO, preserving fallback controls."""
        base = out / label
        command = [args.tools / 'be/be', '-fB,' + str(image), '-fo,' + str(base) + '.o',
                   '-ft,' + str(base) + '.phase.t', '-PHASE:l=on:w=off:c=off:lpath=' + str((lno or args.lno).parent) +
                   ':wpath=' + str(args.tools / 'wopt'), '-O' + str(level),
                   '-DSL:ai_opt_trace=' + trace, '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=0', *extra]
        if enabled is not None:
            command.append('-DSL:partition_axis=' + enabled)
        command.append(Path(__file__).with_name('dsl_common_high.c') if image.name == 'dsl_common_high.B' else args.source)
        run(label, command)
        image = base.with_suffix('.N')
        assert image.is_file(), label
        reopen(label + '-reopen', image)
        return image, base.with_suffix('.phase.t').read_text()

    def compile_view(label, image, allow_depgraph_diagnostic=False):
        """Compile ordinary whirl2c output with strict F32 arithmetic semantics."""
        c = out / (label + '.c')
        emitted = run(label + '-w2c', [args.w2c, '-TARG:abi=n64', '-O0', '-CLIST:dotc_file=' + str(c),
                            '-CLIST:doth_file=' + str(c.with_suffix('.h')),
                            '-ft,' + str(out / (label + '.w2c.phase.t')), image],
                      code=None if allow_depgraph_diagnostic else 0)
        if emitted.returncode:
            assert allow_depgraph_diagnostic and 'Depgraph_Read' in emitted.stderr and 'OPCODE_make_op' in emitted.stderr
            for partial in (c, c.with_suffix('.h')):
                if partial.exists():
                    partial.rename(partial.with_name(partial.stem + '.failed' + partial.suffix))
            return None
        library = out / (label + '.so')
        run(label + '-cc', ['gcc', '-std=c99', '-O2', '-shared', '-fPIC', '-ffp-contract=off',
                           '-fno-fast-math', '-I' + str(args.include), '-include',
                           Path(__file__).with_name('common_high_host.h'), c, '-lm', '-o', library])
        return library

    report = dict(scope='M2a semantic High producer and canonical form admission', contexts={})
    for context in ('short', 'long'):
        recipe = out / (context + '.recipe.json')
        run(context + '-prepare', [args.lowerer, 'prepare', args.previous / 'selected' / context / 'mla.B', recipe, '--verify', '3'])
        before, after = out / (context + '.before.B'), out / (context + '.after.B')
        manifest_path = out / (context + '.high.json')
        fused = out / (context + '.fused.json')
        run(context + '-fuse-negative', [args.lowerer, 'optimize', recipe, fused, '--fuse'])
        rejected = [out / (context + '.rejected.' + suffix) for suffix in ('before.B', 'after.B', 'json')]
        run(context + '-reject-physical', [args.lowerer, 'high', fused, *rejected], code=1)
        assert not any(path.exists() for path in rejected)
        run(context + '-high', [args.lowerer, 'high', recipe, before, after, manifest_path])
        stamps = [hashlib.sha256(path.read_bytes()).hexdigest() for path in (before, after, manifest_path)]
        run(context + '-reject-existing', [args.lowerer, 'high', recipe, before, after, manifest_path], code=1)
        assert stamps == [hashlib.sha256(path.read_bytes()).hexdigest() for path in (before, after, manifest_path)]
        manifest = json.loads(manifest_path.read_text())
        assert len(manifest['recipe']['operations']) == 48 and len(manifest['buffers']) == 41
        for boundary in (before, after):
            reopen(context + '-' + boundary.stem, boundary)
        difference = ''.join(difflib.unified_diff(before.with_suffix('.T').read_text().splitlines(True),
                                                after.with_suffix('.T').read_text().splitlines(True)))
        (out / (context + '.before-after.diff')).write_text(difference)
        assert difference and 'DO_LOOP' in difference and 'ARRAY' in difference and 'COMMENT' in difference
        assert '__open64_dsl_' not in after.with_suffix('.T').read_text()
        enabled, trace = backend(context + '-enabled', after, 'on')
        assert re.search(r'AI_HIGH 1 ADMISSION open64_common_high admitted admitted loops=\d+ memory=\d+', trace)
        assert 'Pre_Optimizer phase=' in trace
        for label, flag, tracing in [('off', 'off', 'on'), ('default', None, 'on'), ('trace-off', 'on', 'off')]:
            other, text = backend(context + '-' + label, after, flag, tracing)
            assert 'AI_HIGH ' not in text
            assert other.read_bytes() == enabled.read_bytes(), label + ' changed executable WHIRL'
        baseline, _ = backend(context + '-baseline', after, 'off', lno=args.tools / 'lno/lno.so')
        assert baseline.read_bytes() == enabled.read_bytes(), 'existing LNO fallback changed'
        # Default native LNO is compared above without changing its controls.
        # Use a separate portable-C lane because ordinary whirl2c cannot print
        # all target SSE REPLICATE operators introduced by later SIMD passes.
        portable, _ = backend(context + '-portable', after, 'on', extra=['-LNO:simd=0'])
        portable_baseline, _ = backend(context + '-portable-baseline', after, 'off',
                                      lno=args.tools / 'lno/lno.so', extra=['-LNO:simd=0'])
        assert portable_baseline.read_bytes() == portable.read_bytes()
        libraries = [compile_view(context + '-high-view', after)]
        post = compile_view(context + '-lno-view', portable, allow_depgraph_diagnostic=True)
        if post is None:
            assert compile_view(context + '-baseline-lno-view', portable_baseline,
                                allow_depgraph_diagnostic=True) is None, 'new reopen regression'
            post_status = 'BASELINE_DEPGRAPH_REOPEN_FAILURE'
        else:
            libraries.append(post)
            post_status = 'REFERENCE_VERIFIED'
        checks, bitwise = {}, {}
        for reference in sorted((args.previous / 'selected' / context).glob('*.reference.json')):
            variant = reference.name[:-len('.reference.json')]
            data = (reference.parent / (variant + '.inputs.bin')).read_bytes()
            outputs = []
            for index, library in enumerate(libraries):
                binary = out / (context + '-' + variant + '-' + str(index) + '.bin')
                binary.write_bytes(execute(library, manifest, data))
                checks[variant + '-' + str(index)] = check_output(reference, binary)
                outputs.append(binary.read_bytes())
            # Both paths must meet the saved source-reference tolerances. Also
            # retain exact parity as an observation: ordinary LNO and its C
            # view can change rounding without violating that numeric oracle.
            bitwise[variant] = outputs[0] == outputs[1] if len(outputs) == 2 else None
        report['contexts'][context] = dict(status='PASS', operations=48, buffers=41,
                                         admission=True, fallback_byte_identical=True,
                                         portable_host_lane_simd_disabled=True,
                                         post_lno_status=post_status,
                                         pre_post_bitwise=bitwise, references=checks)
    fixture = Path(__file__).with_name('dsl_common_high.c')
    if args.fixture_image:
        shutil.copyfile(args.fixture_image, out / 'dsl_common_high.B')
    else:
        run('frontend', [args.cc, '-std=c99', '-O0', '-keep', '-c', '-PHASE:c=off', '-OPT:IEEE_arith=3:roundoff=0', fixture])
    fixture_image = out / 'dsl_common_high.B'
    enabled, trace = backend('forms-enabled', fixture_image, 'on')
    statuses = {m[0]: m[1:] for m in re.findall(r'AI_HIGH 1 ADMISSION (\w+) (admitted|refused) (\w+)', trace)}
    assert statuses['high_affine'][0] == statuses['high_ordered_fold'][0] == 'admitted', statuses
    for name in ('high_dynamic', 'high_unsigned', 'high_stride', 'high_indirect', 'high_volatile', 'high_call'):
        assert statuses[name][0] == 'refused', (name, statuses)
    for label, flag in [('forms-off', 'off'), ('forms-default', None)]:
        other, _ = backend(label, fixture_image, flag)
        assert other.read_bytes() == enabled.read_bytes()
    for level in (0, 1, 2):
        on, trace = backend('forms-O' + str(level) + '-on', fixture_image, 'on', level=level)
        off, _ = backend('forms-O' + str(level) + '-off', fixture_image, 'off', level=level)
        assert 'AI_HIGH ' not in trace and on.read_bytes() == off.read_bytes()
    for label, extra in [('analysis-off', ['-LNO:opt=0'])]:
        _, trace = backend(label, fixture_image, 'on', extra=extra)
        assert 'AI_HIGH ' not in trace
    count = sum(len(c['references']) for c in report['contexts'].values())
    report.update(status='PASS', form_cases=statuses, reference_executions=count,
                  lower_level_fallback=True, no_search_or_snapshot=True, gpu_execution=False,
                  fixture_frontend='cached-image' if args.fixture_image else 'fresh-installed-Open64')
    (out / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    (out / 'SHA256SUMS').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest() + '  ' +
        str(p.relative_to(out)) + '\n' for p in sorted(out.rglob('*')) if p.is_file()))
    print(json.dumps(dict(status='PASS', references=count, form_cases=8, fallback='byte-identical'), indent=2))


def main():
    """Require explicit tool/evidence paths; never silently skip missing tools."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('tools', 'lno', 'b2a', 'lowerer', 'w2c', 'previous', 'include', 'cc', 'source', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--fixture-image', type=Path)
    review(parser.parse_args())


if __name__ == '__main__':
    main()
