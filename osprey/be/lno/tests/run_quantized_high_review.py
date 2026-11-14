"""Execute actual Q1 High WHIRL through whirl2c and compiled host C.

Design: doc/AI-COMPILER-MLA-Q1-HIGH-OWNED-FACTS-v1.0.md. Reuse authenticated
Q1 native source inputs, rebuild only the changed semantic High boundary.
All compilation/reference checks are outside estimation-only exploration.
"""
import argparse
import copy
import ctypes
import difflib
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import torch

DTYPES = dict(float32=torch.float32, bool=torch.bool, int8=torch.int8,
              uint8=torch.uint8, int64=torch.int64)


def execute(library, manifest, inputs, expected_status=0, prefix_bytes=32):
    """Invoke generated C with canaries and separate disjoint storage buffers."""
    values = manifest['recipe']['values']
    buffers, originals, input_ids = {}, {}, set()
    by_input = {i: inputs[v['ordinal']] for i, v in enumerate(values) if v['role'] == 1}
    for storage in manifest['buffers']:
        value = values[storage]
        n = 1
        for extent in value['shape']:
            n *= extent
        size = n * torch.empty((), dtype=DTYPES[value['dtype']]).element_size()
        payload = bytes(by_input[storage].contiguous().view(torch.uint8).flatten().tolist()) if storage in by_input else b'\xcd' * size
        data = b'\xa5' * prefix_bytes + payload + b'\x5a' * 32
        assert len(payload) == size
        buffers[storage] = ctypes.create_string_buffer(data, len(data))
        originals[storage] = data
        if storage in by_input:
            input_ids.add(storage)
    status = ctypes.c_int32(-1)
    function = getattr(ctypes.CDLL(str(library)), manifest['function'])
    function.argtypes = [ctypes.c_void_p] * (len(buffers) + 1)
    function.restype = None
    function(*(ctypes.addressof(buffers[s]) + prefix_bytes for s in manifest['buffers']), ctypes.byref(status))
    assert status.value == expected_status, (status.value, expected_status)
    for s, buffer in buffers.items():
        assert buffer.raw[:prefix_bytes] == originals[s][:prefix_bytes] and buffer.raw[-32:] == originals[s][-32:], ('canary', s)
        if s in input_ids:
            assert buffer.raw == originals[s], ('input mutation', s)
    outputs = []
    for v in values:
        if v['role'] == 2:
            raw = bytearray(buffers[v['storage']].raw[prefix_bytes:-32])
            outputs.append(torch.frombuffer(raw, dtype=DTYPES[v['dtype']]).clone().reshape(v['shape']))
    return outputs


def review(args):
    """Retain separate native snapshots, raw diffs, actual emitted C and logs."""
    root = args.output.resolve()
    root.mkdir(parents=True, exist_ok=False)
    commands, cases = [], {}
    sys.path.insert(0, str(args.source / 'osprey/torch2whirl/python/tests'))
    from models.mla_quantized_paged import CASES, make_fixture
    from models.mla_quantized_reference import reference
    digest = {}
    for line in (args.previous / 'SHA256SUMS').read_text().splitlines():
        sha, name = line.split('  ', 1)
        digest[name] = sha

    def run(label, argv, code=0, cwd=None):
        """Record exact argument vectors and diagnostics, including refusals."""
        result = subprocess.run(list(map(str, argv)), capture_output=True, text=True,
                                cwd=cwd or root, timeout=180)
        (root / (label + '.stdout')).write_text(result.stdout)
        (root / (label + '.stderr')).write_text(result.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=result.returncode))
        (root / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert (result.returncode == 0) == (code == 0), (label, result.returncode, result.stderr[-3000:])
        return result

    for name in CASES:
        case = root / name
        case.mkdir()
        prior = args.previous / 'review' / name
        for filename in ('source.B', 'inputs-and-source.pt'):
            p = prior / filename
            assert hashlib.sha256(p.read_bytes()).hexdigest() == digest[str(p.relative_to(args.previous))]
        recipe = case / 'recipe.json'
        run(name + '-prepare', [args.lowerer, 'q1-prepare', prior / 'source.B', recipe])
        before, after = case / 'high.before.B', case / 'high.after.B'
        run(name + '-high', [args.lowerer, 'q1-high', recipe, before, after, case / 'manifest.json'])
        for phase in ('before', 'after'):
            image = case / ('high.' + phase + '.B')
            run(name + '-' + phase, [args.b2a, '-st', '-src', image, image.with_suffix('.T')])
        (case / 'high.before-after.diff').write_text(''.join(difflib.unified_diff(
            before.with_suffix('.T').read_text().splitlines(True), after.with_suffix('.T').read_text().splitlines(True),
            fromfile='high.before.T', tofile='high.after.T')))
        run(name + '-facts', [args.lowerer, 'q1-facts', after, recipe, case / 'facts.json'])
        if name in ('dense_s8_decode', 'append_u4_verify'):
            nb, na = case / 'mutation.before.B', case / 'mutation.after.B'
            run(name + '-invalidation', [args.lowerer, 'q1-high-facts-test', recipe, nb, na, case / 'invalidation.json'])
            alias_image = Path(str(na) + '.alias.B')
            for image in (nb, na, alias_image):
                run(name + '-' + image.stem, [args.b2a, '-st', '-src', image, image.with_suffix('.T')])
            (case / 'mutation.before-after.diff').write_text(''.join(difflib.unified_diff(
                nb.with_suffix('.T').read_text().splitlines(True), na.with_suffix('.T').read_text().splitlines(True),
                fromfile='mutation.before.T', tofile='mutation.after.T')))
            (case / 'alias.before-after.diff').write_text(''.join(difflib.unified_diff(
                nb.with_suffix('.T').read_text().splitlines(True), alias_image.with_suffix('.T').read_text().splitlines(True),
                fromfile='mutation.before.T', tofile=alias_image.with_suffix('.T').name)))
        c = case / 'high.c'
        run(name + '-w2c', [args.w2c, '-TARG:abi=n64', '-O0', '-ft,' + str(case / 'w2c.phase.t'), '-CLIST:dotc_file=' + str(c),
                           '-CLIST:doth_file=' + str(c.with_suffix('.h')), after], cwd=case)
        library = case / 'high.so'
        run(name + '-cc', ['gcc', '-std=c99', '-O2', '-shared', '-fPIC', '-ffp-contract=off', '-fno-fast-math',
                          '-I' + str(args.source / 'osprey/include'), '-include',
                          args.source / 'osprey/be/lno/tests/common_high_host.h', c, '-lm', '-o', library])
        # macOS bind mounts are commonly case insensitive: a default .t phase
        # trace would overwrite the independently reopened .T review dump.
        trace = after.with_suffix('.T').read_text()
        assert 'DO_LOOP' in trace and 'ARRAY' in trace and 'COMMENT' in trace, 'High trace was lost'
        assert before.with_suffix('.T').stat().st_size > 0
        manifest = json.loads((case / 'manifest.json').read_text())
        if name == 'dense_s8_decode':
            valid = json.loads(recipe.read_text())
            mutations = {}
            for label, key, value in (('physical_level', 'opt_level', 3), ('launch_threads', 'threads', 128),
                                      ('unknown_schedule', 'fuse', True)):
                bad = copy.deepcopy(valid)
                bad[key] = value
                mutations[label] = bad
            bad = copy.deepcopy(valid)
            next(op for op in bad['operations'] if op['name'] == 'common.round_even')['attributes']['attr.rounding'] = 'away_from_zero'
            mutations['rounding'] = bad
            bad = copy.deepcopy(valid)
            bad['operations'][0]['version'] = 65535
            mutations['unknown_version'] = bad
            bad = copy.deepcopy(valid)
            bad['values'][0]['cache_scale'] = 'absent-scale'
            mutations['scale_ssa'] = bad
            bad = copy.deepcopy(valid)
            next(op for op in bad['operations'] if op['name'] == 'common.clamp')['attributes']['attr.max_bits'] = '3f800000'
            mutations['absent_bound'] = bad
            for label, bad in mutations.items():
                invalid = case / ('invalid-' + label + '.json')
                invalid.write_text(json.dumps(bad, indent=2) + '\n')
                outputs = [case / ('refused-' + label + suffix) for suffix in ('.before.B', '.after.B', '.json')]
                run(name + '-reject-' + label, [args.lowerer, 'q1-high', invalid, *outputs], code=1)
                assert not any(p.exists() for p in outputs), label + ': published refused artifacts'
        payload = torch.load(prior / 'inputs-and-source.pt', weights_only=True)
        actual = execute(library, manifest, payload['inputs'])
        _, _, meta = make_fixture(name)
        oracle = reference([x.tolist() for x in payload['inputs']], meta)
        for i, (a, b, ref) in enumerate(zip(actual, payload['source'], oracle)):
            # Source contraction/reduction implementations may use different F32
            # fold trees. Encoded bytes/scales retain exact source arithmetic.
            torch.testing.assert_close(a, b, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
            expected = torch.tensor(ref, dtype=torch.float64 if i < 2 else a.dtype)
            torch.testing.assert_close(a.double() if i < 2 else a, expected,
                                       rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
        boundaries, guards = [], []
        if (prior / 'decoder-boundary.pt').exists():
            boundary = torch.load(prior / 'decoder-boundary.pt', weights_only=True)
            decoded = execute(library, manifest, boundary['inputs'])
            for i, (a, b) in enumerate(zip(decoded, boundary['source'])):
                torch.testing.assert_close(a, b, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
            boundaries.append('signed -128/all sixteen low-high nibbles')
        if name.startswith(('dense', 'fake')):
            # Force exact half-integer quantization with scale=1, including
            # positive/negative ties and signed zero. Source model is a second
            # check here; the normal payload also uses the independent oracle.
            boundary = [x.clone() for x in payload['inputs']]
            maximum = 7 if 'u4' in name else 127
            for index in (2, 3):
                flat = boundary[index].reshape(-1, boundary[index].shape[-1])
                pattern = [0.5, -0.5, 1.5, -1.5, 2.5, -2.5]
                for j in range(flat.shape[1] - 1):
                    flat[:, j] = pattern[j % len(pattern)]
                flat[:, -1] = maximum
            model, _, _ = make_fixture(name)
            with torch.no_grad():
                expected = model(*boundary)
            decoded = execute(library, manifest, boundary)
            for i, (a, b) in enumerate(zip(decoded, expected)):
                torch.testing.assert_close(a, b, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
                if i >= 2 and a.dtype == torch.float32:
                    assert torch.equal(a.view(torch.int32), b.view(torch.int32)), 'source state F32 bits'
            boundaries.append('RNE positive/negative half ties with scale=1')
            for nonfinite in (float('nan'), float('inf')):
                invalid = [x.clone() for x in payload['inputs']]
                invalid[2].reshape(-1)[0] = nonfinite
                execute(library, manifest, invalid, 1)
            guards.append('nonfinite_encoder')
        if name.startswith(('paged', 'append')):
            for label, value in (('page_bounds', 999), ('integer_overflow', 2**63 - 1), ('negative_page', -1)):
                invalid = [x.clone() for x in payload['inputs']]
                invalid[6][0, 0] = value
                execute(library, manifest, invalid, 1)
                guards.append(label)
        if name.startswith('append'):
            invalid = [x.clone() for x in payload['inputs']]
            invalid[12][1, 0] = invalid[12][0, 0]
            execute(library, manifest, invalid, 1)
            guards.append('duplicate_updates')
        refused = case / 'legacy-refused.json'
        run(name + '-legacy-refusal', [args.lowerer, 'prepare', prior / 'source.B', refused, '--verify', '3'], code=1)
        assert not refused.exists()
        facts = json.loads((case / 'facts.json').read_text())
        returned = [s for s in facts['storages'] if s['output'] and s['role']]
        assert len(returned) == 2 and all(s['scale_version'] and s['lineage'] == 'returned-copy' for s in returned)
        if name.startswith('append'):
            assert all(s['scale_pair_ready_group'].startswith('operation:') for s in returned)
        if name.startswith(('paged', 'append')):
            assert any(a['indirect'] for a in facts['accesses']), 'lost scalar page-index provenance'
        cases[name] = dict(outputs=6, compiled_host_correctness=True, source_and_expanded_reference=True,
                           guards=guards, boundaries=boundaries, canaries=True, immutable_inputs=True,
                           owned_storages=len(facts['storages']), owned_accesses=len(facts['accesses']),
                           indirect_accesses=sum(a['indirect'] for a in facts['accesses']))
        print('PASS ' + name, flush=True)
    report = dict(schema='open64.q1-high-validation.v1', status='PASS', cases=cases,
                  search_invoked=False, quantized_optimization_enabled=False, gpu_execution=False,
                  scope='guarded O0 semantic High; compiled whirl2c host execution; owned structural facts')
    (root / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('source', 'previous', 'lowerer', 'b2a', 'w2c'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    if args.output is None:
        base = os.environ.get('OPEN64_DSL_TEST_ARTIFACT_DIR')
        if base is None:
            parser.error('--output or OPEN64_DSL_TEST_ARTIFACT_DIR is required')
        args.output = Path(base) / 'q1-high'
    review(args)
