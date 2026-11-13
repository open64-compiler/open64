"""Isolated negative tests for the Q6b delayed-copy reference model.

These tests validate review instrumentation, not hardware barriers or GPU races.
"""
import argparse
import ctypes as C
import json
import resource
from pathlib import Path
import subprocess
import sys


def child(library, name):
    """Exercise concrete data readiness and invalid lifetime/order transitions."""
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    lib = C.CDLL(str(library))
    ptr, u64, u32 = C.c_void_p, C.c_uint64, C.c_uint32
    for op, types in dict(init=[ptr, ptr, u64, ptr, u64], acquire=[ptr, u64, u32, u64],
                          copy=[ptr, ptr, ptr, u64, u32], wait=[ptr], publish=[ptr],
                          consume=[ptr], release=[ptr], end=[ptr]).items():
        f = getattr(lib, '__open64_mla_stage_'+op)
        f.argtypes, f.restype = types, None
    lib.open64_q6b_model_report.restype = C.c_char_p
    raw = [(C.c_ubyte*32)() for _ in range(2)]
    decoded = (C.c_ubyte*32)()
    tokens = [C.c_uint64() for _ in range(2)]
    identities = [C.byref(t) for t in tokens]
    source = C.create_string_buffer(bytes(range(16)), 16)
    def op(action, slot=0, *args):
        return getattr(lib, '__open64_mla_stage_'+action)(identities[slot], *args)
    for slot in range(2):
        op('init', slot, raw[slot], 32, decoded, 32)
    op('acquire', 0, 0, 0, 16)
    if name == 'wait-without-copy':
        op('wait')
    if name == 'copy-overrun':
        op('copy', 0, C.addressof(raw[0])+31, source, 16, 2)
    op('copy', 0, raw[0], source, 16, 2)
    assert bytes(raw[0]) == b'\xa7'*32
    if name == 'publish-without-wait':
        op('publish')
    if name == 'acquire-busy':
        op('acquire', 0, 2, 0, 16)
    if name == 'end-pending':
        op('end')
    op('wait')
    assert bytes(raw[0])[:16] == bytes(range(16))
    if name == 'release-without-consumers':
        op('release')
    op('acquire', 1, 1, 0, 16)
    op('copy', 1, raw[1], source, 16, 2)
    if name == 'overwrite-decoded':
        op('wait', 1)
    op('publish')
    if name == 'duplicate-publish':
        op('publish')
    op('consume')
    op('release')
    op('wait', 1)
    assert bytes(raw[1])[:16] == bytes(range(16))
    op('publish', 1)
    op('consume', 1)
    op('release', 1)
    for stage_pass in (1, 2):
        op('acquire', 0, 0, stage_pass, 16)
        op('copy', 0, raw[0], source, 16, 2)
        op('wait')
        op('acquire', 1, 1, stage_pass, 16)
        op('copy', 1, raw[1], source, 16, 2)
        op('publish')
        op('consume')
        op('release')
        op('wait', 1)
        op('publish', 1)
        op('consume', 1)
        op('release', 1)
    if name == 'missing-end':
        assert not json.loads(lib.open64_q6b_model_report())['quiescent']
        return
    op('end')
    op('end', 1)
    report = json.loads(lib.open64_q6b_model_report())
    assert report['quiescent'] and report['acquired'] == report['waited'] == report['consumed'] == 6
    assert report['prefetch_while_current_owned'] == 3 and report['max_live_raw_slots'] == 2
    assert report['delayed_bytes'] == 96 and report['sync_bytes'] == 0


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--library', type=Path, required=True)
    parser.add_argument('--output', type=Path)
    parser.add_argument('--case')
    args = parser.parse_args()
    if args.case:
        child(args.library, args.case)
        return
    args.output.mkdir(parents=True, exist_ok=False)
    results = []
    for name in ('positive', 'wait-without-copy', 'copy-overrun', 'publish-without-wait', 'acquire-busy',
                 'end-pending', 'release-without-consumers', 'overwrite-decoded', 'duplicate-publish', 'missing-end'):
        command = [sys.executable, __file__, '--library', str(args.library), '--case', name]
        run = subprocess.run(command, capture_output=True, text=True, timeout=30)
        (args.output/(name+'.stdout')).write_text(run.stdout)
        (args.output/(name+'.stderr')).write_text(run.stderr)
        expected = name in ('positive', 'missing-end')
        assert (run.returncode == 0) == expected, (name, run.returncode, run.stderr)
        if not expected:
            assert 'Q6B_MODEL_REFUSAL' in run.stderr
        results.append(dict(case=name, returncode=run.returncode, expected_refusal=not expected))
    (args.output/'report.json').write_text(json.dumps(dict(status='PASS', cases=results, gpu_execution=False), indent=2)+'\n')
    print('PASS Q6b delayed-copy model cases='+str(len(results)))


if __name__ == '__main__':
    main()
