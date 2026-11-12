"""Q3 actual host-buffer binding, page/version leases and bundle publication.

Uses authenticated WHIRL-derived Q1 entries, not a replacement attention kernel.
All execution/reference checks are outside estimation-only compiler exploration.
Design: doc/AI-COMPILER-MLA-Q3-SERVING-v1.0.md.
"""
import argparse
import copy
import ctypes as C
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import threading
import torch

U = C.c_uint64
class Handle(C.Structure):
    _fields_ = [('owner', U), ('id', U), ('generation', U)]
class View(C.Structure):
    _fields_ = [('handle', Handle), ('offset', U), ('bytes', U)]
CALLBACK = C.CFUNCTYPE(C.c_bool, C.c_void_p, C.POINTER(C.c_void_p), C.c_uint32)
DTYPES = dict(float32=torch.float32, int64=torch.int64, int8=torch.int8, uint8=torch.uint8, bool=torch.bool)


def review(args):
    """Run the native adapter on real registered storage and retained C entries."""
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    sys.path.insert(0, str(args.source / 'osprey/torch2whirl/python/tests'))
    from models.mla_quantized_paged import CASES, make_fixture
    from models.mla_quantized_reference import reference
    api = C.CDLL(str(args.adapter))
    specs = {
        'create': ([C.c_char_p, U], C.c_void_p), 'destroy': ([C.c_void_p], C.c_int),
        'valid': ([C.c_void_p], C.c_int), 'error': ([C.c_void_p], C.c_char_p),
        'register': ([C.c_void_p, C.c_void_p, U, C.c_char_p, C.POINTER(Handle)], C.c_int),
        'write': ([C.c_void_p, Handle, U, C.c_void_p, U], C.c_int),
        'retire': ([C.c_void_p, Handle], C.c_int),
        'import': ([C.c_void_p, C.POINTER(View), C.c_uint, View, C.POINTER(U)], C.c_int),
        'begin': ([C.c_void_p, C.POINTER(View), C.c_uint, View, U, C.POINTER(U)], C.c_int),
        'run': ([C.c_void_p, U, CALLBACK, C.c_void_p, C.POINTER(U)], C.c_int),
        'cancel': ([C.c_void_p, U], C.c_int), 'live': ([C.c_void_p, U], C.c_int),
        'reader': ([C.c_void_p, U, C.POINTER(U)], C.c_int),
        'reader_done': ([C.c_void_p, U], C.c_int), 'bundle_retire': ([C.c_void_p, U], C.c_int),
        'report': ([C.c_void_p], C.c_char_p),
        'bundle_report': ([C.c_void_p, U], C.c_char_p),
    }
    functions = {}
    for name, (types, result) in specs.items():
        f = getattr(api, 'open64_q3_' + name)
        f.argtypes, f.restype = types, result
        functions[name] = f
    checks, commands = [], []
    q4 = None
    if args.q4:
        from mla_q4_costing_review import Q4_REVIEW
        q4 = Q4_REVIEW(api, out)
    digests = dict((name, sha) for sha, name in
                   (line.split('  ', 1) for line in (args.previous / 'SHA256SUMS').read_text().splitlines()))

    def check(name, condition):
        assert condition, name
        checks.append(name)

    class Invocation:
        """Owner keeps every borrowed allocation alive through native teardown."""
        def __init__(self, case, mutate=None, bad_p=False):
            self.case = case
            self.prior = args.previous / 'production' / (case + '-O3')
            for name in ('q1.high.after.B', 'q1.high.after.so', 'q1.high.after.manifest.json'):
                p = self.prior / name
                assert hashlib.sha256(p.read_bytes()).hexdigest() == digests[str(p.relative_to(args.previous))]
            schema_path = out / (case + '.schema.json')
            if not schema_path.exists():
                argv = [args.lowerer, 'q3-schema', self.prior / 'q1.high.after.B', schema_path]
                p = subprocess.run(list(map(str, argv)), capture_output=True, text=True, timeout=180)
                commands.append(dict(argv=list(map(str, argv)), returncode=p.returncode))
                (out / (case + '.schema.stderr')).write_text(p.stderr)
                assert p.returncode == 0, (case, p.stderr)
            self.schema = json.loads(schema_path.read_text())
            self.manifest = json.loads((self.prior / 'q1.high.after.manifest.json').read_text())
            self.model, values, self.meta = make_fixture(case)
            self.inputs = [x.clone() for x in values]
            if mutate:
                mutate(self.inputs)
            self.session = functions['create'](self.schema['program'].encode(), self.schema['page_tokens'] + (1 if bad_p else 0))
            if bad_p:
                return
            assert functions['valid'](self.session), self.error()
            self.values = self.manifest['recipe']['values']
            self.buffers, self.original, self.views, self.handle_by_root = [], {}, [], {}
            by_input = {i: self.inputs[v['ordinal']] for i, v in enumerate(self.values) if v['role'] == 1}
            for root in self.schema['roots']:
                v = self.values[root]
                size = torch.empty((), dtype=DTYPES[v['dtype']]).element_size()
                for n in v['shape']:
                    size *= n
                payload = bytes(by_input[root].contiguous().view(torch.uint8).flatten().tolist()) if root in by_input else b'\xcd' * size
                view = self.allocate(payload)
                self.views.append(view)
                self.handle_by_root[root] = view.handle
                self.original[root] = payload
            self.views.append(self.allocate((-1).to_bytes(4, 'little', signed=True)))
            g = self.schema['geometry']
            rows = []
            for b in range(g['B']):
                q, l, a = g['Q'], g['L'], g['A']
                phase = {'prefill': 1, 'decode': 2, 'speculative-verify': 3}[self.meta['phase']]
                update = 2 if a else 0 if self.schema['paged'] else 1
                rows.append([phase, q, l-a, l, b*q, 1, 0, update, a] + list(range(l-q, l)) + list(range(l)) + list(range(l-a, l)))
            self.metadata_tensor = torch.tensor(rows, dtype=torch.int64)
            assert self.metadata_tensor.shape == (g['B'], self.schema['metadata_columns'])
            self.metadata = self.allocate(bytes(self.metadata_tensor.view(torch.uint8).flatten().tolist()))
            self.parent, self.ticket = 0, 0
            self.function = getattr(C.CDLL(str(self.prior / 'q1.high.after.so')), self.manifest['function'])
            self.function.argtypes = [C.c_void_p] * len(self.views)
            self.function.restype = None
            self.callback_count = 0
            def call(_, pointers, n):
                self.callback_count += 1
                assert n == len(self.views)
                self.function(*(pointers[i] for i in range(n)))
                return True
            self.callback = CALLBACK(call)

        def error(self):
            return functions['error'](self.session).decode()

        def allocate(self, payload):
            data = b'\xa5' * 32 + payload + b'\x5a' * 32
            buffer = C.create_string_buffer(data, len(data))
            handle = Handle()
            assert functions['register'](self.session, C.addressof(buffer)+32, len(payload), b'host-synchronous', C.byref(handle)), self.error()
            self.buffers.append(buffer)
            return View(handle, 0, len(payload))

        def array(self, views=None):
            return (View * len(self.views))(*(views or self.views))

        def address(self, view):
            for buffer in self.buffers:
                if C.addressof(buffer)+32 == self._address_by_handle(view.handle):
                    return C.addressof(buffer)+32+view.offset
            raise AssertionError('missing allocation')

        def _address_by_handle(self, handle):
            # Registrations are ordered exactly like native views, then metadata
            # and extra test allocations. Track handle order without guessing IDs.
            for h, buffer in zip(self._all_handles, self.buffers):
                if h.id == handle.id:
                    return C.addressof(buffer)+32
            raise AssertionError('missing handle')

        def import_ready(self):
            bundle = U()
            ok = functions['import'](self.session, self.array(), len(self.views), self.metadata, C.byref(bundle))
            if ok:
                self.parent = bundle.value
            return ok

        def begin(self, views=None, parent=None):
            ticket = U()
            ok = functions['begin'](self.session, self.array(views), len(self.views), self.metadata,
                                    self.parent if parent is None else parent, C.byref(ticket))
            self.ticket = ticket.value
            return ok

        def run(self, callback=None, ticket=None):
            bundle = U()
            ok = functions['run'](self.session, self.ticket if ticket is None else ticket,
                                  callback or self.callback, None, C.byref(bundle))
            return bool(ok), bundle.value, self.error()

        def verify(self, views=None, inputs=None):
            views = views or self.views
            inputs = inputs or self.inputs
            with torch.no_grad():
                expected = self.model(*inputs)
            oracle = reference([x.tolist() for x in inputs], self.meta)
            actual = []
            for i, v in enumerate(self.values):
                if v['role'] != 2:
                    continue
                view = views[self.schema['roots'].index(v['storage'])]
                raw = bytearray(C.string_at(self.address(view), view.bytes))
                actual.append(torch.frombuffer(raw, dtype=DTYPES[v['dtype']]).clone().reshape(v['shape']))
            for i, (a, e, ref) in enumerate(zip(actual, expected, oracle)):
                torch.testing.assert_close(a, e, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
                r = torch.tensor(ref, dtype=torch.float64 if i < 2 else a.dtype)
                torch.testing.assert_close(a.double() if i < 2 else a, r, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0)
            for root, v in zip(self.schema['roots'], views):
                if self.values[root]['role'] == 1:
                    assert C.string_at(self.address(v), v.bytes) == self.original[root], ('input mutation', root)
            for buffer in self.buffers:
                assert buffer.raw[:32] == b'\xa5'*32 and buffer.raw[-32:] == b'\x5a'*32

        def close(self):
            assert functions['destroy'](self.session), self.error()
            self.session = None

    # Track identities at registration time; the test never chooses generations.
    allocate = Invocation.allocate
    def tracked(self, payload):
        if not hasattr(self, '_all_handles'):
            self._all_handles = []
        view = allocate(self, payload)
        self._all_handles.append(view.handle)
        return view
    Invocation.allocate = tracked

    api.open64_q3_counter_test.restype = C.c_int
    api.open64_q3_counter_test.argtypes = []
    check('generation exhaustion never wraps or resumes', api.open64_q3_counter_test())
    baseline = {}
    for name in CASES:
        c = Invocation(name)
        if c.schema['paged']:
            check(name+' import', c.import_ready())
        check(name+' begin', c.begin())
        report = json.loads(functions['report'](c.session))
        assert report['bound'] and report['phase'] == c.meta['phase']
        (out/(name+'.facts.json')).write_text(json.dumps(report, indent=2)+'\n')
        if q4:
            q4.Before(c, check)
        check(name+' live', functions['live'](c.session, c.ticket))
        byte = C.c_uint8(0)
        check(name+' pinned write refused', not functions['write'](c.session, c.metadata.handle, 0, C.byref(byte), 1))
        check(name+' teardown refused', not functions['destroy'](c.session))
        check(name+' pinned retire refused', not functions['retire'](c.session, c.views[0].handle))
        ok, bundle, why = c.run()
        check(name+' run publish', ok and bundle != 0)
        c.verify()
        published = json.loads(functions['bundle_report'](c.session, bundle))
        (out/(name+'.published.json')).write_text(json.dumps(published, indent=2)+'\n')
        expected_roots = [c.schema['roots'].index(root) for root in c.schema['returned']]
        assert [a[0] for a in published['components'][:4]] == [c.views[i].handle.id for i in expected_roots]
        assert published['valid_lengths'] == [c.schema['geometry']['L']]*c.schema['geometry']['B']
        if c.parent:
            check(name+' stale predecessor refused', not c.begin(parent=c.parent))
        if q4:
            q4.Expired(c, check)
        check(name+' expired', not functions['live'](c.session, c.ticket))
        check(name+' rerun refused', not c.run()[0])
        reader = U()
        check(name+' reader', functions['reader'](c.session, bundle, C.byref(reader)))
        check(name+' reader retirement blocked', not functions['bundle_retire'](c.session, bundle))
        check(name+' release reader', functions['reader_done'](c.session, reader))
        check(name+' double reader completion refused', not functions['reader_done'](c.session, reader))
        check(name+' successor retire', functions['bundle_retire'](c.session, bundle))
        if c.parent:
            check(name+' predecessor retire', functions['bundle_retire'](c.session, c.parent))
        baseline[name] = dict(outputs=6, phase=report['phase'], page_records=len(report['pages']), native_buffer_binding=True)
        c.close()
        print('PASS serving '+name, flush=True)

    # Mutations of actual page/control/scale bytes must be rejected before call.
    for name, mutate, expected in (
        ('invalid-page', lambda x: x[6].__setitem__((0, 0), 999), 'q3_page_bounds'),
        ('negative-page', lambda x: x[6].__setitem__((0, 0), -1), 'q3_page_bounds'),
        ('token-map', lambda x: x[13].__setitem__(0, 1), 'q3_token_enumeration'),
        ('nonfinite-scale', lambda x: x[3].fill_(float('nan')), 'q3_scale_domain'),
        ('zero-scale', lambda x: x[5].zero_(), 'q3_scale_domain'),
        ('append-slot', lambda x: x[12].__setitem__((0, 0), 0), 'q3_append_slot_mapping'),
    ):
        c = Invocation('append_s8_decode', mutate)
        check(name, not c.import_ready() and expected in c.error() and c.callback_count == 0)
        c.close()
    def duplicate_slots(x):
        x[6][1, -1] = x[6][0, -1]
        x[12][1, 0] = x[12][0, 0]
    c = Invocation('append_s8_decode', duplicate_slots)
    check('duplicate physical append destinations', not c.import_ready() and 'q3_shared_prefix_or_duplicate_write' in c.error())
    c.close()
    def prefix_alias(x):
        x[6][0, -1] = x[6][0, 0]
        x[12][0, 0] = int(x[6][0, 0])*4+2
    c = Invocation('append_s8_decode', prefix_alias)
    check('append aliases retained prefix', not c.import_ready() and 'q3_shared_prefix_or_duplicate_write' in c.error())
    c.close()
    c = Invocation('paged_s8_decode')
    assert c.import_ready()
    for field, label in ((c.schema['incoming'][1], 'scale version'), (c.schema['table'], 'page mapping version')):
        views = list(c.views)
        index = c.schema['roots'].index(field)
        views[index] = c.allocate(C.string_at(c.address(c.views[index]), c.views[index].bytes))
        check('mixed '+label+' refused', not c.begin(views) and 'q3_cache_scale_bundle_join' in c.error())
    byte = C.c_uint8(0)
    scale_view = c.views[c.schema['roots'].index(c.schema['incoming'][1])]
    check('published scale immutable without reader', not functions['write'](c.session, scale_view.handle, 0, C.byref(byte), 1))
    assert functions['bundle_retire'](c.session, c.parent)
    c.close()
    c = Invocation('paged_s8_decode', bad_p=True)
    check('allocator P disagreement', not functions['valid'](c.session) and 'q3_allocator_page_size' in c.error())
    c.close()
    c = Invocation('dense_s8_decode')
    h = Handle()
    check('device memory refused', not functions['register'](c.session, c.address(c.views[0]), c.views[0].bytes, b'device', C.byref(h)))
    check('overlap registration refused', not functions['register'](c.session, c.address(c.views[0]), c.views[0].bytes, b'host-synchronous', C.byref(h)))
    alias = list(c.views)
    alias[1] = alias[0]
    check('ABI alias/extent refused', not c.begin(alias))
    short = list(c.views)
    short[0] = View(short[0].handle, 0, short[0].bytes-1)
    check('short extent refused', not c.begin(short) and 'q3_buffer_extent' in c.error())
    old = c.views[0].handle
    check('retire unleased', functions['retire'](c.session, old))
    check('same address re-register', functions['register'](c.session, c.address(c.views[0]), c.views[0].bytes, b'host-synchronous', C.byref(h)))
    check('ABA new generation', h.generation != old.generation)
    check('old allocation rejected before dereference', not c.begin() and 'q3_stale_allocation' in c.error())
    c.close()

    # A foreign session cannot steal an existing owner's live spans or tokens.
    a, b = Invocation('dense_s8_decode'), Invocation('dense_s8_decode')
    check('cross-owner overlapping span', not functions['register'](b.session, a.address(a.views[0]), a.views[0].bytes, b'host-synchronous', C.byref(h)))
    check('cross-owner handle', not functions['retire'](b.session, a.views[0].handle))
    assert a.begin() and b.begin()
    check('foreign ticket rejected', not b.run(ticket=a.ticket)[0] and functions['live'](a.session, a.ticket))
    assert functions['cancel'](a.session, a.ticket) and functions['cancel'](b.session, b.ticket)
    a.close(); b.close()

    # Real callback failures leave source status/dirty scratch without publication.
    c = Invocation('dense_s8_decode')
    assert c.begin()
    empty = CALLBACK(lambda _, p, n: True)
    ok, bundle, why = c.run(empty)
    check('no status completion blocks publication', not ok and not bundle and why == 'q3_source_status_refused')
    c.close()
    c = Invocation('dense_s8_decode', lambda x: x[2].reshape(-1).__setitem__(0, float('nan')))
    assert c.begin()
    ok, bundle, why = c.run()
    check('source runtime refusal not published', not ok and not bundle and why == 'q3_source_status_refused')
    c.close()
    c = Invocation('dense_s8_decode')
    assert c.begin()
    ok, bundle, why = c.run(CALLBACK(lambda _, p, n: False))
    check('callback incomplete not published', not ok and not bundle and why == 'q3_callback_incomplete')
    c.close()
    c = Invocation('dense_s8_decode')
    assert c.begin()
    # Deliberate contract violation through an unmanaged pointer: control images
    # are re-observed at completion and fail closed. Payload hashing is not used.
    def corrupt(_, p, n):
        c.function(*(p[i] for i in range(n)))
        C.c_int64.from_address(c.address(c.metadata)+8).value = 0
        return True
    ok, bundle, why = c.run(CALLBACK(corrupt))
    check('controls changed during callback not published', not ok and not bundle)
    c.close()

    c = Invocation('append_s8_decode')
    assert c.import_ready() and c.begin()
    def bad_scale(_, p, n):
        c.function(*(p[i] for i in range(n)))
        scale_index = c.schema['roots'].index(c.schema['returned'][1])
        # Corrupt an actually read physical slot; unused padding is permitted.
        slot = int(c.inputs[6][0, 0])*4
        C.c_float.from_address(p[scale_index]+slot*4).value = float('nan')
        return True
    ok, bundle, why = c.run(CALLBACK(bad_scale))
    check('bad successor scale not published', not ok and not bundle and why == 'q3_scale_domain')
    reader = U()
    check('old bundle survives rejected successor', functions['reader'](c.session, c.parent, C.byref(reader)))
    assert functions['reader_done'](c.session, reader)
    assert functions['bundle_retire'](c.session, c.parent)
    c.close()

    # Two real synchronous callbacks from one parent use private scratch. One
    # atomic publication wins; the other cannot publish over a newer bundle.
    c = Invocation('append_u4_verify')
    assert c.import_ready() and c.begin()
    first = c.ticket
    second_views = []
    for root, view in zip(c.schema['roots'], c.views):
        second_views.append(view if c.values[root]['role'] == 1 else c.allocate(b'\xcd'*view.bytes))
    second_views.append(c.allocate((-1).to_bytes(4,'little',signed=True)))
    assert c.begin(second_views)
    second = c.ticket
    old_reader = U()
    assert functions['reader'](c.session, c.parent, C.byref(old_reader))
    barrier = threading.Barrier(2)
    results = []
    def concurrent(_, p, n):
        barrier.wait(timeout=20)
        c.function(*(p[i] for i in range(n)))
        return True
    callback = CALLBACK(concurrent)
    def worker(ticket):
        results.append(c.run(callback, ticket))
    threads = [threading.Thread(target=worker, args=(ticket,)) for ticket in (first, second)]
    for t in threads: t.start()
    for t in threads: t.join(timeout=30)
    assert not any(t.is_alive() for t in threads)
    check('atomic publication race single winner', len(results)==2 and sum(r[0] for r in results)==1)
    winner = next(r[1] for r in results if r[0])
    check('stale publication loser', any(r[2]=='q3_stale_bundle' for r in results if not r[0]))
    c.verify();c.verify(second_views)
    check('old reader remains pinned after new publication', not functions['bundle_retire'](c.session, c.parent))
    assert functions['reader_done'](c.session, old_reader)
    assert functions['bundle_retire'](c.session, c.parent)
    assert functions['bundle_retire'](c.session, winner)
    c.close()

    c = Invocation('dense_s8_decode')
    row = c.metadata_tensor.clone(); row[:, 0] = 1
    raw = bytes(row.view(torch.uint8).flatten().tolist())
    data = C.create_string_buffer(raw)
    assert functions['write'](c.session, c.metadata.handle, 0, data, len(raw))
    assert c.begin()
    check('same code accepts bound prefill-tail intent', json.loads(functions['report'](c.session))['phase'] == 'prefill')
    ok, bundle, why = c.run();assert ok
    c.verify();assert functions['bundle_retire'](c.session, bundle)
    c.close()

    c = Invocation('dense_s8_prefill')
    row = c.metadata_tensor.clone(); row[:, 0] = 2
    raw = bytes(row.view(torch.uint8).flatten().tolist()); data = C.create_string_buffer(raw)
    assert functions['write'](c.session, c.metadata.handle, 0, data, len(raw))
    check('contradictory decode declaration refuses before execution', not c.begin() and c.callback_count == 0)
    row[:, 0] = 0
    raw = bytes(row.view(torch.uint8).flatten().tolist()); data = C.create_string_buffer(raw)
    assert functions['write'](c.session, c.metadata.handle, 0, data, len(raw))
    assert c.begin()
    check('unknown phase retains compatible generic source', json.loads(functions['report'](c.session))['phase'] == 'unknown')
    ok, bundle, why = c.run(); assert ok
    c.verify(); assert functions['bundle_retire'](c.session, bundle)
    c.close()

    # Masked zero-query declarations still execute source construction/effects.
    c = Invocation('dense_s8_decode', lambda x: x[6].zero_())
    row = c.metadata_tensor.clone()
    row[:, 0:2] = 0
    row[:, 4] = 0
    raw = bytes(row.view(torch.uint8).flatten().tolist())
    C.memmove(c.address(c.metadata), raw, len(raw))
    assert c.begin()
    report = json.loads(functions['report'](c.session))
    assert report['phase'] == 'unknown'
    if q4:
        q4.Before(c, check, 'zero-active')
    ok, bundle, why = c.run()
    check('zero active queries do not skip source', ok and c.callback_count == 1)
    c.verify()
    assert functions['bundle_retire'](c.session, bundle)
    c.close()

    report = dict(status='PASS', checks=checks, source_cases=baseline, compiled_outputs=60,
                  concurrency_actual_callbacks=True, engine_binding='native-host-synchronous',
                  allocator_owned_generations=True, external_initial_import_ready_precondition=True,
                  search_invoked=bool(q4), gpu_execution=False, q4_costing=bool(q4))
    (out/'commands.json').write_text(json.dumps(commands, indent=2)+'\n')
    (out/'report.json').write_text(json.dumps(report, indent=2)+'\n')
    print('PASS Q3 native checks='+str(len(checks)))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('source', 'previous', 'lowerer', 'adapter', 'output'):
        parser.add_argument('--'+name, type=Path, required=True)
    parser.add_argument('--q4', action='store_true')
    review(parser.parse_args())
