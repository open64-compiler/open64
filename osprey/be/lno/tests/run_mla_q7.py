"""Q7 real NVIDIA execution of authenticated fixed Q6d MLA code.

GPU host needs only Python 3.10+, the actual Driver and the packet's native
binding library. No PyTorch, compiler search, JIT, source fallback launch or
algorithm selection is performed. See doc/AI-COMPILER-MLA-Q7-QUALIFICATION-v1.0.md.
"""
import argparse
import ctypes as C
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import sys
import time
from mla_q7_common import authenticate, compare, summarize, sanitizer_flags, check_sanitizer

U = C.c_uint64
P = C.c_void_p
HASH = C.CFUNCTYPE(C.c_bool, P, C.c_size_t, C.POINTER(C.c_ubyte))
STATE = dict(gpu_execution=False, device_submission_attempted=False)


class HANDLE(C.Structure):
    _fields_ = [('owner', U), ('id', U), ('generation', U)]


class VIEW(C.Structure):
    _fields_ = [('handle', HANDLE), ('offset', U), ('bytes', U)]


class Unavailable(RuntimeError):
    """Absence is an explicit pending hardware gate, never successful execution."""


class DRIVER:
    """Own only this qualification process's context/streams/timing events."""
    def __init__(self, ordinal):
        if platform.system() != 'Linux' or platform.machine() not in ('x86_64', 'AMD64'):
            raise Unavailable('packet requires Linux x86_64 and a real NVIDIA Driver')
        try:
            self.lib = C.CDLL('libcuda.so.1', mode=C.RTLD_GLOBAL)
        except OSError as error:
            raise Unavailable('libcuda.so.1 is unavailable: ' + str(error)) from error
        self.functions = {}
        specs = {'cuInit': [C.c_uint], 'cuDriverGetVersion': [C.POINTER(C.c_int)],
                 'cuDeviceGetCount': [C.POINTER(C.c_int)], 'cuDeviceGet': [C.POINTER(C.c_int), C.c_int],
                 'cuDeviceGetName': [P, C.c_int, C.c_int], 'cuDeviceGetUuid_v2': [P, C.c_int],
                 'cuDeviceGetAttribute': [C.POINTER(C.c_int), C.c_int, C.c_int],
                 'cuCtxCreate_v2': [C.POINTER(P), C.c_uint, C.c_int], 'cuCtxDestroy_v2': [P],
                 'cuStreamCreate': [C.POINTER(P), C.c_uint], 'cuStreamSynchronize': [P], 'cuStreamDestroy_v2': [P],
                 'cuEventCreate': [C.POINTER(P), C.c_uint], 'cuEventRecord': [P, P],
                 'cuEventSynchronize': [P], 'cuEventElapsedTime': [C.POINTER(C.c_float), P, P], 'cuEventDestroy_v2': [P],
                 'cuMemAllocHost_v2': [C.POINTER(P), C.c_size_t], 'cuMemFreeHost': [P],
                 'cuMemcpyDtoHAsync_v2': [P, U, C.c_size_t, P]}
        try:
            for name, args in specs.items():
                f = getattr(self.lib, name); f.argtypes = args; f.restype = C.c_int; self.functions[name] = f
        except AttributeError as error:
            raise Unavailable('Driver lacks required API: ' + str(error)) from error
        result = self.functions['cuInit'](0)
        if result in (34, 35, 100, 803, 804):
            raise Unavailable('cuInit unavailable/stub/incompatible Driver: ' + str(result))
        self.require(result, 'cuInit')
        version = C.c_int(); self.call('cuDriverGetVersion', C.byref(version))
        if version.value < 12080:
            raise Unavailable('CUDA Driver API >=12.8 is required')
        count = C.c_int(); self.call('cuDeviceGetCount', C.byref(count))
        if not count.value:
            raise Unavailable('no visible NVIDIA device')
        if not 0 <= ordinal < count.value:
            raise ValueError('requested device ordinal is not visible')
        device = C.c_int(); self.call('cuDeviceGet', C.byref(device), ordinal)
        def attr(number):
            value = C.c_int(); self.call('cuDeviceGetAttribute', C.byref(value), number, device.value); return value.value
        major, minor = attr(75), attr(76)
        if (major, minor) != (9, 0):
            raise Unavailable('fixed Q6c/Q6d artifact requires compute capability 9.0; found %d.%d' % (major, minor))
        name = C.create_string_buffer(256); uuid = (C.c_ubyte * 16)()
        self.call('cuDeviceGetName', name, len(name), device.value); self.call('cuDeviceGetUuid_v2', uuid, device.value)
        self.info = dict(name=name.value.decode(), uuid=bytes(uuid).hex(), ordinal=ordinal,
                         driver_api_version=version.value, compute_capability=[major, minor], multiprocessors=attr(16),
                         clock_khz=attr(13), memory_clock_khz=attr(36), memory_bus_width_bits=attr(37),
                         shared_bytes_per_block=attr(8), max_threads_per_block=attr(1),
                         CUDA_VISIBLE_DEVICES=os.environ.get('CUDA_VISIBLE_DEVICES'),
                         python=sys.version, platform=platform.platform())
        self.context = P(); self.streams = []; self.events = []
        self.call('cuCtxCreate_v2', C.byref(self.context), 0, device.value)
        for _ in range(3):
            stream = P(); self.call('cuStreamCreate', C.byref(stream), 1); self.streams.append(stream)

    @staticmethod
    def require(result, label):
        """An unexpected CUDA error denies acceptance and implies no drain."""
        if result:
            raise RuntimeError(label + ' CUDA error ' + str(result))

    def call(self, name, *args):
        self.require(self.functions[name](*args), name)

    def event(self):
        event = P(); self.call('cuEventCreate', C.byref(event), 0); self.events.append(event); return event

    def close(self):
        """Called only after every adapter/reader/host-staging owner was retired."""
        for stream in self.streams:
            self.call('cuStreamSynchronize', stream)
        for event in self.events:
            self.call('cuEventDestroy_v2', event)
        for stream in self.streams:
            self.call('cuStreamDestroy_v2', stream)
        self.call('cuCtxDestroy_v2', self.context)


class API:
    """Private qualification ABI over the same production Q6d adapter and Q3."""
    def __init__(self, library):
        self.lib = C.CDLL(str(library)); self.f = {}
        specs = {'create_on': ([C.c_char_p, C.c_char_p, P, C.c_size_t, HASH, P, P, P], P),
                 'valid': ([P], C.c_int), 'error': ([P], C.c_char_p), 'destroy': ([P], C.c_int),
                 'allocate': ([P, U, C.POINTER(HANDLE)], C.c_int), 'retire': ([P, HANDLE], C.c_int),
                 'write': ([P, HANDLE, U, P, U], C.c_int), 'read': ([P, HANDLE, U, P, U], C.c_int),
                 'import': ([P, C.POINTER(VIEW), C.c_uint, VIEW, C.POINTER(U)], C.c_int),
                 'begin': ([P, C.POINTER(VIEW), C.c_uint, VIEW, U, C.POINTER(U)], C.c_int),
                 'submit': ([P, U], C.c_int), 'complete': ([P, U, C.c_int, C.POINTER(U)], C.c_int),
                 'cancel': ([P, U, C.c_int], C.c_int), 'live': ([P, U], C.c_int), 'drain': ([P], C.c_int),
                 'bundle_retire': ([P, U], C.c_int), 'bundle': ([P, U, C.POINTER(U), C.POINTER(C.c_uint)], C.c_int),
                 'reader_on': ([P, U, C.POINTER(U), C.POINTER(U), C.POINTER(C.c_uint), P], C.c_int),
                 'reader_done': ([P, U, C.c_int], C.c_int)}
        for name, (args, result) in specs.items():
            f = getattr(self.lib, 'open64_q6d_' + name); f.argtypes = args; f.restype = result; self.f[name] = f
        @HASH
        def sha(data, size, target):
            C.memmove(target, hashlib.sha256(C.string_at(data, size)).digest(), 32); return True
        self.sha = sha


class SESSION:
    """Persistent owned buffers; each sample gets a fresh Q3 ticket/publication."""
    def __init__(self, api, driver, folder, case, prefix):
        self.api, self.driver, self.folder, self.case = api, driver, folder, case
        self.handles = []; self.views = []; self.guards = []; self.bundles = []; self.tickets = []
        self.pending_host = []; self.readers = []; self.prefix = prefix
        descriptor = (folder / 'code.json').read_bytes(); image = (folder / 'native.cubin').read_bytes()
        self.object = api.f['create_on'](descriptor, descriptor, image, len(image), api.sha,
                                            driver.context, driver.streams[0], driver.streams[1])
        self.check(api.f['valid'](self.object), 'authorized fixed-code Open')
        for buffer in case['buffers']:
            self.views.append(self.allocate(buffer['bytes'], (folder / buffer['input']).read_bytes() if buffer.get('input') else None))
        self.meta = self.allocate(case['metadata_bytes'], (folder / 'metadata.bin').read_bytes())

    def check(self, value, label):
        if not value:
            raise RuntimeError(label + ': ' + self.api.f['error'](self.object).decode())

    def allocate(self, size, payload):
        """Initialize source inputs and guards; leave scratch/output interiors undefined."""
        handle = HANDLE(); total = self.prefix + size + 256
        self.check(self.api.f['allocate'](self.object, total, C.byref(handle)), 'Allocate')
        self.handles.append(handle); self.guards.append((handle, total, size))
        self.write(handle, 0, b'\xa7' * self.prefix); self.write(handle, self.prefix + size, b'\xa7' * 256)
        if payload is not None:
            if len(payload) != size: raise ValueError('input extent differs')
            self.write(handle, self.prefix, payload)
        return VIEW(handle, self.prefix, size)

    def write(self, handle, offset, data):
        self.check(self.api.f['write'](self.object, handle, offset, data, len(data)), 'Write')

    def read(self, handle, offset, size):
        data = C.create_string_buffer(size)
        self.check(self.api.f['read'](self.object, handle, offset, data, size), 'Read')
        return data.raw

    def run(self, timed=False, lifecycle=False):
        """Fresh fixed-request replay, not an evolving serving decode sequence."""
        f = self.api.f; parent = U(); bundle = U(); ticket = U()
        views = (VIEW * len(self.views))(*self.views)
        if self.case['paged']:
            self.check(f['import'](self.object, views, len(views), self.meta, C.byref(parent)), 'fresh Q3 Import')
            self.bundles.append(parent.value)
        started = time.perf_counter_ns()
        self.check(f['begin'](self.object, views, len(views), self.meta, parent, C.byref(ticket)), 'fresh Q3 Begin')
        self.tickets.append(ticket.value)
        if lifecycle:
            if f['write'](self.object, self.views[0].handle, self.views[0].offset, b'\x00', 1):
                raise RuntimeError('prepared source mutation was admitted')
            self.check(f['cancel'](self.object, ticket, 1), 'prepared cancellation')
            if f['live'](self.object, ticket): raise RuntimeError('cancelled ticket remains live')
            self.tickets.remove(ticket.value)
            self.check(f['begin'](self.object, views, len(views), self.meta, parent, C.byref(ticket)), 'fresh Begin after cancellation')
            self.tickets.append(ticket.value)
        if timed:
            if not hasattr(self, 'start'):
                self.start = self.driver.event(); self.stop = self.driver.event()
            self.driver.call('cuEventRecord', self.start, self.driver.streams[1])
        STATE['device_submission_attempted'] = True
        self.check(f['submit'](self.object, ticket), 'Submit')
        if lifecycle and f['submit'](self.object, ticket):
            raise RuntimeError('duplicate Submit was admitted')
        if timed:
            self.driver.call('cuEventRecord', self.stop, self.driver.streams[1])
            self.driver.call('cuEventSynchronize', self.stop)
        self.check(f['complete'](self.object, ticket, 1, C.byref(bundle)), 'completion/status/version/parent publication')
        STATE['gpu_execution'] = True
        if not bundle.value or f['live'](self.object, ticket):
            raise RuntimeError('publication did not expire its ticket')
        self.tickets.remove(ticket.value); self.bundles.append(bundle.value)
        host_us = (time.perf_counter_ns() - started) / 1000
        span = C.c_float()
        if timed:
            self.driver.call('cuEventElapsedTime', C.byref(span), self.start, self.stop)
        return bundle.value, dict(host_begin_submit_complete_us=host_us, submission_stream_span_ms=span.value if timed else None)

    def verify(self, bundle):
        """Check six outputs against all three retained oracles, inputs and guards."""
        results = []
        for output in self.case['outputs']:
            view = self.views[output['ordinal']]; data = self.read(view.handle, view.offset, view.bytes)
            refs = {}
            for ref in output['references']:
                refs[ref['name']] = compare(data, (self.folder / ref['file']).read_bytes(), output['dtype'], ref['dtype'], output['rtol'], output['atol'])
            results.append(refs)
        if len(results) != 6: raise ValueError('incomplete source result tuple')
        for view, buffer in zip(self.views, self.case['buffers']):
            if buffer.get('input') and self.read(view.handle, view.offset, view.bytes) != (self.folder / buffer['input']).read_bytes():
                raise ValueError('immutable device input changed')
        for handle, total, size in self.guards:
            if self.read(handle, 0, self.prefix) != b'\xa7' * self.prefix or self.read(handle, self.prefix + size, 256) != b'\xa7' * 256:
                raise ValueError('device allocation guard changed')
        versions = (U * 16)(); count = C.c_uint()
        self.check(self.api.f['bundle'](self.object, bundle, versions, C.byref(count)), 'whole publication inspection')
        if count.value != len(self.case['publication_bytes']): raise ValueError('partial Q3 publication tuple')
        return results

    def consume(self, bundle):
        """Read the complete tuple on a different stream under a real Q3 reader."""
        reader = U(); addresses = (U * 16)(); count = C.c_uint(); f = self.api.f
        published = [self.views[i] for i in self.case['publication_ordinals']] + [self.meta]
        expected = [self.read(v.handle, v.offset, v.bytes) for v in published]
        self.check(f['reader_on'](self.object, bundle, C.byref(reader), addresses, C.byref(count), self.driver.streams[2]), 'consumer Reader')
        self.readers.append(reader.value)
        if count.value != len(self.case['publication_bytes']): raise ValueError('consumer tuple extent')
        for address, size in zip(addresses[:count.value], self.case['publication_bytes']):
            host = P(); self.driver.call('cuMemAllocHost_v2', C.byref(host), size); self.pending_host.append(host)
            self.driver.call('cuMemcpyDtoHAsync_v2', host, address, size, self.driver.streams[2])
        if f['bundle_retire'](self.object, bundle): raise ValueError('consumer pin did not protect publication')
        self.check(f['reader_done'](self.object, reader, 1), 'actual consumer completion')
        self.readers.remove(reader.value)
        actual = [C.string_at(host,size) for host,size in zip(self.pending_host,self.case['publication_bytes'])]
        for host in self.pending_host: self.driver.call('cuMemFreeHost', host)
        self.pending_host.clear()
        if actual != expected: raise ValueError('consumer did not observe the exact whole publication tuple')

    def guard_refusal(self):
        """Actual emitted NaN guard must refuse publication and leave safe reuse."""
        import struct
        f = self.api.f; ordinal = self.case['query_ordinal']; view = self.views[ordinal]
        original = (self.folder / self.case['buffers'][ordinal]['input']).read_bytes()
        self.write(view.handle, view.offset, struct.pack('<f', float('nan')) + original[4:])
        views = (VIEW * len(self.views))(*self.views); ticket = U(); result = U()
        self.check(f['begin'](self.object, views, len(views), self.meta, 0, C.byref(ticket)), 'guard Begin')
        self.tickets.append(ticket.value); STATE['device_submission_attempted'] = True
        self.check(f['submit'](self.object, ticket), 'guard Submit')
        if f['complete'](self.object, ticket, 1, C.byref(result)) or result.value or f['live'](self.object, ticket):
            raise RuntimeError('source guard did not expire without publication; unsafe fallback refused')
        self.tickets.remove(ticket.value); self.write(view.handle, view.offset, original)

    def retire(self):
        """Only completed readers/tickets allow either publication generation to retire."""
        for bundle in reversed(self.bundles): self.check(self.api.f['bundle_retire'](self.object, bundle), 'Retire_bundle')
        self.bundles.clear()

    def close(self):
        """No automatic destruction/free on an unproved failure or live reader."""
        if self.tickets or self.readers or self.pending_host: raise RuntimeError('live/uncertain resources retained; process must stop')
        self.retire()
        for handle in self.handles: self.check(self.api.f['retire'](self.object, handle), 'Retire allocation')
        self.handles.clear(); self.check(self.api.f['destroy'](self.object), 'Destroy'); self.object = None


def capture_environment(out, label):
    """Record clocks/power/load without changing host or GPU settings."""
    tool = shutil.which('nvidia-smi')
    if tool:
        p = subprocess.run([tool, '-q'], capture_output=True, text=True, timeout=30)
        (out / (label + '.nvidia-smi.txt')).write_text(p.stdout + p.stderr)


def execute(args, out, packet):
    driver = DRIVER(args.device)
    library = args.library.resolve() if args.library else args.packet / 'runtime/ir_tools/libmla_serving_cuda_q7.so'
    if args.library:
        # Rebuilt native bindings are a separate explicit product, authenticated
        # by their build receipt and the packet identity, never selected silently.
        receipt = json.loads(library.with_name('build-report.json').read_text())
        if receipt.get('packet_sha256') != args.expected_sha256 or receipt.get('library_sha256') != hashlib.sha256(library.read_bytes()).hexdigest():
            raise ValueError('rebuilt host binding is not authenticated to this packet')
    api = API(library)
    selected = [x for x in packet['cases'] if not args.case or x['id'] in args.case]
    if not selected or args.case and set(args.case) - {x['id'] for x in selected}: raise ValueError('unknown fixed case')
    capture_environment(out, 'before')
    cases = []
    for case in selected:
        prefixes = [args.prefix] if args.prefix else ([256] if args.mode == 'performance' else [256, 40])
        for prefix in prefixes:
            session = SESSION(api, driver, args.packet / 'cases' / case['id'], case, prefix)
            bundle, _ = session.run(lifecycle=True); refs = session.verify(bundle); session.consume(bundle); session.retire()
            guard = case['id'] == 'dense_s8_decode--async'
            if guard:
                session.guard_refusal()
                bundle, _ = session.run(); session.verify(bundle); session.retire()
            host = []; spans = []
            if args.mode == 'performance':
                for _ in range(args.warmup):
                    session.run(); session.retire()
                for _ in range(args.iterations):
                    _, timing = session.run(timed=True); host.append(timing['host_begin_submit_complete_us']); spans.append(timing['submission_stream_span_ms']); session.retire()
                bundle, _ = session.run(); session.verify(bundle); session.consume(bundle); session.retire()
            session.close()
            result = dict(id=case['id'], prefix_bytes=prefix, status='PASS', references=refs,
                          ordered_kernels_per_request=3, complete_Q3_tuple=True, consumer_completion=True,
                          input_immutability=True, allocation_guards=True, prepared_cancel=True, source_guard_refusal=guard)
            if host:
                result['host_begin_submit_complete_us'] = summarize(host)
                result['submission_stream_span_ms'] = summarize(spans)
            cases.append(result)
            (out / 'progress.json').write_text(json.dumps(cases, indent=2) + '\n')
            print('PASS real GPU', case['id'], 'prefix', prefix, flush=True)
    driver.close(); capture_environment(out, 'after')
    return dict(status='PASS', gpu_execution=True, driver_model=False, device=driver.info, cases=cases,
                all_fixed_cases=len(selected)==len(packet['cases']), mode=args.mode,
                timing_scope='persistent buffers; fixed independent requests; allocations/uploads/downloads/consumer validation excluded; host timing includes Begin and Q3 completion/publication; event span includes Submit control transfers and host gaps, NOT pure kernel duration',
                search_invocations=0, aot_selection='deferred', race_qualified=False)


def orchestrate(args, out, packet):
    """Separate processes keep tool instrumentation out of performance samples."""
    records = {}; commands = []
    base = [sys.executable, str(Path(__file__).resolve()), '--packet', str(args.packet), '--expected-sha256', args.expected_sha256, '--device', str(args.device)]
    if args.library: base += ['--library', str(args.library.resolve())]
    for name in args.case or []: base += ['--case', name]
    if args.prefix: base += ['--prefix', str(args.prefix)]
    def run(label, argv):
        try:
            p = subprocess.run(argv, capture_output=True, text=True, timeout=args.timeout)
        except subprocess.TimeoutExpired as error:
            (out / (label + '.timeout.txt')).write_text(str(error)); raise RuntimeError('GPU/tool child timed out: ' + label) from error
        (out / (label + '.stdout')).write_text(p.stdout); (out / (label + '.stderr')).write_text(p.stderr)
        commands.append(dict(label=label, argv=argv, returncode=p.returncode)); (out / 'commands.json').write_text(json.dumps(commands, indent=2)+'\n')
        return p
    first = run('correctness', base + ['--mode', 'correctness', '--output', str(out/'correctness')])
    child_report = json.loads((out/'correctness/report.json').read_text()) if (out/'correctness/report.json').exists() else {}
    STATE.update({key:child_report.get(key,False) for key in STATE})
    if first.returncode == 77: raise Unavailable('correctness child: hardware unavailable; see logs')
    if first.returncode: raise RuntimeError('correctness failed; see child report')
    records['correctness'] = json.loads((out/'correctness/report.json').read_text())
    tool = shutil.which(args.sanitizer)
    if not tool: return dict(status='TOOLS_UNAVAILABLE', gpu_execution=True, records=records, reason='Compute Sanitizer required', exit_code=78)
    help_result = run('sanitizer-help', [tool, '--help']); run('sanitizer-version', [tool, '--version'])
    help_text = help_result.stdout + help_result.stderr
    for name in ('memcheck', 'racecheck', 'synccheck', 'initcheck'):
        try:
            flags = sanitizer_flags(name, help_text)
        except ValueError as error:
            return dict(status='TOOL_COVERAGE_INCOMPLETE',gpu_execution=True,records=records,reason=str(error),exit_code=78)
        p = run(name, [tool] + flags + base + ['--mode', 'correctness', '--output', str(out/name)])
        child = json.loads((out/name/'report.json').read_text()) if (out/name/'report.json').exists() else {}
        if child.get('cases') != records['correctness']['cases']: raise ValueError('sanitizer did not validate the same complete case matrix')
        records[name] = check_sanitizer(name, p.returncode, p.stdout+p.stderr, child)
    perf = run('performance', base + ['--mode', 'performance', '--output', str(out/'performance'), '--warmup', str(args.warmup), '--iterations', str(args.iterations)])
    if perf.returncode: raise RuntimeError('performance child failed; see report')
    records['performance'] = json.loads((out/'performance/report.json').read_text())
    return dict(status='PASS', gpu_execution=True, mode='all', records=records, race_qualified=True,
                all_fixed_cases=records['correctness']['all_fixed_cases'],
                scope='fixed cases on recorded NVIDIA device only; no global optimality/speedup or serving throughput claim',
                search_invocations=0, aot_selection='deferred')


def main(argv=None):
    STATE.update(gpu_execution=False, device_submission_attempted=False)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--packet', type=Path, required=True); parser.add_argument('--expected-sha256', required=True)
    parser.add_argument('--output', type=Path, required=True); parser.add_argument('--mode', choices=('correctness','performance','all'), default='all')
    parser.add_argument('--device', type=int, default=0); parser.add_argument('--case', action='append'); parser.add_argument('--prefix', type=int, choices=(40,256))
    parser.add_argument('--iterations', type=int, default=20); parser.add_argument('--warmup', type=int, default=3)
    parser.add_argument('--timeout', type=int, default=3600); parser.add_argument('--sanitizer', default='compute-sanitizer'); parser.add_argument('--library', type=Path)
    args = parser.parse_args(argv); args.packet = args.packet.resolve(); out = args.output.resolve(); out.mkdir(parents=True, exist_ok=False)
    report = dict(status='FAIL', gpu_execution=False, search_invocations=0, aot_selection='deferred'); code = 1
    try:
        if not 1 <= args.iterations <= 10000 or not 0 <= args.warmup <= 1000 or args.timeout <= 0: raise ValueError('invalid sample/timeout budget')
        verified = authenticate(args.packet, args.expected_sha256)
        packet = json.loads((args.packet/'packet.json').read_text())
        if packet['schema'] != 'open64.mla-q7.packet.v1': raise ValueError('unknown packet schema')
        report = orchestrate(args, out, packet) if args.mode=='all' else execute(args, out, packet)
        report['authenticated_files'] = verified; report['packet_sha256'] = args.expected_sha256
        code = report.pop('exit_code', 0)
    except Unavailable as error:
        report.update(status='GPU_UNAVAILABLE', reason=str(error)); code = 77
    except Exception as error:
        report.update(status='FAIL', reason=str(error), cleanup='on a live/uncertain adapter failure, no in-process source fallback or reclamation is attempted; stop this qualification process'); code = 1
    if code:
        report.update(STATE)
    (out/'report.json').write_text(json.dumps(report, indent=2)+'\n'); print(json.dumps(report, indent=2)); return code


if __name__ == '__main__':
    raise SystemExit(main())
