"""Authenticated loopback GS2 API over the persistent GS1 store and workers.

No numerical policy or device execution in HTTP handlers. Registered packets
own those gates. Contract: doc/GPU-TEST-SERVER-GS2-v1.0.md. OpenCTI is deferred.
"""
import argparse
import hashlib
import hmac
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import re
import secrets
import signal
import sqlite3
import stat
import subprocess
import sys
import threading
import time
from urllib.parse import parse_qs, urlsplit
import uuid

from gpu_test_results import loads
from gpu_test_worker_state import Store, AdmissionError, TERMINAL, valid_request, utc, encoded
from gpu_test_worker import Worker, identity, still_alive, child_environment

JOB = r'job_[0-9a-f]{32}'
ARTIFACT = r'(?:artifact_[0-9a-f]{32}|source_[0-9a-f]{64}|source_sha_[0-9a-f]{64})'
HERE = Path(__file__).resolve().parent


def atomic_json(path, value):
    path = Path(path)
    temporary = path.with_name('.' + path.name + '.' + uuid.uuid4().hex)
    descriptor = os.open(str(temporary), os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    with os.fdopen(descriptor, 'w') as handle:
        handle.write(encoded(value) + '\n')
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temporary, path)


def credentials(path):
    """Load hashes only; never log or forward a bearer credential to workers."""
    path = Path(path)
    if path.stat().st_mode & 0o077:
        raise ValueError('credential configuration must have mode 0600')
    value = loads(path.read_bytes())
    if set(value) != {'schema', 'tokens'} or value['schema'] != 'open64.gpu-test.credentials.v1':
        raise ValueError('unsupported credential configuration')
    result = []
    for row in value['tokens']:
        if set(row) != {'principal', 'sha256'} or not re.fullmatch(r'[A-Za-z0-9_.-]{1,128}', row['principal']) or not re.fullmatch('[0-9a-f]{64}', row['sha256']):
            raise ValueError('invalid credential entry')
        if any(x['sha256'] == row['sha256'] for x in result):
            raise ValueError('duplicate bearer identity')
        result.append(row)
    if not result:
        raise ValueError('at least one principal is required')
    return result


class APIError(Exception):
    def __init__(self, status, code, message):
        self.status, self.code, self.message = status, code, message


class Service:
    def __init__(self, store, tokens, quota=4, max_events=100, request_timeout=10):
        self.store, self.tokens = store, tokens
        self.quota, self.max_events, self.request_timeout = quota, max_events, request_timeout
        if not 1 <= quota <= 1000 or not 1 <= max_events <= 1000:
            raise ValueError('invalid service limits')
        with store.transaction() as db:
            row = db.execute("SELECT value FROM metadata WHERE key='api_identity'").fetchone()
            self.server_id = row[0] if row else 'server_' + uuid.uuid4().hex
            if not row:
                db.execute('INSERT INTO metadata VALUES (?,?)', ('api_identity', self.server_id))

    def principal(self, authorization):
        if not authorization.startswith('Bearer ') or len(authorization) > 512:
            raise APIError(401, 'AUTHENTICATION_REQUIRED', 'A configured bearer credential is required.')
        token = authorization[7:]
        digest = hashlib.sha256(token.encode()).hexdigest()
        found = None
        for row in self.tokens:
            if hmac.compare_digest(row['sha256'], digest):
                found = row['principal']
        if found is None:
            raise APIError(401, 'AUTHENTICATION_REQUIRED', 'A configured bearer credential is required.')
        return found

    def owned(self, job_id, principal):
        try:
            job = self.store.get(job_id)
        except KeyError:
            raise APIError(404, 'NOT_FOUND', 'Resource absent or inaccessible.') from None
        if job['principal'] != principal:
            raise APIError(404, 'NOT_FOUND', 'Resource absent or inaccessible.')
        return job

    def packets(self):
        with self.store.connect() as db:
            return [json.loads(x[0]) for x in db.execute('SELECT data FROM packets ORDER BY id')]

    def source_artifacts(self, packet):
        name = Path(packet['archive']).name
        if not re.fullmatch(r'[A-Za-z0-9_.-]+', name):
            name = 'source.tar.gz'
        data = (packet['archive_sha256'] + '  ' + name + '\n').encode()
        return [dict(artifact_id='source_' + packet['packet_id'], name=name,
            kind='source_archive', media_type='application/gzip', sha256=packet['archive_sha256'],
            size_bytes=packet['archive_size_bytes'], expires_at=None,
            download_url='/v1/artifacts/source_' + packet['packet_id']),
            dict(artifact_id='source_sha_' + packet['packet_id'], name=name + '.sha256',
            kind='source_checksum', media_type='text/plain', sha256=hashlib.sha256(data).hexdigest(),
            size_bytes=len(data), expires_at=None, download_url='/v1/artifacts/source_sha_' + packet['packet_id'])]

    def packet_view(self, packet):
        """Expose authenticated registered profiles, including optional H7 families."""
        profiles=packet.get('profiles',['gemm','mla-core','mla-q7'])
        requirements=['Linux x86_64 for qualification','physical sm_90, MIG disabled','registered packet CUDA/Driver/sanitizer requirements']
        if packet.get('h20_target'):requirements.append('H7 explicitly selected device: '+packet['h20_target'])
        return dict(packet_id=packet['packet_id'], archive_sha256=packet['archive_sha256'],
            archive_size_bytes=packet['archive_size_bytes'], producer_commit=packet['producer_commit'],
            name=Path(packet['path']).name, registry_version='open64.gpu-test.worker-registry.v1',
            profiles=profiles, modes=['qualify', 'check-only'],
            requirements=requirements,
            source_artifacts=self.source_artifacts(packet), result_schema='open64.gpu-test.results.v1')

    def artifact_view(self, row):
        return dict(artifact_id=row['artifact_id'], name=row['name'], kind='job_evidence',
            media_type='application/octet-stream', sha256=row['sha256'], size_bytes=row['size_bytes'],
            expires_at=None, download_url='/v1/artifacts/' + row['artifact_id'])

    def job_view(self, job):
        # Expose state and original review facts, never owner tokens, argv, local
        # paths, raw environment or an invented numerical acceptance projection.
        state = job['state']
        phase = {'preparing': 'preparation', 'running': 'execution', 'cancelling': 'drain',
                 'collecting': 'collection', 'validating': 'review'}.get(state, 'none')
        return dict(schema='open64.gpu-test.job.v1', job_id=job['job_id'], request=job['request'],
            packet=self.packet_view(job['packet']), server_id=self.server_id,
            registry_version='open64.gpu-test.worker-registry.v1', runner_id=job['packet']['runner_sha256'],
            state=state, phase=phase, created_at=job['created_at'], started_at=job['started_at'],
            finished_at=job['finished_at'], selected_gpu_uuid=job['selected_gpu_uuid'],
            runner_exit_code=job['runner_exit_code'], termination_reason=job['termination_reason'],
            retry_of=job['retry_of'], collection_status=job['collection_status'], review_status=job['review_status'],
            receiver_verdict=job['receiver_verdict'], lease_binding=job['lease_binding'],
            diagnostics=job['diagnostics'], profiles=(job.get('review_summary') or {}).get('profiles', {}),
            failure_count=len(job['failures']), artifacts=[self.artifact_view(x) for x in job.get('artifacts', [])],
            links={x: '/v1/jobs/' + job['job_id'] + suffix for x, suffix in
                (('status', ''), ('events', '/events'), ('cancel', '/cancel'))})

    def capabilities(self, principal):
        slots = []
        for path in sorted(self.store.root.glob('service-worker-*.json')):
            try:
                if path.stat().st_size < 8192:
                    record = loads(path.read_bytes())
                    if still_alive(record):
                        slots.append(dict(slot=record['slot'], available=True))
            except (OSError, KeyError, ValueError):
                pass
        return dict(schema='open64.gpu-test.capabilities.v1', api_version='v1', contract_version='0.2.0',
            server_id=self.server_id, principal=principal, registry_version='open64.gpu-test.worker-registry.v1',
            service_status='ready' if slots else 'degraded', workers=slots,
            limits=dict(max_samples=10000, max_warmup=1000,
                max_execution_seconds=self.store.limits['max_execution_seconds'],
                max_queue_wait_seconds=self.store.limits['max_queue_wait_seconds'],
                collection_grace_seconds=self.store.limits['collection_grace_seconds'],
                max_jobs_per_principal=self.quota, max_events_per_page=self.max_events,
                artifact_retention_seconds=None, max_request_bytes=65536),
            gpus=[dict(uuid=x['uuid'], name=None, compute_capability=None, memory_bytes=None,
                available=x['state'] == 'free', sharing='unknown', mig_enabled=None, mps_detected=None,
                qualification='pending', lease_state=x['state']) for x in self.store.gpus()],
            tools=[], profiling_modes=['off', 'auto'], timeline_available=None,
            packet_ids=[x['packet_id'] for x in self.packets()],
            observations='Registry/worker ownership only; no live hardware/tool probes in HTTP handlers.')

    def events(self, job_id, principal, after, limit):
        self.owned(job_id, principal)
        with self.store.connect() as db:
            db.execute('BEGIN')
            low, high = db.execute('SELECT MIN(seq),MAX(seq) FROM events WHERE job=?', (job_id,)).fetchone()
            if low is not None and after + 1 < low:
                raise APIError(409, 'EVENT_CURSOR_EXPIRED', 'The event cursor no longer covers retained history.')
            if after > (high or 0):
                raise APIError(409, 'EVENT_CURSOR_AHEAD', 'The event cursor is beyond the job history.')
            rows = db.execute('SELECT data FROM events WHERE job=? AND seq>? ORDER BY seq LIMIT ?',
                (job_id, after, limit + 1)).fetchall()
        events = [json.loads(x[0]) for x in rows[:limit]]
        return dict(schema='open64.gpu-test.events.v1', events=events,
            next_after=events[-1]['sequence'] if events else after, has_more=len(rows) > limit)

    def artifact(self, artifact_id, principal):
        for packet in self.packets():
            for row in self.source_artifacts(packet):
                if row['artifact_id'] == artifact_id:
                    if row['kind'] == 'source_checksum':
                        archive_name = self.source_artifacts(packet)[0]['name']
                        return row, (packet['archive_sha256'] + '  ' + archive_name + '\n').encode()
                    return row, Path(packet['archive'])
        with self.store.connect() as db:
            rows = db.execute('SELECT artifacts.job,artifacts.data FROM artifacts JOIN jobs ON jobs.id=artifacts.job WHERE jobs.principal=?', (principal,))
            for job_id, data in rows:
                row = json.loads(data)
                if row['artifact_id'] == artifact_id:
                    path = Path(row['path'])
                    root = self.store.root / 'artifacts' / job_id
                    if path.is_symlink() or not path.resolve().is_relative_to(root):
                        raise APIError(409, 'ARTIFACT_CHANGED', 'Artifact storage no longer matches its identity.')
                    return self.artifact_view(row), path
        raise APIError(404, 'NOT_FOUND', 'Resource absent or inaccessible.')


class Handler(BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'

    def setup(self):
        super().setup()
        self.connection.settimeout(self.server.service.request_timeout)

    def log_message(self, format, *args):
        # Credentials and URL/query strings never enter the default access log.
        pass

    def json_response(self, status, value):
        raw = encoded(value).encode()
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(raw)))
        self.send_header('Connection', 'close')
        self.send_header('Cache-Control', 'no-store')
        self.end_headers()
        self.wfile.write(raw)
        self.close_connection = True

    def body(self, empty=False):
        lengths = self.headers.get_all('Content-Length', [])
        if self.headers.get('Transfer-Encoding') or len(lengths) != 1 or not re.fullmatch('[0-9]+', lengths[0]):
            raise APIError(400, 'INVALID_BODY', 'One explicit content length is required.')
        size = int(lengths[0])
        if size > 65536 or size < (0 if empty else 1):
            raise APIError(400, 'INVALID_BODY', 'Request body is outside the supported limit.')
        if size and self.headers.get_content_type() != 'application/json':
            raise APIError(400, 'INVALID_BODY', 'JSON content is required.')
        data = self.rfile.read(size)
        if len(data) != size:
            raise APIError(400, 'INVALID_BODY', 'Request body is truncated.')
        try:
            return loads(data) if data else {}
        except (ValueError, TypeError, RecursionError):
            raise APIError(400, 'INVALID_JSON', 'JSON must be finite and contain unique object members.') from None

    def stream(self, row, source):
        if isinstance(source, bytes):
            if hashlib.sha256(source).hexdigest() != row['sha256'] or len(source) != row['size_bytes']:
                raise APIError(409, 'ARTIFACT_CHANGED', 'Artifact integrity check failed.')
            handle = None
        else:
            try:
                descriptor = os.open(str(source), os.O_RDONLY | getattr(os, 'O_NOFOLLOW', 0))
                handle = os.fdopen(descriptor, 'rb')
            except OSError:
                raise APIError(404, 'NOT_FOUND', 'Artifact unavailable.') from None
            before = os.fstat(handle.fileno())
            h = hashlib.sha256()
            if not stat.S_ISREG(before.st_mode) or before.st_size != row['size_bytes']:
                handle.close()
                raise APIError(409, 'ARTIFACT_CHANGED', 'Artifact length/type differs.')
            for block in iter(lambda: handle.read(1048576), b''):
                h.update(block)
            after = os.fstat(handle.fileno())
            if h.hexdigest() != row['sha256'] or (before.st_size, before.st_mtime_ns, before.st_ctime_ns) != (after.st_size, after.st_mtime_ns, after.st_ctime_ns):
                handle.close()
                raise APIError(409, 'ARTIFACT_CHANGED', 'Artifact integrity check failed.')
            handle.seek(0)
        try:
            self.send_response(200)
            self.send_header('Content-Type', row['media_type'])
            self.send_header('Content-Length', str(row['size_bytes']))
            self.send_header('X-Content-SHA256', row['sha256'])
            self.send_header('Connection', 'close')
            self.end_headers()
            if handle:
                for block in iter(lambda: handle.read(1048576), b''):
                    self.wfile.write(block)
            else:
                self.wfile.write(source)
            self.close_connection = True
        finally:
            if handle:
                handle.close()

    def dispatch(self):
        service = self.server.service
        if len(self.headers.get_all('Authorization', [])) != 1:
            raise APIError(401, 'AUTHENTICATION_REQUIRED', 'One bearer credential is required.')
        principal = service.principal(self.headers['Authorization'])
        parsed = urlsplit(self.path)
        path = parsed.path
        if '%' in path or parsed.scheme or parsed.netloc or parsed.fragment:
            raise APIError(404, 'NOT_FOUND', 'Resource absent or inaccessible.')
        query = parse_qs(parsed.query, keep_blank_values=True)
        if self.command == 'GET' and path == '/v1/capabilities' and not query:
            return self.json_response(200, service.capabilities(principal))
        if self.command == 'GET' and path == '/v1/packets' and not query:
            return self.json_response(200, dict(schema='open64.gpu-test.packet-list.v1', packets=[service.packet_view(x) for x in service.packets()]))
        if self.command == 'POST' and path == '/v1/jobs' and not query:
            keys = self.headers.get_all('Idempotency-Key', [])
            if len(keys) != 1 or not 1 <= len(keys[0]) <= 128 or any(ord(x) < 32 for x in keys[0]):
                raise APIError(400, 'INVALID_IDEMPOTENCY_KEY', 'One bounded idempotency key is required.')
            try:
                request = valid_request(self.body())
            except ValueError as error:
                raise APIError(400, 'INVALID_REQUEST', str(error)) from None
            if request['retry_of'] is not None:
                service.owned(request['retry_of'], principal)
            try:
                job, created = service.store.submit(request, principal, keys[0], quota=service.quota, return_created=True)
            except AdmissionError as error:
                raise APIError(409 if error.code == 'IDEMPOTENCY_CONFLICT' else 429, error.code, str(error)) from None
            except KeyError:
                raise APIError(404, 'NOT_FOUND', 'Retry resource absent or inaccessible.') from None
            except ValueError:
                raise APIError(422, 'INCOMPATIBLE_REQUEST', 'Packet, target, retry lineage or execution budget is incompatible.') from None
            return self.json_response(202 if created else 200, service.job_view(service.store.get(job['job_id'])))
        match = re.fullmatch('/v1/jobs/(' + JOB + r')(/events|/cancel)?', path)
        if match:
            job_id, suffix = match.groups()
            job = service.owned(job_id, principal)
            if suffix == '/events' and self.command == 'GET':
                if set(query) - {'after', 'limit'} or any(len(v) != 1 or not re.fullmatch('[0-9]{1,18}', v[0]) for v in query.values()):
                    raise APIError(400, 'INVALID_CURSOR', 'Expected bounded integer after/limit values.')
                after, limit = int(query.get('after', ['0'])[0]), int(query.get('limit', ['100'])[0])
                if not 1 <= limit <= service.max_events:
                    raise APIError(400, 'INVALID_CURSOR', 'Event page limit exceeds server capability.')
                return self.json_response(200, service.events(job_id, principal, after, limit))
            if suffix == '/cancel' and self.command == 'POST' and not query:
                if self.body(empty=True) != {}:
                    raise APIError(400, 'INVALID_REQUEST', 'Cancellation body must be empty.')
                service.store.cancel(job_id, principal)
                job = service.store.get(job_id)
                return self.json_response(200 if job['state'] in TERMINAL else 202, service.job_view(job))
            if suffix is None and self.command == 'GET' and not query:
                return self.json_response(200, service.job_view(job))
        match = re.fullmatch('/v1/artifacts/(' + ARTIFACT + ')', path)
        if match and self.command == 'GET' and not query:
            return self.stream(*service.artifact(match[1], principal))
        raise APIError(404, 'NOT_FOUND', 'Resource absent or inaccessible.')

    def handle_request(self):
        try:
            self.dispatch()
        except APIError as error:
            self.json_response(error.status, dict(schema='open64.gpu-test.api-error.v1', code=error.code,
                message=error.message, retryable=error.status in (429, 503), request_id='request_' + uuid.uuid4().hex))
        except (BrokenPipeError, ConnectionResetError, TimeoutError):
            self.close_connection = True
        except (OSError, sqlite3.Error, ValueError, TypeError, KeyError):
            self.json_response(503, dict(schema='open64.gpu-test.api-error.v1', code='SERVICE_UNAVAILABLE',
                message='Service could not complete the operation.', retryable=True, request_id='request_' + uuid.uuid4().hex))

    do_GET = handle_request
    do_POST = handle_request
    do_PUT = handle_request
    do_DELETE = handle_request


class Server(ThreadingHTTPServer):
    daemon_threads = True
    allow_reuse_address = True

    def __init__(self, address, service):
        if address[0] != '127.0.0.1':
            raise ValueError('initial HTTP listener must bind 127.0.0.1; use an authenticated SSH tunnel')
        self.service = service
        super().__init__(address, Handler)


def worker_loop(store, slot, supervisor=None):
    import fcntl
    with (store.root / ('service-worker-' + str(slot) + '.lock')).open('a+b') as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            return 0
        atomic_json(store.root / ('service-worker-' + str(slot) + '.json'), dict(identity(), slot=slot))
        worker = Worker(store)
        for number in (signal.SIGINT, signal.SIGTERM):
            signal.signal(number, worker.signal)
        # API loss leaves an accepted execution running. Once that call closes,
        # retire this orphaned poller so a replacement service can own its slot.
        while not worker.signal_received and (supervisor is None or still_alive(supervisor)):
            result = worker.run_once()
            if result is not None:
                worker.collection_deadlines.pop(result['job_id'], None)
            if result is None:
                time.sleep(.25)
    return 0


def serve(args):
    store = Store(args.store)
    service = Service(store, credentials(args.credentials), args.quota)
    server = Server(('127.0.0.1', args.port), service)
    stopped = threading.Event()
    children = {}
    logs = store.root / 'service-logs'
    logs.mkdir(exist_ok=True)
    def pool():
        while not stopped.is_set():
            for slot in range(args.workers):
                child = children.get(slot)
                if child is not None and child.poll() is None:
                    continue
                path = store.root / ('service-worker-' + str(slot) + '.json')
                if path.exists():
                    try:
                        if still_alive(loads(path.read_bytes())):
                            continue
                    except (OSError, ValueError, KeyError):
                        pass
                with (logs / ('worker-' + str(slot) + '.log')).open('ab') as output:
                    children[slot] = subprocess.Popen([sys.executable, str(Path(__file__).resolve()), 'worker',
                        '--store', str(store.root), '--slot', str(slot), '--supervisor', encoded(identity())], stdout=output, stderr=output,
                        start_new_session=True, env=child_environment(dict(selected_gpu_uuid=None)))
            stopped.wait(1)
    thread = threading.Thread(target=pool, daemon=True)
    thread.start()
    def shutdown(number, frame):
        stopped.set()
        threading.Thread(target=server.shutdown, daemon=True).start()
    for number in (signal.SIGINT, signal.SIGTERM):
        signal.signal(number, shutdown)
    print(encoded(dict(listening='http://127.0.0.1:' + str(server.server_port), server_id=service.server_id)), flush=True)
    try:
        server.serve_forever(poll_interval=.2)
    finally:
        stopped.set()
        thread.join(timeout=3)
        server.server_close()
        for child in children.values():
            if child.poll() is None:
                child.send_signal(signal.SIGTERM)
        deadline = time.monotonic() + store.limits['collection_grace_seconds'] + 10
        for child in children.values():
            try:
                child.wait(timeout=max(.1, deadline - time.monotonic()))
            except subprocess.TimeoutExpired:
                child.kill()
                child.wait(timeout=5)
                # GS1 persistent ownership/recovery retains quarantine on loss.
    return 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest='command', required=True)
    init = commands.add_parser('init-credentials')
    init.add_argument('--credentials', type=Path, required=True)
    init.add_argument('--token-file', type=Path, required=True)
    init.add_argument('--principal', required=True)
    service = commands.add_parser('serve')
    service.add_argument('--store', type=Path, required=True)
    service.add_argument('--credentials', type=Path, required=True)
    service.add_argument('--port', type=int, default=8765)
    service.add_argument('--workers', type=int, default=1)
    service.add_argument('--quota', type=int, default=4)
    worker = commands.add_parser('worker')
    worker.add_argument('--store', type=Path, required=True)
    worker.add_argument('--slot', type=int, default=0)
    worker.add_argument('--supervisor', help=argparse.SUPPRESS)
    args = parser.parse_args(argv)
    try:
        if args.command == 'init-credentials':
            if not re.fullmatch(r'[A-Za-z0-9_.-]{1,128}', args.principal):
                raise ValueError('invalid principal')
            if args.credentials == args.token_file or any(path.exists() or path.is_symlink() for path in (args.credentials, args.token_file)):
                raise ValueError('credential output files must be distinct and fresh')
            token = secrets.token_urlsafe(32)
            # Explicit fresh files; never replace an existing credential/config.
            for path, data in ((args.token_file, token + '\n'), (args.credentials, encoded(dict(
                schema='open64.gpu-test.credentials.v1', tokens=[dict(principal=args.principal,
                    sha256=hashlib.sha256(token.encode()).hexdigest())])) + '\n')):
                descriptor = os.open(str(path), os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
                with os.fdopen(descriptor, 'w') as handle:
                    handle.write(data)
            print('Credential files created; bearer value is not printed.')
            return 0
        if args.command == 'worker':
            if args.slot < 0:
                raise ValueError('invalid slot')
            return worker_loop(Store(args.store), args.slot, loads(args.supervisor) if args.supervisor else None)
        if not 0 <= args.port <= 65535 or not 0 <= args.workers <= 32:
            raise ValueError('invalid server port/worker count')
        return serve(args)
    except (OSError, ValueError, sqlite3.Error):
        print('GPU service configuration or storage unavailable.', file=sys.stderr)
        return 1


if __name__ == '__main__':
    raise SystemExit(main())
