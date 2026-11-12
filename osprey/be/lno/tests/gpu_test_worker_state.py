"""Host-local durable GS1 jobs and physical-GPU leases (Python standard library).

One shared store per GPU host; SQLite transactions persist claims before work.
Numerical acceptance stays in the registered packet. Controlling contract:
doc/GPU-TEST-WORKER-GS1-v1.0.md. No HTTP or OpenCTI dependency.
"""
from contextlib import contextmanager
import datetime
import json
import os
from pathlib import Path
import re
import socket
import sqlite3
import time
import uuid

TERMINAL = {'completed', 'failed', 'cancelled', 'interrupted'}
DIGEST = re.compile(r'[0-9a-f]{64}')
GPU_UUID = re.compile(r'GPU-[0-9a-fA-F]{8}(?:-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}')
DEFAULT_LIMITS = dict(max_queue_wait_seconds=86400, collection_grace_seconds=1200,
                      max_execution_seconds=86400)


class AdmissionError(ValueError):
    """Expose a stable admission reason without changing GS1 ValueError callers."""
    def __init__(self, code, message):
        super().__init__(message)
        self.code = code


def utc():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def encoded(value):
    return json.dumps(value, sort_keys=True, separators=(',', ':'), allow_nan=False)


def canonical_gpu(value):
    return 'GPU-' + value[4:].lower()


def valid_request(value):
    """Reject untyped options before storing a runnable job or building argv."""
    keys = {'schema', 'packet_id', 'profile', 'mode', 'retry_of', 'target_gpu_uuid',
            'samples', 'warmup', 'profiling', 'profile_timeline', 'execution_timeout_seconds'}
    if not isinstance(value, dict) or set(value) != keys:
        raise ValueError('request must contain exactly the v1 fields')
    if value['schema'] != 'open64.gpu-test.job-request.v1' or value['profile'] != 'combined':
        raise ValueError('unsupported request schema/profile')
    if not isinstance(value['packet_id'], str) or not DIGEST.fullmatch(value['packet_id']):
        raise ValueError('invalid immutable packet identity')
    if value['mode'] not in ('qualify', 'check-only') or value['profiling'] not in ('off', 'auto'):
        raise ValueError('unsupported execution/profiling mode')
    if type(value['profile_timeline']) is not bool:
        raise ValueError('timeline must be boolean')
    for key, low, high in (('samples', 1, 10000), ('warmup', 0, 1000), ('execution_timeout_seconds', 1, 86400)):
        if type(value[key]) is not int or not low <= value[key] <= high:
            raise ValueError('invalid ' + key)
    gpu = value['target_gpu_uuid']
    if gpu is not None and (not isinstance(gpu, str) or not GPU_UUID.fullmatch(gpu)):
        raise ValueError('target must be a physical GPU UUID')
    if value['profile_timeline'] and value['profiling'] != 'auto':
        raise ValueError('timeline requires profiling auto')
    if value['mode'] == 'check-only' and (gpu is not None or value['profiling'] != 'off'):
        raise ValueError('check-only cannot select a GPU or request profiling')
    if value['retry_of'] is not None and (not isinstance(value['retry_of'], str) or not re.fullmatch(r'job_[0-9a-f]{32}', value['retry_of'])):
        raise ValueError('invalid retry lineage')
    # Detach from caller-owned dictionaries before hashing or committing.
    return json.loads(encoded(value))


class Store:
    """Transactions are the ownership boundary; lease expiry never frees a GPU."""
    def __init__(self, root, limits=None):
        self.root = Path(root).resolve()
        self.root.mkdir(parents=True, exist_ok=True, mode=0o700)
        self.path = self.root / 'worker.sqlite3'
        for name in ('jobs', 'artifacts'):
            (self.root / name).mkdir(exist_ok=True, mode=0o700)
        with self.connect() as db:
            db.execute('PRAGMA journal_mode=WAL')
            db.executescript('''
                CREATE TABLE IF NOT EXISTS metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL);
                CREATE TABLE IF NOT EXISTS packets (id TEXT PRIMARY KEY, data TEXT NOT NULL);
                CREATE TABLE IF NOT EXISTS gpus (id TEXT PRIMARY KEY, data TEXT NOT NULL);
                CREATE TABLE IF NOT EXISTS jobs (id TEXT PRIMARY KEY, state TEXT NOT NULL, principal TEXT NOT NULL,
                    idem TEXT NOT NULL, request TEXT NOT NULL, data TEXT NOT NULL, UNIQUE(principal, idem));
                CREATE TABLE IF NOT EXISTS events (job TEXT NOT NULL, seq INTEGER NOT NULL, data TEXT NOT NULL,
                    PRIMARY KEY(job, seq));
                CREATE TABLE IF NOT EXISTS artifacts (job TEXT NOT NULL, name TEXT NOT NULL, data TEXT NOT NULL,
                    PRIMARY KEY(job, name));
            ''')
        with self.transaction() as db:
            policy = db.execute("SELECT value FROM metadata WHERE key='policy'").fetchone()
            if policy:
                policy = json.loads(policy[0])
                if policy.get('schema') != 'open64.gpu-test.worker-store.v1':
                    raise ValueError('unsupported worker store schema')
                if policy['host'] != socket.gethostname():
                    raise ValueError('worker store belongs to another host')
                self.limits = policy['limits']
                if limits is not None and limits != self.limits:
                    raise ValueError('existing store policy differs')
            else:
                self.limits = dict(DEFAULT_LIMITS if limits is None else limits)
                if set(self.limits) != set(DEFAULT_LIMITS) or any(type(x) is not int or x < 1 for x in self.limits.values()):
                    raise ValueError('invalid worker limits')
                db.execute('INSERT INTO metadata VALUES (?,?)', ('policy', encoded(dict(
                    schema='open64.gpu-test.worker-store.v1', host=socket.gethostname(), limits=self.limits))))

    @contextmanager
    def connect(self):
        db = sqlite3.connect(str(self.path), timeout=30, isolation_level=None)
        db.execute('PRAGMA synchronous=FULL')
        try:
            yield db
        finally:
            db.close()

    @contextmanager
    def transaction(self):
        with self.connect() as db:
            db.execute('BEGIN IMMEDIATE')
            try:
                yield db
                db.commit()
            except BaseException:
                db.rollback()
                raise

    def _get(self, db, job_id):
        row = db.execute('SELECT data FROM jobs WHERE id=?', (job_id,)).fetchone()
        if row is None:
            raise KeyError('unknown job')
        return json.loads(row[0])

    def _save(self, db, job, message):
        db.execute('UPDATE jobs SET state=?,data=? WHERE id=?', (job['state'], encoded(job), job['job_id']))
        seq = db.execute('SELECT COALESCE(MAX(seq),0)+1 FROM events WHERE job=?', (job['job_id'],)).fetchone()[0]
        event = dict(sequence=seq, at=utc(), state=job['state'], message=message)
        db.execute('INSERT INTO events VALUES (?,?,?)', (job['job_id'], seq, encoded(event)))

    def packet(self, identity):
        with self.connect() as db:
            row = db.execute('SELECT data FROM packets WHERE id=?', (identity,)).fetchone()
        if row is None:
            raise ValueError('packet is not registered')
        return json.loads(row[0])

    def register_packet(self, record):
        source = Path(record['path']).resolve()
        if source == self.root or source in self.root.parents or self.root in source.parents:
            raise ValueError('worker store and source installation must not overlap')
        with self.transaction() as db:
            old = db.execute('SELECT data FROM packets WHERE id=?', (record['packet_id'],)).fetchone()
            if old and old[0] != encoded(record):
                raise ValueError('immutable registry binding conflict')
            db.execute('INSERT OR IGNORE INTO packets VALUES (?,?)', (record['packet_id'], encoded(record)))

    def register_gpu(self, gpu_uuid):
        if not isinstance(gpu_uuid, str) or not GPU_UUID.fullmatch(gpu_uuid):
            raise ValueError('physical GPU UUID required; ordinals/MIG aliases are not accepted')
        gpu_uuid = canonical_gpu(gpu_uuid)
        with self.transaction() as db:
            # Registration is not permission to clear an existing quarantine.
            db.execute('INSERT OR IGNORE INTO gpus VALUES (?,?)', (gpu_uuid, encoded(dict(
                uuid=gpu_uuid, state='free', job_id=None, token=None, reason='operator registered; qualification pending'))))

    def gpus(self):
        with self.connect() as db:
            return [json.loads(x[0]) for x in db.execute('SELECT data FROM gpus ORDER BY id')]

    def submit(self, request, principal, key, quota=None, return_created=False):
        """Atomically replay or admit; optional API quotas never block a replay.

        GS1 callers retain a job dictionary. API callers can request the
        (job, created) result for exact 200/202 admission status.
        """
        request = valid_request(request)
        if any(not isinstance(x, str) or not 1 <= len(x) <= 128 or any(ord(c) < 32 for c in x) for x in (principal, key)):
            raise ValueError('principal/idempotency key must be bounded text')
        with self.transaction() as db:
            old = db.execute('SELECT request,data FROM jobs WHERE principal=? AND idem=?', (principal, key)).fetchone()
            if old:
                if old[0] != encoded(request):
                    raise AdmissionError('IDEMPOTENCY_CONFLICT', 'idempotency key conflicts with another request')
                return (json.loads(old[1]), False) if return_created else json.loads(old[1])
            if quota is not None:
                if type(quota) is not int or quota < 1:
                    raise ValueError('invalid admission quota')
                count = db.execute("SELECT COUNT(*) FROM jobs WHERE principal=? AND state NOT IN ('completed','failed','cancelled','interrupted')", (principal,)).fetchone()[0]
                if count >= quota:
                    raise AdmissionError('ADMISSION_QUOTA', 'active job quota reached')
            packet = self.packet(request['packet_id'])
            if request['execution_timeout_seconds'] > self.limits['max_execution_seconds']:
                raise ValueError('execution budget exceeds server policy')
            if request['retry_of']:
                prior = self._get(db, request['retry_of'])
                if prior['principal'] != principal or prior['state'] not in TERMINAL:
                    raise ValueError('retry must reference an accessible terminal job')
            if request['mode'] == 'qualify':
                target = request['target_gpu_uuid']
                if target is not None:
                    target = canonical_gpu(target)
                available = [json.loads(x[0]) for x in db.execute('SELECT data FROM gpus')]
                if not any(target is None or x['uuid'] == target for x in available):
                    raise ValueError('no registered compatible target; register a physical sm_90 GPU first')
            job_id = 'job_' + uuid.uuid4().hex
            job = dict(schema='open64.gpu-test.worker-job.v1', job_id=job_id, request=request,
                principal=principal, packet=packet, state='queued', created_at=utc(), created_seconds=time.time(),
                started_at=None, finished_at=None, owner_token=None, owner_process=None,
                selected_gpu_uuid=None, active_process=None, runner_exit_code=None, runner_returncode=None,
                termination_reason=None, cancel_requested_at=None, collection_status='not_started', review_status='not_started',
                receiver_verdict='NOT_RUN', lease_binding='not_applicable' if request['mode'] == 'check-only' else 'unknown',
                diagnostics=dict(requested=request['profiling'], assessment='DEFER_TO_ORIGINAL_REPORTS', profiling_separate=True),
                failures=[], steps=[], retry_of=request['retry_of'])
            db.execute('INSERT INTO jobs VALUES (?,?,?,?,?,?)', (job_id, job['state'], principal, key, encoded(request), encoded(job)))
            self._save(db, job, 'request durably accepted; no work launched')
            return (job, True) if return_created else job

    def get(self, job_id):
        with self.connect() as db:
            job = self._get(db, job_id)
            job['artifacts'] = [json.loads(x[0]) for x in db.execute('SELECT data FROM artifacts WHERE job=? ORDER BY name', (job_id,))]
            return job

    def events(self, job_id, after=0):
        with self.connect() as db:
            self._get(db, job_id)
            return [json.loads(x[0]) for x in db.execute('SELECT data FROM events WHERE job=? AND seq>? ORDER BY seq', (job_id, after))]

    def cancel(self, job_id, principal):
        with self.transaction() as db:
            job = self._get(db, job_id)
            if job['principal'] != principal:
                raise KeyError('unknown job')
            if job['state'] in TERMINAL or job['cancel_requested_at']:
                return job
            job['cancel_requested_at'] = utc()
            if job['state'] == 'queued':
                job.update(state='cancelled', finished_at=utc(), termination_reason='CANCELLED_WHILE_QUEUED')
            self._save(db, job, 'cancellation requested')
            return job

    def claim(self, owner_process, job_id=None):
        """Atomically choose ready work and acquire a non-expiring physical lease."""
        with self.transaction() as db:
            rows = db.execute("SELECT data FROM jobs WHERE state='queued' ORDER BY rowid").fetchall()
            for row in rows:
                job = json.loads(row[0])
                if job_id is not None and job['job_id'] != job_id:
                    continue
                if time.time() - job['created_seconds'] > self.limits['max_queue_wait_seconds']:
                    job.update(state='failed', termination_reason='QUEUE_DEADLINE', finished_at=utc())
                    self._save(db, job, 'queue deadline elapsed without launching work')
                    continue
                token = uuid.uuid4().hex
                if job['request']['mode'] == 'qualify':
                    target = job['request']['target_gpu_uuid']
                    candidates = [json.loads(x[0]) for x in db.execute('SELECT data FROM gpus ORDER BY id')]
                    chosen = next((x for x in candidates if x['state'] == 'free' and (target is None or x['uuid'] == canonical_gpu(target))), None)
                    if chosen is None:
                        continue
                    chosen.update(state='leased', job_id=job['job_id'], token=token, reason='execution ownership')
                    db.execute('UPDATE gpus SET data=? WHERE id=?', (encoded(chosen), chosen['uuid']))
                    job['selected_gpu_uuid'] = chosen['uuid']
                job.update(state='preparing', started_at=utc(), owner_token=token, owner_process=owner_process)
                self._save(db, job, 'worker claim and GPU lease committed before launch')
                return job
        return None

    def change(self, job_id, token, message, **fields):
        with self.transaction() as db:
            job = self._get(db, job_id)
            if job['owner_token'] != token or job['state'] in TERMINAL:
                raise ValueError('stale worker ownership or terminal job')
            job.update(fields)
            self._save(db, job, message)
            return job

    def recovery_claim(self, job_id, owner_process):
        """Caller holds the job lock and has proved the previous owner is gone."""
        with self.transaction() as db:
            job = self._get(db, job_id)
            if job['state'] in TERMINAL or job['state'] == 'queued':
                return None
            token = uuid.uuid4().hex
            job.update(owner_token=token, owner_process=owner_process, state='cancelling', termination_reason='WORKER_LOST')
            if job['selected_gpu_uuid']:
                gpu = json.loads(db.execute('SELECT data FROM gpus WHERE id=?', (job['selected_gpu_uuid'],)).fetchone()[0])
                gpu.update(state='quarantined', token=token, reason='worker lost; device ownership uncertain')
                db.execute('UPDATE gpus SET data=? WHERE id=?', (encoded(gpu), gpu['uuid']))
            self._save(db, job, 'lost owner reconciled; no device reuse or blind execution retry')
            return job

    def unfinished(self):
        with self.connect() as db:
            return [json.loads(x[0]) for x in db.execute("SELECT data FROM jobs WHERE state NOT IN ('queued','completed','failed','cancelled','interrupted')")]

    def finish(self, job_id, token, state, gpu_safe, reason=None):
        if state not in TERMINAL:
            raise ValueError('terminal state required')
        with self.transaction() as db:
            job = self._get(db, job_id)
            if job['owner_token'] != token or job['state'] in TERMINAL:
                raise ValueError('stale worker cannot finish or release GPU')
            if job['selected_gpu_uuid']:
                gpu = json.loads(db.execute('SELECT data FROM gpus WHERE id=?', (job['selected_gpu_uuid'],)).fetchone()[0])
                if gpu['job_id'] != job_id or gpu['token'] != token:
                    raise ValueError('GPU lease ownership differs')
                gpu.update(state='free' if gpu_safe else 'quarantined',
                    job_id=None if gpu_safe else job_id, token=None,
                    reason='closed finalizer and device-idle check' if gpu_safe else 'explicit operator drain/release required')
                db.execute('UPDATE gpus SET data=? WHERE id=?', (encoded(gpu), gpu['uuid']))
            job.update(state=state, finished_at=utc(), owner_token=None, owner_process=None, active_process=None)
            if reason is not None:
                job['termination_reason'] = reason
            self._save(db, job, 'terminal record and lease disposition committed')
            return job

    def release_quarantine(self, gpu_uuid, reason):
        """Operator calls only after a fresh device-idle proof; retain its reason."""
        if not isinstance(reason, str) or not reason.strip():
            raise ValueError('operator release reason is required')
        with self.transaction() as db:
            row = db.execute('SELECT data FROM gpus WHERE id=?', (canonical_gpu(gpu_uuid),)).fetchone()
            if row is None:
                raise ValueError('unknown GPU')
            gpu = json.loads(row[0])
            if gpu['state'] != 'quarantined':
                raise ValueError('only a quarantined GPU can be explicitly released')
            if gpu['job_id'] and self._get(db, gpu['job_id'])['state'] not in TERMINAL:
                raise ValueError('reconcile the unfinished job before releasing its GPU')
            gpu.update(state='free', job_id=None, token=None, reason=reason, released_at=utc())
            db.execute('UPDATE gpus SET data=? WHERE id=?', (encoded(gpu), gpu['uuid']))
