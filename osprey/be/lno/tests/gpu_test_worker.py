"""GS1 local worker: persistent jobs, leases and existing packet execution.

No HTTP, source-upload, numerical policy or OpenCTI implementation. Execute only
operator-registered packets; reuse their runner, collector and offline receiver.
Contract: doc/GPU-TEST-WORKER-GS1-v1.0.md.
"""
import argparse
from contextlib import contextmanager
import fcntl
import getpass
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import sqlite3
import subprocess
import sys
import tarfile
import time
import uuid

from gpu_test_suite_common import catalog, roles
from gpu_test_worker_state import Store, TERMINAL, DIGEST, encoded, utc
from package_mla_q7_results import digest_file, snapshot_file
from validate_mla_q7_results import manifest, safe_name

HERE = Path(__file__).resolve().parent


def process_stamp(pid):
    """Distinguish dead/reused PIDs without ever signalling a recovered process."""
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return None
    except PermissionError:
        return 'unknown'
    proc = Path('/proc') / str(pid) / 'stat'
    try:
        if proc.is_file():
            fields = proc.read_text().rsplit(')', 1)[1].split()
            if fields[0] == 'Z':
                return None
            return Path('/proc/sys/kernel/random/boot_id').read_text().strip() + ':' + fields[19]
        result = subprocess.run(['ps', '-p', str(pid), '-o', 'stat=', '-o', 'lstart='],
            capture_output=True, text=True, timeout=3)
        text = result.stdout.strip()
        if not text or text.startswith('Z'):
            return None
        return ' '.join(text.split()[1:])
    except (OSError, IndexError, subprocess.TimeoutExpired):
        return 'unknown'


def identity(pid=None):
    pid = os.getpid() if pid is None else pid
    return dict(pid=pid, stamp=process_stamp(pid))


def still_alive(record):
    if not record:
        return False
    current = process_stamp(record['pid'])
    return current is not None and (current == 'unknown' or record['stamp'] == 'unknown' or current == record['stamp'])


@contextmanager
def job_lock(store, job_id):
    """A recovery attempt cannot steal work from a live supervising process."""
    if not isinstance(job_id, str) or not re.fullmatch(r'job_[0-9a-f]{32}', job_id):
        raise ValueError('invalid job identity')
    folder = store.root / 'jobs' / job_id
    folder.mkdir(exist_ok=True, mode=0o700)
    with (folder / 'owner.lock').open('a+b') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        try:
            yield folder
        finally:
            fcntl.flock(lock, fcntl.LOCK_UN)


def authenticate_registration(packet, archive, expected):
    """Bind installed source to transport bytes; never extract registration data.

    Optional H7 capabilities are captured at registration; v1 record spelling
    remains unchanged for immutable idempotent re-registration.
    Source packets omit PACKET_ID from SHA256SUMS, unlike the result Archive
    scanner. Reuse its name/manifest rules but check this source-only inventory.
    """
    packet = Path(packet).resolve()
    archive = Path(archive).resolve()
    if not DIGEST.fullmatch(expected) or digest_file(archive) != expected:
        raise ValueError('source archive transport digest differs')
    packet_id = (packet / 'PACKET_ID').read_text().strip()
    table = catalog(packet, packet_id)
    expected_files = manifest((packet / 'SHA256SUMS').read_bytes())
    expected_files.update(SHA256SUMS=packet_id, PACKET_ID=digest_file(packet / 'PACKET_ID'))
    actual_files = set()
    for path in packet.rglob('*'):
        if path.is_symlink() or not (path.is_file() or path.is_dir()):
            raise ValueError('unsafe installed source entry')
        if path.is_file():
            actual_files.add(path.relative_to(packet).as_posix())
    if actual_files != set(expected_files):
        raise ValueError('installed source contains missing/unlisted files')
    prefix = packet.name + '/'
    seen = set()
    total = 0
    with tarfile.open(archive, 'r|*') as tar:
        for count, entry in enumerate(tar, 1):
            name = entry.name.rstrip('/') if entry.isdir() else entry.name
            if count > 20000 or not safe_name(name) or name in seen:
                raise ValueError('unsafe/duplicate source archive member')
            seen.add(name)
            if entry.isdir():
                if name != packet.name and not name.startswith(prefix):
                    raise ValueError('source archive root differs')
                continue
            if not entry.isfile() or entry.sparse or not name.startswith(prefix):
                raise ValueError('source archive link/special/root differs')
            relative = name[len(prefix):]
            total += entry.size
            if total > 16 * 1024**3 or relative not in expected_files:
                raise ValueError('source archive budget/inventory differs')
            h = hashlib.sha256()
            stream = tar.extractfile(entry)
            for block in iter(lambda: stream.read(1048576), b''):
                h.update(block)
            if h.hexdigest() != expected_files.pop(relative):
                raise ValueError('source archive and installed packet differ: ' + relative)
    if expected_files or digest_file(archive) != expected:
        raise ValueError('source archive incomplete or changed during registration')
    for name in ('run_gpu_test_suite.py', 'package_gpu_test_suite_results.py', 'validate_gpu_test_suite.py'):
        if 'tools/' + name not in manifest((packet / 'SHA256SUMS').read_bytes()):
            raise ValueError('registered packet lacks combined worker entry points')
    record=dict(packet_id=packet_id, path=str(packet), archive=str(archive), archive_sha256=expected,
        archive_size_bytes=archive.stat().st_size, producer_commit=table.get('producer_commit'),
        registry_version='open64.gpu-test.worker-registry.v1', runner_sha256=digest_file(packet / 'tools/run_gpu_test_suite.py'))
    # Preserve exact old registry data for idempotent legacy re-registration.
    if 'h20' in table['packets']:record.update(profiles=list(roles(table)),h20_target=table['h20_target'])
    return record


class NvidiaDevice:
    """Read-only preflight/idle observations; real GPU qualification is GS3."""
    def inspect(self, gpu_uuid):
        if os.uname().sysname != 'Linux' or os.uname().machine != 'x86_64':
            raise ValueError('full packet requires Linux x86_64')
        def query(arguments):
            result = subprocess.run(['nvidia-smi', '--id=' + gpu_uuid, '--format=csv,noheader,nounits', *arguments],
                capture_output=True, text=True, timeout=15)
            if result.returncode:
                raise ValueError('nvidia-smi observation unavailable: ' + result.stderr[:1000])
            return result.stdout.strip()
        device = query(['--query-gpu=uuid,compute_cap,mig.mode.current'])
        fields = [x.strip() for x in device.split(',')]
        if len(fields) != 3 or fields[0].lower() != gpu_uuid.lower() or fields[1] != '9.0' or fields[2] != 'Disabled':
            raise ValueError('physical sm_90 GPU with MIG disabled required: ' + device)
        processes = query(['--query-compute-apps=pid,gpu_uuid'])
        if processes:
            raise ValueError('GPU has active compute contexts; lease is not system-wide isolation')
        return dict(uuid=gpu_uuid, device_query=device, compute_processes=processes, at=utc(),
            proof='packet finalizer plus read-only device-idle observation; hardware qualification pending')


class PacketBackend:
    """Only translate typed controls; the packet owns all measurement policy."""
    def verify(self, packet):
        catalog(Path(packet['path']), packet['packet_id'])
        if (Path(packet['path']) / 'PACKET_ID').read_text().strip() != packet['packet_id']:
            raise ValueError('installed packet identity file changed')
        if digest_file(Path(packet['path']) / 'tools/run_gpu_test_suite.py') != packet['runner_sha256']:
            raise ValueError('registered runner identity changed')
        if digest_file(packet['archive']) != packet['archive_sha256']:
            raise ValueError('registered source archive changed')
        allowed = set(manifest((Path(packet['path']) / 'SHA256SUMS').read_bytes())) | {'SHA256SUMS', 'PACKET_ID'}
        for path in Path(packet['path']).rglob('*'):
            if path.is_symlink() or path.is_file() and path.relative_to(packet['path']).as_posix() not in allowed:
                raise ValueError('unlisted or linked source added after registration')

    def runner(self, job, folder, grace):
        request = job['request']
        command = [sys.executable, str(Path(job['packet']['path']) / 'tools/run_gpu_test_suite.py'),
            '--packet', job['packet']['path'], '--results-parent', str(folder / 'work'),
            '--samples', str(request['samples']), '--warmup', str(request['warmup']),
            '--profiling', request['profiling'], '--collection-timeout', str(max(1, grace // 3))]
        if request['mode'] == 'check-only':
            command.append('--check-only')
        else:
            command += ['--gpu', job['selected_gpu_uuid']]
        if request['profile_timeline']:
            command.append('--profile-timeline')
        return command

    def collector(self, job, run, output):
        return [sys.executable, str(Path(job['packet']['path']) / 'tools/package_gpu_test_suite_results.py'),
            '--packet', job['packet']['path'], '--expected-sha256', job['packet']['packet_id'],
            '--run', str(run), '--output', str(output)]

    def validator(self, job, archive, output):
        return [sys.executable, str(Path(job['packet']['path']) / 'tools/validate_gpu_test_suite.py'),
            '--archive', str(archive), '--packet', job['packet']['path'],
            '--expected-packet-sha256', job['packet']['packet_id'], '--output', str(output)]


def child_environment(job):
    """Do not forward service credentials, PYTHONPATH or arbitrary request env."""
    allowed = ('PATH', 'HOME', 'USER', 'LANG', 'LC_ALL', 'TMPDIR', 'CUDA_HOME', 'CUDA_PATH',
               'CUDA_MODULE_LOADING', 'NVIDIA_VISIBLE_DEVICES', 'NVIDIA_DRIVER_CAPABILITIES',
               'LD_LIBRARY_PATH', 'LIBRARY_PATH', 'CPATH', 'CPLUS_INCLUDE_PATH')
    env = {key: os.environ[key] for key in allowed if key in os.environ}
    env['PYTHONDONTWRITEBYTECODE'] = '1'
    if job['selected_gpu_uuid']:
        env['CUDA_VISIBLE_DEVICES'] = job['selected_gpu_uuid']
    return env


class Worker:
    def __init__(self, store, backend=None, device=None, poll_seconds=0.1):
        self.store = store
        self.backend = backend or PacketBackend()
        self.device = device or NvidiaDevice()
        self.poll_seconds = poll_seconds
        self.signal_received = None
        self.collection_deadlines = {}

    def signal(self, number, frame):
        # The monitor performs cooperative cancellation; do not unwind past its
        # Popen cleanup/finalization boundary on SIGINT or SIGTERM.
        self.signal_received = number

    def change(self, job, message, **fields):
        return self.store.change(job['job_id'], job['owner_token'], message, **fields)

    def collection_deadline(self, job):
        """One bounded budget includes finalization, recovery and review work."""
        if job['job_id'] not in self.collection_deadlines:
            self.collection_deadlines[job['job_id']] = time.monotonic() + self.store.limits['collection_grace_seconds']
            self.change(job, 'bounded collection/review grace started', collection_started_at=utc())
        return self.collection_deadlines[job['job_id']]

    def collection_remaining(self, job):
        remaining = self.collection_deadline(job) - time.monotonic()
        if remaining <= 0:
            raise ValueError('collection/review grace exhausted; retain existing partial evidence')
        return remaining

    def step(self, job, folder, label, command, seconds, execution=False):
        """Persist a gated child identity before allowing it to execute code."""
        logs = folder / 'logs'
        logs.mkdir(exist_ok=True)
        started = time.monotonic()
        deadline = started + seconds
        stopping = False
        collecting = not execution
        forced = False
        reason = None
        read_fd, write_fd = os.pipe()
        child = None
        try:
            with (logs / (label + '.stdout')).open('xb') as stdout, (logs / (label + '.stderr')).open('xb') as stderr:
                child = subprocess.Popen([sys.executable, str(Path(__file__).resolve()), '_child', str(read_fd), *map(str, command)],
                    pass_fds=(read_fd,), stdout=stdout, stderr=stderr, start_new_session=True,
                    env=child_environment(job))
                os.close(read_fd)
                read_fd = None
                process = identity(child.pid)
                process.update(label=label, argv=list(map(str, command)), started_at=utc())
                self.change(job, 'gated ' + label + ' child persisted', active_process=process)
                os.write(write_fd, b'G')
                os.close(write_fd)
                write_fd = None
                while child.poll() is None:
                    current = self.store.get(job['job_id'])
                    now = time.monotonic()
                    if execution:
                        # This fixed registered runner already announces its
                        # native finalizer. It is progress only, never evidence
                        # acceptance. Exclude its collection from execution time.
                        with (logs / (label + '.stdout')).open('rb') as progress:
                            marker = b'\nQ7 step: q7-results\n' in b'\n' + progress.read(1024 * 1024)
                        if marker and not collecting:
                            collecting = True
                            if not stopping:
                                deadline = self.collection_deadline(job)
                                self.change(job, 'runner entered automatic collection', state='collecting')
                        if not stopping and (current['cancel_requested_at'] or self.signal_received or (not collecting and now >= deadline)):
                            reason = 'CANCELLED' if current['cancel_requested_at'] or self.signal_received else 'EXECUTION_DEADLINE'
                            stopping = True
                            deadline = self.collection_deadline(job)
                            self.change(job, 'cooperative runner stop; collection grace reserved', state='cancelling', termination_reason=reason)
                            if not collecting:
                                child.send_signal(signal.SIGTERM)
                    if now >= deadline:
                        forced = True
                        reason = reason or 'COLLECTION_DEADLINE'
                        # Only our direct new-session child is signalled. Its
                        # separate-session descendants may be uncertain: retain
                        # quarantine and never assert a successful drain.
                        try:
                            os.killpg(child.pid, signal.SIGKILL)
                        except ProcessLookupError:
                            pass
                        break
                    time.sleep(self.poll_seconds)
                raw = child.wait(timeout=10)
        finally:
            for fd in (read_fd, write_fd):
                if fd is not None:
                    os.close(fd)
            if child is not None and child.poll() is None:
                os.killpg(child.pid, signal.SIGKILL)
                child.wait(timeout=10)
        record = dict(label=label, argv=list(map(str, command)), returncode=raw,
            exit_code=128 - raw if raw < 0 else raw, forced=forced, reason=reason, finished_at=utc())
        current = self.store.get(job['job_id'])
        self.change(job, label + ' process reaped; original exit retained', active_process=None,
            steps=current['steps'] + [record])
        return record

    def publish(self, job, source, name):
        """Snapshot only closed regular evidence; commit immutable hash metadata."""
        if not re.fullmatch(r'[A-Za-z0-9_.-]+', name):
            raise ValueError('unsafe artifact name')
        source = Path(source)
        if not source.is_file() or source.is_symlink():
            return
        with self.store.connect() as db:
            old = db.execute('SELECT data FROM artifacts WHERE job=? AND name=?', (job['job_id'], name)).fetchone()
        if old:
            if json.loads(old[0])['sha256'] != digest_file(source):
                raise ValueError('closed artifact changed; refusing replacement')
            return
        folder = self.store.root / 'artifacts' / job['job_id']
        folder.mkdir(exist_ok=True, mode=0o700)
        artifact_id = 'artifact_' + uuid.uuid4().hex
        path = folder / artifact_id
        if snapshot_file(source, path):
            raise ValueError('live artifact cannot be published: ' + name)
        # Publish durable bytes before the SQLite reference. A crash can leave
        # an unreferenced file, but must not acknowledge a not-yet-flushed one.
        with path.open('rb') as handle:
            os.fsync(handle.fileno())
        path.chmod(0o444)
        for directory in (folder, folder.parent):
            descriptor = os.open(str(directory), os.O_RDONLY | getattr(os, 'O_DIRECTORY', 0))
            try:
                os.fsync(descriptor)
            finally:
                os.close(descriptor)
        record = dict(artifact_id=artifact_id, name=name, path=str(path), sha256=digest_file(path),
            size_bytes=path.stat().st_size, created_at=utc())
        with self.store.transaction() as db:
            current = self.store._get(db, job['job_id'])
            if current['owner_token'] != job['owner_token'] or current['state'] in TERMINAL:
                raise ValueError('artifact publisher lost ownership')
            db.execute('INSERT INTO artifacts VALUES (?,?,?)', (job['job_id'], name, encoded(record)))

    def run_directory(self, folder):
        work = folder / 'work'
        work.mkdir(exist_ok=True)
        candidates = sorted(work.glob('gpu-suite-run-*'))
        if len(candidates) > 1 or any(x.is_symlink() or not x.is_dir() for x in candidates):
            raise ValueError('ambiguous or unsafe runner directory')
        return candidates[0] if candidates else work

    def closed_runner(self, folder):
        run = self.run_directory(folder)
        path = run / 'completion.json'
        if not path.is_file() or path.is_symlink() or path.stat().st_size > 65536:
            return None
        value = json.loads(path.read_text())
        if not isinstance(value, dict) or type(value.get('exit_code')) is not int or type(value.get('archive_created')) is not bool:
            raise ValueError('malformed runner completion; closure not established')
        return value

    def finalize(self, job, folder, closed, recovering=False):
        """Collect/review through existing tools without changing native evidence."""
        self.change(job, 'collect closed complete/partial results', state='collecting', collection_status='pending')
        self.collection_deadline(job)
        run = self.run_directory(folder)
        archive = run / 'Open64_GPU_Test_results.tar.gz'
        if not archive.is_file() or not Path(str(archive) + '.sha256').is_file():
            recovery = folder / ('recovery-' + uuid.uuid4().hex)
            recovery.mkdir()
            archive = recovery / 'Open64_GPU_Test_results.tar.gz'
            result = self.step(job, folder, 'collect-' + uuid.uuid4().hex,
                self.backend.collector(job, run, archive), self.collection_remaining(job))
            if result['exit_code']:
                self.change(job, 'recovery collector failed; logs retained', collection_status='failed')
        if archive.is_file() and Path(str(archive) + '.sha256').is_file():
            self.publish(job, archive, 'Open64_GPU_Test_results.tar.gz')
            self.publish(job, Path(str(archive) + '.sha256'), 'Open64_GPU_Test_results.tar.gz.sha256')
            self.change(job, 'result transport published; not qualification', state='validating', collection_status='complete', review_status='running')
            review = folder / ('review-' + uuid.uuid4().hex)
            result = self.step(job, folder, 'review-' + uuid.uuid4().hex,
                self.backend.validator(job, archive, review), self.collection_remaining(job))
            summary = review / 'summary.json'
            if summary.is_file():
                value = json.loads(summary.read_text())
                verdict = {0: 'PASS_REPORTED', 1: 'FAIL', 2: 'INCOMPLETE', 3: 'INVALID'}.get(result['exit_code'])
                if verdict is None or value.get('overall') != verdict:
                    raise ValueError('receiver summary and original exit disagree')
                binding = 'not_applicable' if not job['selected_gpu_uuid'] else 'unknown'
                observed = value.get('execution_gpu_uuids', [])
                if job['selected_gpu_uuid'] and observed:
                    expected = job['selected_gpu_uuid'][4:].replace('-', '').lower()
                    binding = 'verified' if observed == [expected] else 'fail'
                self.change(job, 'original receiver verdict retained', receiver_verdict=verdict,
                    review_summary=value, lease_binding=binding, review_status='complete')
                self.publish(job, summary, 'review.json')
                self.publish(job, review / 'summary.md', 'review.md')
            else:
                self.change(job, 'receiver did not produce a summary', review_status='failed')
        else:
            self.change(job, 'return unavailable; retain closed logs and failure', collection_status='failed')
        for path in sorted((folder / 'logs').glob('*')):
            self.publish(job, path, path.name)
        current = self.store.get(job['job_id'])
        safe = not job['selected_gpu_uuid']
        if job['selected_gpu_uuid'] and closed and not recovering and current['lease_binding'] != 'fail':
            try:
                observation = self.device.inspect(job['selected_gpu_uuid'])
                self.change(job, 'device idle observed after packet finalizer', drain_observation=observation)
                safe = True
            except (OSError, ValueError, subprocess.SubprocessError) as error:
                self.change(job, 'device drain not established', failures=current['failures'] + [str(error)])
        cancelled = current['termination_reason'] == 'CANCELLED' or current['cancel_requested_at'] or self.signal_received
        state = 'interrupted' if recovering else 'cancelled' if cancelled else 'failed'
        if not recovering and not cancelled and current['termination_reason'] is None and current['runner_exit_code'] == 0 and current['collection_status'] == 'complete' and current['receiver_verdict'] in ('PASS_REPORTED', 'INCOMPLETE'):
            state = 'completed'
        if current['lease_binding'] == 'fail':
            state = 'failed'
        if job['selected_gpu_uuid'] and not safe and not recovering:
            state = 'failed' if state != 'cancelled' else state
        return self.store.finish(job['job_id'], job['owner_token'], state, safe)

    def execute(self, job):
        with job_lock(self.store, job['job_id']) as folder:
            token = job['owner_token']
            closed = False
            launched = False
            authenticated = False
            started = time.monotonic()
            try:
                self.backend.verify(job['packet'])
                authenticated = True
                if job['selected_gpu_uuid']:
                    proof = self.device.inspect(job['selected_gpu_uuid'])
                    self.change(job, 'physical device preflight recorded', preflight=proof)
                (folder / 'work').mkdir(exist_ok=True)
                if self.store.get(job['job_id'])['cancel_requested_at'] or self.signal_received:
                    self.change(job, 'cancelled before launching runner', termination_reason='CANCELLED')
                    return self.finalize(job, folder, True)
                remaining = job['request']['execution_timeout_seconds'] - (time.monotonic() - started)
                if remaining <= 0:
                    self.change(job, 'preparation exhausted execution budget', termination_reason='EXECUTION_DEADLINE')
                    return self.finalize(job, folder, True)
                self.change(job, 'existing combined runner selected; no policy substitution', state='running')
                launched = True
                result = self.step(job, folder, 'runner', self.backend.runner(job, folder,
                    self.store.limits['collection_grace_seconds']), remaining, execution=True)
                self.change(job, 'runner execution status recorded', runner_returncode=result['returncode'],
                    runner_exit_code=result['exit_code'])
                completion = self.closed_runner(folder)
                closed = bool(completion) and not result['forced'] and completion['exit_code'] == result['exit_code']
                if result['reason']:
                    self.change(job, 'runner termination reason retained', termination_reason=result['reason'])
                if not closed:
                    raise ValueError('runner closure unproven; descendants/evidence may still be live')
                return self.finalize(job, folder, closed)
            except Exception as error:
                current = self.store.get(job['job_id'])
                self.change(job, 'worker failure retained', failures=current['failures'] + [str(error)])
                # Source reauthentication failure must not run more code from
                # that packet. Other prelaunch failures have no live child and
                # can still return partial evidence using authenticated tools.
                if authenticated and not launched:
                    try:
                        return self.finalize(job, folder, True)
                    except Exception as collection_error:
                        self.change(job, 'failure collector unavailable', collection_status='failed', failures=current['failures'] + [str(error), str(collection_error)])
                for path in sorted((folder / 'logs').glob('*')):
                    self.publish(job, path, path.name)
                current = self.store.get(job['job_id'])
                self.change(job, 'retain collection and review outcomes separately',
                    collection_status='complete' if current['collection_status'] == 'complete' else 'failed',
                    review_status='failed' if current['review_status'] == 'running' else current['review_status'])
                return self.store.finish(job['job_id'], token, 'failed', not job['selected_gpu_uuid'])

    def recover(self, job_id):
        with job_lock(self.store, job_id) as folder:
            prior = self.store.get(job_id)
            if prior['state'] in TERMINAL or prior['state'] == 'queued':
                return prior
            if still_alive(prior['owner_process']):
                raise ValueError('original worker is still alive; recovery refused')
            job = self.store.recovery_claim(job_id, identity())
            active = job.get('active_process')
            if still_alive(active):
                self.change(job, 'orphaned child still active; defer snapshots and device reuse',
                    owner_process=None, collection_status='pending')
                return self.store.get(job_id)
            try:
                completion = self.closed_runner(folder)
                if not completion:
                    self.change(job, 'lost runner has no closed finalizer; preserve uncertain evidence', collection_status='partial')
                    return self.store.finish(job_id, job['owner_token'], 'interrupted', not job['selected_gpu_uuid'])
                self.backend.verify(job['packet'])
                self.change(job, 'closed original runner evidence recovered without re-execution',
                    runner_exit_code=completion['exit_code'], active_process=None)
                return self.finalize(job, folder, True, recovering=True)
            except Exception as error:
                current = self.store.get(job_id)
                self.change(job, 'recovery failed without rerunning original job',
                    collection_status='complete' if current['collection_status'] == 'complete' else 'failed',
                    review_status='failed' if current['review_status'] == 'running' else current['review_status'],
                    failures=current['failures'] + [str(error)])
                return self.store.finish(job_id, job['owner_token'], 'interrupted', not job['selected_gpu_uuid'])

    def release_gpu(self, gpu_uuid, reason):
        self.device.inspect(gpu_uuid)
        self.store.release_quarantine(gpu_uuid, reason)

    def run_once(self, job_id=None):
        # Reconcile abandoned claims before claiming queued work. A held lock or
        # live owner is left alone; neither age nor a new worker clears a lease.
        for job in self.store.unfinished():
            if not still_alive(job['owner_process']):
                try:
                    self.recover(job['job_id'])
                except (BlockingIOError, ValueError):
                    pass
        if self.signal_received:
            return None
        job = self.store.claim(identity(), job_id)
        return self.execute(job) if job else None


def main(argv=None):
    argv = sys.argv[1:] if argv is None else argv
    if argv and argv[0] == '_child':
        fd = int(argv[1])
        with os.fdopen(fd, 'rb') as gate:
            allowed = gate.read(1)
        if allowed != b'G':
            return 125
        os.execvpe(argv[2], argv[2:], os.environ)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--store', type=Path, required=True)
    commands = parser.add_subparsers(dest='command', required=True)
    register = commands.add_parser('register-packet')
    register.add_argument('--packet', type=Path, required=True)
    register.add_argument('--archive', type=Path, required=True)
    register.add_argument('--archive-sha256', required=True)
    gpu = commands.add_parser('register-gpu')
    gpu.add_argument('--uuid', required=True)
    submit = commands.add_parser('submit')
    submit.add_argument('--request', type=Path, required=True)
    submit.add_argument('--principal', default=getpass.getuser())
    submit.add_argument('--key', required=True)
    for name in ('status', 'events', 'cancel', 'recover'):
        command = commands.add_parser(name)
        command.add_argument('job_id')
        if name == 'cancel':
            command.add_argument('--principal', default=getpass.getuser())
    run = commands.add_parser('run-once')
    run.add_argument('--job-id')
    release = commands.add_parser('release-gpu')
    release.add_argument('--uuid', required=True)
    release.add_argument('--reason', required=True)
    commands.add_parser('gpus')
    args = parser.parse_args(argv)
    try:
        if args.command == 'register-packet':
            packet = args.packet.resolve()
            state_root = args.store.resolve()
            if state_root == packet or packet in state_root.parents or state_root in packet.parents:
                raise ValueError('worker store and source installation must not overlap')
        store = Store(args.store)
        worker = Worker(store)
        if args.command == 'register-packet':
            value = authenticate_registration(packet, args.archive, args.archive_sha256)
            store.register_packet(value)
        elif args.command == 'register-gpu':
            worker.device.inspect(args.uuid)
            store.register_gpu(args.uuid)
            value = store.gpus()
        elif args.command == 'submit':
            from gpu_test_results import loads
            value = store.submit(loads(args.request.read_bytes()), args.principal, args.key)
        elif args.command == 'status':
            value = store.get(args.job_id)
        elif args.command == 'events':
            value = store.events(args.job_id)
        elif args.command == 'cancel':
            value = store.cancel(args.job_id, args.principal)
        elif args.command == 'gpus':
            value = store.gpus()
        elif args.command == 'release-gpu':
            worker.release_gpu(args.uuid, args.reason)
            value = store.gpus()
        else:
            old = {sig: signal.signal(sig, worker.signal) for sig in (signal.SIGINT, signal.SIGTERM)}
            try:
                value = worker.recover(args.job_id) if args.command == 'recover' else worker.run_once(args.job_id)
            finally:
                for sig, previous in old.items():
                    signal.signal(sig, previous)
        print(json.dumps(value, indent=2, allow_nan=False))
        return 0
    except (OSError, ValueError, KeyError, sqlite3.Error, tarfile.TarError) as error:
        print(json.dumps(dict(error=str(error)), indent=2), file=sys.stderr)
        return 1


if __name__ == '__main__':
    raise SystemExit(main())
