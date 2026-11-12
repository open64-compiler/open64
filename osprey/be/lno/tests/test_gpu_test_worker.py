"""GS1 host-only state/process tests; synthetic children never execute GPU code."""
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tarfile
import tempfile
import threading
import time
import unittest

from gpu_test_worker_state import Store, DEFAULT_LIMITS, valid_request
from gpu_test_worker import Worker, PacketBackend, identity, process_stamp, authenticate_registration, child_environment
import test_gpu_test_suite as suite_fixtures

HERE = Path(__file__).resolve().parent
GPU1 = 'GPU-00000000-0000-0000-0000-000000000001'
GPU2 = 'GPU-00000000-0000-0000-0000-000000000002'
PACKET_ID = 'a' * 64


def request(mode='check-only', **changes):
    value = dict(schema='open64.gpu-test.job-request.v1', packet_id=PACKET_ID,
        profile='combined', mode=mode, retry_of=None, target_gpu_uuid=None,
        samples=20, warmup=3, profiling='off', profile_timeline=False, execution_timeout_seconds=15)
    value.update(changes)
    return value


FAKE = '''import argparse,hashlib,json,os,signal,sys,time
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('phase');p.add_argument('folder',type=Path);p.add_argument('controls',type=Path)
p.add_argument('--output',type=Path);p.add_argument('--verdict',type=int,default=2);p.add_argument('--gpu');a=p.parse_args()
c=json.loads(a.controls.read_text());a.folder.mkdir(exist_ok=True,parents=True)
if a.phase=='runner':
 r=a.folder/'gpu-suite-run-fixture';r.mkdir()
 (r/'received.json').write_text(json.dumps(dict(environment=dict(os.environ),controls=c)))
 (r/'ready').write_text(str(os.getpid()))
 stop=[False]
 def term(sig,frame):
  if not c.get('ignore'):stop[0]=True
 signal.signal(signal.SIGTERM,term)
 while c.get('wait') and not stop[0] and not (a.controls.parent/'release').exists():time.sleep(.02)
 code=143 if stop[0] else c.get('exit',0)
 print('Q7 step: q7-results',flush=True)
 if c.get('collection_delay'):time.sleep(c['collection_delay'])
 if not c.get('no_completion'):(r/'completion.json').write_text(json.dumps(dict(exit_code=code,archive_created=False)))
 raise SystemExit(code)
if a.phase=='collector':
 (a.folder/'collector-called').write_text('called')
 if c.get('collector_delay'):time.sleep(c['collector_delay'])
 if c.get('collector_failure'):raise SystemExit(19)
 a.output.write_bytes(b'SYNTHETIC control-only evidence; not a GPU return')
 Path(str(a.output)+'.sha256').write_text(hashlib.sha256(a.output.read_bytes()).hexdigest()+'  '+a.output.name+'\\n')
if a.phase=='validator':
 if c.get('validator_delay'):time.sleep(c['validator_delay'])
 a.output.mkdir()
 code=c.get('verdict',a.verdict)
 value=dict(overall={0:'PASS_REPORTED',1:'FAIL',2:'INCOMPLETE',3:'INVALID'}[code],synthetic_control_fixture=True,
  execution_gpu_uuids=[(c.get('wrong_gpu') or a.gpu)[4:].replace('-','').lower()] if a.gpu else [])
 (a.output/'summary.json').write_text(json.dumps(value));(a.output/'summary.md').write_text('Synthetic controls only')
 raise SystemExit(code)
'''


class FakeBackend(PacketBackend):
    def verify(self, packet):
        controls = json.loads(Path(packet['path'], 'controls.json').read_text())
        if controls.get('verify_failure'):
            raise ValueError('injected source-binding refusal')
        if controls.get('verify_delay'):
            time.sleep(controls['verify_delay'])

    def command(self, job, phase, folder):
        return [sys.executable, str(Path(job['packet']['path']) / 'fake.py'), phase,
                str(folder), str(Path(job['packet']['path']) / 'controls.json')]

    def runner(self, job, folder, grace):
        return self.command(job, 'runner', folder / 'work')

    def collector(self, job, run, output):
        return self.command(job, 'collector', run) + ['--output', str(output)]

    def validator(self, job, archive, output):
        command = self.command(job, 'validator', archive.parent) + ['--output', str(output),
            '--verdict', '2' if job['request']['mode'] == 'check-only' else '0']
        if job['selected_gpu_uuid']:
            command += ['--gpu', job['selected_gpu_uuid']]
        return command


class FakeDevice:
    def __init__(self, fail_at=None):
        self.calls = 0
        self.fail_at = fail_at

    def inspect(self, gpu_uuid):
        self.calls += 1
        if self.calls == self.fail_at:
            raise ValueError('synthetic missing drain proof')
        return dict(uuid=gpu_uuid, synthetic=True, gpu_execution=False)


def run_fake(store, job_id):
    worker = Worker(Store(store), FakeBackend(), FakeDevice(), .02)
    old = {sig: signal.signal(sig, worker.signal) for sig in (signal.SIGINT, signal.SIGTERM)}
    try:
        return worker.run_once(job_id)
    finally:
        for sig, previous in old.items():signal.signal(sig, previous)


class WorkerTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.fixture = self.root / 'fixture'
        self.fixture.mkdir()
        (self.fixture / 'fake.py').write_text(FAKE)
        self.controls()
        limits = dict(DEFAULT_LIMITS, collection_grace_seconds=2)
        self.store = Store(self.root / 'store', limits)
        self.store.register_packet(dict(packet_id=PACKET_ID, path=str(self.fixture),
            archive=str(self.root / 'fixture.tar.gz'), archive_sha256='b' * 64,
            archive_size_bytes=1, producer_commit='synthetic', runner_sha256='c' * 64))
        self.worker = Worker(self.store, FakeBackend(), FakeDevice(), .02)

    def tearDown(self):
        self.temporary.cleanup()

    def controls(self, **values):
        (self.fixture / 'controls.json').write_text(json.dumps(values))

    def submit(self, mode='check-only', key='one', **changes):
        if mode == 'qualify':
            self.store.register_gpu(GPU1)
        return self.store.submit(request(mode, **changes), 'operator', key)

    def wait_ready(self, job_id):
        folder = self.store.root / 'jobs' / job_id / 'work/gpu-suite-run-fixture'
        deadline = time.monotonic() + 12
        while time.monotonic() < deadline:
            if (folder / 'ready').exists():
                return folder
            time.sleep(.02)
        self.fail('fake worker did not become ready')

    def process(self, job_id):
        code = 'import sys;sys.path.insert(0,sys.argv[1]);from test_gpu_test_worker import run_fake;run_fake(sys.argv[2],sys.argv[3])'
        return subprocess.Popen([sys.executable, '-c', code, str(HERE), str(self.store.root), job_id],
            stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, env=dict(os.environ, PYTHONDONTWRITEBYTECODE='1'))

    def test_durable_idempotency_and_principal_isolation(self):
        job = self.submit()
        reopened = Store(self.store.root)
        same = reopened.submit(dict(reversed(list(request().items()))), 'operator', 'one')
        self.assertEqual(same['job_id'], job['job_id'])
        with self.assertRaises(ValueError):
            reopened.submit(request(samples=21), 'operator', 'one')
        other = reopened.submit(request(), 'second', 'one')
        self.assertNotEqual(other['job_id'], job['job_id'])
        with self.assertRaises(KeyError):
            reopened.cancel(job['job_id'], 'second')
        self.assertEqual(reopened.get(job['job_id'])['state'], 'queued')

    def test_request_rejection_creates_no_runnable_job(self):
        for invalid in (request(extra='shell'), request(samples=True), request(packet_id='latest'),
                        request(profiling='auto'), request(target_gpu_uuid=GPU1),
                        request('qualify', profile_timeline=True), request(execution_timeout_seconds=0)):
            with self.assertRaises(ValueError):
                self.store.submit(invalid, 'operator', 'bad')
        with self.store.connect() as db:
            self.assertEqual(db.execute('SELECT COUNT(*) FROM jobs').fetchone()[0], 0)

    def test_concurrent_claims_cannot_share_one_gpu(self):
        one = self.submit('qualify')
        self.submit('qualify', key='two')
        def claim(_):
            return Store(self.store.root).claim(identity())
        with ThreadPoolExecutor(max_workers=2) as pool:
            claims = list(pool.map(claim, range(2)))
        self.assertEqual(sum(x is not None for x in claims), 1)
        held = Store(self.store.root).gpus()[0]
        self.assertEqual(held['state'], 'leased')
        self.assertEqual(held['job_id'], one['job_id'])
        self.store.register_gpu(GPU1)
        self.assertEqual(self.store.gpus()[0]['state'], 'leased')

    def test_different_physical_gpus_allow_parallel_claims(self):
        self.store.register_gpu(GPU1)
        self.store.register_gpu(GPU2)
        self.submit('qualify', target_gpu_uuid=GPU1)
        self.submit('qualify', key='two', target_gpu_uuid=GPU2)
        first = self.store.claim(identity())
        second = self.store.claim(identity())
        self.assertNotEqual(first['selected_gpu_uuid'], second['selected_gpu_uuid'])

    def test_cpu_job_bypasses_a_busy_gpu_without_borrowing_its_lease(self):
        first = self.submit('qualify')
        self.store.claim(identity(), first['job_id'])
        second = self.submit('qualify', key='two')
        cpu = self.submit(key='cpu')
        claimed = self.store.claim(identity())
        self.assertEqual(claimed['job_id'], cpu['job_id'])
        self.assertIsNone(claimed['selected_gpu_uuid'])
        self.assertEqual(self.store.get(second['job_id'])['state'], 'queued')

    def test_queued_cancellation_is_terminal_and_never_launches(self):
        job = self.submit()
        cancelled = self.store.cancel(job['job_id'], 'operator')
        events = self.store.events(job['job_id'])
        self.assertEqual(cancelled['state'], 'cancelled')
        self.assertIsNone(self.worker.run_once(job['job_id']))
        self.store.cancel(job['job_id'], 'operator')
        self.assertEqual(self.store.events(job['job_id']), events)

    def test_check_only_finishes_with_incomplete_review_and_immutable_artifacts(self):
        job = self.submit()
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['state'], 'completed')
        current = self.store.get(job['job_id'])
        self.assertEqual(current['runner_exit_code'], 0)
        self.assertEqual(current['receiver_verdict'], 'INCOMPLETE')
        self.assertFalse(self.store.gpus())
        names = {x['name'] for x in current['artifacts']}
        self.assertIn('Open64_GPU_Test_results.tar.gz', names)
        self.assertIn('review.json', names)
        for artifact in current['artifacts']:
            self.assertEqual(hashlib.sha256(Path(artifact['path']).read_bytes()).hexdigest(), artifact['sha256'])
        original = current['finished_at']
        self.assertIsNone(self.worker.run_once(job['job_id']))
        self.assertEqual(self.store.get(job['job_id'])['finished_at'], original)

    def test_event_sequence_survives_reopen(self):
        job = self.submit()
        self.worker.run_once(job['job_id'])
        events = Store(self.store.root).events(job['job_id'])
        self.assertEqual([x['sequence'] for x in events], list(range(1, len(events) + 1)))
        self.assertEqual(events[-1]['state'], 'completed')

    def test_gpu_success_requires_finalizer_and_fresh_device_idle_observation(self):
        job = self.submit('qualify')
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['state'], 'completed')
        self.assertEqual(self.store.gpus()[0]['state'], 'free')
        self.assertEqual(self.worker.device.calls, 2)
        self.assertEqual(result['lease_binding'], 'verified')

    def test_ordinary_failure_keeps_original_exit_and_collects_results(self):
        self.controls(exit=17, verdict=1)
        job = self.submit('qualify')
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['state'], 'failed')
        self.assertEqual(result['runner_exit_code'], 17)
        self.assertEqual(result['receiver_verdict'], 'FAIL')
        self.assertEqual(result['collection_status'], 'complete')
        self.assertEqual(self.store.gpus()[0]['state'], 'free')

    def test_receiver_failure_does_not_rewrite_original_runner_exit(self):
        self.controls(verdict=3)
        job = self.submit()
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['runner_exit_code'], 0)
        self.assertEqual(result['receiver_verdict'], 'INVALID')
        self.assertEqual(result['state'], 'failed')

    def test_collector_failure_keeps_original_test_failure_and_logs(self):
        self.controls(exit=17, collector_failure=True)
        job = self.submit()
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['runner_exit_code'], 17)
        self.assertEqual(result['collection_status'], 'failed')
        self.assertEqual(result['receiver_verdict'], 'NOT_RUN')
        self.assertTrue(any(x['name'].startswith('collect-') for x in self.store.get(job['job_id'])['artifacts']))

    def test_source_refusal_does_not_invoke_runner_or_collector(self):
        self.controls(verify_failure=True)
        job = self.submit()
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['state'], 'failed')
        self.assertIsNone(result['runner_exit_code'])
        self.assertFalse(list((self.store.root / 'jobs' / job['job_id']).rglob('collector-called')))

    def test_preparation_counts_against_execution_deadline(self):
        self.controls(verify_delay=1.1)
        job = self.submit(execution_timeout_seconds=1)
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['termination_reason'], 'EXECUTION_DEADLINE')
        self.assertIsNone(result['runner_exit_code'])
        self.assertEqual(result['collection_status'], 'complete')

    def test_active_cancel_is_cooperative_and_finalizes_before_lease_release(self):
        self.controls(wait=True)
        job = self.submit('qualify')
        process = self.process(job['job_id'])
        try:
            self.wait_ready(job['job_id'])
            self.assertEqual(self.store.gpus()[0]['state'], 'leased')
            self.store.cancel(job['job_id'], 'operator')
            _, stderr = process.communicate(timeout=15)
            self.assertEqual(process.returncode, 0, stderr)
            result = self.store.get(job['job_id'])
            self.assertEqual(result['state'], 'cancelled')
            self.assertEqual(result['runner_exit_code'], 143)
            self.assertEqual(result['collection_status'], 'complete')
            self.assertEqual(self.store.gpus()[0]['state'], 'free')
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate()

    def test_deadline_allows_cooperative_collection(self):
        self.controls(wait=True)
        job = self.submit('qualify', execution_timeout_seconds=1)
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['termination_reason'], 'EXECUTION_DEADLINE')
        self.assertEqual(result['runner_exit_code'], 143)
        self.assertEqual(result['collection_status'], 'complete')
        self.assertEqual(result['state'], 'failed')

    def test_collection_does_not_consume_execution_budget(self):
        self.controls(collection_delay=1.2)
        job = self.submit(execution_timeout_seconds=1)
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['state'], 'completed')
        self.assertIsNone(result['termination_reason'])

    def test_collection_grace_is_shared_not_restarted_per_stage(self):
        self.controls(collection_delay=1.2, collector_delay=1.2)
        job = self.submit()
        started = time.monotonic()
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['state'], 'failed')
        self.assertEqual(result['receiver_verdict'], 'NOT_RUN')
        self.assertLess(time.monotonic() - started, 3.5)

    def test_review_timeout_does_not_erase_successful_collection(self):
        self.controls(validator_delay=3)
        job = self.submit()
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['state'], 'failed')
        self.assertEqual(result['runner_exit_code'], 0)
        self.assertEqual(result['collection_status'], 'complete')
        self.assertEqual(result['review_status'], 'failed')
        self.assertEqual(result['receiver_verdict'], 'NOT_RUN')
        self.assertIn('Open64_GPU_Test_results.tar.gz', {x['name'] for x in self.store.get(job['job_id'])['artifacts']})

    def test_worker_sigterm_reaches_packet_finalizer(self):
        self.controls(wait=True)
        job = self.submit('qualify')
        process = self.process(job['job_id'])
        try:
            self.wait_ready(job['job_id'])
            process.send_signal(signal.SIGTERM)
            _, stderr = process.communicate(timeout=15)
            self.assertEqual(process.returncode, 0, stderr)
            result = self.store.get(job['job_id'])
            self.assertEqual(result['state'], 'cancelled')
            self.assertEqual(result['collection_status'], 'complete')
            self.assertEqual(self.store.gpus()[0]['state'], 'free')
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate()

    def test_forced_stop_quarantines_and_does_not_snapshot_live_workload(self):
        self.controls(wait=True, ignore=True)
        job = self.submit('qualify', execution_timeout_seconds=1)
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['state'], 'failed')
        self.assertEqual(self.store.gpus()[0]['state'], 'quarantined')
        self.assertEqual(result['collection_status'], 'failed')
        self.assertFalse(list((self.store.root / 'jobs' / job['job_id']).rglob('collector-called')))

    def test_postrun_drain_failure_keeps_gpu_quarantined(self):
        job = self.submit('qualify')
        self.worker.device = FakeDevice(fail_at=2)
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['state'], 'failed')
        self.assertEqual(self.store.gpus()[0]['state'], 'quarantined')
        self.worker.device = FakeDevice()
        self.worker.release_gpu(GPU1, 'synthetic operator recovery proof')
        self.assertEqual(self.store.gpus()[0]['state'], 'free')

    def test_returned_gpu_mismatch_fails_worker_and_quarantines_lease(self):
        self.controls(wrong_gpu=GPU2)
        job = self.submit('qualify')
        result = self.worker.run_once(job['job_id'])
        self.assertEqual(result['lease_binding'], 'fail')
        self.assertEqual(result['receiver_verdict'], 'PASS_REPORTED')
        self.assertEqual(result['state'], 'failed')
        self.assertEqual(self.store.gpus()[0]['state'], 'quarantined')

    def test_live_worker_recovery_is_refused(self):
        job = self.submit()
        self.store.claim(identity(), job['job_id'])
        with self.assertRaises(ValueError):
            self.worker.recover(job['job_id'])

    def test_recovery_rejects_path_names_before_creating_workspaces(self):
        with self.assertRaises(ValueError):
            self.worker.recover('../../outside-worker-store')
        self.assertFalse((self.root / 'outside-worker-store').exists())

    def test_worker_loss_waits_for_orphan_then_recovers_without_rerun(self):
        self.controls(wait=True)
        job = self.submit('qualify')
        process = self.process(job['job_id'])
        runner_pid = None
        try:
            run = self.wait_ready(job['job_id'])
            runner_pid = int((run / 'ready').read_text())
            process.kill()
            process.communicate(timeout=5)
            result = self.worker.recover(job['job_id'])
            self.assertEqual(result['state'], 'cancelling')
            self.assertEqual(self.store.gpus()[0]['state'], 'quarantined')
            self.assertFalse((run / 'collector-called').exists())
            with self.assertRaises(ValueError):
                self.worker.release_gpu(GPU1, 'cannot release unfinished work')
            (self.fixture / 'release').write_text('release')
            deadline = time.monotonic() + 8
            while time.monotonic() < deadline and (not (run / 'completion.json').exists() or process_stamp(runner_pid) is not None):
                time.sleep(.03)
            result = self.worker.recover(job['job_id'])
            self.assertEqual(result['state'], 'interrupted')
            self.assertEqual(result['receiver_verdict'], 'PASS_REPORTED')
            self.assertEqual(self.store.gpus()[0]['state'], 'quarantined')
            self.assertEqual(len(list((self.store.root / 'jobs' / job['job_id'] / 'work').glob('gpu-suite-run-*'))), 1)
            self.worker.release_gpu(GPU1, 'synthetic completed orphan recovery')
            self.assertEqual(self.store.gpus()[0]['state'], 'free')
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate()
            if runner_pid and process_stamp(runner_pid) is not None:
                try:os.kill(runner_pid, signal.SIGKILL)
                except ProcessLookupError:pass

    def test_stale_owner_token_cannot_release_new_claim(self):
        job = self.submit('qualify')
        claim = self.store.claim(identity(), job['job_id'])
        with self.assertRaises(ValueError):
            self.store.finish(job['job_id'], 'stale', 'completed', True)
        self.assertEqual(self.store.gpus()[0]['token'], claim['owner_token'])

    def test_retry_creates_fresh_job_and_requires_terminal_same_principal(self):
        job = self.submit()
        with self.assertRaises(ValueError):
            self.store.submit(request(retry_of=job['job_id']), 'operator', 'retry')
        self.store.cancel(job['job_id'], 'operator')
        retry = self.store.submit(request(retry_of=job['job_id']), 'operator', 'retry')
        self.assertNotEqual(retry['job_id'], job['job_id'])
        self.assertEqual(retry['retry_of'], job['job_id'])

    def test_no_service_credentials_or_pythonpath_in_child_environment(self):
        job = self.submit()
        os.environ['GPU_SERVER_TOKEN'] = 'fixture-secret'
        os.environ['PYTHONPATH'] = 'fixture-injection'
        try:
            env = child_environment(job)
            self.assertNotIn('GPU_SERVER_TOKEN', env)
            self.assertNotIn('PYTHONPATH', env)
        finally:
            os.environ.pop('GPU_SERVER_TOKEN')
            os.environ.pop('PYTHONPATH')

    def test_registered_runner_preserves_measurement_flags_and_no_search(self):
        job = self.submit('qualify', profiling='auto', profile_timeline=True)
        job['selected_gpu_uuid'] = GPU1
        command = PacketBackend().runner(job, self.root, 1200)
        self.assertIn('--profile-timeline', command)
        self.assertEqual(command[command.index('--profiling') + 1], 'auto')
        self.assertEqual(command[command.index('--gpu') + 1], GPU1)
        self.assertNotIn('--check-only', command)
        self.assertFalse(any('search' in x or 'jit' in x for x in command))


class RegistrationTests(unittest.TestCase):
    def fixture(self, directory):
        packet = suite_fixtures.Controls().fixture(directory)
        tools = packet / 'tools'
        tools.mkdir()
        for name in ('run_gpu_test_suite.py', 'package_gpu_test_suite_results.py', 'validate_gpu_test_suite.py'):
            (tools / name).write_text('# static registration fixture only\n')
        suite_fixtures.catalog(packet)
        archive = directory / 'source.tar.gz'
        with tarfile.open(archive, 'w:gz') as tar:
            for path in sorted(packet.rglob('*')):
                if path.is_file():tar.add(path, arcname='packet/' + path.relative_to(packet).as_posix(), recursive=False)
        return packet, archive, hashlib.sha256(archive.read_bytes()).hexdigest()

    def test_source_registration_authenticates_transport_and_installed_file_sets(self):
        with tempfile.TemporaryDirectory() as directory:
            packet, archive, digest = self.fixture(Path(directory))
            record = authenticate_registration(packet, archive, digest)
            self.assertEqual(record['packet_id'], (packet / 'PACKET_ID').read_text().strip())
            with self.assertRaises(ValueError):authenticate_registration(packet, archive, '0' * 64)
            (packet / 'tools/unlisted.py').write_text('extra')
            with self.assertRaises(ValueError):authenticate_registration(packet, archive, digest)

    def test_source_archive_cannot_differ_from_installed_packet(self):
        with tempfile.TemporaryDirectory() as directory:
            packet, archive, digest = self.fixture(Path(directory))
            (packet / 'tools/run_gpu_test_suite.py').write_text('different source')
            suite_fixtures.catalog(packet)
            with self.assertRaises(ValueError):authenticate_registration(packet, archive, digest)

    def test_launch_gate_eof_never_executes_target(self):
        with tempfile.TemporaryDirectory() as directory:
            marker = Path(directory) / 'not-created'
            read_fd, write_fd = os.pipe()
            process = subprocess.Popen([sys.executable, str(HERE / 'gpu_test_worker.py'), '_child', str(read_fd),
                sys.executable, '-c', 'from pathlib import Path;Path(__import__("sys").argv[1]).write_text("wrong")', str(marker)],
                pass_fds=(read_fd,), env=dict(os.environ, PYTHONDONTWRITEBYTECODE='1'))
            os.close(read_fd)
            os.close(write_fd)
            self.assertEqual(process.wait(timeout=8), 125)
            self.assertFalse(marker.exists())


if __name__ == '__main__':
    unittest.main()
