"""Durability and lease invariants before introducing worker subprocesses."""
from pathlib import Path
import tempfile
import unittest
from gpu_test_worker_state import Store

GPU='GPU-00000000-0000-0000-0000-000000000001'
def request(mode='check-only'):
    return dict(schema='open64.gpu-test.job-request.v1',packet_id='a'*64,profile='combined',
        mode=mode,retry_of=None,target_gpu_uuid=None,samples=20,warmup=3,
        profiling='off',profile_timeline=False,execution_timeout_seconds=10)
class State(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.root=Path(self.temp.name)
        self.store=Store(self.root/'store')
        self.store.register_packet(dict(packet_id='a'*64,path=str(self.root/'packet')))
    def tearDown(self):self.temp.cleanup()
    def test_idempotent_request_survives_reopen(self):
        job=self.store.submit(request(),'owner','key')
        self.assertEqual(Store(self.store.root).submit(request(),'owner','key')['job_id'],job['job_id'])
        changed=request();changed['samples']=21
        with self.assertRaises(ValueError):self.store.submit(changed,'owner','key')
    def test_principal_cannot_cancel_another_job(self):
        job=self.store.submit(request(),'owner','key')
        with self.assertRaises(KeyError):self.store.cancel(job['job_id'],'other')
    def test_one_lease_and_no_registration_reset(self):
        self.store.register_gpu(GPU)
        one=self.store.submit(request('qualify'),'owner','one')
        two=self.store.submit(request('qualify'),'owner','two')
        claim=self.store.claim(dict(pid=1,stamp='synthetic'))
        self.assertEqual(claim['job_id'],one['job_id']);self.assertIsNone(self.store.claim(dict(pid=2,stamp='synthetic')))
        self.store.register_gpu(GPU);self.assertEqual(self.store.gpus()[0]['state'],'leased')
        with self.assertRaises(ValueError):self.store.finish(one['job_id'],'stale','completed',True)
        self.store.finish(one['job_id'],claim['owner_token'],'failed',False)
        self.assertEqual(self.store.gpus()[0]['state'],'quarantined')
    def test_cancelled_queue_never_claimed(self):
        job=self.store.submit(request(),'owner','key');self.store.cancel(job['job_id'],'owner')
        self.assertIsNone(self.store.claim(dict(pid=1,stamp='synthetic')))
    def test_cpu_claim_never_leases_a_gpu(self):
        self.store.register_gpu(GPU);self.store.submit(request(),'owner','key')
        self.assertIsNone(self.store.claim(dict(pid=1,stamp='synthetic'))['selected_gpu_uuid'])
        self.assertEqual(self.store.gpus()[0]['state'],'free')
