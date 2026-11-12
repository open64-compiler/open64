"""HTTP server contracts, independent of the later remote-client module."""
from concurrent.futures import ThreadPoolExecutor
import hashlib,json,threading,unittest
from urllib.request import Request,urlopen
from urllib.error import HTTPError
from gpu_test_http import Service,Server,APIError
import test_gpu_test_worker as fixtures
TOKEN='Server_fixture_0123456789ABCDEFGHIJKLMNOP'
class ServerTests(unittest.TestCase):
    def setUp(self):
        self.fixture=fixtures.WorkerTests();self.fixture.setUp()
        self.store=self.fixture.store
        self.service=Service(self.store,[dict(principal='owner',sha256=hashlib.sha256(TOKEN.encode()).hexdigest())],quota=2,max_events=2)
        self.server=Server(('127.0.0.1',0),self.service)
        self.thread=threading.Thread(target=lambda:self.server.serve_forever(poll_interval=.02),daemon=True);self.thread.start()
        self.url='http://127.0.0.1:'+str(self.server.server_port)
    def tearDown(self):
        self.server.shutdown();self.server.server_close();self.thread.join();self.fixture.tearDown()
    def call(self,method,path,value=None,key=None,token=TOKEN):
        headers={'Authorization':'Bearer '+token,'Content-Type':'application/json'}
        if key:headers['Idempotency-Key']=key
        data=None if value is None else json.dumps(value).encode()
        try:
            with urlopen(Request(self.url+path,data=data,headers=headers,method=method),timeout=5) as r:return r.status,json.loads(r.read())
        except HTTPError as e:return e.code,json.loads(e.read())
    def test_auth_and_no_private_state(self):
        self.assertEqual(self.call('GET','/v1/capabilities',token='wrong')[0],401)
        code,job=self.call('POST','/v1/jobs',fixtures.request(),key='one')
        self.assertEqual(code,202);self.assertNotIn('owner_token',job);self.assertNotIn('path',job['packet'])
    def test_concurrent_same_key_has_one_creation(self):
        def submit(_):return self.call('POST','/v1/jobs',fixtures.request(),key='same')
        with ThreadPoolExecutor(max_workers=4) as pool:rows=list(pool.map(submit,range(4)))
        self.assertEqual(sum(x[0]==202 for x in rows),1);self.assertEqual(len({x[1]['job_id'] for x in rows}),1)
    def test_quota_and_replay(self):
        self.call('POST','/v1/jobs',fixtures.request(),key='one');self.call('POST','/v1/jobs',fixtures.request(),key='two')
        self.assertEqual(self.call('POST','/v1/jobs',fixtures.request(),key='three')[0],429)
        self.assertEqual(self.call('POST','/v1/jobs',fixtures.request(),key='one')[0],200)
    def test_request_conflict_and_unknown_fields(self):
        self.call('POST','/v1/jobs',fixtures.request(),key='same')
        self.assertEqual(self.call('POST','/v1/jobs',fixtures.request(samples=21),key='same')[0],409)
        self.assertEqual(self.call('POST','/v1/jobs',fixtures.request(extra='shell'),key='bad')[0],400)
    def test_ownership_and_idempotent_queued_cancel(self):
        _,job=self.call('POST','/v1/jobs',fixtures.request(),key='one');path='/v1/jobs/'+job['job_id']
        with self.assertRaises(APIError):self.service.owned(job['job_id'],'other')
        self.assertEqual(self.call('POST',path+'/cancel',{})[1]['state'],'cancelled')
        self.assertEqual(self.call('POST',path+'/cancel',{})[0],200)
    def test_bounded_event_page_and_future_cursor(self):
        _,job=self.call('POST','/v1/jobs',fixtures.request(),key='one');path='/v1/jobs/'+job['job_id']+'/events'
        self.assertEqual(self.call('GET',path+'?limit=1')[1]['next_after'],1)
        self.assertEqual(self.call('GET',path+'?limit=3')[0],400)
        self.assertEqual(self.call('GET',path+'?after=999&limit=2')[0],409)
    def test_server_identity_is_persistent(self):
        self.assertEqual(Service(self.store,self.service.tokens).server_id,self.service.server_id)
