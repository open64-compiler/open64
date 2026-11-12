"""Real loopback HTTP/client controls; synthetic workers never execute GPU code."""
from concurrent.futures import ThreadPoolExecutor
import hashlib
import io
import json
import os
import subprocess
import sys
from pathlib import Path
import tarfile
import threading
import unittest
from unittest.mock import patch
from urllib.error import HTTPError
from urllib.request import Request, urlopen

from gpu_test_http import Server, Service
from gpu_test_client import Client, ClientError, unpack_source, review, main as client_main
from gpu_test_worker import Worker, authenticate_registration
from gpu_test_worker_state import Store
import test_gpu_test_worker as fixtures

HERE=Path(__file__).resolve().parent

TOKEN='GS2_fixture_token_0123456789ABCDEFGHIJKLMN'
OTHER='GS2_second_token_0123456789ABCDEFGHIJKLMN'


class HTTPTests(unittest.TestCase):
    def setUp(self):
        self.fixture=fixtures.WorkerTests();self.fixture.setUp()
        self.root,self.store=self.fixture.root,self.fixture.store
        self.tokens=[dict(principal=name,sha256=hashlib.sha256(token.encode()).hexdigest()) for name,token in [('one',TOKEN),('two',OTHER)]]
        self.service=Service(self.store,self.tokens,quota=2,max_events=2)
        self.server=Server(('127.0.0.1',0),self.service)
        self.thread=threading.Thread(target=lambda:self.server.serve_forever(poll_interval=.02),daemon=True);self.thread.start()
        self.url='http://127.0.0.1:'+str(self.server.server_port)
        self.token_file=self.root/'token';self.token_file.write_text(TOKEN);self.token_file.chmod(0o600)
        self.client=Client(self.url,self.token_file,attempts=1);self.client.discover()
        self.workers=[]

    def tearDown(self):
        for job,thread in self.workers:
            if thread.is_alive():
                self.store.cancel(job['job_id'],job['principal']);thread.join(timeout=8)
        self.server.shutdown();self.server.server_close();self.thread.join(timeout=3)
        self.fixture.tearDown()

    def raw(self,method,path,data=None,token=TOKEN,key=None):
        headers={'Content-Type':'application/json'}
        if token is not None:headers['Authorization']='Bearer '+token
        if key is not None:headers['Idempotency-Key']=key
        if data is not None and not isinstance(data,bytes):data=json.dumps(data).encode()
        try:
            with urlopen(Request(self.url+path,data=data,headers=headers,method=method),timeout=10) as r:return r.status,json.loads(r.read())
        except HTTPError as error:return error.code,json.loads(error.read())

    def submit(self,key='one',**changes):
        status,value=self.raw('POST','/v1/jobs',fixtures.request(**changes),key=key)
        self.assertIn(status,(200,202),value);return value

    def start_worker(self,job):
        thread=threading.Thread(target=lambda:Worker(self.store,fixtures.FakeBackend(),fixtures.FakeDevice(),.02).run_once(job['job_id']))
        self.workers.append((job,thread));thread.start();return thread

    def test_auth_and_principal_ownership(self):
        self.assertEqual(self.raw('GET','/v1/capabilities',token=None)[0],401)
        self.assertEqual(self.raw('GET','/v1/capabilities',token='invalid')[0],401)
        job=self.submit()
        for method,suffix,data in [('GET','',None),('GET','/events',None),('POST','/cancel',{})]:
            self.assertEqual(self.raw(method,'/v1/jobs/'+job['job_id']+suffix,data,token=OTHER)[0],404)
        self.assertEqual(self.store.get(job['job_id'])['state'],'queued')
        self.assertNotIn('owner_token',job);self.assertNotIn('path',job['packet']);self.assertNotIn(TOKEN,json.dumps(job))

    def test_concurrent_idempotency_and_atomic_quota(self):
        def submit(_):return self.raw('POST','/v1/jobs',fixtures.request(),key='same')
        with ThreadPoolExecutor(max_workers=4) as pool:results=list(pool.map(submit,range(4)))
        self.assertEqual(sum(code==202 for code,_ in results),1)
        self.assertEqual(len({job['job_id'] for _,job in results}),1)
        self.assertEqual(self.raw('POST','/v1/jobs',fixtures.request(samples=21),key='same')[0],409)
        self.submit(key='second')
        self.assertEqual(self.raw('POST','/v1/jobs',fixtures.request(),key='third')[0],429)
        self.assertEqual(self.raw('POST','/v1/jobs',fixtures.request(),key='same')[0],200)

    def test_invalid_requests_create_no_job(self):
        self.assertEqual(self.raw('POST','/v1/jobs',fixtures.request())[0],400)
        self.assertEqual(self.raw('POST','/v1/jobs',fixtures.request(extra='shell'),key='bad')[0],400)
        self.assertEqual(self.raw('POST','/v1/jobs',b'{"schema":1,"schema":2}',key='bad')[0],400)
        self.assertEqual(self.raw('POST','/v1/jobs',fixtures.request(packet_id='b'*64),key='missing')[0],422)
        with self.store.connect() as db:self.assertEqual(db.execute('SELECT COUNT(*) FROM jobs').fetchone()[0],0)

    def test_concurrent_distinct_requests_cannot_exceed_quota(self):
        def submit(index):return self.raw('POST','/v1/jobs',fixtures.request(),key='key-'+str(index))[0]
        with ThreadPoolExecutor(max_workers=4) as pool:codes=list(pool.map(submit,range(4)))
        self.assertEqual(codes.count(202),2);self.assertEqual(codes.count(429),2)

    def test_bounded_persistent_events_and_bad_cursors(self):
        job=self.submit();self.start_worker(job).join(timeout=8)
        cursor=0;seen=[]
        while True:
            status,page=self.raw('GET','/v1/jobs/'+job['job_id']+'/events?after='+str(cursor)+'&limit=2')
            self.assertEqual(status,200);self.assertLessEqual(len(page['events']),2)
            seen.extend(page['events']);cursor=page['next_after']
            if not page['has_more']:break
        self.assertEqual([x['sequence'] for x in seen],list(range(1,len(seen)+1)))
        for query in ('after=-1','limit=3','after=0&after=1','after=999999'):
            self.assertIn(self.raw('GET','/v1/jobs/'+job['job_id']+'/events?'+query)[0],(400,409))
        with self.store.transaction() as db:db.execute('DELETE FROM events WHERE job=? AND seq=1',(job['job_id'],))
        self.assertEqual(self.raw('GET','/v1/jobs/'+job['job_id']+'/events?after=0&limit=2')[1]['code'],'EVENT_CURSOR_EXPIRED')

    def test_identity_survives_restart(self):
        job=self.submit();reopened=Service(Store(self.store.root),self.tokens)
        self.assertEqual(reopened.server_id,self.service.server_id)
        self.assertEqual(reopened.owned(job['job_id'],'one')['state'],'queued')

    def test_queued_and_active_cancellation_collects(self):
        first=self.submit();self.assertEqual(self.raw('POST','/v1/jobs/'+first['job_id']+'/cancel',{})[1]['state'],'cancelled')
        self.fixture.controls(wait=True);second=self.submit(key='second');thread=self.start_worker(second)
        self.fixture.wait_ready(second['job_id'])
        self.assertEqual(self.raw('POST','/v1/jobs/'+second['job_id']+'/cancel',{})[0],202)
        thread.join(timeout=8);self.assertFalse(thread.is_alive())
        job=self.client.job(second['job_id'])
        self.assertEqual(job['state'],'cancelled');self.assertEqual(job['collection_status'],'complete');self.assertEqual(job['runner_exit_code'],143)

    def test_artifact_ownership_and_corruption(self):
        job=self.submit();self.start_worker(job).join(timeout=8);job=self.client.job(job['job_id'])
        row=next(x for x in job['artifacts'] if x['name']=='review.json')
        target=self.root/'download/review.json';self.client.download(row,target)
        self.assertEqual(hashlib.sha256(target.read_bytes()).hexdigest(),row['sha256'])
        self.assertEqual(self.raw('GET',row['download_url'],token=OTHER)[0],404)
        stored=next(x for x in self.store.get(job['job_id'])['artifacts'] if x['artifact_id']==row['artifact_id'])
        path=Path(stored['path']);path.chmod(0o600);path.write_text('tampered')
        self.assertEqual(self.raw('GET',row['download_url'])[0],409)

    def test_saved_key_recovers_lost_acceptance_response(self):
        output=self.root/'client';output.mkdir();original=self.client.json;lost=[True]
        def uncertain(method,endpoint,*args,**kwargs):
            value=original(method,endpoint,*args,**kwargs)
            if method=='POST' and endpoint=='/v1/jobs' and lost[0]:lost[0]=False;raise ClientError('synthetic lost response')
            return value
        with patch.object(self.client,'json',side_effect=uncertain):
            with self.assertRaises(ClientError):self.client.submit(fixtures.request(),output)
        self.assertIsNone(json.loads((output/'submission.json').read_text())['job_id'])
        resumed=self.client.submit(fixtures.request(),output)
        self.assertEqual(resumed['job_id'],json.loads((output/'submission.json').read_text())['job_id'])
        with self.store.connect() as db:self.assertEqual(db.execute('SELECT COUNT(*) FROM jobs').fetchone()[0],1)
        with self.assertRaises(ClientError):self.client.submit(fixtures.request(samples=21),output)

    def test_watch_resume_never_submits(self):
        output=self.root/'watch';output.mkdir();job=self.submit()
        with self.assertRaises(ClientError):self.client.watch(job['job_id'],output,wait_timeout=.04,poll_seconds=.02,progress=False)
        self.start_worker(job).join(timeout=8)
        self.assertEqual(self.client.watch(job['job_id'],output,wait_timeout=3,poll_seconds=.02,progress=False)['state'],'completed')
        with self.store.connect() as db:self.assertEqual(db.execute('SELECT COUNT(*) FROM jobs').fetchone()[0],1)

    def test_output_is_bound_to_principal(self):
        output=self.root/'bound';output.mkdir();self.client.bind(output)
        other=self.root/'other-token';other.write_text(OTHER);other.chmod(0o600)
        client=Client(self.url,other);client.discover()
        with self.assertRaises(ClientError):client.bind(output)

    def test_redirects_are_not_followed(self):
        from http.server import BaseHTTPRequestHandler,HTTPServer
        seen=[];destination=self.url
        class Redirect(BaseHTTPRequestHandler):
            def do_GET(handler):
                seen.append(handler.path);handler.send_response(302);handler.send_header('Location',destination+'/v1/capabilities');handler.end_headers()
            def log_message(*args):pass
        server=HTTPServer(('127.0.0.1',0),Redirect);thread=threading.Thread(target=server.serve_forever,daemon=True);thread.start()
        try:
            client=Client('http://127.0.0.1:'+str(server.server_port),self.token_file,attempts=1)
            with self.assertRaises(ClientError):client.discover()
            self.assertEqual(seen,['/v1/capabilities'])
        finally:server.shutdown();server.server_close();thread.join()

    def test_bad_download_is_not_published_or_used_to_replace_evidence(self):
        row=dict(name='result.bin',size_bytes=3,sha256=hashlib.sha256(b'abc').hexdigest(),artifact_id='artifact_'+'1'*32,
            download_url='/v1/artifacts/artifact_'+'1'*32)
        class Response(io.BytesIO):headers={'Content-Length':'3','X-Content-SHA256':row['sha256']}
        target=self.root/'result.bin'
        with patch.object(self.client,'open',return_value=Response(b'bad')):
            with self.assertRaises(ClientError):self.client.download(row,target)
        self.assertFalse(target.exists());target.write_bytes(b'keep')
        with self.assertRaises(ClientError):self.client.download(row,target)
        self.assertEqual(target.read_bytes(),b'keep')

    def test_source_download_extracts_as_data(self):
        directory=self.root/'source-fixture';directory.mkdir()
        packet,archive,digest=fixtures.RegistrationTests().fixture(directory)
        record=authenticate_registration(packet,archive,digest);self.store.register_packet(record)
        packets=self.client.json('GET','/v1/packets')['packets'];value=next(x for x in packets if x['packet_id']==record['packet_id'])
        saved=self.client.download(value['source_artifacts'][0],self.root/'source.tar.gz')
        parent=self.root/'unpacked';parent.mkdir();source=unpack_source(saved,parent,record['packet_id'],digest)
        self.assertEqual((source/'PACKET_ID').read_text().strip(),record['packet_id'])
        self.assertFalse(os.access(source/'tools/run_gpu_test_suite.py',os.X_OK))

    def test_unsafe_tar_refused_before_extraction(self):
        path=self.root/'unsafe.tar.gz'
        with tarfile.open(path,'w:gz') as tar:
            entry=tarfile.TarInfo('../escape');entry.size=3;tar.addfile(entry,io.BytesIO(b'bad'))
        parent=self.root/'unpacked';parent.mkdir()
        with self.assertRaises(ClientError):unpack_source(path,parent,'0'*64,hashlib.sha256(path.read_bytes()).hexdigest())
        self.assertFalse(list(parent.iterdir()));self.assertFalse((self.root/'escape').exists())

    def test_source_symlink_is_rejected_before_extraction(self):
        path=self.root/'link.tar.gz'
        with tarfile.open(path,'w:gz') as tar:
            entry=tarfile.TarInfo('packet/link');entry.type=tarfile.SYMTYPE;entry.linkname='/private/tmp';tar.addfile(entry)
        parent=self.root/'unpacked';parent.mkdir()
        with self.assertRaises(ClientError):unpack_source(path,parent,'0'*64,hashlib.sha256(path.read_bytes()).hexdigest())
        self.assertFalse(list(parent.iterdir()))

    def test_offline_review_never_executes_downloaded_validator(self):
        directory=self.root/'source-fixture';directory.mkdir()
        source,archive,digest=fixtures.RegistrationTests().fixture(directory)
        marker=self.root/'must-not-execute'
        (source/'tools/validate_gpu_test_suite.py').write_text('from pathlib import Path\nPath('+repr(str(marker))+').write_text("wrong")\n')
        from test_gpu_test_suite import catalog
        catalog(source)
        packet_id=(source/'PACKET_ID').read_text().strip()
        output=self.root/'offline';(output/'artifacts').mkdir(parents=True)
        artifact=output/'artifacts/Open64_GPU_Test_results.tar.gz';artifact.write_bytes(b'invalid synthetic return')
        job=self.submit();job['packet']['packet_id']=packet_id
        value=dict(job=job,artifacts=[dict(relative_path='artifacts/'+artifact.name,
            metadata=dict(name=artifact.name,sha256=hashlib.sha256(artifact.read_bytes()).hexdigest(),size_bytes=artifact.stat().st_size))])
        (output/'fetch.json').write_text(json.dumps(value))
        result=review(output,packet_dir=source)
        self.assertEqual(result['exit_code'],3);self.assertFalse(marker.exists())

    def test_cross_principal_retry_lineage_is_inaccessible(self):
        job=self.submit()
        self.raw('POST','/v1/jobs/'+job['job_id']+'/cancel',{})
        code,_=self.raw('POST','/v1/jobs',fixtures.request(retry_of=job['job_id']),token=OTHER,key='retry')
        self.assertEqual(code,404)

    def test_dead_supervisor_worker_does_not_claim_queued_work(self):
        job=self.submit()
        parent=subprocess.Popen([sys.executable,'-c','pass']);parent.wait(timeout=5)
        result=subprocess.run([sys.executable,str(HERE/'gpu_test_http.py'),'worker','--store',str(self.store.root),
            '--slot','0','--supervisor',json.dumps(dict(pid=parent.pid,stamp='dead-supervisor'))],
            capture_output=True,text=True,timeout=8)
        self.assertEqual(result.returncode,0,result.stderr)
        self.assertEqual(self.store.get(job['job_id'])['state'],'queued')

    def test_profiling_options_remain_explicit_and_separate(self):
        self.store.register_gpu(fixtures.GPU1)
        job=self.submit(mode='qualify',profiling='auto',profile_timeline=True)
        self.assertEqual(job['request']['profiling'],'auto')
        self.assertTrue(job['request']['profile_timeline'])
        self.assertTrue(job['diagnostics']['profiling_separate'])
        self.assertEqual(job['diagnostics']['assessment'],'DEFER_TO_ORIGINAL_REPORTS')

    def test_remote_plain_http_and_public_bind_refused(self):
        with self.assertRaises(ValueError):Server(('0.0.0.0',0),self.service)
        with self.assertRaises(ClientError):Client('http://example.invalid',self.token_file)
        with self.assertRaises(ClientError):Client('http://user:secret@127.0.0.1',self.token_file)

    def test_client_output_does_not_pollute_a_source_packet(self):
        source=self.root/'immutable-source';source.mkdir()
        with patch('sys.stderr',new=io.StringIO()):
            self.assertEqual(client_main(['review','--packet-dir',str(source),'--output',str(source/'results')]),4)
        self.assertFalse(list(source.iterdir()))


if __name__=='__main__':unittest.main()
