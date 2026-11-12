"""Synthetic report-format fixtures test validation, never GPU qualification."""
import copy
import hashlib
import io
import json
from pathlib import Path
import struct
import tarfile
import tempfile
import unittest
import validate_mla_q7_results as validator


class ValidationTests(unittest.TestCase):
    def fixture(self, root):
        source=root/'source';source.mkdir();cases=[]
        for name,length in [('dense_s8_decode--async',17),('dense_s8_decode_long--async',8193)]:
            folder=source/'cases'/name;folder.mkdir(parents=True);outputs=[]
            (folder/'code.json').write_text(json.dumps(dict(stages=[dict(function=x) for x in ('Prepare','Attention','Finalize')])))
            for i in range(6):
                refs=[]
                for lane in validator.LANES:
                    dtype='float64' if i<2 and lane=='scalar' else 'float32'
                    filename='out-%d-%s.bin'%(i,lane);(folder/filename).write_bytes(struct.pack('<d' if dtype=='float64' else '<f',1.0))
                    refs.append(dict(name=lane,file=filename,dtype=dtype))
                outputs.append(dict(dtype='float32',shape=[1],rtol=2e-5 if i<2 else 0,atol=2e-6 if i<2 else 0,references=refs))
            cases.append(dict(id=name,outputs=outputs,source_geometry=dict(B=1,H=1,Q=1,L=length,A=0)))
        (source/'packet.json').write_text(json.dumps(dict(schema='open64.mla-q7.packet.v1',architecture='sm_90',ptx_isa='8.6',cases=cases)))
        manifest=''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+p.relative_to(source).as_posix()+'\n' for p in sorted(source.rglob('*')) if p.is_file())
        (source/'SHA256SUMS').write_text(manifest);identity=hashlib.sha256(manifest.encode()).hexdigest();(source/'PACKET_ID').write_text(identity+'\n')
        device=dict(name='synthetic report fixture, not an executed GPU',uuid='0123456789abcdef'*2,compute_capability=[9,0],driver_api_version=12080,platform='Linux-test-x86_64')
        def record(case,prefix,performance=False):
            item=dict(id=case['id'],prefix_bytes=prefix,status='PASS',ordered_kernels_per_request=3,source_guard_refusal=case['id']=='dense_s8_decode--async')
            item.update({x:True for x in validator.FLAGS})
            item['references']=[{lane:dict(elements=1,max_absolute_error=0,max_relative_error=0,rms_error=0) for lane in validator.LANES} for _ in range(6)]
            if performance:
                item['host_begin_submit_complete_us']=self.stats([10.,12.,11.])
                item['submission_stream_span_ms']=self.stats([.01,.03,.02])
            return item
        correct=dict(status='PASS',mode='correctness',gpu_execution=True,driver_model=False,device=device,packet_sha256=identity,authenticated_files=len(manifest.splitlines()),all_fixed_cases=True,
                     cases=[record(c,p) for c in cases for p in (256,40)])
        perf=dict(correct,mode='performance',cases=[record(c,256,True) for c in cases],timing_scope='submission stream includes host gaps; NOT pure kernel duration')
        base='results/03-qualification';files={}
        def put(name,value):files[name]=json.dumps(value).encode()
        records=dict(correctness=copy.deepcopy(correct),performance=copy.deepcopy(perf));commands=[]
        def command(label,mode,prefix=None):
            args=(prefix or [])+['python3','run_mla_q7.py','--packet','/packet','--expected-sha256',identity,'--mode',mode]
            if label=='performance':args+=['--iterations','3']
            commands.append(dict(label=label,argv=args,returncode=0))
        put(base+'/correctness/report.json',correct);put(base+'/performance/report.json',perf)
        command('correctness','correctness');command('performance','performance')
        commands += [dict(label='sanitizer-help',argv=['compute-sanitizer','--help'],returncode=0),dict(label='sanitizer-version',argv=['compute-sanitizer','--version'],returncode=0)]
        flags=['check-bulk-copy','error-exitcode','tool','leak-check','racecheck-memcpy-async','racecheck-fence-proxy-async','racecheck-detect-level','racecheck-report','initcheck-address-space']
        files[base+'/sanitizer-help.stdout']=' '.join(flags).encode();files[base+'/sanitizer-help.stderr']=b''
        files[base+'/sanitizer-version.stdout']=b'Synthetic sanitizer version fixture, not an executed tool';files[base+'/sanitizer-version.stderr']=b''
        for tool in validator.TOOLS:
            put(base+'/'+tool+'/report.json',correct)
            records[tool]=dict(status='PASS',tool=tool,clean_summaries=True,actual_GPU_child=True)
            args=['compute-sanitizer','--tool',tool,'--error-exitcode','86','--check-bulk-copy','yes']
            if tool=='memcheck':args+=['--leak-check','full']
            if tool=='racecheck':args+=['--racecheck-memcpy-async','yes','--racecheck-fence-proxy-async','yes','--racecheck-detect-level','info','--racecheck-report','all']
            if tool=='initcheck':args+=['--initcheck-address-space','all']
            command(tool,'correctness',args)
            files[base+'/'+tool+'.stdout']=b'COMPUTE-SANITIZER\n'
            files[base+'/'+tool+'.stderr']=(b'RACECHECK SUMMARY: 0 hazards displayed (0 errors, 0 warnings)\n' if tool=='racecheck' else b'ERROR SUMMARY: 0 errors\n')
        put(base+'/report.json',dict(status='PASS',mode='all',gpu_execution=True,race_qualified=True,packet_sha256=identity,records=records))
        put(base+'/commands.json',commands)
        put('bundle.json',dict(schema='open64.mla-q7.return-bundle.v1',packet_sha256=identity,collection_notes=[]))
        put('environment/capture.json',dict(system='Linux',machine='x86_64',commands=[dict(label='nvcc',status='CAPTURED')]))
        files['environment/nvcc.stdout']=b'Cuda compilation tools, release 12.8, V12.8.93\n'
        files['environment/nvidia-smi-query.stdout']=b'Driver Version: 570.00 CUDA Version: 13.0\n'
        files['packet/SHA256SUMS']=manifest.encode();files['packet/packet.json']=(source/'packet.json').read_bytes();files['packet/PACKET_ID']=(identity+'\n').encode()
        return source,identity,files,base

    @staticmethod
    def stats(samples):
        ordered=sorted(samples);mean=sum(samples)/len(samples)
        return dict(samples=samples,count=3,minimum=min(samples),median=ordered[1],p95=ordered[1]+.9*(ordered[2]-ordered[1]),mean=mean,
                    standard_deviation=(sum((x-mean)**2 for x in samples)/len(samples))**.5)

    def archive(self, root, files, extra=None, bad_manifest=False):
        archive=root/'returned.tar.gz';manifest=''.join(hashlib.sha256(data).hexdigest()+'  '+name+'\n' for name,data in sorted(files.items()))
        if bad_manifest:manifest=manifest.replace(next(iter(hashlib.sha256(x).hexdigest() for x in files.values())),'0'*64)
        with tarfile.open(archive,'w:gz') as tar:
            for name,data in list(sorted(files.items()))+[('SHA256SUMS',manifest.encode())]:
                info=tarfile.TarInfo(validator.ROOT+name);info.size=len(data);tar.addfile(info,io.BytesIO(data))
            if extra:
                info,data=extra;tar.addfile(info,io.BytesIO(data) if data is not None else None)
        (root/'returned.tar.gz.sha256').write_text(validator.digest_file(archive)+'  returned.tar.gz\n')
        return archive

    def review(self, root, source, identity, files, extra_args=None, **archive_args):
        archive=self.archive(root,files,**archive_args);out=root/'review'
        code=validator.main(['--archive',str(archive),'--packet',str(source),'--expected-packet-sha256',identity,'--output',str(out)]+(extra_args or []))
        return code,json.loads((out/'summary.json').read_text())

    def rewrite(self, files, name, function):
        value=json.loads(files[name]);function(value);files[name]=json.dumps(value).encode()

    def sync_parent(self, files, base):
        def update(parent):
            for name in ('correctness','performance'):parent['records'][name]=json.loads(files[base+'/'+name+'/report.json'])
        self.rewrite(files,base+'/report.json',update)

    def test_complete_consistent_reported_evidence_and_recomputed_timings(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);source,identity,files,base=self.fixture(root);code,report=self.review(root,source,identity,files)
            self.assertEqual(code,0);self.assertEqual(report['overall'],'PASS_REPORTED')
            self.assertEqual(report['sections']['racecheck']['status'],'PASS_REPORTED');self.assertEqual(len(report['performance']),2)
            self.assertEqual(report['performance'][0]['host_us']['median'],11.)
            self.assertEqual(report['compatibility']['installed_toolkit_release'],'12.8')
            self.assertEqual(report['compatibility']['driver_cuda_capability'],'13.0')
            self.assertEqual(report['gpu_workloads_launched'],0)

    def test_missing_sanitizer_log_is_incomplete_despite_parent_pass(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);source,identity,files,base=self.fixture(root);del files[base+'/racecheck.stderr']
            code,report=self.review(root,source,identity,files)
            self.assertEqual(code,2);self.assertEqual(report['sections']['racecheck']['status'],'INCOMPLETE')

    def test_hazards_overflow_and_missing_proxy_coverage(self):
        for label,code in [('hazard',1),('overflow',2),('proxy',2)]:
            with self.subTest(label=label),tempfile.TemporaryDirectory() as folder:
                root=Path(folder);source,identity,files,base=self.fixture(root)
                if label=='hazard':files[base+'/racecheck.stderr']=b'RACECHECK SUMMARY: 1 hazard displayed (1 error, 0 warnings)'
                elif label=='overflow':files[base+'/racecheck.stderr']+=b'barrier tracking overflow'
                else:
                    def remove(rows):
                        for row in rows:
                            if row['label']=='racecheck':
                                i=row['argv'].index('--racecheck-fence-proxy-async');del row['argv'][i:i+2]
                    self.rewrite(files,base+'/commands.json',remove)
                status,report=self.review(root,source,identity,files);self.assertEqual(status,code)

    def test_gpu_driver_platform_and_mixed_device_issues(self):
        for field,value in [('compute_capability',[8,0]),('driver_api_version',11070),('platform','Linux-aarch64'),('uuid','f'*32)]:
            with self.subTest(field=field),tempfile.TemporaryDirectory() as folder:
                root=Path(folder);source,identity,files,base=self.fixture(root)
                self.rewrite(files,base+'/performance/report.json',lambda r:r['device'].update({field:value}));self.sync_parent(files,base)
                code,report=self.review(root,source,identity,files)
                self.assertEqual(code,1);self.assertEqual(report['compatibility']['status'],'INCOMPATIBLE')

    def test_bad_reference_errors_cannot_be_hidden_by_pass(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);source,identity,files,base=self.fixture(root)
            self.rewrite(files,base+'/correctness/report.json',lambda r:r['cases'][0]['references'][2]['scalar'].update(max_absolute_error=.01))
            self.sync_parent(files,base);code,report=self.review(root,source,identity,files)
            self.assertEqual(code,1);self.assertIn('REFERENCE_ERROR_OUT_OF_BOUNDS',{x['code'] for x in report['findings']})

    def test_timing_tamper_missing_samples_and_instrumented_baseline(self):
        for label,expected in [('median',1),('samples',2),('profiled',1),('nan',1)]:
            with self.subTest(label=label),tempfile.TemporaryDirectory() as folder:
                root=Path(folder);source,identity,files,base=self.fixture(root)
                if label=='profiled':
                    def instrument(rows):
                        for row in rows:
                            if row['label']=='performance':row['argv'].insert(0,'ncu')
                    self.rewrite(files,base+'/commands.json',instrument)
                else:
                    def change(report):
                        metric=report['cases'][0]['host_begin_submit_complete_us']
                        if label=='median':metric['median']=100
                        elif label=='samples':metric.pop('samples')
                        else:metric['samples'][0]=float('nan')
                    self.rewrite(files,base+'/performance/report.json',change);self.sync_parent(files,base)
                code,report=self.review(root,source,identity,files);self.assertEqual(code,expected)

    def test_case_residue_coverage_is_checked_independently(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);source,identity,files,base=self.fixture(root)
            for name in ('correctness',)+validator.TOOLS:
                self.rewrite(files,base+'/'+name+'/report.json',lambda r:r['cases'].pop())
            self.sync_parent(files,base);code,report=self.review(root,source,identity,files)
            self.assertEqual(code,2);self.assertIn('CASE_COVERAGE',{x['code'] for x in report['findings']})

    def test_source_mismatch_corrupt_file_and_missing_transport_checksum(self):
        for label in ('identity','inner','outer','not-tar','truncated'):
            with self.subTest(label=label),tempfile.TemporaryDirectory() as folder:
                root=Path(folder);source,identity,files,base=self.fixture(root)
                if label=='identity':self.rewrite(files,'bundle.json',lambda b:b.update(packet_sha256='0'*64))
                archive=self.archive(root,files,bad_manifest=label=='inner')
                if label=='outer':Path(str(archive)+'.sha256').write_text('0'*64+'  returned.tar.gz\n')
                if label in ('not-tar','truncated'):
                    archive.write_bytes(b'not a tar' if label=='not-tar' else archive.read_bytes()[:archive.stat().st_size//2])
                    Path(str(archive)+'.sha256').write_text(validator.digest_file(archive)+'  returned.tar.gz\n')
                code=validator.main(['--archive',str(archive),'--packet',str(source),'--expected-packet-sha256',identity,'--output',str(root/'review')])
                self.assertEqual(code,3)

    def test_unsafe_duplicate_and_special_members_never_extract(self):
        for label in ('traversal','absolute','symlink','duplicate'):
            with self.subTest(label=label),tempfile.TemporaryDirectory() as folder:
                root=Path(folder);source,identity,files,base=self.fixture(root)
                name={'traversal':validator.ROOT+'../escape','absolute':'/tmp/escape','symlink':validator.ROOT+'link','duplicate':validator.ROOT+'bundle.json'}[label]
                info=tarfile.TarInfo(name);data=b'x';info.size=1
                if label=='symlink':info.type=tarfile.SYMTYPE;info.linkname='/tmp/escape';info.size=0;data=None
                code,report=self.review(root,source,identity,files,extra=(info,data));self.assertEqual(code,3)
                self.assertFalse((root/'escape').exists())

    def test_archive_resource_budgets(self):
        for option in ('--max-members','--max-expanded-bytes'):
            with self.subTest(option=option),tempfile.TemporaryDirectory() as folder:
                root=Path(folder);source,identity,files,base=self.fixture(root)
                code,report=self.review(root,source,identity,files,extra_args=[option,'1']);self.assertEqual(code,3)

    def test_multiple_runs_require_explicit_selection(self):
        for selected in (False,True):
            with self.subTest(selected=selected),tempfile.TemporaryDirectory() as folder:
                root=Path(folder);source,identity,files,base=self.fixture(root)
                files.update({name.replace(base,'results/04-other'):data for name,data in list(files.items()) if name.startswith(base+'/')})
                code,report=self.review(root,source,identity,files,extra_args=['--run',base] if selected else [])
                self.assertEqual(code,0 if selected else 2)

    def test_actual_protocol_unavailable_is_incomplete_not_a_pass(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);source,identity,files,base=self.fixture(root)
            files={name:data for name,data in files.items() if not name.startswith(base+'/')}
            unavailable=dict(status='GPU_UNAVAILABLE',gpu_execution=False,reason='no Driver')
            files[base+'/report.json']=json.dumps(unavailable).encode();files[base+'/correctness/report.json']=json.dumps(unavailable).encode()
            files[base+'/commands.json']=json.dumps([dict(label='correctness',argv=['python3','run_mla_q7.py','--mode','correctness','--expected-sha256',identity],returncode=77)]).encode()
            code,report=self.review(root,source,identity,files)
            self.assertEqual(code,2);self.assertEqual(report['compatibility']['status'],'NOT_VERIFIED')

    def test_standalone_performance_is_summarized_without_closing_full_gate(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);source,identity,files,base=self.fixture(root);perf=files[base+'/performance/report.json']
            files={name:data for name,data in files.items() if not name.startswith(base+'/')};files['results/03-performance/report.json']=perf
            code,report=self.review(root,source,identity,files)
            self.assertEqual(code,2);self.assertEqual(len(report['performance']),2)


if __name__=='__main__':unittest.main()
