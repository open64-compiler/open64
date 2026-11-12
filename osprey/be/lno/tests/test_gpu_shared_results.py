"""Shared-format synthetic fixtures; no GPU execution or performance evidence."""
import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import shutil
import tempfile
import unittest
import gpu_test_results as shared
import validate_gpu_results as validator
import test_mla_q7_validation as q7_tests

HERE=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('shared_scaffold_data',HERE/'gpu_scaffold/server/data.py')
data=importlib.util.module_from_spec(spec);spec.loader.exec_module(data)


def identity(items):
    digest=hashlib.sha256()
    for name,content in sorted(items):digest.update(name.encode()+b'\0'+content+b'\0')
    return digest.hexdigest()


class SharedTests(unittest.TestCase):
    def scaffold(self,root,kind='mla'):
        packet=root/'packet';packet.mkdir()
        shutil.copytree(HERE/'gpu_scaffold/submissions',packet/'submissions')
        shutil.copytree(HERE/'gpu_scaffold/server/include',packet/'server/include')
        corpus=data.generate(kind,packet/'data');index=data.verify(corpus)
        folder=packet/'submissions'/kind
        files=[(p.relative_to(folder).as_posix(),p.read_bytes()) for p in folder.rglob('*') if p.is_file()]
        native=dict(schema_version=1,submission_id=identity(files),workload_id=index['workload_id'],suite_id=index['suite_id'],
            corpus_sha256=data.digest(corpus/'index.json'),baseline_id=identity([('baseline.cu',(folder/'baseline.cu').read_bytes()),
                ('server/workload.h',(packet/'server/include'/kind/'workload.h').read_bytes())]),
            status='completed',environment=dict(gpu_uuid='01'*16,same_device_and_session=True,sm=90,driver=12080,cuda_runtime=12080),session_id='synthetic-session',
            policy=dict(samples=3,warmup=0,min_improvement=.02,noise_multiplier=3,atol=.003,rtol=.02,flops=index['flops']),
            decision='accepted',reason=None,cases=[])
        for item in index['cases']:
            meta=json.loads((corpus/item['case_id']/'case.json').read_text())
            case=dict(case_id=item['case_id'],status='passed',useful_flops=meta['useful_flops'],
                correctness=dict(baseline_pass=True,candidate_pass=True),wall_time_ratio=.75)
            for role,wall,device in [('baseline',[98.,100.,102.],[78.,80.,82.]),('candidate',[74.,75.,76.],[58.,60.,62.])]:
                case[role]=dict(wall_us=shared.stats(wall)['median'],device_us=shared.stats(device)['median'],
                    wall_mad_us=shared.stats(wall)['mad'],samples=3,workspace_bytes=0,wall_samples_us=wall,device_samples_us=device)
            native['cases'].append(case)
        manifest=''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+p.relative_to(packet).as_posix()+'\n' for p in sorted(packet.rglob('*')) if p.is_file())
        (packet/'SHA256SUMS').write_text(manifest);expected=hashlib.sha256(manifest.encode()).hexdigest()
        (packet/'PACKET_ID').write_text(expected+'\n')
        return packet,expected,native

    def review(self,root,packet,expected,result):
        path=root/'result.json';path.write_text(json.dumps(result))
        code=validator.main(['--result',str(path),'--packet',str(packet),'--expected-packet-sha256',expected,'--output',str(root/'review')])
        return code,json.loads((root/'review/summary.json').read_text())

    def test_both_scaffold_profiles_use_the_same_validator_and_recompute_ratio(self):
        for kind in ('gemm','mla'):
            with self.subTest(kind=kind),tempfile.TemporaryDirectory() as directory:
                root=Path(directory);packet,expected,native=self.scaffold(root,kind)
                result=shared.from_scaffold(native);code,report=self.review(root,packet,expected,result)
                self.assertEqual(code,0,report);self.assertTrue(all(x['wall_time_ratio']['value']==.75 for x in result['cases']))

    def test_reciprocal_profiled_timing_and_forged_statistics_fail(self):
        for which in ('ratio','profiled','summary'):
            with self.subTest(which=which),tempfile.TemporaryDirectory() as directory:
                root=Path(directory);packet,expected,native=self.scaffold(root);result=shared.from_scaffold(native)
                row=result['cases'][0]
                if which=='ratio':row['wall_time_ratio']['value']=1/.75
                elif which=='profiled':row['timing']['candidate']['wall']['profiled']=True
                else:row['timing']['candidate']['wall']['summary']['median']=1
                code,report=self.review(root,packet,expected,result);self.assertEqual(code,1,report)

    def test_trusted_flops_tolerances_and_source_binding_are_not_relaxed(self):
        for which in ('flops','tolerance','baseline'):
            with self.subTest(which=which),tempfile.TemporaryDirectory() as directory:
                root=Path(directory);packet,expected,native=self.scaffold(root)
                if which=='flops':native['cases'][0]['useful_flops']+=100
                elif which=='tolerance':native['policy']['rtol']=.5
                else:native['baseline_id']='0'*64
                code,report=self.review(root,packet,expected,shared.from_scaffold(native));self.assertEqual(code,1,report)

    def test_baseline_correctness_failure_cannot_be_accepted(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);packet,expected,native=self.scaffold(root)
            native['cases'][0]['correctness']['baseline_pass']=False
            code,report=self.review(root,packet,expected,shared.from_scaffold(native));self.assertEqual(code,1)

    def test_missing_raw_samples_cannot_be_reconstructed_from_summary(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);packet,expected,native=self.scaffold(root)
            del native['cases'][0]['baseline']['wall_samples_us']
            result=shared.from_scaffold(native)
            self.assertIsNone(result['cases'][0]['wall_time_ratio']['value'])
            code,report=self.review(root,packet,expected,result);self.assertNotEqual(code,0)

    def test_profile_failure_preserves_unprofiled_values_and_acceptance(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);packet,expected,native=self.scaffold(root)
            before=shared.from_scaffold(copy.deepcopy(native))
            native['cases'][0]['diagnostics']=dict(status='failed',reason='synthetic tool failure')
            result=shared.from_scaffold(native)
            self.assertEqual(before['cases'][0]['timing'],result['cases'][0]['timing'])
            self.assertEqual(before['cases'][0]['wall_time_ratio'],result['cases'][0]['wall_time_ratio'])
            code,report=self.review(root,packet,expected,result);self.assertEqual(code,0,report)
            self.assertIn('DIAGNOSTIC_INCOMPLETE',{x['code'] for x in report['findings']})

    def test_pm_multiplass_and_synthetic_data_cannot_be_presented_as_measured(self):
        data=dict(method='ncu_pm_sampling',single_pass_verified=True,kernels=[dict(pass_groups=2)])
        with self.assertRaises(ValueError):shared.validate_pm(data)
        data['synthetic_fixture']=True
        with self.assertRaises(ValueError):shared.validate_pm(data)

    def test_duplicate_nonfinite_and_unsafe_files_refuse(self):
        for raw in ('{"x":1,"x":2}','{"x":NaN}','{"x":1e999}'):
            with self.assertRaises(ValueError):shared.loads(raw)
        with tempfile.TemporaryDirectory() as directory:
            with self.assertRaises(ValueError):validator.read(Path(directory),'../escape')


class Q7SharedTests(unittest.TestCase):
    fixture=q7_tests.ValidationTests.fixture
    stats=staticmethod(q7_tests.ValidationTests.stats)
    archive=q7_tests.ValidationTests.archive

    def prepare(self,root):
        source,old,files,base=self.fixture(root)
        packet=json.loads((source/'packet.json').read_text());packet['shared_results_contract']=shared.SCHEMA
        (source/'packet.json').write_text(json.dumps(packet))
        manifest=''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+p.relative_to(source).as_posix()+'\n' for p in sorted(source.rglob('*')) if p.is_file() and p.name not in ('SHA256SUMS','PACKET_ID'))
        (source/'SHA256SUMS').write_text(manifest);expected=hashlib.sha256(manifest.encode()).hexdigest();(source/'PACKET_ID').write_text(expected+'\n')
        files={name:content.replace(old.encode(),expected.encode()) for name,content in files.items()}
        files['packet/SHA256SUMS']=manifest.encode();files['packet/PACKET_ID']=(expected+'\n').encode();files['packet/packet.json']=(source/'packet.json').read_bytes()
        perf=json.loads(files[base+'/performance/report.json']);perf.update(wall_timing_cuda_events=False,device_timing_separate_batch=True)
        files[base+'/performance/report.json']=json.dumps(perf).encode()
        parent=json.loads(files[base+'/report.json']);parent['records']['performance']=perf;files[base+'/report.json']=json.dumps(parent).encode()
        result=shared.from_q7(packet,expected,files);files['result.json']=json.dumps(result).encode()
        return source,expected,files,base,result

    def audit(self,root,source,expected,files):
        archive=self.archive(root,files)
        code=validator.main(['--archive',str(archive),'--packet',str(source),'--expected-packet-sha256',expected,'--output',str(root/'shared-review')])
        return code,json.loads((root/'shared-review/summary.json').read_text())

    def test_q7_same_format_keeps_unknown_baseline_and_full_native_audit(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);source,expected,files,base,result=self.prepare(root)
            code,review=self.audit(root,source,expected,files);self.assertEqual(code,0,review)
            self.assertTrue(all(x['useful_flops']['value'] is None and x['wall_time_ratio']['value'] is None for x in result['cases']))
            self.assertEqual(result['cases'][0]['timing']['candidate']['wall']['scope'],shared.Q7_WALL)
            self.assertTrue((root/'shared-review/q7-review/summary.json').is_file())

    def test_native_q3_failure_cannot_be_hidden_by_valid_common_json(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);source,expected,files,base,result=self.prepare(root)
            child=json.loads(files[base+'/correctness/report.json']);child['cases'][0]['complete_Q3_tuple']=False
            files[base+'/correctness/report.json']=json.dumps(child).encode()
            code,review=self.audit(root,source,expected,files);self.assertEqual(code,1,review)

    def test_forged_shared_q7_result_fails_against_native_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);source,expected,files,base,result=self.prepare(root)
            result['cases'][0]['wall_time_ratio'].update(value=.5,evidence='derived')
            files['result.json']=json.dumps(result).encode()
            code,review=self.audit(root,source,expected,files);self.assertEqual(code,1,review)
            self.assertIn('Q7_SHARED_DIFFERS',{x['code'] for x in review['findings']})

    def test_q7_json_without_raw_archive_cannot_close_acceptance(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);source,expected,files,base,result=self.prepare(root)
            path=root/'result.json';path.write_text(json.dumps(result))
            code=validator.main(['--result',str(path),'--output',str(root/'review')]);self.assertEqual(code,2)

    def test_profile_subset_does_not_claim_other_cases_or_missing_measurements(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);source,expected,files,base,result=self.prepare(root)
            first=result['cases'][0]['case_id']
            files['results/04-profiling/report.json']=json.dumps(dict(schema='open64.mla-q7.profiling.v1',
                status='INCOMPLETE',cases=[dict(id=first,status='INCOMPLETE')])).encode()
            result=shared.from_q7(json.loads((source/'packet.json').read_text()),expected,files)
            self.assertEqual(result['cases'][0]['diagnostics']['status'],'incomplete')
            self.assertEqual(result['cases'][1]['diagnostics']['status'],'not_collected')
            self.assertEqual(result['cases'][1]['diagnostics']['methods'],[])

    def test_torn_child_report_is_failed_evidence_and_keeps_native_review(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);source,expected,files,base,result=self.prepare(root)
            files[base+'/correctness/report.json']=b'{"status":'
            code,review=self.audit(root,source,expected,files);self.assertEqual(code,1,review)
            self.assertIn('SHARED_ADAPTATION_FAILED',{x['code'] for x in review['findings']})
            self.assertTrue((root/'shared-review/q7-review/summary.md').is_file())


if __name__=='__main__':unittest.main()
