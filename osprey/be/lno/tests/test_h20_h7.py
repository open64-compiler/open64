"""Synthetic H7 contract/receiver/lifecycle tests; never GPU qualification."""
import argparse
import copy
import hashlib
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import gpu_test_results as shared
from h20_h7_common import ROLES, PROFILES, WALL, DEVICE, write, metadata, eligibility, compare
from prepare_h20_h7 import seal
import run_h20_h7 as runner
import validate_h20_h7 as validator
from validate_gpu_results import Packet


def observed():
    """Synthetic inventory with a full H20-96 capacity and bounded limits."""
    return dict(name='NVIDIA H20',uuid='abc',compute_capability=[9,0],total_memory_bytes=96*1024**3,mig_enabled=False,
        max_threads_per_block=1024,shared_bytes_per_block=49152,registers_per_block=65536,grid_x=2147483647,grid_y=65535,grid_z=65535)


def fixture(root):
    """Create inert authenticated metadata and complete synthetic reported data."""
    packet=root/'packet';packet.mkdir();art=root/'results';art.mkdir()
    profiles={}
    for role in ROLES:
        folder=packet/role;folder.mkdir();(folder/'candidate.cu').write_text('// synthetic compiler source fixture\nextern \"C\" __global__ void kernel(float*, float*) {}\n')
        (folder/'native.T').write_text('COMMENT selected explanation; synthetic trace fixture\n')
        (folder/'in.bin').write_bytes(b'\0\0\x80?');(folder/'gold.bin').write_bytes(b'\0\0\x80?')
        profiles[role]=[dict(id=role,compiler_binding=dict(gpu_device='h20_96gb',profile='profile.json',model='model.json',profile_sha256=hashlib.sha256(b'{"device":"h20_96gb"}').hexdigest(),model_sha256=hashlib.sha256(b'{}').hexdigest()),gpu_device='h20_96gb',source=role+'/candidate.cu',native_trace=role+'/native.T',buffers=[dict(id='0',bytes=4,role='input'),dict(id='1',bytes=4,role='output')],
            kernels=[dict(name='kernel',grid=[1,1,1],threads=32,shared_bytes=0,arguments=['0','1'])],
            variants=[dict(id='original',inputs={'0':role+'/in.bin'},outputs={'1':role+'/gold.bin'})],numerical=dict(kind='f32-bits-nan-class'),registers_estimate=32,useful_flops=None,baseline=None)]
    (packet/'profile.json').write_bytes(b'{"device":"h20_96gb"}');(packet/'model.json').write_bytes(b'{}')
    table=dict(schema='open64.h20-h7.packet.v1',gpu_device='h20_96gb',profiles=profiles,search_invocations=0);write(packet/'h20.json',table);identity=seal(packet)
    args=argparse.Namespace(packet=packet,expected_sha256=identity,profiling='off',check_only=False,timeout=1,samples=2,warmup=0,profile_timeline=False)
    build={};records=[]
    for role in ROLES:
        case=profiles[role][0];folder=art/'build'/role;folder.mkdir(parents=True);(folder/'selected.cubin').write_bytes(b'synthetic-cubin')
        binary=hashlib.sha256(b'synthetic-cubin').hexdigest();build[role]=dict(binary_sha256=binary,source_sha256=hashlib.sha256((packet/case['source']).read_bytes()).hexdigest())
        for prefix in (16,256):
            records.append(dict(case_id=role,variant='original',prefix=prefix,outputs={'1':role+'-'+str(prefix)+'.bin'},guards={name:(b'\xa7'*(prefix+256)).hex() for name in ('0','1')},
                input_sha256={'0':hashlib.sha256((packet/(role+'/in.bin')).read_bytes()).hexdigest()},binary_sha256=binary,passed=True,
                resources=[dict(kernel='kernel',selected=case['kernels'][0],compiler_binding=case['compiler_binding'],attributes=dict(max_threads=1024,static_shared=0,registers=20),active_blocks_per_sm=4,register_estimate=32,estimate_exceeded=False,binary_sha256=binary)]))
    (art/'logs').mkdir()
    for lane in ('correctness','memcheck','racecheck','initcheck','synccheck'):
        folder=art/lane;folder.mkdir()
        for record in records:(folder/record['outputs']['1']).write_bytes(b'\0\0\x80?')
        write(folder/'report.json',dict(status='PASS',gpu_execution=True,device=observed(),cases=records,mode='correctness',profiled=False))
        if lane!='correctness':
            (art/'logs'/(lane+'.stdout')).write_text('RACECHECK SUMMARY: 0 hazards displayed\n' if lane=='racecheck' else 'ERROR SUMMARY: 0 errors\n')
            (art/'logs'/(lane+'.stderr')).write_text('')
    write(art/'build/report.json',dict(status='PASS',cases=build))
    (art/'timing').mkdir()
    timed=[r for r in records if r['prefix']==256]
    for record in timed:(art/'timing'/record['outputs']['1']).write_bytes(b'\0\0\x80?')
    write(art/'timing/report.json',dict(status='PASS',gpu_execution=True,device=observed(),mode='timing',profiled=False,timing_samples_emitted=True,cases=timed,wall_checks=timed,
        performance=[dict(case_id=r,wall_us=[10.,12.],device_us=[5.,6.],profiled=False,wall_scope=WALL,device_scope=DEVICE,wall_cuda_events=False) for r in ROLES]))
    workflow=dict(packet_sha256=identity,session_id='synthetic-session',steps=[dict(label=t,exit_code=0) for t in ('memcheck','racecheck','initcheck','synccheck')],search_invocations=0,check_only=False,exit_code=0,profiling='off',model_calibration='UNQUALIFIED_SYNTHETIC_PROFILE')
    write(art/'workflow.json',workflow);runner.common_results(args,art,table,workflow)
    result=json.loads((art/ROLES[0]/'result.json').read_text())
    return args,table,art,result


class H7Contracts(unittest.TestCase):
    """Exercise meaningful device, failure-gate and independent receiver boundaries."""
    def test_device_identity_not_isa_alias(self):
        """Same SM does not make H200, another capacity or a partition eligible."""
        self.assertTrue(eligibility('h20_96gb',observed())[0])
        for change in [dict(name='NVIDIA H200'),dict(total_memory_bytes=141*1024**3),dict(mig_enabled=None),dict(mig_enabled=True),dict(uuid='')]:
            self.assertFalse(eligibility('h20_96gb',dict(observed(),**change))[0])
        self.assertTrue(eligibility('h20_141gb',dict(observed(),total_memory_bytes=141*1024**3))[0])

    def test_exact_numerical_classes(self):
        """NaN payloads may differ; signed zero and infinity signs may not."""
        rule=dict(kind='f32-bits-nan-class')
        self.assertTrue(compare(bytes.fromhex('0100c07f'),bytes.fromhex('0200c07f'),rule))
        self.assertFalse(compare(bytes.fromhex('00000080'),bytes(4),rule))
        self.assertFalse(compare(bytes.fromhex('000080ff'),bytes.fromhex('0000807f'),rule))
        self.assertFalse(compare(b'\0',b'\0',rule))

    def test_complete_report_keeps_calibration_and_baseline_unknown(self):
        """Synthetic complete gates cannot become measured model/speedup acceptance."""
        with tempfile.TemporaryDirectory() as tmp:
            args,table,art,result=fixture(Path(tmp));issues=validator.audit(result,Packet(args.packet,args.expected_sha256),art/ROLES[0])
            self.assertEqual({i['code'] for i in issues},{'H7_MODEL_CALIBRATION','H7_BASELINE_UNKNOWN'})
            self.assertEqual(shared.validate(result)['overall'],'PASS_REPORTED')
            self.assertIsNone(result['cases'][0]['wall_time_ratio']['value'])

    def test_returned_data_corruptions(self):
        """Recompute references/guards and join source, resources, devices and timing."""
        for fault in ('output','guard','gpu','profiled','source','resource','race','timed-output','timing-duplicate','grid','register-estimate'):
            with self.subTest(fault=fault),tempfile.TemporaryDirectory() as tmp:
                args,table,art,result=fixture(Path(tmp))
                if fault=='output':(art/'correctness'/(ROLES[0]+'-16.bin')).write_bytes(bytes(4))
                if fault=='timed-output':(art/'timing'/(ROLES[0]+'-256.bin')).write_bytes(bytes(4))
                if fault in ('timing-duplicate','grid','register-estimate'):
                    path=art/('timing/report.json' if fault=='timing-duplicate' else 'correctness/report.json');value=json.loads(path.read_text())
                    if fault=='timing-duplicate':value['performance'].append(value['performance'][0])
                    if fault=='grid':value['device']['grid_x']=0;result['environment']=value['device']
                    if fault=='register-estimate':value['cases'][0]['resources'][0]['estimate_exceeded']=True
                    write(path,value)
                if fault in ('guard','resource'):
                    p=art/'correctness/report.json';value=json.loads(p.read_text())
                    if fault=='guard':value['cases'][0]['guards']['0']='00'
                    else:value['cases'][0]['resources'][0]['attributes']['static_shared']=999999
                    write(p,value)
                if fault=='gpu':result['environment']=dict(observed(),name='NVIDIA H200')
                if fault=='profiled':
                    p=art/'timing/report.json';v=json.loads(p.read_text());v['profiled']=True;write(p,v)
                if fault=='source':
                    p=art/'build/report.json';v=json.loads(p.read_text());v['cases'][ROLES[0]]['source_sha256']='0'*64;write(p,v)
                if fault=='race':(art/'logs/racecheck.stdout').write_text('RACECHECK SUMMARY: 1 hazards displayed\n')
                try:issues=validator.audit(result,Packet(args.packet,args.expected_sha256),art/ROLES[0]);self.assertTrue(any(i['severity']=='ERROR' for i in issues))
                except ValueError:pass

    def test_missing_sanitizer_is_incomplete(self):
        """Missing tools/logs cannot stand in for a clean GPU race check."""
        with tempfile.TemporaryDirectory() as tmp:
            args,table,art,result=fixture(Path(tmp));(art/'racecheck/report.json').unlink()
            issues=validator.audit(result,Packet(args.packet,args.expected_sha256),art/ROLES[0])
            self.assertIn('H7_SANITIZER',{i['code'] for i in issues if i['severity']=='MISSING'})

    def test_early_failure_is_not_hidden_by_missing_correctness(self):
        """A compile failure remains failure even before the first GPU report exists."""
        with tempfile.TemporaryDirectory() as tmp:
            args,table,art,result=fixture(Path(tmp));(art/'correctness/report.json').unlink()
            path=art/'workflow.json';workflow=json.loads(path.read_text());workflow['exit_code']=1;write(path,workflow)
            issues=validator.audit(result,Packet(args.packet,args.expected_sha256),art/ROLES[0])
            self.assertIn('H7_WORKFLOW_STATUS',{i['code'] for i in issues if i['severity']=='ERROR'})

    def test_failed_correctness_prevents_timing_and_profiles(self):
        """The real orchestrator stops dependent lanes before any performance call."""
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);args,table,art,result=fixture(root);out=root/'run';out.mkdir();labels=[]
            def step(label,argv,logs,timeout,record):
                """Synthetic child denial; this fixture launches no GPU or compiler."""
                labels.append(label);record.update(label=label,exit_code=1);return 1
            with patch.object(runner,'build'),patch.object(runner,'run_step',step):
                code=runner.orchestrate(args,out,table)
            self.assertEqual(code,1);self.assertEqual(labels,['correctness'])

    def test_check_only_never_calls_compiler_or_gpu(self):
        """Source-only success remains not_run in every shared H20 family."""
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);args,table,art,result=fixture(root);args.check_only=True;out=root/'run';out.mkdir()
            with patch.object(runner,'build',side_effect=AssertionError('compiler')),patch.object(runner,'H7_DRIVER',side_effect=AssertionError('GPU')):
                self.assertEqual(runner.orchestrate(args,out,table),0)
            for role in ROLES:
                value=json.loads((out/role/'result.json').read_text());self.assertEqual(shared.validate(value)['overall'],'INCOMPLETE')

    def test_unsafe_or_incomplete_source_admission(self):
        """Reject traversal-like IDs, duplicate buffers and extent drift before GPU."""
        for fault in ('variant','buffer','extent','symbol'):
            with self.subTest(fault=fault),tempfile.TemporaryDirectory() as tmp:
                args,table,art,result=fixture(Path(tmp));case=table['profiles'][ROLES[0]][0]
                if fault=='variant':case['variants'][0]['id']='../escape'
                if fault=='buffer':case['buffers'].append(case['buffers'][0])
                if fault=='extent':case['buffers'][0]['bytes']=8
                if fault=='symbol':case['kernels'][0]['name']='absent'
                write(args.packet/'h20.json',table);identity=seal(args.packet)
                with self.assertRaises(ValueError):metadata(args.packet,identity)

    def test_unavailable_gpu_is_incomplete(self):
        """A missing Driver/physical device cannot turn absence into a race failure."""
        with tempfile.TemporaryDirectory() as tmp:
            args,table,art,result=fixture(Path(tmp))
            write(art/'correctness/report.json',dict(status='UNAVAILABLE',gpu_execution=False,reason='no NVIDIA hardware'))
            issues=validator.audit(result,Packet(args.packet,args.expected_sha256),art/ROLES[0])
            self.assertEqual({i['severity'] for i in issues},{'MISSING'})

    def test_v2_combined_check_only_and_collection(self):
        """Run the real combined orchestrator/collector with the optional H7 packet."""
        import shutil
        import tarfile
        from test_gpu_test_suite import Controls
        from gpu_test_suite_common import roles, catalog
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);helper=Controls();packet=helper.fixture(root)
            nested=root/'fixture';nested.mkdir();args,table,art,result=fixture(nested)
            shutil.copytree(args.packet,packet/'h20')
            outer=json.loads((packet/'suite.json').read_text());outer['schema']='open64.gpu-test.suite.v2'
            outer['h20_target']='h20_96gb';outer['packets']['h20']=dict(directory='h20',packet_sha256=args.expected_sha256)
            write(packet/'suite.json',outer);identity=seal(packet)
            self.assertEqual(len(roles(catalog(packet,identity))),7)
            process,run,status=helper.execute(packet,'--check-only')
            self.assertEqual(process.returncode,0,process.stderr)
            self.assertEqual(set(status['workloads']),set(roles(outer)))
            self.assertIn('h20-qualification',[s['label'] for s in status['steps']])
            with tarfile.open(run/'Open64_GPU_Test_results.tar.gz') as archive:
                from gpu_test_suite_common import ROOT
                index=json.load(archive.extractfile(ROOT+'suite-results.json'))
                self.assertEqual(set(index['profiles']),set(roles(outer)))
                for role in ROLES:self.assertTrue(index['profiles'][role]['present'])

    def test_profile_child_has_no_benchmark_samples(self):
        """Mock CUDA mechanics only; actual execution branch must emit no timings."""
        from unittest.mock import MagicMock
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);args,table,art,result=fixture(root);args.mode='profile';args.case=None;args.build=art/'build'
            driver=MagicMock();driver.info=observed();driver.streams=[0]
            session=MagicMock();session.verify.return_value=dict(synthetic=True)
            with patch.object(runner,'H7_DRIVER',return_value=driver),patch.object(runner,'SESSION',return_value=session):
                report=runner.execute(args,art,table)
            self.assertTrue(report['profiled']);self.assertFalse(report['timing_samples_emitted']);self.assertEqual(report['performance'],[])
            self.assertEqual(driver.profile.call_count,8);driver.event.assert_not_called()


if __name__=='__main__':unittest.main()
