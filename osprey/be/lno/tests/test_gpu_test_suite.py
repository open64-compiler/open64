"""CPU-only combined-suite control and receiver fixtures; no GPU claims."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import signal
import shutil
import subprocess
import sys
import tarfile
import tempfile
import time
import unittest
from unittest.mock import patch
import gpu_test_results as shared
import gpu_test_suite_common as common
import package_gpu_test_suite_results as collector
import validate_gpu_test_suite as validator
import test_gpu_shared_results as fixtures

HERE=Path(__file__).resolve().parent


def manifest(root):
    content=''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+p.relative_to(root).as_posix()+'\n'
        for p in sorted(root.rglob('*')) if p.is_file() and p not in (root/'SHA256SUMS',root/'PACKET_ID'))
    (root/'SHA256SUMS').write_text(content);identity=hashlib.sha256(content.encode()).hexdigest()
    (root/'PACKET_ID').write_text(identity+'\n');return identity


def catalog(root):
    entries={name:dict(directory=name,packet_sha256=(root/name/'PACKET_ID').read_text().strip()) for name in ('scaffold','mla-q7')}
    common.write(root/'suite.json',dict(schema=common.SCHEMA,packets=entries));return manifest(root)


def environment(folder,timeout):
    folder.mkdir();value=dict(commands=[],scope='synthetic environment fixture; no GPU queried')
    common.write(folder/'capture.json',value);return value


class Controls(unittest.TestCase):
    def fixture(self,root,gemm=0,mla=0,build=0,qualify=0,profile=0,wait=False):
        packet=root/'packet';packet.mkdir();scaf=packet/'scaffold';q7=packet/'mla-q7'
        (scaf/'server').mkdir(parents=True);(scaf/'tests').mkdir();q7.mkdir()
        common.write(scaf/'controls.json',dict(gemm=gemm,mla=mla,wait=wait));common.write(q7/'controls.json',dict(build=build,qualify=qualify,profile=profile))
        for name in ('gemm','mla'):(scaf/'submissions'/name).mkdir(parents=True)
        (scaf/'tests/test_fixture.py').write_text('import unittest\nclass Fixture(unittest.TestCase):\n def test_case(self):self.assertTrue(True)\n')
        (q7/'test_mla_q7_fixture.py').write_text((scaf/'tests/test_fixture.py').read_text())
        (scaf/'server/run.py').write_text('''import argparse,json,os,time
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('submission');p.add_argument('--output',type=Path);a,rest=p.parse_known_args()
c=json.loads((Path(__file__).parents[1]/'controls.json').read_text());a.output.parent.mkdir(parents=True,exist_ok=True)
(a.output.parent/'received.json').write_text(json.dumps(dict(argv=rest,visibility=os.environ.get('CUDA_VISIBLE_DEVICES'))))
if c[Path(a.submission).name]:
 cache=Path(__file__).parents[1]/'.build'/Path(a.submission).name;cache.mkdir(parents=True,exist_ok=True)
 (cache/'build.log').write_text('synthetic compiler failure diagnostics')
if c['wait'] and Path(a.submission).name=='gemm':
 (a.output.parent/'pid').write_text(str(os.getpid()));(a.output.parent/'ready').write_text('ready');time.sleep(60)
a.output.write_text(json.dumps(dict(status='CONTROL_ONLY',gpu_execution=False)))
raise SystemExit(c[Path(a.submission).name])
''')
        base='''import argparse,json
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--output',type=Path);a,rest=p.parse_known_args();a.output.mkdir(parents=True)
c=json.loads((Path(__file__).parent/'controls.json').read_text())
'''
        for filename,key in [('build_mla_q7.py','build'),('run_mla_q7.py','qualify'),('profile_mla_q7.py','profile')]:
            (q7/filename).write_text(base+"(a.output/'report.json').write_text(json.dumps(dict(status='CONTROL_ONLY',gpu_execution=False)))\nraise SystemExit(c['"+key+"'])\n")
        common.write(q7/'packet.json',dict(schema='open64.mla-q7.packet.v1'))
        shutil.copy2(HERE/'package_mla_q7_results.py',q7/'real_collector.py')
        (q7/'package_mla_q7_results.py').write_text("import os,runpy\nfrom pathlib import Path\nos.environ['PATH']=''\nrunpy.run_path(str(Path(__file__).with_name('real_collector.py')),run_name='__main__')\n")
        manifest(scaf);manifest(q7);catalog(packet)
        return packet

    def command(self,packet,*extra):
        return [sys.executable,str(HERE/'run_gpu_test_suite.py'),'--packet',str(packet),'--collection-timeout','60']+list(extra)

    def execute(self,packet,*extra):
        process=subprocess.run(self.command(packet,*extra),capture_output=True,text=True,timeout=90,
            env=dict(os.environ,PYTHONDONTWRITEBYTECODE='1',PATH='',CUDA_HOME='/__no_cuda_tools__'))
        run=next(packet.parent.glob('gpu-suite-run-*'))
        self.assertTrue((run/'Open64_GPU_Test_results.tar.gz').is_file(),process.stderr)
        return process,run,json.loads((run/'workflow/status.json').read_text())

    def test_default_runs_all_jobs_and_returns_one_archive(self):
        with tempfile.TemporaryDirectory() as directory:
            packet=self.fixture(Path(directory));process,run,status=self.execute(packet,'--gpu','0')
            self.assertEqual(process.returncode,0,process.stderr);self.assertEqual(set(status['workloads']),set(common.ROLES))
            self.assertNotIn('q7-profiling',[x['label'] for x in status['steps']])
            for name in ('gemm','mla-core'):
                received=json.loads((run/name/'received.json').read_text());self.assertNotIn('--profile-pm',received['argv']);self.assertEqual(received['visibility'],'0')
            with tarfile.open(run/'Open64_GPU_Test_results.tar.gz') as tar:
                names=tar.getnames();self.assertIn(common.ROOT+'results/mla-q7/Open64_MLA_Q7_results.tar.gz',names)
                self.assertFalse(any('/raw-partial/' in x for x in names))

    def test_independent_workloads_continue_and_first_failure_is_preserved(self):
        with tempfile.TemporaryDirectory() as directory:
            packet=self.fixture(Path(directory),gemm=17,build=19);process,run,status=self.execute(packet)
            self.assertEqual(process.returncode,17);self.assertEqual(status['workloads']['mla-core']['exit_code'],0)
            self.assertEqual(status['workloads']['mla-q7']['exit_code'],19);self.assertFalse((run/'mla-q7/qualification').exists())
            self.assertFalse((packet/'scaffold/.build').exists())
            with tarfile.open(run/'Open64_GPU_Test_results.tar.gz') as tar:
                self.assertEqual(tar.extractfile(common.ROOT+'build-evidence/gemm/build.log').read(),b'synthetic compiler failure diagnostics')

    def test_optional_profile_failure_does_not_rewrite_qualification(self):
        with tempfile.TemporaryDirectory() as directory:
            packet=self.fixture(Path(directory),profile=23);process,run,status=self.execute(packet,'--profiling','auto')
            self.assertEqual(process.returncode,0,process.stderr)
            self.assertEqual(next(x for x in status['steps'] if x['label']=='q7-profiling')['exit_code'],23)
            self.assertEqual(status['workloads']['mla-q7']['exit_code'],0)
            self.assertIn('--profile-pm',json.loads((run/'gemm/received.json').read_text())['argv'])

    def test_check_only_has_no_q7_compile_or_device_execution(self):
        with tempfile.TemporaryDirectory() as directory:
            packet=self.fixture(Path(directory),build=31);process,run,status=self.execute(packet,'--check-only')
            self.assertEqual(process.returncode,0);self.assertFalse((run/'mla-q7/compile-generated').exists())
            self.assertTrue(status['check_only']);self.assertIn('--check-only',json.loads((run/'mla-core/received.json').read_text())['argv'])

    def test_staging_does_not_inherit_read_only_source_modes(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);packet=self.fixture(root)
            for path in (packet/'scaffold').rglob('*'):
                if path.is_file():path.chmod(0o444)
            (packet/'scaffold').chmod(0o555)
            target=common.stage_scaffold(packet/'scaffold',root/'staged')
            (target/'.build').mkdir();(target/'.build/log').write_text('writable')
            self.assertEqual((target/'server/run.py').read_bytes(),(packet/'scaffold/server/run.py').read_bytes())
            (packet/'scaffold').chmod(0o755)

    def test_timeout_finalizes_and_stops_active_child(self):
        with tempfile.TemporaryDirectory() as directory:
            packet=self.fixture(Path(directory),wait=True);process,run,status=self.execute(packet,'--step-timeout','1')
            self.assertEqual(process.returncode,124)
            with self.assertRaises(ProcessLookupError):os.kill(int((run/'gemm/pid').read_text()),0)

    def test_sigterm_finalizes_after_stopping_work(self):
        with tempfile.TemporaryDirectory() as directory:
            packet=self.fixture(Path(directory),wait=True)
            process=subprocess.Popen(self.command(packet),stdout=subprocess.PIPE,stderr=subprocess.PIPE,env=dict(os.environ,PATH='',PYTHONDONTWRITEBYTECODE='1'))
            try:
                deadline=time.monotonic()+15;ready=[]
                while time.monotonic()<deadline:
                    ready=list(packet.parent.glob('gpu-suite-run-*/gemm/ready'))
                    if ready:break
                    time.sleep(.02)
                self.assertTrue(ready);process.send_signal(signal.SIGTERM);stdout,stderr=process.communicate(timeout=60)
                self.assertEqual(process.returncode,143,stderr.decode())
                self.assertTrue((ready[0].parents[1]/'Open64_GPU_Test_results.tar.gz').is_file())
                with self.assertRaises(ProcessLookupError):os.kill(int((ready[0].parent/'pid').read_text()),0)
            finally:
                if process.poll() is None:process.kill();process.wait()


class Receiver(unittest.TestCase):
    def fixture(self,root):
        packet=root/'packet';packet.mkdir();run=root/'run';run.mkdir()
        maker=fixtures.SharedTests()
        for kind,role in [('gemm','gemm'),('mla','mla-core')]:
            temp=root/('source-'+kind);temp.mkdir();source,identity,native=maker.scaffold(temp,kind)
            shutil.copytree(source,packet/'scaffold',dirs_exist_ok=True)
            native['environment']['gpu_uuid']='0123456789abcdef'*2
            (run/role).mkdir();common.write(run/role/'result.json',shared.from_scaffold(native))
        manifest(packet/'scaffold')
        temp=root/'source-q7';temp.mkdir();qmaker=fixtures.Q7SharedTests()
        source,identity,files,base,result=qmaker.prepare(temp);shutil.copytree(source,packet/'mla-q7')
        (run/'mla-q7').mkdir();qmaker.archive(run/'mla-q7',files)
        (run/'mla-q7/returned.tar.gz').rename(run/'mla-q7/Open64_MLA_Q7_results.tar.gz')
        (run/'mla-q7/returned.tar.gz.sha256').rename(run/'mla-q7/Open64_MLA_Q7_results.tar.gz.sha256')
        identity=catalog(packet);(run/'workflow').mkdir()
        common.write(run/'workflow/status.json',dict(packet_sha256=identity,exit_code=0,inner_collection_exit_code=0,
            steps=[dict(label=x,exit_code=0) for x in ('scaffold-unit','gemm','mla-core','q7-unit','q7-compile-generated','q7-qualification')],
            workloads={x:dict(exit_code=0) for x in common.ROLES}))
        args=argparse.Namespace(packet=packet,expected_sha256=identity,run=run,output=root/'combined.tar.gz',command_timeout=1)
        with patch.object(collector,'capture_environment',side_effect=environment):collector.collect(args)
        return packet,identity,args.output

    def review(self,root,packet,identity,archive,extra=()):
        code=validator.main(['--archive',str(archive),'--packet',str(packet),'--expected-packet-sha256',identity,
            '--output',str(root/'review')]+list(extra))
        return code,json.loads((root/'review/summary.json').read_text())

    def test_one_receiver_preserves_all_three_acceptance_paths(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);packet,identity,archive=self.fixture(root);code,report=self.review(root,packet,identity,archive)
            self.assertEqual(code,0,report);self.assertEqual(set(report['profiles']),set(common.ROLES));self.assertFalse(report['timings_pooled'])

    def test_outer_corruption_and_expansion_budget_refuse(self):
        for which in ('checksum','budget'):
            with self.subTest(which=which),tempfile.TemporaryDirectory() as directory:
                root=Path(directory);packet,identity,archive=self.fixture(root)
                if which=='checksum':Path(str(archive)+'.sha256').write_text('0'*64+'  combined.tar.gz\n')
                code,report=self.review(root,packet,identity,archive,['--max-expanded-bytes','1'] if which=='budget' else [])
                self.assertEqual(code,3,report)

    def test_malformed_and_missing_results_are_returnable_but_not_accepted(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);packet,identity,archive=self.fixture(root)
            # Recollect a deliberately torn report. Original raw bytes must survive.
            (root/'run/gemm/result.json').write_bytes(b'{"schema":')
            args=argparse.Namespace(packet=packet,expected_sha256=identity,run=root/'run',output=root/'partial.tar.gz',command_timeout=1)
            with patch.object(collector,'capture_environment',side_effect=environment):collector.collect(args)
            code,report=self.review(root,packet,identity,args.output);self.assertEqual(code,1,report)

    def test_mixed_gpu_and_swapped_workload_results_cannot_pass(self):
        for which in ('gpu','swap'):
            with self.subTest(which=which),tempfile.TemporaryDirectory() as directory:
                root=Path(directory);packet,identity,archive=self.fixture(root)
                first=root/'run/gemm/result.json';second=root/'run/mla-core/result.json'
                if which=='gpu':
                    result=json.loads(first.read_text());native=result['extensions']['scaffold_v1'];native['environment']['gpu_uuid']='f'*32
                    common.write(first,shared.from_scaffold(native))
                else:
                    a,b=first.read_bytes(),second.read_bytes();first.write_bytes(b);second.write_bytes(a)
                args=argparse.Namespace(packet=packet,expected_sha256=identity,run=root/'run',output=root/'changed.tar.gz',command_timeout=1)
                with patch.object(collector,'capture_environment',side_effect=environment):collector.collect(args)
                code,report=self.review(root,packet,identity,args.output);self.assertEqual(code,1,report)
                self.assertIn('MIXED_GPUS' if which=='gpu' else 'WORKLOAD_BINDING',{x['code'] for x in report['findings']})

    def test_missing_native_archive_preserves_raw_partial_and_is_incomplete(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);packet,identity,archive=self.fixture(root)
            for path in (root/'run/mla-q7').iterdir():path.unlink()
            (root/'run/mla-q7/partial.txt').write_text('unfinished native evidence')
            args=argparse.Namespace(packet=packet,expected_sha256=identity,run=root/'run',output=root/'partial.tar.gz',command_timeout=1)
            with patch.object(collector,'capture_environment',side_effect=environment):collector.collect(args)
            with tarfile.open(args.output) as tar:self.assertIsNotNone(tar.getmember(common.ROOT+'results/mla-q7/raw-partial/partial.txt'))
            code,report=self.review(root,packet,identity,args.output);self.assertEqual(code,2,report)


if __name__=='__main__':unittest.main()
