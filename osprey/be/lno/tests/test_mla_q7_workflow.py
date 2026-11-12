"""Real process/collector tests for Q7 workflow finalization, never GPU tests."""
import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tarfile
import tempfile
import time
import unittest

HERE = Path(__file__).resolve().parent


class WorkflowTests(unittest.TestCase):
    def fixture(self, root, unit_fail=False, build_exit=0, qualify_exit=0, collect_exit=0, wait=False):
        packet=root/'packet';packet.mkdir()
        controls=dict(build_exit=build_exit,qualify_exit=qualify_exit,collect_exit=collect_exit,wait=wait)
        (packet/'controls.json').write_text(json.dumps(controls))
        (packet/'packet.json').write_text('{"schema":"open64.mla-q7.packet.v1"}')
        (packet/'test_mla_q7_fixture.py').write_text('import unittest\nclass ControlOnly(unittest.TestCase):\n def test_control(self): self.assertTrue('+str(not unit_fail)+')\n')
        common='''import argparse,json,sys,os,time
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--output',type=Path,required=True);a,unused=p.parse_known_args()
a.output.mkdir(parents=True);c=json.loads((Path(__file__).parent/'controls.json').read_text())
'''
        (packet/'build_mla_q7.py').write_text(common+'''(a.output/'pid').write_text(str(os.getpid()))
if c['wait']:
 (a.output/'ready').write_text('ready');time.sleep(60)
(a.output/'build-report.json').write_text(json.dumps(dict(status='CONTROL_FLOW_TEST_ONLY',gpu_execution=False)))
print('build diagnostic');sys.exit(c['build_exit'])
''')
        (packet/'run_mla_q7.py').write_text(common+'''(a.output/'report.json').write_text(json.dumps(dict(status='GPU_UNAVAILABLE' if c['qualify_exit']==77 else 'CONTROL_FLOW_TEST_ONLY',gpu_execution=False)))
sys.exit(c['qualify_exit'])
''')
        (packet/'real_collector.py').write_bytes((HERE/'package_mla_q7_results.py').read_bytes())
        (packet/'package_mla_q7_results.py').write_text('''import json,sys,runpy,os
from pathlib import Path
c=json.loads((Path(__file__).parent/'controls.json').read_text())
if c['collect_exit']:sys.exit(c['collect_exit'])
# Control tests do not launch installed NVIDIA tools just to query versions.
os.environ['PATH']='';os.environ['CUDA_HOME']='/__q7_control_test_no_tools__'
runpy.run_path(str(Path(__file__).parent/'real_collector.py'),run_name='__main__')
''')
        manifest=''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+p.name+'\n' for p in sorted(packet.iterdir()) if p.is_file())
        (packet/'SHA256SUMS').write_text(manifest);(packet/'PACKET_ID').write_text(hashlib.sha256(manifest.encode()).hexdigest()+'\n')
        return packet

    def command(self, packet, *extra):
        return [sys.executable,str(HERE/'run_mla_q7_workflow.py'),'--packet',str(packet),'--collection-timeout','30']+list(extra)

    def execute(self, packet, *extra):
        result=subprocess.run(self.command(packet,*extra),capture_output=True,text=True,timeout=45,
                              env=dict(os.environ,PYTHONDONTWRITEBYTECODE='1'))
        runs=sorted(packet.parent.glob('q7-run-*'));run=runs[-1]
        complete=json.loads((run/'completion.json').read_text())
        self.assertEqual(result.returncode,complete['exit_code'],result.stderr)
        return result,run,complete

    def archived_status(self, run):
        with tarfile.open(run/'Open64_MLA_Q7_results.tar.gz') as tar:
            prefix='Open64_MLA_Q7_results/'
            status=json.loads(tar.extractfile(prefix+'results/01-workflow/status.json').read())
            bundle=json.loads(tar.extractfile(prefix+'bundle.json').read())
            self.assertEqual(bundle['qualification_status'],'NOT_ASSESSED_BY_COLLECTOR')
            for row in tar.extractfile(prefix+'SHA256SUMS').read().decode().splitlines():
                digest,name=row.split('  ',1);self.assertEqual(hashlib.sha256(tar.extractfile(prefix+name).read()).hexdigest(),digest)
            return status

    def test_unit_failure_still_collects(self):
        with tempfile.TemporaryDirectory() as folder:
            packet=self.fixture(Path(folder),unit_fail=True);result,run,complete=self.execute(packet)
            self.assertEqual(result.returncode,1);self.assertEqual(complete['collector_exit_code'],0)
            self.assertEqual([x['label'] for x in self.archived_status(run)['steps']],['unit'])

    def test_compile_failure_still_collects_and_does_not_run_qualification(self):
        with tempfile.TemporaryDirectory() as folder:
            packet=self.fixture(Path(folder),build_exit=17);result,run,complete=self.execute(packet)
            self.assertEqual(result.returncode,17);self.assertTrue(complete['archive_created'])
            self.assertFalse((run/'qualification').exists());self.assertEqual(self.archived_status(run)['exit_code'],17)

    def test_unavailable_qualification_preserves_77_and_archive(self):
        with tempfile.TemporaryDirectory() as folder:
            packet=self.fixture(Path(folder),qualify_exit=77);result,run,complete=self.execute(packet)
            self.assertEqual(result.returncode,77);self.assertEqual(complete['collector_exit_code'],0)
            self.assertEqual(self.archived_status(run)['steps'][-1]['exit_code'],77)

    def test_success_collects_and_each_invocation_has_fresh_outputs(self):
        with tempfile.TemporaryDirectory() as folder:
            packet=self.fixture(Path(folder));result,run,complete=self.execute(packet)
            self.assertEqual(result.returncode,0);self.assertTrue(complete['archive_created']);self.archived_status(run)
            digest=hashlib.sha256((run/'Open64_MLA_Q7_results.tar.gz').read_bytes()).hexdigest()
            subprocess.run(self.command(packet),capture_output=True,timeout=45,check=True,env=dict(os.environ,PYTHONDONTWRITEBYTECODE='1'))
            self.assertEqual(len(list(packet.parent.glob('q7-run-*'))),2)
            self.assertEqual(hashlib.sha256((run/'Open64_MLA_Q7_results.tar.gz').read_bytes()).hexdigest(),digest)

    def test_diagnostic_failure_is_packaged_without_changing_qualification_status(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);packet=self.fixture(root)
            (packet/'profile_mla_q7.py').write_text('''import argparse,json
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--output',type=Path);a,unused=p.parse_known_args()
a.output.mkdir();(a.output/'report.json').write_text(json.dumps(dict(status='FAIL',reason='synthetic profiler failure')))
raise SystemExit(19)
''')
            manifest=''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+p.name+'\n' for p in sorted(packet.iterdir()) if p.name not in ('SHA256SUMS','PACKET_ID'))
            (packet/'SHA256SUMS').write_text(manifest);(packet/'PACKET_ID').write_text(hashlib.sha256(manifest.encode()).hexdigest()+'\n')
            result,run,complete=self.execute(packet,'--profiling','auto')
            self.assertEqual(result.returncode,0);self.assertEqual(complete['profiling_exit_code'],19)
            self.assertEqual(self.archived_status(run)['profiling_exit_code'],19)
            with tarfile.open(run/'Open64_MLA_Q7_results.tar.gz') as tar:
                self.assertIsNotNone(tar.getmember('Open64_MLA_Q7_results/results/04-profiling/report.json'))

    def test_collection_failure_is_nonzero_and_does_not_mask_prior_failure(self):
        for build,expected in ((0,23),(17,17)):
            with self.subTest(build=build),tempfile.TemporaryDirectory() as folder:
                packet=self.fixture(Path(folder),build_exit=build,collect_exit=23);result,run,complete=self.execute(packet)
                self.assertEqual(result.returncode,expected);self.assertEqual(complete['collector_exit_code'],23)
                self.assertFalse(complete['archive_created']);self.assertIn('RESULT COLLECTION FAILED',result.stderr)

    def test_timeout_stops_child_then_collects(self):
        with tempfile.TemporaryDirectory() as folder:
            packet=self.fixture(Path(folder),wait=True);result,run,complete=self.execute(packet,'--step-timeout','1')
            self.assertEqual(result.returncode,124);self.assertTrue(complete['archive_created'])
            self.assertEqual(self.archived_status(run)['steps'][-1]['reason'],'step timeout')
            with self.assertRaises(ProcessLookupError):os.kill(int((run/'compile-generated/pid').read_text()),0)

    def test_sigint_and_sigterm_stop_child_and_collect(self):
        for sig in (signal.SIGINT,signal.SIGTERM):
            with self.subTest(signal=sig),tempfile.TemporaryDirectory() as folder:
                packet=self.fixture(Path(folder),wait=True)
                process=subprocess.Popen(self.command(packet),stdout=subprocess.PIPE,stderr=subprocess.PIPE,
                                         env=dict(os.environ,PYTHONDONTWRITEBYTECODE='1'))
                try:
                    deadline=time.monotonic()+10;ready=[]
                    while time.monotonic()<deadline:
                        ready=list(packet.parent.glob('q7-run-*/compile-generated/ready'))
                        if ready:break
                        time.sleep(.02)
                    self.assertTrue(ready);process.send_signal(sig);stdout,stderr=process.communicate(timeout=35)
                    self.assertEqual(process.returncode,128+sig,stderr.decode())
                    run=ready[0].parent.parent;self.assertEqual(self.archived_status(run)['exit_code'],128+sig)
                    with self.assertRaises(ProcessLookupError):os.kill(int((ready[0].parent/'pid').read_text()),0)
                finally:
                    if process.poll() is None:process.kill();process.wait()


if __name__=='__main__':unittest.main()
