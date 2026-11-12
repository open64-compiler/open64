"""Return-archive integration checks; no GPU qualification is simulated."""
import argparse
import hashlib
import json
from pathlib import Path
import tarfile
import tempfile
import unittest
from unittest.mock import patch
import package_mla_q7_results as collector


class ReturnBundleTests(unittest.TestCase):
    def fixture(self, root):
        packet = root / 'packet'; packet.mkdir()
        (packet/'packet.json').write_text('{"schema":"open64.mla-q7.packet.v1"}')
        manifest = collector.digest_file(packet/'packet.json')+'  packet.json\n'
        (packet/'SHA256SUMS').write_text(manifest)
        identity = hashlib.sha256(manifest.encode()).hexdigest(); (packet/'PACKET_ID').write_text(identity+'\n')
        results = root/'q7-all'; results.mkdir()
        return argparse.Namespace(packet=packet, expected_sha256=identity, results=[results], output=root/'return.tar.gz', command_timeout=1)

    @staticmethod
    def environment(folder, timeout):
        folder.mkdir(); data=dict(commands=[dict(label='nvidia-smi-query',status='UNAVAILABLE')])
        (folder/'capture.json').write_text(json.dumps(data)); return data

    def collect(self, args):
        with patch.object(collector,'capture_environment',side_effect=self.environment):
            return collector.collect_results(args)

    def read(self, args):
        with tarfile.open(args.output) as tar:
            names=tar.getnames()
            self.assertTrue(all(not Path(n).is_absolute() and '..' not in Path(n).parts for n in names))
            self.assertFalse(any(m.issym() or m.islnk() for m in tar.getmembers()))
            prefix='Open64_MLA_Q7_results/'
            manifest=tar.extractfile(prefix+'SHA256SUMS').read().decode()
            for row in manifest.splitlines():
                digest,name=row.split('  ',1)
                self.assertEqual(hashlib.sha256(tar.extractfile(prefix+name).read()).hexdigest(),digest)
            return json.loads(tar.extractfile(prefix+'bundle.json').read()), {n:tar.extractfile(n).read() for n in names if tar.getmember(n).isfile()}

    def test_reports_logs_raw_timings_and_packet_identity_are_preserved(self):
        with tempfile.TemporaryDirectory() as folder:
            args=self.fixture(Path(folder)); result=args.results[0]
            report=dict(status='PASS',gpu_execution=True,packet_sha256=args.expected_sha256,mode='all')
            raw=json.dumps(report,indent=4).encode()+b'\n';(result/'report.json').write_bytes(raw)
            (result/'performance').mkdir();(result/'performance/samples.json').write_text('[1.0, 3.0, 2.0]\n')
            (result/'racecheck.stderr').write_text('RACECHECK SUMMARY: 0 hazards displayed\n')
            summary=self.collect(args); metadata,files=self.read(args)
            self.assertEqual(files['Open64_MLA_Q7_results/results/01-q7-all/report.json'],raw)
            self.assertIn(b'[1.0, 3.0, 2.0]\n',files.values())
            self.assertEqual(metadata['report_inventory'][0]['packet_binding'],'MATCH')
            self.assertEqual(metadata['qualification_status'],'NOT_ASSESSED_BY_COLLECTOR')
            self.assertEqual(metadata['GPU_workloads_launched_by_collector'],0)
            self.assertEqual(summary['copied_result_files'],3)
            self.assertEqual(Path(summary['checksum']).read_text().split()[0],collector.digest_file(args.output))

    def test_failed_partial_missing_and_wrong_packet_results_remain_returnable(self):
        with tempfile.TemporaryDirectory() as folder:
            args=self.fixture(Path(folder));result=args.results[0]
            (result/'report.json').write_text(json.dumps(dict(status='GPU_UNAVAILABLE',gpu_execution=False,packet_sha256='other')))
            (result/'performance').mkdir();(result/'performance/report.json').write_bytes(b'{"status":')
            args.results.append(Path(folder)/'not-run')
            self.collect(args);metadata,files=self.read(args)
            self.assertEqual(metadata['packaging_status'],'PACKAGED_WITH_NOTES')
            self.assertEqual(metadata['report_inventory'][0]['status'],'GPU_UNAVAILABLE')
            self.assertEqual(metadata['report_inventory'][0]['packet_binding'],'MISMATCH')
            self.assertIn(b'{"status":',files.values())
            self.assertGreaterEqual(len(metadata['collection_notes']),3)

    def test_shared_conversion_failure_does_not_block_raw_return_archive(self):
        with tempfile.TemporaryDirectory() as folder:
            args=self.fixture(Path(folder))
            (args.packet/'packet.json').write_text(json.dumps(dict(schema='open64.mla-q7.packet.v1',shared_results_contract='open64.gpu-test.results.v1',cases=[])))
            manifest=collector.digest_file(args.packet/'packet.json')+'  packet.json\n'
            (args.packet/'SHA256SUMS').write_text(manifest);args.expected_sha256=hashlib.sha256(manifest.encode()).hexdigest()
            (args.packet/'PACKET_ID').write_text(args.expected_sha256+'\n')
            raw=b'{"status":';(args.results[0]/'report.json').write_bytes(raw)
            self.collect(args);metadata,files=self.read(args)
            self.assertEqual(metadata['packaging_status'],'PACKAGED_WITH_NOTES')
            self.assertEqual(files['Open64_MLA_Q7_results/results/01-q7-all/report.json'],raw)
            self.assertIn('Open64_MLA_Q7_results/shared-result-error.json',files)
            self.assertNotIn('Open64_MLA_Q7_results/result.json',files)

    def test_symlink_files_and_directories_do_not_pull_other_data(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);args=self.fixture(root);(root/'unrelated').mkdir();(root/'unrelated/private').write_text('not results')
            (args.results[0]/'linked-file').symlink_to(root/'unrelated/private')
            (args.results[0]/'linked-dir').symlink_to(root/'unrelated',target_is_directory=True)
            self.collect(args);metadata,files=self.read(args)
            self.assertEqual(metadata['result_roots'][0]['files'],0)
            self.assertNotIn(b'not results',files.values());self.assertEqual(len(metadata['collection_notes']),2)

    def test_existing_delivery_and_nested_output_refuse_without_overwrite(self):
        with tempfile.TemporaryDirectory() as folder:
            args=self.fixture(Path(folder));args.output.write_bytes(b'original')
            with self.assertRaises(ValueError):self.collect(args)
            self.assertEqual(args.output.read_bytes(),b'original')
            args.output=args.results[0]/'recursive.tar.gz'
            with self.assertRaises(ValueError):self.collect(args)
            args.output=args.packet/'mutating.tar.gz'
            with self.assertRaises(ValueError):self.collect(args)

    def test_tampered_packet_refuses_before_environment_or_publication(self):
        with tempfile.TemporaryDirectory() as folder:
            args=self.fixture(Path(folder));(args.packet/'packet.json').write_text('changed')
            with patch.object(collector,'capture_environment') as capture:
                with self.assertRaises(ValueError):collector.collect_results(args)
            capture.assert_not_called();self.assertFalse(args.output.exists())

    def test_environment_records_missing_tools_and_only_allowlisted_variables(self):
        with tempfile.TemporaryDirectory() as folder:
            with patch.dict(collector.os.environ,{'CUDA_VISIBLE_DEVICES':'0','UNRELATED_SECRET':'do not collect'}),patch.object(collector.shutil,'which',return_value=None),patch.object(collector.subprocess,'run') as run:
                record=collector.capture_environment(Path(folder)/'environment',1)
            run.assert_not_called();self.assertEqual(record['variables']['CUDA_VISIBLE_DEVICES'],'0')
            self.assertNotIn('UNRELATED_SECRET',record['variables'])
            self.assertTrue(all(x['status']=='UNAVAILABLE' for x in record['commands']))

    def test_environment_timeouts_keep_partial_diagnostics_and_continue(self):
        with tempfile.TemporaryDirectory() as folder:
            timeout=collector.subprocess.TimeoutExpired('version-command',1,output=b'partial version',stderr=b'timed out')
            with patch.object(collector.shutil,'which',return_value='/tool'),patch.object(collector.subprocess,'run',side_effect=timeout) as run:
                record=collector.capture_environment(Path(folder)/'environment',1)
            self.assertTrue(all(x['status']=='TIMEOUT' for x in record['commands']))
            self.assertTrue(all(x.kwargs['timeout']==1 for x in run.call_args_list))
            self.assertEqual((Path(folder)/'environment/nvcc.stdout').read_text(),'partial version')
            self.assertEqual((Path(folder)/'environment/nvcc.stderr').read_text(),'timed out')


if __name__=='__main__':unittest.main()
