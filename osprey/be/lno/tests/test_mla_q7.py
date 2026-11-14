"""Q7 control-plane tests: no GPU or mock numerical acceptance is claimed."""
import contextlib
import hashlib
import io
import json
from pathlib import Path
import struct
import tempfile
import unittest
from unittest.mock import patch
from mla_q7_common import authenticate, compare, summarize, sanitizer_flags, check_sanitizer
import run_mla_q7


class Q7Contracts(unittest.TestCase):
    def test_all_oracle_elements_classes_and_exact_integer(self):
        report=compare(struct.pack('<4f',1.000001,-2,float('-inf'),float('nan')),
                       struct.pack('<4d',1,-2,float('-inf'),float('nan')),'float32','float64',2e-5,2e-6)
        self.assertEqual(report['elements'],4)
        # Integer equality must not round through binary64.
        with self.assertRaises(ValueError):
            compare(struct.pack('<q',2**60+1),struct.pack('<q',2**60),'int64','int64',0,0)

    def test_wrong_classes_truncation_and_values_refuse(self):
        for got,ref in ((float('inf'),float('-inf')),(float('nan'),1.0),(2.0,1.0)):
            with self.assertRaises(ValueError): compare(struct.pack('<f',got),struct.pack('<d',ref),'float32','float64',2e-5,2e-6)
        for data in (b'',b'\x00'):
            with self.assertRaises(ValueError): compare(data,struct.pack('<d',1),'float32','float64',0,0)

    def test_packet_identity_payload_and_path_refusal(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);(root/'data').write_bytes(b'approved')
            manifest=hashlib.sha256(b'approved').hexdigest()+'  data\n';(root/'SHA256SUMS').write_text(manifest)
            identity=hashlib.sha256(manifest.encode()).hexdigest()
            self.assertEqual(authenticate(root,identity),1)
            with self.assertRaises(ValueError): authenticate(root,'0'*64)
            (root/'data').write_bytes(b'changed')
            with self.assertRaises(ValueError): authenticate(root,identity)
            for name in ('../escape','/tmp/escape','data\n'+manifest.rstrip()):
                text=hashlib.sha256(b'changed').hexdigest()+'  '+name+'\n';(root/'SHA256SUMS').write_text(text)
                with self.assertRaises((ValueError,FileNotFoundError)): authenticate(root,hashlib.sha256(text.encode()).hexdigest())

    def test_symlink_and_duplicate_refuse(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);(root/'data').write_bytes(b'a');(root/'link').symlink_to('data')
            for text in (hashlib.sha256(b'a').hexdigest()+'  link\n',(hashlib.sha256(b'a').hexdigest()+'  data\n')*2):
                (root/'SHA256SUMS').write_text(text)
                with self.assertRaises(ValueError):authenticate(root,hashlib.sha256(text.encode()).hexdigest())

    def test_sanitizer_requires_actual_execution_and_positive_summaries(self):
        child=dict(status='PASS',gpu_execution=True,cases=[dict(status='PASS')])
        for tool,log in [('memcheck','ERROR SUMMARY: 0 errors'),('racecheck','RACECHECK SUMMARY: 0 hazards displayed (0 errors, 0 warnings)'),
                         ('synccheck','ERROR SUMMARY: 0 errors'),('initcheck','ERROR SUMMARY: 0 errors')]:
            self.assertEqual(check_sanitizer(tool,0,log,child)['status'],'PASS')
            for bad in ('',log+'\nunsupported instruction',log+'\nbarrier tracking overflow',log+'\nERROR SUMMARY: 1 errors'):
                with self.assertRaises(ValueError):check_sanitizer(tool,0,bad,child)
            with self.assertRaises(ValueError):check_sanitizer(tool,86,log,child)
            with self.assertRaises(ValueError):check_sanitizer(tool,0,log,dict(child,gpu_execution=False))
            with self.assertRaises(ValueError):check_sanitizer(tool,0,log,dict(child,cases=[]))
        with self.assertRaises(ValueError):check_sanitizer('racecheck',0,'RACECHECK SUMMARY: 1 hazard displayed (0 errors, 1 warnings)',child)
        with self.assertRaises(ValueError):check_sanitizer('racecheck',0,'RACECHECK SUMMARY: 0 hazards displayed (0 errors, 1 warnings)',child)

    def test_bulk_proxy_coverage_is_mandatory(self):
        help_text='check-bulk-copy error-exitcode report-api-errors racecheck-fence-proxy-async racecheck-memcpy-async racecheck-detect-level initcheck-address-space'
        for tool in ('memcheck','racecheck','synccheck','initcheck'):
            self.assertIn('86',sanitizer_flags(tool,help_text))
        with self.assertRaises(ValueError):sanitizer_flags('racecheck',help_text.replace('racecheck-fence-proxy-async',''))
        with self.assertRaises(ValueError):sanitizer_flags('initcheck',help_text.replace('initcheck-address-space',''))

    def test_timing_samples_retain_values_and_refuse_nonfinite(self):
        report=summarize([3.0,1.0,2.0,4.0]);self.assertEqual(report['samples'],[3.0,1.0,2.0,4.0]);self.assertEqual(report['median'],2.5)
        self.assertAlmostEqual(report['p95'],3.85)
        for samples in ([],[-1],[float('nan')],[float('inf')]):
            with self.assertRaises(ValueError):summarize(samples)

    def test_unavailable_device_never_passes_or_loads_native_binding(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);(root/'packet.json').write_text(json.dumps(dict(schema='open64.mla-q7.packet.v1',cases=[])))
            manifest=hashlib.sha256((root/'packet.json').read_bytes()).hexdigest()+'  packet.json\n';(root/'SHA256SUMS').write_text(manifest)
            with patch.object(run_mla_q7,'DRIVER',side_effect=run_mla_q7.Unavailable('no GPU')),patch.object(run_mla_q7,'API') as api,contextlib.redirect_stdout(io.StringIO()):
                code=run_mla_q7.main(['--packet',str(root),'--expected-sha256',hashlib.sha256(manifest.encode()).hexdigest(),'--output',str(root/'result'),'--mode','correctness'])
            self.assertEqual(code,77);api.assert_not_called()
            report=json.loads((root/'result/report.json').read_text());self.assertEqual(report['status'],'GPU_UNAVAILABLE');self.assertFalse(report['gpu_execution'])

    def test_authentication_failure_precedes_driver_and_never_passes(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);(root/'SHA256SUMS').write_text('')
            with patch.object(run_mla_q7,'DRIVER') as driver,contextlib.redirect_stdout(io.StringIO()):
                code=run_mla_q7.main(['--packet',str(root),'--expected-sha256','bad','--output',str(root/'result'),'--mode','correctness'])
            self.assertEqual(code,1);driver.assert_not_called();self.assertEqual(json.loads((root/'result/report.json').read_text())['status'],'FAIL')


if __name__=='__main__':unittest.main()
