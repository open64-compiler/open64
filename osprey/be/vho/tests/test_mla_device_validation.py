"""Portable oracle checker controls; these tests do not run or emulate CUDA."""
import json
from pathlib import Path
import struct
import tempfile
import unittest
from validate_mla_gpu import check_output


class DeviceOracleTests(unittest.TestCase):
    def test_finite_tolerance_and_negative_infinity(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root / 'oracle.json').write_text(json.dumps(dict(schema='open64.mla-output-oracle.v1',
                values=[[1., -2.], ['-inf']], rtol=2e-5, atol=2e-6)))
            (root / 'result.bin').write_bytes(struct.pack('<3f', 1.000001, -2., float('-inf')))
            self.assertEqual(check_output(root / 'oracle.json', root / 'result.bin')['status'], 'PASS')

    def test_wrong_number_infinity_nan_and_length_reject(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root / 'oracle.json').write_text(json.dumps(dict(schema='open64.mla-output-oracle.v1',
                values=[[1.], ['-inf']], rtol=2e-5, atol=2e-6)))
            for raw in (struct.pack('<2f', 2., float('-inf')), struct.pack('<2f', 1., float('inf')),
                        struct.pack('<2f', float('nan'), float('-inf')), b''):
                (root / 'result.bin').write_bytes(raw)
                with self.assertRaises(ValueError):
                    check_output(root / 'oracle.json', root / 'result.bin')


if __name__ == '__main__':
    unittest.main()
