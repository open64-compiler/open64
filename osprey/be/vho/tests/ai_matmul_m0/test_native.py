"""Host-only checks for the native m0 handoff and timing contract."""

import json
from pathlib import Path
import tempfile
import unittest

from prepare_native import prepare, sha256
from run_native import validate_samples


class NativePackageTest(unittest.TestCase):
    def test_compiled_pu_package_is_non_overwriting_and_hashed(self):
        with tempfile.TemporaryDirectory() as temporary:
            smoke = Path(temporary) / "smoke"
            for name in ("binary", "source", "traces"):
                (smoke / name).mkdir(parents=True)
            files = ("binary/matmul-m0", "binary/matmul.B", "binary/matmul.I",
                     "source/fixture.py", "source/runtime.c", "source/runtime.h",
                     "source/benchmark.c", "traces/matmul.T")
            for name in files:
                (smoke / name).write_bytes(name.encode("ascii"))
            (smoke / "receipt.json").write_text(json.dumps({
                "status": "functional_smoke_emulated",
                "numerical_check":
                    "all 16384 elements exactly 128.0f on every run",
            }))
            manifest_files = sorted(path for path in smoke.rglob("*")
                                    if path.is_file())
            (smoke / "SHA256SUMS").write_text(
                "".join(f"{sha256(path)}  {path.relative_to(smoke)}\n"
                        for path in manifest_files))
            output = prepare(smoke, Path(temporary) / "native")
            metadata = json.loads((output / "native.json").read_text())
            self.assertEqual(metadata["executable_sha256"],
                             sha256(output / "binary/matmul-m0"))
            self.assertFalse(metadata["native_verified"])
            with self.assertRaises(FileExistsError):
                prepare(smoke, output)
            (smoke / "binary/matmul-m0").write_bytes(b"changed")
            with self.assertRaisesRegex(ValueError, "checksum failed"):
                prepare(smoke, Path(temporary) / "tampered")

    def test_native_timing_contract(self):
        rows = [{"sample": str(index), "elapsed_ns": "1000"}
                for index in range(30)]
        self.assertEqual(validate_samples(rows), [1000] * 30)
        with self.assertRaisesRegex(ValueError, "incomplete or malformed"):
            validate_samples(rows[:-1])
        with self.assertRaisesRegex(ValueError, "incomplete or malformed"):
            validate_samples(rows[:-1] + [{"sample": "29", "elapsed_ns": "0"}])


if __name__ == "__main__":
    unittest.main()
