"""Fail-closed tests for the S6-0c source-family replay gate.

Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
"""

import hashlib
import json
import struct
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import fhe_ckks_replay_gate as gate


class ReplayGateTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="fhe-replay-test-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)

    def make_tensor(self, name="weights.safetensors", data=b"abcdefgh"):
        path = self.root / name
        header = json.dumps({"weight": {"dtype": "U8", "shape": [len(data)],
                                         "data_offsets": [0, len(data)]}},
                            separators=(",", ":")).encode("utf-8")
        path.write_bytes(struct.pack("<Q", len(header)) + header + data)
        return path

    def make_trace(self, tensor, data=b"abcdefgh", suffix=""):
        trace = self.root / "source.T"
        trace.write_text(
            " LOC 1 12 source statement\n"
            f"safetensors://{tensor.name}#weight?offset=0&length={len(data)}"
            f"&checksum={hashlib.sha256(data).hexdigest()}{suffix}\n",
            encoding="utf-8")
        return trace

    def write_sealed_manifest(self, path, manifest):
        canonical = json.dumps({key: value for key, value in manifest.items()
                                if key != "manifest_sha256"},
                               separators=(",", ":")).encode("utf-8")
        manifest["manifest_sha256"] = hashlib.sha256(canonical).hexdigest()
        path.write_text(json.dumps(manifest, separators=(",", ":")),
                        encoding="utf-8")

    def test_external_reference_exact_slice_and_checksum(self):
        tensor = self.make_tensor()
        trace = self.make_trace(tensor)
        self.assertEqual(gate.check_external_references(
            trace, {tensor.name: tensor}), 1)
        tensor.write_bytes(tensor.read_bytes()[:-1] + b"x")
        with self.assertRaisesRegex(ValueError, "checksum mismatch"):
            gate.check_external_references(trace, {tensor.name: tensor})

    def test_unparsed_and_wrong_range_rejected(self):
        tensor = self.make_tensor()
        trace = self.make_trace(tensor, suffix="]")
        self.assertEqual(gate.check_external_references(
            trace, {tensor.name: tensor}), 1)
        trace.write_text(trace.read_text(encoding="utf-8") +
                         "safetensors://weights.safetensors#bad?offset=0\n",
                         encoding="utf-8")
        with self.assertRaisesRegex(ValueError, "unparsed or malformed"):
            gate.check_external_references(trace, {tensor.name: tensor})
        trace = self.make_trace(tensor, data=b"abcdefg")
        with self.assertRaisesRegex(ValueError, "range mismatch"):
            gate.check_external_references(trace, {tensor.name: tensor})

    def test_wrong_ckks_config_rejected(self):
        config = ("FHE Compilation Configuration Table:\n"
                  "  [1] scheme=ckks security=1 ring_dimension=65536 "
                  "depth=33 scale_bits=56 first_modulus_bits=60 "
                  "slots=32768 bootstrap=auto backend=openfhe flags=0x0\n")
        self.assertEqual(gate.check_config(config)["ring_dimension"],
                         "65536")
        with self.assertRaisesRegex(ValueError, "differs"):
            gate.check_config(config.replace("slots=32768", "slots=16384"))

    def test_approved_ranges_match_all_nineteen_identities(self):
        payload = self.root / "source.safetensors"
        payload.write_bytes(b"source payload")
        coefficient = self.root / "coefficient.json"
        coefficient.write_bytes(b"pinned coefficient bytes")
        trace = self.root / "source.T"
        trace.write_text(" LOC 1 12 source statement\n", encoding="utf-8")
        manifest_path = self.root / "ranges.json"
        events = {"events": [{"owner_pu_st": 12801,
                              "source_value_id": 7,
                              "context_pu_identity_id": 1,
                              "context_callsite_id": call}
                             for call in range(19)]}
        row = {"owner_pu_st": "<1,50>", "source_relu_value_id": 7,
               "context_pu_identity_id": 1, "bound_b": 2.0,
               "observed_abs_max": 1.0, "outlier_count": 0,
               "nonfinite_count": 0}
        manifest = {"schema": "open64.fhe.relu.context-ranges.v1",
                    "status": "approved",
                    "coefficient_manifest_sha256": gate.sha256(coefficient),
                    "source_artifact": {
                        "parameter_payload_sha256": gate.sha256(payload)},
                    "contexts": [dict(row, context_callsite_id=call)
                                 for call in range(19)]}
        with patch.object(gate, "read_trace", return_value=(None, None,
                                                           {1: ("OPR_DSLRELU", 7)})):
            self.write_sealed_manifest(manifest_path, manifest)
            self.assertEqual(gate.check_ranges(
                manifest_path, coefficient, payload, events, trace), 19)
            tampered = manifest_path.read_text(encoding="utf-8").replace(
                '"bound_b":2.0', '"bound_b":3.0', 1)
            manifest_path.write_text(tampered, encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "canonical-content hash"):
                gate.check_ranges(manifest_path, coefficient, payload,
                                  events, trace)
            manifest["contexts"][1]["context_callsite_id"] = 0
            self.write_sealed_manifest(manifest_path, manifest)
            with self.assertRaisesRegex(ValueError, "duplicate"):
                gate.check_ranges(manifest_path, coefficient, payload,
                                  events, trace)
            manifest["contexts"][1]["context_callsite_id"] = 1
            manifest["contexts"][1]["bound_b"] = 0
            self.write_sealed_manifest(manifest_path, manifest)
            with self.assertRaisesRegex(ValueError, "bound"):
                gate.check_ranges(manifest_path, coefficient, payload,
                                  events, trace)
            manifest["contexts"][1]["bound_b"] = 2
            manifest["source_artifact"]["parameter_payload_sha256"] = "0" * 64
            self.write_sealed_manifest(manifest_path, manifest)
            with self.assertRaisesRegex(ValueError, "different source payload"):
                gate.check_ranges(manifest_path, coefficient, payload,
                                  events, trace)
            manifest["source_artifact"]["parameter_payload_sha256"] = (
                gate.sha256(payload))
            manifest["coefficient_manifest_sha256"] = "0" * 64
            self.write_sealed_manifest(manifest_path, manifest)
            with self.assertRaisesRegex(ValueError, "coefficient manifest"):
                gate.check_ranges(manifest_path, coefficient, payload,
                                  events, trace)
            manifest["coefficient_manifest_sha256"] = gate.sha256(coefficient)
            manifest["status"] = "candidate"
            self.write_sealed_manifest(manifest_path, manifest)
            with self.assertRaisesRegex(ValueError, "not approved"):
                gate.check_ranges(manifest_path, coefficient, payload,
                                  events, trace)


if __name__ == "__main__":
    unittest.main()
