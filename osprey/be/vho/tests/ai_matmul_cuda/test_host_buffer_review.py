"""Verify host-adapter review artifacts remain joined to the selected plan."""

import json
from pathlib import Path
import tempfile
import unittest

from host_buffer_review import HERE, digest, publish_host_buffer_review


class HostBufferReviewTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        for name in ("source", "plans", "traces"):
            (self.root / name).mkdir()
        self.macros = dict(AI_TILE_PLAN_ID=4, AI_OPTIMIZATION_PLAN_ID=4,
                           M=128, N=128, K=32, BM=128, BN=128, BK=32, TM=16, TN=2)
        self.kernel = self.root / "source/matmul.generated.cu"
        self.kernel.write_text("".join(f"#define {key} {value}\n"
                                      for key, value in self.macros.items()))
        self.trace = self.root / "traces/compile.t"
        self.trace.write_text("selected-plan-fixture\n")
        self.handoff = dict(schema="ai-matmul-m2-handoff-v1",
                            whirl_rewrite_applied=True,
                            status="generated_runtime_call_unverified",
                            source_sha256=digest(self.kernel),
                            trace_sha256=digest(self.trace),
                            selected_tile_plan_id=4, selected_optimization_plan_id=4,
                            problem={key: self.macros[key] for key in ("M", "N", "K")},
                            tile={key: self.macros[key] for key in ("BM", "BN", "BK", "TM", "TN")})
        self.save_handoff()

    def save_handoff(self):
        (self.root / "plans/m2-selected-tile.json").write_text(json.dumps(self.handoff))

    def test_preserves_adapter_dependencies_and_provenance(self):
        record = publish_host_buffer_review(self.root)
        host = self.root / record["host_source_path"]
        self.assertEqual(host.read_bytes(), (HERE / "generated_runtime.cu").read_bytes())
        self.assertIn("cudaMalloc", host.read_text())
        self.assertIn("open64_target_buffer_bind_input", host.read_text())
        self.assertEqual(record["buffer_bytes"], dict(A=16384, B=16384, C=65536))
        self.assertFalse(record["compiler_generated_host_pu"])
        for relative, expected in record["files"].items():
            self.assertEqual(digest(self.root / relative), expected)
        self.assertTrue((self.root / "reports/host-buffer-review.log").read_text().startswith(
            "# What it summarizes:"))
        with self.assertRaisesRegex(ValueError, "already exists"):
            publish_host_buffer_review(self.root)

    def test_rejects_trace_tampering_before_publication(self):
        self.trace.write_text("changed trace\n")
        with self.assertRaisesRegex(ValueError, "disagrees"):
            publish_host_buffer_review(self.root)
        self.assertFalse((self.root / "source/matmul.host.cu").exists())

    def test_rejects_wrong_plan_even_with_updated_source_hash(self):
        self.kernel.write_text(self.kernel.read_text().replace(
            "#define AI_TILE_PLAN_ID 4", "#define AI_TILE_PLAN_ID 8"))
        self.handoff["source_sha256"] = digest(self.kernel)
        self.save_handoff()
        with self.assertRaisesRegex(ValueError, "AI_TILE_PLAN_ID"):
            publish_host_buffer_review(self.root)

    def test_rejects_unapplied_plan(self):
        self.handoff["whirl_rewrite_applied"] = False
        self.save_handoff()
        with self.assertRaisesRegex(ValueError, "applied selected plan"):
            publish_host_buffer_review(self.root)


if __name__ == "__main__":
    unittest.main()
