"""The emitted CUDA geometry must match the cost-selected tile."""

import unittest
from pathlib import Path
import tempfile

from compile_source import m2_handoff, validate_selected_source


SOURCE = """// tile_plan=3 optimization_plan=3 candidate=3 family=cuda_128
#define M 128
#define N 128
#define K 128
#define BM 128
#define BN 128
#define BK 32
#define TM 8
#define TN 8
"""
CANDIDATES = [dict(plan=3, tile=3, candidate=3, family="cuda_128",
                   legality="proven", cta=(128, 128, 32), threads=256,
                   shared=32768, registers=320, unit="relative",
                   confidence="low", total=4290, compute=4096,
                   memory=192, communication=0, sync=2, launch=0,
                   runtime=0, source_line=28)]
SELECTION = dict(selected=3, reason="lowest_complete_relative_cost",
                 version=7, semantic=[
    dict(rank=2, shape="[128,128]"),
    dict(rank=2, shape="[128,128]"),
    dict(rank=2, shape="[128,128]"),
])


class SourceContractTest(unittest.TestCase):
    def test_selected_tile_matches_source(self):
        validate_selected_source(SOURCE, CANDIDATES, SELECTION)

    def test_changed_geometry_is_rejected(self):
        with self.assertRaisesRegex(RuntimeError, "tile geometry"):
            validate_selected_source(SOURCE.replace("#define BK 32", "#define BK 16"),
                                     CANDIDATES, SELECTION)

    def test_changed_plan_identity_is_rejected(self):
        with self.assertRaisesRegex(RuntimeError, "plan identity"):
            validate_selected_source(SOURCE.replace("optimization_plan=3",
                                                    "optimization_plan=2"),
                                     CANDIDATES, SELECTION)

    def test_changed_problem_shape_is_rejected(self):
        with self.assertRaisesRegex(RuntimeError, "problem dimensions"):
            validate_selected_source(SOURCE.replace("#define K 128", "#define K 64"),
                                     CANDIDATES, SELECTION)

    def test_m2_handoff_preserves_plan_and_whirl_provenance(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for name, data in (("kernel.cu", SOURCE), ("compile.t", "trace"),
                               ("input.B", "before"), ("lowered.I", "after")):
                (root / name).write_text(data, encoding="utf-8")
            handoff = m2_handoff(root / "kernel.cu", root / "compile.t",
                                 root / "input.B", root / "lowered.I",
                                 CANDIDATES, SELECTION)
            self.assertEqual(handoff["selected_optimization_plan_id"], 3)
            self.assertEqual(handoff["tile"], dict(BM=128, BN=128, BK=32,
                                                    TM=8, TN=8))
            self.assertEqual(handoff["cost"]["total"], 4290)
            self.assertEqual(handoff["resources"]["register_bytes_per_thread"],
                             320)
            self.assertFalse(handoff["whirl_rewrite_applied"])
            self.assertEqual(len(handoff["source_sha256"]), 64)


if __name__ == "__main__":
    unittest.main()
