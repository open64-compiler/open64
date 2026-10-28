"""Independent descriptor, fragment and fail-closed WGMMA report regressions."""
import unittest
from wgmma_report import parse, pack, fragment, audit, host_source

TRACE = """AI_WGMMA 1 CONTRACT MMA4_H_V1 id=1 owner=12801 generation=1 contract=1 acceptance=1 freedoms=31 profile=1 arch=2 ptx=807 toolkit=12081 input=5 output=1 permissions=1 max_span=16 evidence_kind=1 evidence_id=12081 M=64 N=64 K=16 threads=128 smem=4096 LBO=128 SBO=256 swizzle=0 timing=unknown
AI_WGMMA 1 ACTION atom=m64n64k16 iterations=1 cta_tiles=1 proxy=1 barriers=2 register_fence=1 mma=1 commit=1 wait_zero=1 fragment_loads=32 fragment_stores=32 default=SIMT timing=unknown
"""


class WgmmaReportTests(unittest.TestCase):
    def test_exact_contract_and_dtype(self):
        self.assertEqual(parse(TRACE)["input"], 5)
        self.assertEqual(parse(TRACE.replace("input=5", "input=6").replace("profile=1", "profile=2"))["input"], 6)

    def test_no_implicit_permission_target_or_narrowing(self):
        for old, new in (("freedoms=31", "freedoms=1"), ("arch=2", "arch=1"), ("profile=1", "profile=3"),
                         ("input=5", "input=1"), ("output=1", "output=5"), ("ptx=807", "ptx=799"),
                         ("toolkit=12081", "toolkit=11999"), ("acceptance=1", "acceptance=0"),
                         ("timing=unknown", "timing=0"), ("permissions=1", "permissions=0"),
                         ("max_span=16", "max_span=1"), ("evidence_id=12081", "evidence_id=0")):
            with self.assertRaises(ValueError):
                parse(TRACE.replace(old, new))

    def test_shape_layout_participation_and_completion_fail_closed(self):
        for old, new in (("M=64", "M=63"), ("K=16", "K=15"), ("threads=128", "threads=64"),
                         ("LBO=128", "LBO=16"), ("SBO=256", "SBO=128"), ("swizzle=0", "swizzle=1"),
                         ("iterations=1", "iterations=2"), ("wait_zero=1", "wait_zero=0"),
                         ("default=SIMT", "default=WGMMA")):
            with self.assertRaises(ValueError):
                parse(TRACE.replace(old, new))

    def test_no_duplicate_or_missing_action_evidence(self):
        for trace in (TRACE + TRACE, TRACE.splitlines()[0], TRACE.splitlines()[1], TRACE.replace("id=1 owner", "id=1 id=1 owner")):
            with self.assertRaises(ValueError):
                parse(trace)

    def test_descriptor_packing_and_fragments_are_bijections(self):
        self.assertEqual({pack(mn, reduction) for mn in range(64) for reduction in range(16)}, set(range(1024)))
        self.assertEqual([pack(0, 0), pack(0, 8), pack(8, 0), pack(63, 15)], [0, 64, 128, 1023])
        coordinates = [fragment(thread, reg) for thread in range(128) for reg in range(32)]
        self.assertEqual(len(set(coordinates)), 4096)
        self.assertEqual(set(coordinates), {(row, column) for row in range(64) for column in range(64)})
        self.assertEqual([fragment(0, reg) for reg in range(4)], [(0, 0), (0, 1), (8, 0), (8, 1)])

    def test_labels_alone_do_not_prove_executable_lowering(self):
        with self.assertRaises(ValueError):
            audit("/* MMA4_H_V1 */", "LOC 1 1 COMMENT MMA4_H_V1", parse(TRACE))
        with self.assertRaises(ValueError):
            host_source("scalar_fma_loop();")


if __name__ == "__main__":
    unittest.main()
