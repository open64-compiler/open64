"""The emitted CUDA geometry must match the cost-selected tile."""

import unittest
from pathlib import Path
import tempfile

from compile_source import (m2_handoff, selected_k_loop_dump,
                            validate_device_publication_order,
                            validate_device_whirl_output,
                            validate_selected_source)


SOURCE = """// tile_plan=3 optimization_plan=3 candidate=3 family=cuda_128
#define M 128
#define N 128
#define K 128
#define BM 128
#define BN 128
#define BK 32
#define TM 8
#define TN 8
#define AI_TILE_PLAN_ID 3
#define AI_OPTIMIZATION_PLAN_ID 3
// whirl_k_loop=DO_LOOP scope=detached_plan_only start=0 end=128 step=32
for (int base = 0; base < 128; base += 32) {
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
DEVICE_SOURCE = '''extern "C" __global__ void ai_matmul_tile_f32(
  _IEEE32 * a, _IEEE32 * b, _IEEE32 * c)
{
  __shared__ _IEEE32 as[4096LL];
  __shared__ _IEEE32 bs[4096LL];
  tid = (_INT32)((threadIdx).x);
  row0 = (_INT32)((blockIdx).y) * 128;
  for(base = 0; base < 128; base = base + 32) {
    __syncthreads();
    __syncthreads();
  }
}
'''
DEVICE_DUMP = '''FUNC_ENTRY <1,2,ai_matmul_tile_f32>
(DEVICE_SYMBOL) 1
(DEVICE_SYMBOL) 4
(DEVICE_SYMBOL) 4
DO_LOOP {line: 0}
'''


class SourceContractTest(unittest.TestCase):
    def test_device_publication_follows_tile_action(self):
        selection = dict(SELECTION, whirl_kernel=dict(
            plan=3, tile=3, image="matmul.device.B"))
        action = "AI_OPT 7 TILE_ACTION 1 3 source_sidecar generated_runtime_call\n"
        publication = "AI_OPT 7 WHIRL_KERNEL 1 3 3 matmul.device.B\n"
        validate_device_publication_order(action + publication, selection)
        with self.assertRaisesRegex(RuntimeError, "did not follow"):
            validate_device_publication_order(publication + action, selection)
        with self.assertRaisesRegex(RuntimeError, "did not follow"):
            validate_device_publication_order(action + publication * 2,
                                              selection)

    def test_device_whirl_matches_selected_tile(self):
        selected = dict(CANDIDATES[0], grid=dict(problem=(128, 128, 128)))
        selection = dict(SELECTION, whirl_kernel=dict(plan=3, tile=3))
        validate_device_whirl_output(DEVICE_SOURCE, DEVICE_DUMP,
                                     selection, selected)
        with self.assertRaisesRegex(RuntimeError, "selected tile"):
            validate_device_whirl_output(
                DEVICE_SOURCE.replace("base + 32", "base + 16"), DEVICE_DUMP,
                selection, selected)
        with self.assertRaisesRegex(RuntimeError, "shared staging"):
            validate_device_whirl_output(
                DEVICE_SOURCE, DEVICE_DUMP.replace("(DEVICE_SYMBOL) 4", "", 1),
                selection, selected)
        with self.assertRaisesRegex(RuntimeError, "plan identity"):
            validate_device_whirl_output(
                DEVICE_SOURCE, DEVICE_DUMP,
                dict(selection, whirl_kernel=dict(plan=2, tile=3)), selected)

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

    def test_changed_whirl_loop_is_rejected(self):
        with self.assertRaisesRegex(RuntimeError, "WHIRL loop"):
            validate_selected_source(SOURCE.replace("step=32", "step=16"),
                                     CANDIDATES, SELECTION)

    def test_changed_cuda_loop_is_rejected(self):
        with self.assertRaisesRegex(RuntimeError, "CUDA K loop"):
            validate_selected_source(SOURCE.replace("base += 32", "base += 16"),
                                     CANDIDATES, SELECTION)

    def test_traced_whirl_loop_is_required_in_source(self):
        selection = dict(SELECTION, k_loop=dict(start=0, end=128, step=32))
        validate_selected_source(SOURCE, CANDIDATES, selection)
        with self.assertRaisesRegex(RuntimeError, "WHIRL loop disagrees with trace"):
            validate_selected_source(
                SOURCE.replace("// whirl_k_loop=DO_LOOP", "// no_whirl_loop"),
                CANDIDATES, selection)

    def test_selected_grid_matches_generated_problem(self):
        selected = dict(CANDIDATES[0], grid=dict(
            problem=(128, 128, 128), grid=(1, 1), ctas=1, k_iterations=4))
        validate_selected_source(SOURCE, [selected], SELECTION)
        selected["grid"] = dict(selected["grid"], problem=(256, 128, 128))
        with self.assertRaisesRegex(RuntimeError, "problem grid"):
            validate_selected_source(SOURCE, [selected], SELECTION)

    def test_selected_whirl_loop_dump_is_retained(self):
        selection = dict(SELECTION, k_loop=dict(plan=3, tile=3,
                                               scope="detached_plan_only"))
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            trace = root / "compile.t"
            output = root / "selected-k-loop.T"
            trace.write_text(
                "AI_OPT 7 K_LOOP 1 3 3 0 128 32 detached_plan_only\n"
                "========== AI selected K-tile WHIRL plan loop ==========\n"
                "plan=3 tile=3 scope=detached_plan_only\n"
                "DO_LOOP\nEND_DO\n"
                "AI_OPT 7 TRANSFORM 1 3 generated_runtime_call\n",
                encoding="utf-8")
            dump = selected_k_loop_dump(trace, output, selection)
            self.assertEqual(output.read_text(encoding="utf-8"), dump)
            self.assertNotIn("TRANSFORM", dump)
            with self.assertRaisesRegex(RuntimeError, "incomplete"):
                selected_k_loop_dump(trace, output,
                                     dict(selection, k_loop=dict(plan=4, tile=3)))

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
