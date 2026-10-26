"""Independent scalar-warp bank model and column permutation contracts."""

import unittest

from ai_opt_report import source_smem_work


class SharedBankModelTest(unittest.TestCase):
    def row(self, tm=8, tn=8, xor=False, padded=False):
        threads = (128 // tm) * (128 // tn)
        return dict(family="cuda_128", cta=(128, 128, 32), threads=threads,
                    layout=dict(thread_columns=128 // tn,
                                a_layout="transposed_padded" if padded else "row_major"),
                    smem=dict(b_layout="xor_columns" if xor else "row_major"),
                    grid=dict(ctas=1, k_iterations=1))

    def test_same_word_broadcast_not_lane_conflict(self):
        work = source_smem_work(self.row())
        self.assertEqual(work["a_read"], 32 * 8 * 8)
        self.assertEqual(work["b_read"], 3 * 32 * 8 * 8 * 8)

    def test_coalesced_map_has_two_b_rounds(self):
        work = source_smem_work(self.row(tm=16, tn=2))
        self.assertEqual(work["a_read"], 0)
        self.assertEqual(work["b_read"], 32 * 16 * 16 * 2)

    def test_xor_removes_b_conflicts_without_copy_conflicts(self):
        for tm, tn in ((8, 8), (16, 2)):
            work = source_smem_work(self.row(tm, tn, xor=True, padded=True))
            self.assertEqual(work["b_copy"], 0)
            self.assertEqual(work["b_read"], 0)
            self.assertEqual(work["a_read"], 0)
            self.assertGreater(work["address"], 0)

    def test_xor_is_bijection_and_preserves_32_column_group(self):
        for width in (32, 64, 128, 256, 512, 1024):
            mapped = [col ^ (col // 32) for col in range(width)]
            self.assertEqual(sorted(mapped), list(range(width)))
            self.assertTrue(all(col // 32 == physical // 32
                                for col, physical in enumerate(mapped)))

    def test_padded_copy_conflicts_are_charged(self):
        row = self.row(tm=4, tn=8, padded=True)
        row.update(cta=(64, 64, 16), threads=128)
        row["layout"]["thread_columns"] = 8
        work = source_smem_work(row)
        self.assertEqual(work["a_copy"], 32)
        self.assertEqual(work["b_copy"], 0)


if __name__ == "__main__":
    unittest.main()
