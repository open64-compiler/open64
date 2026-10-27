"""Analytic oracle checks that also run on hosts without PyTorch."""

import unittest

from models.building_block_reference import reference


class BuildingBlockReferenceTests(unittest.TestCase):
    def test_gemm_known_answer(self):
        self.assertEqual(reference("gemm", ([[1, 2]], [[3], [4]])), [[11]])

    def test_reduction_cancellation(self):
        self.assertEqual(reference("reduction", ([[1e16, 1, -1e16]],)), [1])

    def test_softmax_constant_and_large_shift(self):
        self.assertEqual(reference("softmax", ([[10000, 10000]],)), [[.5, .5]])

    def test_layernorm_constant_affine(self):
        self.assertEqual(reference("layernorm", ([[7, 7]], [2, 3], [4, 5])), [[4, 5]])

    def test_attention_single_allowed_key(self):
        qk = [[[[0], [0]]]]
        value = [[[[3], [7]]]]
        self.assertEqual(reference("attention", (qk, qk, value, [[True, False], [True, True]])),
                         [[[[3], [5]]]])

    def test_moe_duplicate_routes_accumulate(self):
        self.assertEqual(reference("moe", ([[2]], [[0, 0]], [[.25, .75]], [[[3]]], [[[4]]])),
                         [[24]])

    def test_unknown_block_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "unknown building block"):
            reference("unknown", ())
