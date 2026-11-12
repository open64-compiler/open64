"""Dependency-free known-answer checks for the independent MLA oracle."""

import math
import unittest

from models.mla_reference import expanded_reference


class MLAReferenceTests(unittest.TestCase):
    def test_one_token_non_square_weights(self):
        output, lse = expanded_reference(
            [[[2.0]]], [[[3.0]]], [[[1.0, 2.0]]], [[[4.0]]],
            [[[5.0, 6.0]]], [[[7.0, 8.0], [9.0, 10.0]]], [[[True]]], 0.5)
        self.assertEqual(output, [[[23.0, 29.0]]])
        self.assertEqual(lse, [[23.0]])

    def test_uniform_scores_average_projected_values(self):
        output, lse = expanded_reference(
            [[[0.0]]], [[[0.0]]], [[[1.0], [3.0]]], [[[0.0], [0.0]]],
            [[[2.0]]], [[[4.0]]], [[[True, True]]], 1.0)
        self.assertEqual(output, [[[8.0]]])
        self.assertAlmostEqual(lse[0][0], math.log(2))

    def test_masked_value_excluded_and_all_masked_identity(self):
        args = ([[[1.0]]], [[[0.0]]], [[[2.0], [999.0]]],
                [[[0.0], [0.0]]], [[[1.0]]], [[[3.0]]])
        self.assertEqual(expanded_reference(*args, [[[True, False]]], 1.0),
                         ([[[6.0]]], [[2.0]]))
        self.assertEqual(expanded_reference(*args, [[[False, False]]], 1.0),
                         ([[[0.0]]], [[-math.inf]]))

    def test_large_logits_use_stable_softmax(self):
        output, lse = expanded_reference(
            [[[1000.0]]], [[[0.0]]], [[[1.0], [-1.0]]], [[[0.0], [0.0]]],
            [[[1.0]]], [[[2.0]]], [[[True, True]]], 1.0)
        self.assertEqual(output, [[[2.0]]])
        self.assertEqual(lse, [[1000.0]])


if __name__ == "__main__":
    unittest.main()
