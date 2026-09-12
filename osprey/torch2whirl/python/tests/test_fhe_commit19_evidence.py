from __future__ import annotations

import unittest

import numpy
import torch

try:
    from .fhe_ace_resnet20_fixture import _batch_norm_path, _conv_module_path
    from .fhe_cifar10_commit19_protocol import _stratified_indices
    from .fhe_relu_commit19_accuracy import _chebyshev_clenshaw, _composite_relu
except ImportError:
    from fhe_ace_resnet20_fixture import _batch_norm_path, _conv_module_path
    from fhe_cifar10_commit19_protocol import _stratified_indices
    from fhe_relu_commit19_accuracy import _chebyshev_clenshaw, _composite_relu


class FHECommit19EvidenceTest(unittest.TestCase):
    def test_ace_conv_names_map_to_open64_modules(self) -> None:
        cases = {
            "/conv1/Conv": ("conv1", "bn1"),
            "/layer1/layer1.0/conv1/Conv": (
                "layer1.0.conv1", "layer1.0.bn1"
            ),
            "/layer2/layer2.0/downsample/downsample.0/Conv": (
                "layer2.0.downsample.0", "layer2.0.downsample.1"
            ),
        }
        for source, expected in cases.items():
            conv = _conv_module_path(source)
            self.assertEqual((conv, _batch_norm_path(conv)), expected)

    def test_stratified_selection_is_deterministic_and_excludes_prior(self) -> None:
        labels = numpy.repeat(numpy.arange(10), 20)
        first = _stratified_indices(labels, 4, 17)
        repeated = _stratified_indices(labels, 4, 17)
        second = _stratified_indices(labels, 4, 18, set(first))
        self.assertEqual(first, repeated)
        self.assertFalse(set(first) & set(second))
        for selected in (first, second):
            counts = numpy.bincount(labels[selected], minlength=10)
            self.assertEqual(counts.tolist(), [4] * 10)

    def test_clenshaw_matches_numpy_direct_chebyshev(self) -> None:
        values = numpy.linspace(-1.0, 1.0, 31)
        coefficients = [0.25, -0.5, 0.75, -0.125]
        expected = numpy.polynomial.chebyshev.chebval(values, coefficients)
        actual = _chebyshev_clenshaw(
            torch.tensor(values, dtype=torch.float64), coefficients
        ).numpy()
        numpy.testing.assert_allclose(actual, expected, rtol=0.0, atol=1.0e-14)

    def test_composite_relu_preserves_dtype_and_shape(self) -> None:
        values = torch.linspace(-1.0, 1.0, 17, dtype=torch.float32)
        identity = [[0.0, 1.0]] * 3
        result = _composite_relu(values, 1.0, identity)
        self.assertEqual(result.dtype, values.dtype)
        self.assertEqual(result.shape, values.shape)
        torch.testing.assert_close(result, 0.5 * values * values + 0.5 * values)


if __name__ == "__main__":
    unittest.main()
