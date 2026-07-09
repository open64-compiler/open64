from __future__ import annotations

import importlib.util
import unittest

from open64_dsc import export_to_whirl


TORCH_AVAILABLE = importlib.util.find_spec("torch") is not None


def residual_add(lhs, rhs):
    return lhs + rhs


if TORCH_AVAILABLE:
    import torch.fx

    torch.fx.wrap("residual_add")


@unittest.skipUnless(TORCH_AVAILABLE, "torch is not installed")
class Open64DscFxCaptureOptionalTest(unittest.TestCase):
    def test_fx_add_maps_to_common_add(self) -> None:
        import torch

        class AddModule(torch.nn.Module):
            def forward(self, lhs, rhs):
                return lhs + rhs

        lhs = torch.ones((2, 3), dtype=torch.float32)
        rhs = torch.ones((2, 3), dtype=torch.float32)
        module = export_to_whirl(AddModule(), [lhs, rhs])

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["common.add"])
        self.assertEqual(module.entry_function.body_markers[-1], "common.add")
        self.assertEqual(module.graph_operators[0].kids, ["input0", "input1"])
        self.assertEqual(module.tensor_types[0].dtype, "float32")
        self.assertEqual(module.tensor_types[0].rank, 2)
        self.assertEqual(module.tensor_types[0].logical_shape, "[2,3]")

    def test_fx_matmul_maps_to_common_matmul(self) -> None:
        import torch

        class MatmulModule(torch.nn.Module):
            def forward(self, lhs, rhs):
                return torch.matmul(lhs, rhs)

        lhs = torch.ones((2, 3), dtype=torch.float32)
        rhs = torch.ones((3, 4), dtype=torch.float32)
        module = export_to_whirl(MatmulModule(), [lhs, rhs])

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["common.matmul"])
        self.assertEqual(
            module.entry_function.body_markers[-1],
            "common.matmul",
        )
        self.assertEqual(module.graph_operators[0].kids, ["input0", "input1"])
        self.assertEqual(
            module.graph_operators[0].attrs["attr.transpose_kid0"],
            "false",
        )

    def test_fx_residual_add_maps_to_common_residual_add(self) -> None:
        import torch

        class ResidualAddModule(torch.nn.Module):
            def forward(self, lhs, rhs):
                return residual_add(lhs, rhs)

        lhs = torch.ones((1, 64, 56, 56), dtype=torch.float32)
        rhs = torch.ones((1, 64, 56, 56), dtype=torch.float32)
        module = export_to_whirl(ResidualAddModule(), [lhs, rhs])

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["common.residual_add"])
        self.assertEqual(
            module.entry_function.body_markers[-1],
            "common.residual_add",
        )
        self.assertEqual(module.graph_operators[0].kids, ["input0", "input1"])
        self.assertEqual(
            module.graph_operators[0].attrs["attr.shape_check"],
            "exact",
        )

    def test_fx_linear_maps_to_common_linear(self) -> None:
        import torch
        import torch.nn.functional as F

        class LinearModule(torch.nn.Module):
            def forward(self, value, weight, bias):
                return F.linear(value, weight, bias)

        value = torch.ones((1, 2048), dtype=torch.float32)
        weight = torch.ones((1000, 2048), dtype=torch.float32)
        bias = torch.ones((1000,), dtype=torch.float32)
        module = export_to_whirl(LinearModule(), [value, weight, bias])

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(
            module.operators,
            ["common.linear", "common.output_logits"],
        )
        self.assertEqual(
            module.entry_function.body_markers[-1],
            "common.output_logits",
        )
        self.assertEqual(
            module.graph_operators[0].kids,
            ["input0", "input1", "input2"],
        )
        self.assertEqual(
            module.graph_operators[0].attrs["attr.has_bias"],
            "true",
        )
        self.assertEqual(
            module.graph_operators[0].attrs["attr.weight_layout"],
            "OI",
        )
        self.assertEqual(
            module.graph_operators[1].attrs["attr.semantic"],
            "classifier_logits",
        )

    def test_fx_resnet_like_sequence_gets_ordered_markers(self) -> None:
        import torch
        import torch.nn.functional as F

        class ResnetLikeModule(torch.nn.Module):
            def forward(
                self,
                value,
                conv_weight,
                conv_bias,
                bn_weight,
                bn_bias,
                running_mean,
                running_var,
                linear_weight,
                linear_bias,
            ):
                conv = F.conv2d(
                    value,
                    conv_weight,
                    conv_bias,
                    stride=1,
                    padding=1,
                )
                norm = F.batch_norm(
                    conv,
                    running_mean,
                    running_var,
                    bn_weight,
                    bn_bias,
                    training=False,
                )
                relu = torch.relu(norm)
                pool = F.max_pool2d(
                    relu,
                    kernel_size=3,
                    stride=2,
                    padding=1,
                )
                residual = residual_add(pool, pool)
                pooled = F.adaptive_avg_pool2d(residual, (1, 1))
                flattened = torch.flatten(pooled, 1)
                return F.linear(flattened, linear_weight, linear_bias)

        value = torch.ones((1, 3, 32, 32), dtype=torch.float32)
        conv_weight = torch.ones((64, 3, 3, 3), dtype=torch.float32)
        conv_bias = torch.ones((64,), dtype=torch.float32)
        bn_weight = torch.ones((64,), dtype=torch.float32)
        bn_bias = torch.ones((64,), dtype=torch.float32)
        running_mean = torch.ones((64,), dtype=torch.float32)
        running_var = torch.ones((64,), dtype=torch.float32)
        linear_weight = torch.ones((1000, 64), dtype=torch.float32)
        linear_bias = torch.ones((1000,), dtype=torch.float32)
        module = export_to_whirl(
            ResnetLikeModule(),
            [
                value,
                conv_weight,
                conv_bias,
                bn_weight,
                bn_bias,
                running_mean,
                running_var,
                linear_weight,
                linear_bias,
            ],
        )

        expected_operators = [
            "cnn.conv2d",
            "cnn.batch_norm_infer",
            "common.relu",
            "cnn.max_pool2d",
            "common.residual_add",
            "cnn.global_avg_pool2d",
            "common.flatten",
            "common.linear",
            "common.output_logits",
        ]
        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, expected_operators)
        self.assertEqual(
            module.entry_function.body_markers[-len(expected_operators):],
            expected_operators,
        )
        self.assertEqual(
            module.graph_operators[-1].attrs["attr.semantic"],
            "classifier_logits",
        )

    def test_fx_relu_maps_to_common_relu(self) -> None:
        import torch

        class ReluModule(torch.nn.Module):
            def forward(self, value):
                return torch.relu(value)

        value = torch.ones((2, 3), dtype=torch.float32)
        module = export_to_whirl(ReluModule(), [value])

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["common.relu"])
        self.assertEqual(module.entry_function.body_markers[-1], "common.relu")
        self.assertEqual(module.graph_operators[0].kids, ["input0"])
        self.assertEqual(module.graph_operators[0].attrs, {})

    def test_fx_flatten_maps_to_common_flatten(self) -> None:
        import torch

        class FlattenModule(torch.nn.Module):
            def forward(self, value):
                return torch.flatten(value, 1)

        value = torch.ones((2, 3, 4), dtype=torch.float32)
        module = export_to_whirl(FlattenModule(), [value])

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["common.flatten"])
        self.assertEqual(module.entry_function.body_markers[-1], "common.flatten")
        self.assertEqual(module.graph_operators[0].kids, ["input0"])
        self.assertEqual(
            module.graph_operators[0].attrs["attr.start_dim"],
            "1",
        )

    def test_fx_max_pool2d_maps_to_cnn_max_pool2d(self) -> None:
        import torch
        import torch.nn.functional as F

        class MaxPoolModule(torch.nn.Module):
            def forward(self, value):
                return F.max_pool2d(value, kernel_size=3, stride=2, padding=1)

        value = torch.ones((1, 3, 8, 8), dtype=torch.float32)
        module = export_to_whirl(MaxPoolModule(), [value])

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["cnn.max_pool2d"])
        self.assertEqual(
            module.entry_function.body_markers[-1],
            "cnn.max_pool2d",
        )
        self.assertEqual(module.graph_operators[0].kids, ["input0"])
        self.assertEqual(
            module.graph_operators[0].attrs["attr.kernel_shape"],
            "3,3",
        )

    def test_fx_adaptive_avg_pool2d_maps_to_cnn_global_avg_pool2d(self) -> None:
        import torch
        import torch.nn.functional as F

        class GlobalAvgPoolModule(torch.nn.Module):
            def forward(self, value):
                return F.adaptive_avg_pool2d(value, (1, 1))

        value = torch.ones((1, 3, 8, 8), dtype=torch.float32)
        module = export_to_whirl(GlobalAvgPoolModule(), [value])

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["cnn.global_avg_pool2d"])
        self.assertEqual(
            module.entry_function.body_markers[-1],
            "cnn.global_avg_pool2d",
        )
        self.assertEqual(module.graph_operators[0].kids, ["input0"])
        self.assertEqual(
            module.graph_operators[0].attrs["attr.output_size"],
            "1,1",
        )

    def test_fx_conv2d_maps_to_cnn_conv2d_with_static_attrs(self) -> None:
        import torch
        import torch.nn.functional as F

        class Conv2dModule(torch.nn.Module):
            def forward(self, value, weight, bias):
                return F.conv2d(
                    value,
                    weight,
                    bias,
                    stride=2,
                    padding=3,
                    dilation=1,
                    groups=1,
                )

        value = torch.ones((1, 3, 224, 224), dtype=torch.float32)
        weight = torch.ones((64, 3, 7, 7), dtype=torch.float32)
        bias = torch.ones((64,), dtype=torch.float32)
        module = export_to_whirl(Conv2dModule(), [value, weight, bias])

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["cnn.conv2d"])
        self.assertEqual(module.entry_function.body_markers[-1], "cnn.conv2d")
        self.assertEqual(
            module.graph_operators[0].kids,
            ["input0", "input1", "input2"],
        )
        self.assertEqual(
            module.graph_operators[0].attrs["attr.stride"],
            "2,2",
        )
        self.assertEqual(
            module.graph_operators[0].attrs["attr.padding"],
            "3,3",
        )
        self.assertEqual(
            module.graph_operators[0].attrs["attr.weight_layout"],
            "OIHW",
        )

    def test_fx_batch_norm_maps_to_cnn_batch_norm_infer(self) -> None:
        import torch
        import torch.nn.functional as F

        class BatchNormModule(torch.nn.Module):
            def forward(self, value, scale, bias, running_mean, running_var):
                return F.batch_norm(
                    value,
                    running_mean,
                    running_var,
                    scale,
                    bias,
                    training=False,
                    momentum=0.1,
                    eps=1e-5,
                )

        value = torch.ones((1, 64, 8, 8), dtype=torch.float32)
        scale = torch.ones((64,), dtype=torch.float32)
        bias = torch.ones((64,), dtype=torch.float32)
        running_mean = torch.ones((64,), dtype=torch.float32)
        running_var = torch.ones((64,), dtype=torch.float32)
        module = export_to_whirl(
            BatchNormModule(),
            [value, scale, bias, running_mean, running_var],
        )

        self.assertEqual(module.graph_source, "torch.fx")
        self.assertEqual(module.operators, ["cnn.batch_norm_infer"])
        self.assertEqual(
            module.entry_function.body_markers[-1],
            "cnn.batch_norm_infer",
        )
        self.assertEqual(
            module.graph_operators[0].kids,
            ["input0", "input1", "input2", "input3", "input4"],
        )
        self.assertEqual(
            module.graph_operators[0].attrs["attr.epsilon"],
            "1e-05",
        )
        self.assertEqual(
            module.graph_operators[0].attrs["attr.training"],
            "false",
        )

    def test_fx_unsupported_operator_fails_loudly(self) -> None:
        import torch

        class MulModule(torch.nn.Module):
            def forward(self, lhs, rhs):
                return lhs * rhs

        lhs = torch.ones((2, 3), dtype=torch.float32)
        rhs = torch.ones((2, 3), dtype=torch.float32)

        with self.assertRaisesRegex(NotImplementedError, "unsupported FX"):
            export_to_whirl(MulModule(), [lhs, rhs])


if __name__ == "__main__":
    unittest.main()
