from __future__ import annotations

import importlib.util
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from open64_dsc import export_to_whirl


TORCH_AVAILABLE = importlib.util.find_spec("torch") is not None


def residual_add(lhs, rhs):
    return lhs + rhs


if TORCH_AVAILABLE:
    import torch.fx

    torch.fx.wrap("residual_add")


@unittest.skipUnless(TORCH_AVAILABLE, "torch is not installed")
class Open64DscFxCaptureOptionalTest(unittest.TestCase):
    def _value_by_tensor_key(self, module, tensor_key):
        for value in module.values:
            if value.metadata.get("storage_tensor_key") == tensor_key:
                return value
        self.fail(f"missing external tensor value for {tensor_key}")

    def _value_by_name(self, module, name):
        for value in module.values:
            if value.name == name:
                return value
        self.fail(f"missing value {name}")

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
        self.assertEqual(module.graph_operators[1].kids, ["common.linear"])

    def test_fx_get_attr_parameters_become_external_tensor_operands(self) -> None:
        import torch
        import torch.nn.functional as F

        class LinearParameterModule(torch.nn.Module):
            def __init__(self):
                super().__init__()
                self.weight = torch.nn.Parameter(
                    torch.ones((4, 8), dtype=torch.float32)
                )
                self.bias = torch.nn.Parameter(
                    torch.ones((4,), dtype=torch.float32)
                )

            def forward(self, value):
                return F.linear(value, self.weight, self.bias)

        value = torch.ones((1, 8), dtype=torch.float32)
        module = export_to_whirl(
            LinearParameterModule(),
            [value],
        )

        self.assertEqual(
            module.operators,
            ["common.linear", "common.output_logits"],
        )
        self.assertEqual(
            module.graph_operators[0].kids,
            ["input0", "weight", "bias"],
        )
        self.assertEqual(module.values[1].value_kind, "external_data")
        self.assertEqual(module.values[1].metadata["tensor_role"], "weight")
        self.assertEqual(module.values[1].metadata["storage_format"], "safetensors")
        self.assertEqual(
            module.values[1].metadata["storage_file"],
            "LinearParameterModule.safetensors",
        )
        self.assertEqual(module.values[1].metadata["storage_tensor_key"], "weight")
        self.assertEqual(module.values[1].metadata["storage_byte_offset"], "0")
        self.assertEqual(module.values[1].metadata["storage_byte_length"], "128")
        self.assertEqual(module.values[2].metadata["tensor_role"], "bias")
        self.assertEqual(module.values[2].metadata["storage_byte_offset"], "128")
        self.assertEqual(module.values[2].metadata["storage_byte_length"], "16")

    def test_fx_call_module_resnet_stem_tail_maps_with_parameters(self) -> None:
        import torch

        class ModuleResnetStemTail(torch.nn.Module):
            def __init__(self):
                super().__init__()
                self.conv = torch.nn.Conv2d(
                    3,
                    64,
                    kernel_size=7,
                    stride=2,
                    padding=3,
                    bias=False,
                )
                self.bn = torch.nn.BatchNorm2d(64)
                self.relu = torch.nn.ReLU()
                self.pool = torch.nn.MaxPool2d(
                    kernel_size=3,
                    stride=2,
                    padding=1,
                )
                self.avgpool = torch.nn.AdaptiveAvgPool2d((1, 1))
                self.flatten = torch.nn.Flatten(1)
                self.fc = torch.nn.Linear(64, 1000)

            def forward(self, value):
                value = self.conv(value)
                value = self.bn(value)
                value = self.relu(value)
                value = self.pool(value)
                value = self.avgpool(value)
                value = self.flatten(value)
                return self.fc(value)

        value = torch.ones((1, 3, 224, 224), dtype=torch.float32)
        module = export_to_whirl(ModuleResnetStemTail().eval(), [value])

        self.assertEqual(
            module.operators,
            [
                "cnn.conv2d",
                "cnn.batch_norm_infer",
                "common.relu",
                "cnn.max_pool2d",
                "cnn.global_avg_pool2d",
                "common.flatten",
                "common.linear",
                "common.output_logits",
            ],
        )
        self.assertEqual(
            module.graph_operators[0].kids,
            ["input0", "conv_weight", "conv_bias"],
        )
        self.assertEqual(
            module.graph_operators[1].kids,
            [
                "cnn.conv2d",
                "bn_weight",
                "bn_bias",
                "bn_running_mean",
                "bn_running_var",
            ],
        )
        self.assertEqual(
            module.graph_operators[6].kids,
            ["common.flatten", "fc_weight", "fc_bias"],
        )
        self.assertEqual(module.values[1].metadata["storage_tensor_key"], "conv.weight")
        self.assertEqual(module.values[1].metadata["storage_byte_length"], "37632")
        self.assertEqual(module.values[2].value_kind, "absent_parameter")
        self.assertEqual(module.values[3].metadata["tensor_role"], "batchnorm_scale")
        self.assertEqual(
            module.values[5].metadata["tensor_role"],
            "batchnorm_running_mean",
        )
        self.assertEqual(module.values[-2].metadata["storage_tensor_key"], "fc.weight")
        self.assertEqual(module.values[-1].metadata["storage_tensor_key"], "fc.bias")
        self.assertEqual(
            module.graph_operators[-1].kids,
            ["common.linear"],
        )

    def test_fx_call_module_projection_residual_block(self) -> None:
        import torch

        class ProjectionBlock(torch.nn.Module):
            def __init__(self):
                super().__init__()
                self.main_conv = torch.nn.Conv2d(
                    64,
                    128,
                    kernel_size=3,
                    stride=2,
                    padding=1,
                    bias=False,
                )
                self.main_bn = torch.nn.BatchNorm2d(128)
                self.downsample = torch.nn.Sequential(
                    torch.nn.Conv2d(
                        64,
                        128,
                        kernel_size=1,
                        stride=2,
                        bias=False,
                    ),
                    torch.nn.BatchNorm2d(128),
                )

            def forward(self, value):
                main = self.main_bn(self.main_conv(value))
                shortcut = self.downsample(value)
                return residual_add(main, shortcut)

        value = torch.ones((1, 64, 56, 56), dtype=torch.float32)
        module = export_to_whirl(ProjectionBlock().eval(), [value])

        self.assertEqual(
            module.operators,
            [
                "cnn.conv2d",
                "cnn.batch_norm_infer",
                "cnn.conv2d",
                "cnn.batch_norm_infer",
                "common.residual_add",
            ],
        )
        self.assertEqual(
            module.graph_operators[-1].kids,
            ["cnn.batch_norm_infer", "cnn.batch_norm_infer"],
        )
        self.assertEqual(
            module.graph_operators[0].kids,
            ["input0", "main_conv_weight", "main_conv_bias"],
        )
        self.assertEqual(
            module.graph_operators[2].kids,
            ["input0", "downsample_0_weight", "downsample_0_bias"],
        )
        self.assertEqual(
            module.values[1].metadata["storage_tensor_key"],
            "main_conv.weight",
        )
        self.assertEqual(
            module.values[1].metadata["storage_byte_length"],
            "294912",
        )
        self.assertEqual(module.values[2].value_kind, "absent_parameter")
        self.assertEqual(
            module.values[7].metadata["storage_tensor_key"],
            "downsample.0.weight",
        )
        self.assertEqual(module.values[8].value_kind, "absent_parameter")
        self.assertEqual(
            module.values[9].metadata["tensor_role"],
            "batchnorm_scale",
        )
        self.assertEqual(
            module.values[11].metadata["tensor_role"],
            "batchnorm_running_mean",
        )

    def test_fx_call_module_basic_block_maps_resnet_residual_pattern(self) -> None:
        import torch

        class BasicBlock(torch.nn.Module):
            def __init__(self):
                super().__init__()
                self.conv1 = torch.nn.Conv2d(
                    64,
                    64,
                    kernel_size=3,
                    stride=1,
                    padding=1,
                    bias=False,
                )
                self.bn1 = torch.nn.BatchNorm2d(64)
                self.relu = torch.nn.ReLU()
                self.conv2 = torch.nn.Conv2d(
                    64,
                    64,
                    kernel_size=3,
                    stride=1,
                    padding=1,
                    bias=False,
                )
                self.bn2 = torch.nn.BatchNorm2d(64)

            def forward(self, value):
                residual = value
                out = self.conv1(value)
                out = self.bn1(out)
                out = self.relu(out)
                out = self.conv2(out)
                out = self.bn2(out)
                out = residual_add(out, residual)
                return self.relu(out)

        value = torch.ones((1, 64, 56, 56), dtype=torch.float32)
        module = export_to_whirl(BasicBlock().eval(), [value])

        self.assertEqual(
            module.operators,
            [
                "cnn.conv2d",
                "cnn.batch_norm_infer",
                "common.relu",
                "cnn.conv2d",
                "cnn.batch_norm_infer",
                "common.residual_add",
                "common.relu",
            ],
        )
        self.assertEqual(
            module.graph_operators[5].kids,
            ["cnn.batch_norm_infer", "input0"],
        )
        self.assertEqual(
            module.graph_operators[6].kids,
            ["common.residual_add"],
        )
        self.assertEqual(
            self._value_by_tensor_key(module, "conv1.weight")
                .metadata["storage_byte_length"],
            "147456",
        )
        self.assertEqual(
            self._value_by_name(module, "conv1_bias").value_kind,
            "absent_parameter",
        )
        self.assertEqual(
            self._value_by_tensor_key(module, "bn2.running_mean")
                .metadata["tensor_role"],
            "batchnorm_running_mean",
        )
        self.assertEqual(
            self._value_by_tensor_key(module, "bn2.running_var")
                .metadata["tensor_role"],
            "batchnorm_running_var",
        )

    def test_fx_call_module_stem_projection_tail_maps_vertical_slice(self) -> None:
        import torch

        class ProjectionBlock(torch.nn.Module):
            def __init__(self):
                super().__init__()
                self.conv1 = torch.nn.Conv2d(
                    64,
                    128,
                    kernel_size=3,
                    stride=2,
                    padding=1,
                    bias=False,
                )
                self.bn1 = torch.nn.BatchNorm2d(128)
                self.relu = torch.nn.ReLU()
                self.conv2 = torch.nn.Conv2d(
                    128,
                    128,
                    kernel_size=3,
                    stride=1,
                    padding=1,
                    bias=False,
                )
                self.bn2 = torch.nn.BatchNorm2d(128)
                self.downsample = torch.nn.Sequential(
                    torch.nn.Conv2d(
                        64,
                        128,
                        kernel_size=1,
                        stride=2,
                        bias=False,
                    ),
                    torch.nn.BatchNorm2d(128),
                )

            def forward(self, value):
                residual = self.downsample(value)
                out = self.conv1(value)
                out = self.bn1(out)
                out = self.relu(out)
                out = self.conv2(out)
                out = self.bn2(out)
                out = residual_add(out, residual)
                return self.relu(out)

        class StemProjectionTail(torch.nn.Module):
            def __init__(self):
                super().__init__()
                self.stem_conv = torch.nn.Conv2d(
                    3,
                    64,
                    kernel_size=7,
                    stride=2,
                    padding=3,
                    bias=False,
                )
                self.stem_bn = torch.nn.BatchNorm2d(64)
                self.relu = torch.nn.ReLU()
                self.pool = torch.nn.MaxPool2d(
                    kernel_size=3,
                    stride=2,
                    padding=1,
                )
                self.block = ProjectionBlock()
                self.avgpool = torch.nn.AdaptiveAvgPool2d((1, 1))
                self.flatten = torch.nn.Flatten(1)
                self.fc = torch.nn.Linear(128, 1000)

            def forward(self, value):
                out = self.stem_conv(value)
                out = self.stem_bn(out)
                out = self.relu(out)
                out = self.pool(out)
                out = self.block(out)
                out = self.avgpool(out)
                out = self.flatten(out)
                return self.fc(out)

        value = torch.ones((1, 3, 224, 224), dtype=torch.float32)
        module = export_to_whirl(StemProjectionTail().eval(), [value])

        self.assertEqual(
            module.operators,
            [
                "cnn.conv2d",
                "cnn.batch_norm_infer",
                "common.relu",
                "cnn.max_pool2d",
                "cnn.conv2d",
                "cnn.batch_norm_infer",
                "cnn.conv2d",
                "cnn.batch_norm_infer",
                "common.relu",
                "cnn.conv2d",
                "cnn.batch_norm_infer",
                "common.residual_add",
                "common.relu",
                "cnn.global_avg_pool2d",
                "common.flatten",
                "common.linear",
                "common.output_logits",
            ],
        )
        self.assertEqual(
            module.graph_operators[11].kids,
            ["cnn.batch_norm_infer", "cnn.batch_norm_infer"],
        )
        self.assertEqual(
            module.graph_operators[-1].kids,
            ["common.linear"],
        )
        self.assertEqual(
            self._value_by_tensor_key(module, "stem_conv.weight")
                .metadata["storage_byte_length"],
            "37632",
        )
        self.assertEqual(
            self._value_by_tensor_key(module, "block.downsample.0.weight")
                .metadata["storage_byte_length"],
            "32768",
        )
        self.assertEqual(
            self._value_by_tensor_key(module, "block.downsample.1.running_mean")
                .metadata["tensor_role"],
            "batchnorm_running_mean",
        )
        self.assertEqual(
            self._value_by_tensor_key(module, "fc.weight")
                .metadata["storage_byte_length"],
            "512000",
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
        self.assertEqual(
            module.graph_operators[1].kids,
            ["cnn.conv2d", "input3", "input4", "input5", "input6"],
        )
        self.assertEqual(
            module.graph_operators[4].kids,
            ["cnn.max_pool2d", "cnn.max_pool2d"],
        )
        self.assertEqual(
            module.graph_operators[7].kids,
            ["common.flatten", "input7", "input8"],
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

    def test_fx_training_batch_norm_fails_loudly(self) -> None:
        import torch
        import torch.nn.functional as F

        class TrainingBatchNormModule(torch.nn.Module):
            def forward(self, value, scale, bias, running_mean, running_var):
                return F.batch_norm(
                    value,
                    running_mean,
                    running_var,
                    scale,
                    bias,
                    training=True,
                )

        value = torch.ones((1, 64, 8, 8), dtype=torch.float32)
        scale = torch.ones((64,), dtype=torch.float32)
        bias = torch.ones((64,), dtype=torch.float32)
        running_mean = torch.ones((64,), dtype=torch.float32)
        running_var = torch.ones((64,), dtype=torch.float32)

        with self.assertRaisesRegex(NotImplementedError, "inference batchnorm"):
            export_to_whirl(
                TrainingBatchNormModule(),
                [value, scale, bias, running_mean, running_var],
            )

    def test_fx_batch_norm_missing_running_stats_fails_loudly(self) -> None:
        import torch
        import torch.nn.functional as F

        class MissingStatsBatchNormModule(torch.nn.Module):
            def forward(self, value, scale, bias):
                return F.batch_norm(
                    value,
                    None,
                    None,
                    scale,
                    bias,
                    training=False,
                )

        value = torch.ones((1, 64, 8, 8), dtype=torch.float32)
        scale = torch.ones((64,), dtype=torch.float32)
        bias = torch.ones((64,), dtype=torch.float32)

        with self.assertRaisesRegex(ValueError, "cnn.batch_norm_infer"):
            export_to_whirl(MissingStatsBatchNormModule(), [value, scale, bias])

    def test_fx_residual_add_shape_mismatch_fails_loudly(self) -> None:
        import torch

        class MismatchedResidualModule(torch.nn.Module):
            def forward(self, lhs, rhs):
                return residual_add(lhs, rhs)

        lhs = torch.ones((1, 64, 56, 56), dtype=torch.float32)
        rhs = torch.ones((1, 128, 28, 28), dtype=torch.float32)

        with self.assertRaisesRegex(ValueError, "exact shape match"):
            export_to_whirl(MismatchedResidualModule(), [lhs, rhs])

    def test_cli_exports_torch_model_file_with_shape_input(self) -> None:
        with tempfile.TemporaryDirectory() as work_dir:
            model_path = Path(work_dir) / "model.py"
            output_path = Path(work_dir) / "model.B"
            model_path.write_text(
                "\n".join(
                    [
                        "import torch",
                        "class AddModel(torch.nn.Module):",
                        "    def forward(self, value):",
                        "        return value + value",
                        "def create_model():",
                        "    return AddModel()",
                    ]
                ) + "\n",
                encoding="utf-8",
            )

            result = subprocess.run(
                [
                    sys.executable,
                    "-m",
                    "open64_dsc.cli",
                    str(model_path),
                    "--entry",
                    "forward",
                    "--sample-input",
                    "shape:1,3",
                    "-o",
                    str(output_path),
                ],
                check=False,
                stderr=subprocess.PIPE,
                stdout=subprocess.PIPE,
                text=True,
            )

            self.assertEqual(result.returncode, 0, result.stderr)
            text = output_path.read_text(encoding="utf-8")

        self.assertIn("format=mock", text)
        self.assertIn("model_name=AddModel", text)
        self.assertIn("entry_function=forward", text)
        self.assertIn("operator.0=common.add", text)
        self.assertIn("graph_operator.0=common.add:input0,input0", text)


if __name__ == "__main__":
    unittest.main()
