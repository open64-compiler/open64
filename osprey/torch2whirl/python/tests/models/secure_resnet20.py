"""Deterministic ResNet-20/CIFAR-10 fixture for FHE capture certification."""

from __future__ import annotations

import torch
import torch.fx


def residual_add(lhs, rhs):
    return lhs + rhs


torch.fx.wrap("residual_add")


class ResNet20Block(torch.nn.Module):
    def __init__(self, in_channels: int, out_channels: int, stride: int = 1):
        super().__init__()
        self.conv1 = torch.nn.Conv2d(
            in_channels,
            out_channels,
            kernel_size=3,
            stride=stride,
            padding=1,
            bias=False,
        )
        self.bn1 = torch.nn.BatchNorm2d(out_channels)
        self.relu = torch.nn.ReLU()
        self.conv2 = torch.nn.Conv2d(
            out_channels,
            out_channels,
            kernel_size=3,
            stride=1,
            padding=1,
            bias=False,
        )
        self.bn2 = torch.nn.BatchNorm2d(out_channels)
        if stride != 1 or in_channels != out_channels:
            self.downsample = torch.nn.Sequential(
                torch.nn.Conv2d(
                    in_channels,
                    out_channels,
                    kernel_size=1,
                    stride=stride,
                    bias=False,
                ),
                torch.nn.BatchNorm2d(out_channels),
            )
        else:
            self.downsample = None

    def forward(self, value):
        identity = value if self.downsample is None else self.downsample(value)
        out = self.conv1(value)
        out = self.bn1(out)
        out = self.relu(out)
        out = self.conv2(out)
        out = self.bn2(out)
        out = residual_add(out, identity)
        return self.relu(out)


class SecureResNet20(torch.nn.Module):
    def __init__(self):
        super().__init__()
        self.conv1 = torch.nn.Conv2d(
            3,
            16,
            kernel_size=3,
            stride=1,
            padding=1,
            bias=False,
        )
        self.bn1 = torch.nn.BatchNorm2d(16)
        self.relu = torch.nn.ReLU()
        self.layer1 = self._make_layer(16, 16, 3, 1)
        self.layer2 = self._make_layer(16, 32, 3, 2)
        self.layer3 = self._make_layer(32, 64, 3, 2)
        self.avgpool = torch.nn.AdaptiveAvgPool2d((1, 1))
        self.flatten = torch.nn.Flatten(1)
        self.fc = torch.nn.Linear(64, 10)
        self._initialize_deterministic_parameters()

    def _make_layer(
        self,
        in_channels: int,
        out_channels: int,
        block_count: int,
        stride: int,
    ) -> torch.nn.Sequential:
        blocks = [ResNet20Block(in_channels, out_channels, stride)]
        for _ in range(1, block_count):
            blocks.append(ResNet20Block(out_channels, out_channels, 1))
        return torch.nn.Sequential(*blocks)

    def _initialize_deterministic_parameters(self) -> None:
        with torch.no_grad():
            for ordinal, parameter in enumerate(self.parameters()):
                values = torch.arange(
                    parameter.numel(),
                    dtype=torch.float32,
                ).reshape(parameter.shape)
                parameter.copy_((values.remainder(17) - 8.0) / 256.0)
            for ordinal, module in enumerate(self.modules()):
                if isinstance(module, torch.nn.BatchNorm2d):
                    module.running_mean.copy_(
                        torch.full_like(module.running_mean, ordinal / 1000.0)
                    )
                    module.running_var.copy_(
                        torch.full_like(module.running_var, 1.0 + ordinal / 1000.0)
                    )

    def forward(self, value):
        out = self.conv1(value)
        out = self.bn1(out)
        out = self.relu(out)
        out = self.layer1(out)
        out = self.layer2(out)
        out = self.layer3(out)
        out = self.avgpool(out)
        out = self.flatten(out)
        return self.fc(out)


def create_model():
    return SecureResNet20().eval()


def open64_sample_inputs():
    return (torch.ones((1, 3, 32, 32), dtype=torch.float32),)
