"""Dependency-light ResNet fixture for torch2whirl certification."""

import torch
import torch.fx


def residual_add(lhs, rhs):
    return lhs + rhs


torch.fx.wrap("residual_add")


class BasicBlock(torch.nn.Module):
    def __init__(self, in_channels, out_channels, stride=1):
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
        if self.downsample is None:
            identity = value
        else:
            identity = self.downsample(value)
        out = self.conv1(value)
        out = self.bn1(out)
        out = self.relu(out)
        out = self.conv2(out)
        out = self.bn2(out)
        out = residual_add(out, identity)
        return self.relu(out)


class LocalResNet(torch.nn.Module):
    def __init__(self):
        super().__init__()
        self.conv1 = torch.nn.Conv2d(
            3,
            8,
            kernel_size=7,
            stride=2,
            padding=3,
            bias=False,
        )
        self.bn1 = torch.nn.BatchNorm2d(8)
        self.relu = torch.nn.ReLU()
        self.maxpool = torch.nn.MaxPool2d(
            kernel_size=3,
            stride=2,
            padding=1,
        )
        self.layer1 = torch.nn.Sequential(BasicBlock(8, 8))
        self.layer2 = torch.nn.Sequential(BasicBlock(8, 16, stride=2))
        self.avgpool = torch.nn.AdaptiveAvgPool2d((1, 1))
        self.flatten = torch.nn.Flatten(1)
        self.fc = torch.nn.Linear(16, 10)

    def forward(self, value):
        out = self.conv1(value)
        out = self.bn1(out)
        out = self.relu(out)
        out = self.maxpool(out)
        out = self.layer1(out)
        out = self.layer2(out)
        out = self.avgpool(out)
        out = self.flatten(out)
        return self.fc(out)


def create_model():
    return LocalResNet().eval()
