"""Smoke test for the C++ torch2whirl driver invoking open64_dsc.cli."""

from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import sys
import struct
import tempfile
from textwrap import dedent


def _require_torch() -> None:
    try:
        import torch  # noqa: F401
    except ImportError as exc:
        raise RuntimeError("torch is required for driver_torch_smoke.py") from exc


def _write_model(path: Path) -> None:
    path.write_text(
        dedent(
            """
            import torch


            class AddModel(torch.nn.Module):
                def forward(self, value):
                    return value + value


            def create_model():
                return AddModel()
            """
        ).lstrip(),
        encoding="utf-8",
    )


def _write_resnet_model(path: Path) -> None:
    path.write_text(
        dedent(
            """
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
                    self.layer2 = torch.nn.Sequential(
                        BasicBlock(8, 16, stride=2)
                    )
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
            """
        ).lstrip(),
        encoding="utf-8",
    )


def _write_invalid_add_model(path: Path) -> None:
    path.write_text(
        dedent(
            """
            import torch


            class BadAddModel(torch.nn.Module):
                def forward(self, lhs, rhs):
                    return lhs + rhs


            def create_model():
                return BadAddModel()
            """
        ).lstrip(),
        encoding="utf-8",
    )


def _check_artifact(path: Path) -> None:
    text = path.read_text(encoding="utf-8")
    expected_fragments = (
        "format=mock",
        "model_name=AddModel",
        "operator.0=common.add",
        "graph_operator.0=common.add:input0,input0",
    )
    for fragment in expected_fragments:
        if fragment not in text:
            raise AssertionError(
                f"missing {fragment!r} in driver output:\n{text}"
            )


def _check_resnet_artifact(path: Path) -> None:
    text = path.read_text(encoding="utf-8")
    expected_fragments = (
        "format=mock",
        "model_name=LocalResNet",
        "operator.0=cnn.conv2d",
        "operator.1=cnn.batch_norm_infer",
        "common.residual_add",
        "common.output_logits",
        "value_metadata.1=conv1_weight:external_tensor_constant",
        "graph_operator_attrs.0=attr.dilation=1,1",
    )
    for fragment in expected_fragments:
        if fragment not in text:
            raise AssertionError(
                f"missing {fragment!r} in driver ResNet output:\n{text}"
            )


def _check_resnet_side_file(path: Path) -> None:
    payload = path.read_bytes()
    header_length = struct.unpack("<Q", payload[:8])[0]
    header = json.loads(payload[8:8 + header_length].decode("utf-8"))
    data = payload[8 + header_length:]

    for tensor_key in ("conv1.weight", "bn1.running_mean", "fc.weight"):
        if tensor_key not in header:
            raise AssertionError(
                f"missing {tensor_key!r} in driver ResNet side file header"
            )
    if header["conv1.weight"]["dtype"] != "F32":
        raise AssertionError("driver ResNet conv1.weight dtype is not F32")
    if header["conv1.weight"]["shape"] != [8, 3, 7, 7]:
        raise AssertionError("driver ResNet conv1.weight shape mismatch")
    if header["fc.weight"]["shape"] != [10, 16]:
        raise AssertionError("driver ResNet fc.weight shape mismatch")
    if not data:
        raise AssertionError("driver ResNet side file has no tensor data")


def _run_driver(
    driver: Path,
    model_path: Path,
    output_path: Path,
    sample_inputs: tuple[str, ...],
) -> subprocess.CompletedProcess[str]:
    command = [
        str(driver),
        str(model_path),
        "--entry",
        "forward",
    ]
    for sample_input in sample_inputs:
        command.extend(["--sample-input", sample_input])
    command.extend(["--output", str(output_path)])
    return subprocess.run(
        command,
        check=False,
        env=os.environ.copy(),
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    )


def _check_invalid_graph_rejected(driver: Path, tmpdir: Path) -> None:
    model_path = tmpdir / "invalid_model.py"
    output_path = tmpdir / "invalid_model.B"
    _write_invalid_add_model(model_path)

    completed = _run_driver(
        driver,
        model_path,
        output_path,
        ("shape:1,3", "shape:1,4"),
    )
    if completed.returncode == 0:
        raise AssertionError("driver accepted verifier-invalid add graph")
    error_text = completed.stdout + completed.stderr
    if "shape" not in error_text:
        raise AssertionError(
            "driver rejection did not report verifier shape context:\n" +
            error_text
        )
    if output_path.exists():
        raise AssertionError("verifier-invalid driver run wrote an artifact")


def main() -> int:
    _require_torch()

    driver = Path(os.environ.get("TORCH2WHIRL_DRIVER", "torch2whirl"))
    if not driver.is_file():
        raise FileNotFoundError(f"torch2whirl driver not found: {driver}")

    with tempfile.TemporaryDirectory(prefix="torch2whirl-driver-") as tmp:
        tmpdir = Path(tmp)
        model_path = tmpdir / "model.py"
        output_path = tmpdir / "model.B"
        _write_model(model_path)

        completed = _run_driver(
            driver,
            model_path,
            output_path,
            ("shape:1,3",),
        )
        if completed.returncode != 0:
            sys.stderr.write(completed.stdout)
            sys.stderr.write(completed.stderr)
            return completed.returncode

        _check_artifact(output_path)
        _check_invalid_graph_rejected(driver, tmpdir)

        resnet_path = tmpdir / "resnet.py"
        resnet_output = tmpdir / "resnet.B"
        _write_resnet_model(resnet_path)
        completed = _run_driver(
            driver,
            resnet_path,
            resnet_output,
            ("shape:1,3,64,64",),
        )
        if completed.returncode != 0:
            sys.stderr.write(completed.stdout)
            sys.stderr.write(completed.stderr)
            return completed.returncode

        _check_resnet_artifact(resnet_output)
        side_file = tmpdir / "LocalResNet.safetensors"
        if not side_file.exists() or side_file.stat().st_size == 0:
            raise AssertionError("driver ResNet export did not write side file")
        _check_resnet_side_file(side_file)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
