"""Independent numerical checks for the ReLU-free BN-fold checkpoint."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import struct
import sys

import torch


def _load_source_model(source: Path):
    spec = importlib.util.spec_from_file_location("bn_fold_model", source)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load model source: {source}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module.create_model()


def _read_safetensors(path: Path) -> dict[str, torch.Tensor]:
    data = path.read_bytes()
    if len(data) < 8:
        raise RuntimeError(f"truncated SafeTensors file: {path}")
    header_size = struct.unpack("<Q", data[:8])[0]
    header_end = 8 + header_size
    header = json.loads(data[8:header_end].decode("utf-8"))
    tensors: dict[str, torch.Tensor] = {}
    for name, record in header.items():
        if name == "__metadata__":
            continue
        if record["dtype"] != "F32":
            raise RuntimeError(f"unexpected dtype for {name}: {record['dtype']}")
        begin, end = record["data_offsets"]
        raw = data[header_end + begin:header_end + end]
        values = [item[0] for item in struct.iter_unpack("<f", raw)]
        tensors[name] = torch.tensor(values, dtype=torch.float32).reshape(
            record["shape"]
        )
    return tensors


def _expected_fold(conv, bn):
    weight = conv.weight.detach().double()
    bias = (
        conv.bias.detach().double()
        if conv.bias is not None
        else torch.zeros_like(bn.running_mean, dtype=torch.float64)
    )
    scale = bn.weight.detach().double()
    factor = scale / torch.sqrt(bn.running_var.detach().double() + bn.eps)
    folded_weight = weight * factor.reshape((-1,) + (1,) * (weight.ndim - 1))
    folded_bias = (
        bn.bias.detach().double()
        + (bias - bn.running_mean.detach().double()) * factor
    )
    return folded_weight.float(), folded_bias.float()


def _check_pair(tensors, prefix, conv, bn):
    expected_weight, expected_bias = _expected_fold(conv, bn)
    actual_weight = tensors[f"{prefix}_folded_weight"]
    actual_bias = tensors[f"{prefix}_folded_bias"]
    if not torch.equal(actual_weight, expected_weight):
        error = torch.max(torch.abs(actual_weight - expected_weight)).item()
        raise AssertionError(f"{prefix} folded weight mismatch: {error}")
    if not torch.equal(actual_bias, expected_bias):
        error = torch.max(torch.abs(actual_bias - expected_bias)).item()
        raise AssertionError(f"{prefix} folded bias mismatch: {error}")


def certify(artifact_dir: Path) -> None:
    source_payload = artifact_dir / "secure_resnet20.safetensors"
    converted_payload = artifact_dir / "secure_resnet20.fhe.safetensors"
    report = artifact_dir / "secure_resnet20.fhe.conversion-report.txt"
    trace = artifact_dir / "secure_resnet20.fhe.T"
    binary = artifact_dir / "secure_resnet20.fhe.B"
    for path in (source_payload, converted_payload, report, trace, binary):
        if not path.is_file():
            raise AssertionError(f"missing BN-fold certification artifact: {path}")

    model = _load_source_model(artifact_dir / "secure_resnet20.py")
    source_tensors = _read_safetensors(source_payload)
    tensors = _read_safetensors(converted_payload)
    state = model.state_dict()
    for name, tensor in source_tensors.items():
        if name not in state or not torch.equal(tensor, state[name].float()):
            raise AssertionError(f"source payload changed or mismatches model: {name}")
    _check_pair(tensors, "entry_stem_conv", model.conv1, model.bn1)
    paths = [
        "layer1.0", "layer1.1", "layer1.2",
        "layer2.0", "layer2.1", "layer2.2",
        "layer3.0", "layer3.1", "layer3.2",
    ]
    for ordinal, path in enumerate(paths, 1):
        block = model.get_submodule(path)
        prefix = f"call{ordinal}_cnn_basic_block"
        _check_pair(tensors, f"{prefix}_conv1", block.conv1, block.bn1)
        _check_pair(tensors, f"{prefix}_conv2", block.conv2, block.bn2)
        if block.downsample is not None:
            _check_pair(
                tensors,
                f"{prefix}_downsample_conv",
                block.downsample[0],
                block.downsample[1],
            )

    if len(tensors) != 42:
        raise AssertionError(f"expected 42 folded tensors, found {len(tensors)}")
    text = report.read_text(encoding="utf-8")
    for evidence in (
        "rewritten_values=13",
        "folded_batch_norm=21",
        "context_folds=21",
        "converted_tensors=42",
    ):
        if evidence not in text:
            raise AssertionError(f"missing report evidence: {evidence}")
    dump = trace.read_text(encoding="utf-8", errors="replace")
    if dump.count("status=retired redirected_to=") < 13:
        raise AssertionError("converted trace lacks 13 retired BN values")
    if "common.relu" in dump:
        raise AssertionError("BN-only certification unexpectedly contains ReLU")
    rewritten_arguments = re.findall(
        r"argument_value=(\d+) role=cnn\.basic_block\.(?:conv[12]|"
        r"downsample\.conv)\.(?:weight|bias)",
        dump,
    )
    if len(rewritten_arguments) != 40 or len(set(rewritten_arguments)) != 40:
        raise AssertionError(
            "expected 40 distinct rewritten shared-callee weight/bias actuals"
        )
    for value_id in rewritten_arguments:
        pattern = rf"\[{value_id}\] kind=constant name=[^\n]*_folded_(?:weight|bias)"
        if re.search(pattern, dump) is None:
            raise AssertionError(
                f"rewritten call actual value{value_id} is not a folded constant"
            )
    if dump.count("dsl.converted_from_value_id = ") != 42:
        raise AssertionError("converted tensor lineage count is not 42")

    shared_hashes = {
        hashlib.sha256(
            tensors[f"call{ordinal}_cnn_basic_block_conv1_folded_weight"]
            .numpy().tobytes()
        ).hexdigest()
        for ordinal in (1, 2, 3)
    }
    if len(shared_hashes) < 2:
        raise AssertionError("shared-PU contexts did not retain distinct payloads")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("artifact_dir", type=Path)
    args = parser.parse_args()
    certify(args.artifact_dir)
    print(f"certified ReLU-free BN-fold artifacts: {args.artifact_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
