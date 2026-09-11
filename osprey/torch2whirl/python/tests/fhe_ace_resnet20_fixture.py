"""Adapt the pinned pre-folded ACE ResNet-20 ONNX model for Open64 capture."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import sys
from typing import Optional, Sequence

import numpy
import torch


ACE_REVISION = "fb76131171b9f82aa6387f84dd73684fba5277e8"
ACE_ONNX_SHA256 = (
    "6627e8494884a7422fd8f2247469ae12cf8167bcabeda0033725387b8089f9d9"
)


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def canonical_json_bytes(value: object) -> bytes:
    return (
        json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True)
        + "\n"
    ).encode("ascii")


def _load_source_model(source: Path):
    spec = importlib.util.spec_from_file_location("secure_resnet20_ace", source)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"failed to load model source: {source}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.create_model().eval()


def _conv_module_path(node_name: str) -> str:
    parts = [part for part in node_name.split("/") if part]
    if not parts or parts[-1] != "Conv":
        raise RuntimeError(f"unexpected ACE Conv name: {node_name}")
    parts.pop()
    if len(parts) >= 2 and parts[1].startswith(parts[0] + "."):
        parts.pop(0)
    if len(parts) >= 2 and parts[-1].startswith(parts[-2] + "."):
        parts.pop(-2)
    return ".".join(parts)


def _batch_norm_path(conv_path: str) -> str:
    if conv_path == "conv1":
        return "bn1"
    if conv_path.endswith(".conv1"):
        return conv_path[:-5] + "bn1"
    if conv_path.endswith(".conv2"):
        return conv_path[:-5] + "bn2"
    if conv_path.endswith(".downsample.0"):
        return conv_path[:-1] + "1"
    raise RuntimeError(f"Conv has no Open64 BatchNorm partner: {conv_path}")


def _tensor_sha256(array: numpy.ndarray) -> str:
    return hashlib.sha256(numpy.ascontiguousarray(array).tobytes()).hexdigest()


def _assign_tensor(state: dict[str, torch.Tensor], key: str,
                   array: numpy.ndarray) -> None:
    if key not in state:
        raise RuntimeError(f"Open64 model state lacks {key}")
    value = torch.from_numpy(numpy.asarray(array).copy()).to(state[key].dtype)
    if value.shape != state[key].shape:
        raise RuntimeError(
            f"shape mismatch for {key}: ACE {tuple(value.shape)}, "
            f"Open64 {tuple(state[key].shape)}"
        )
    state[key] = value


def adapt_ace_model(onnx_path: Path, model_source: Path):
    import onnx
    from onnx import numpy_helper

    if file_sha256(onnx_path) != ACE_ONNX_SHA256:
        raise RuntimeError("pinned ACE ONNX SHA-256 does not match")
    graph = onnx.load(str(onnx_path))
    if graph.graph.name != "main_graph":
        raise RuntimeError("unexpected ACE ONNX graph identity")
    initializers = {
        item.name: numpy_helper.to_array(item) for item in graph.graph.initializer
    }
    model = _load_source_model(model_source)
    state = dict(model.state_dict())
    assigned: set[str] = set()
    conv_rows = []

    for node in graph.graph.node:
        if node.op_type != "Conv":
            continue
        if len(node.input) != 3:
            raise RuntimeError(f"ACE Conv lacks folded weight/bias: {node.name}")
        path = _conv_module_path(node.name)
        bn_path = _batch_norm_path(path)
        weight = initializers[node.input[1]]
        bias = initializers[node.input[2]]
        _assign_tensor(state, path + ".weight", weight)
        assigned.add(path + ".weight")

        bn_eps = float(dict(model.named_modules())[bn_path].eps)
        bn_scale = numpy.full(
            bias.shape, numpy.sqrt(1.0 + bn_eps), dtype=numpy.float32
        )
        bn_values = {
            bn_path + ".weight": bn_scale,
            bn_path + ".bias": bias,
            bn_path + ".running_mean": numpy.zeros_like(bias),
            bn_path + ".running_var": numpy.ones_like(bias),
            bn_path + ".num_batches_tracked": numpy.asarray(0, dtype=numpy.int64),
        }
        for key, value in bn_values.items():
            _assign_tensor(state, key, value)
            assigned.add(key)
        conv_rows.append({
            "onnx_node": node.name,
            "open64_conv_module": path,
            "open64_batch_norm_module": bn_path,
            "weight_initializer": node.input[1],
            "bias_initializer": node.input[2],
            "weight_sha256": _tensor_sha256(weight),
            "bias_sha256": _tensor_sha256(bias),
            "identity_bn_contract": (
                "running_mean=0;running_var=1;gamma=sqrt(1+epsilon);beta=ace_bias"
            ),
        })

    gemm = [node for node in graph.graph.node if node.op_type == "Gemm"]
    if len(gemm) != 1 or len(gemm[0].input) != 3:
        raise RuntimeError("ACE graph must contain one folded Gemm")
    _assign_tensor(state, "fc.weight", initializers[gemm[0].input[1]])
    _assign_tensor(state, "fc.bias", initializers[gemm[0].input[2]])
    assigned.update(("fc.weight", "fc.bias"))
    if set(state) != assigned:
        missing = sorted(set(state) - assigned)
        raise RuntimeError(f"ACE adaptation left Open64 state unassigned: {missing}")
    model.load_state_dict(state, strict=True)
    model.eval()
    return model, conv_rows


def _verify_onnx_equivalence(model, onnx_path: Path) -> dict[str, object]:
    import onnxruntime

    session = onnxruntime.InferenceSession(
        str(onnx_path), providers=["CPUExecutionProvider"]
    )
    samples = [
        numpy.ones((1, 3, 32, 32), dtype=numpy.float32),
        numpy.linspace(-1.0, 1.0, 3 * 32 * 32, dtype=numpy.float32).reshape(
            1, 3, 32, 32
        ),
    ]
    maximum = 0.0
    with torch.no_grad():
        for sample in samples:
            expected = session.run(None, {"input": sample})[0]
            actual = model(torch.from_numpy(sample.copy())).detach().numpy()
            maximum = max(maximum, float(numpy.max(numpy.abs(expected - actual))))
    if maximum > 1.0e-5:
        raise RuntimeError(
            f"Open64 identity-BN decomposition differs from ACE ONNX: {maximum}"
        )
    return {
        "sample_count": len(samples),
        "maximum_logit_absolute_difference": maximum,
        "threshold": 1.0e-5,
    }


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ace-onnx", type=Path, required=True)
    parser.add_argument("--model-source", type=Path, required=True)
    parser.add_argument("--checkpoint-output", type=Path, required=True)
    parser.add_argument("--manifest-output", type=Path, required=True)
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = _parser().parse_args(argv)
    model, conv_rows = adapt_ace_model(args.ace_onnx, args.model_source)
    equivalence = _verify_onnx_equivalence(model, args.ace_onnx)
    args.checkpoint_output.parent.mkdir(parents=True, exist_ok=True)
    torch.save({
        "state_dict": model.state_dict(),
        "open64_fixture": {
            "ace_revision": ACE_REVISION,
            "ace_onnx_sha256": ACE_ONNX_SHA256,
            "bn_decomposition": "synthetic_identity_for_open64_bn_fold_certification",
        },
    }, args.checkpoint_output)
    manifest = {
        "schema": "open64.fhe.ace-resnet20-fixture.v1",
        "status": "accepted_compiler_fixture",
        "ace_repository": "https://github.com/ant-research/ace-compiler",
        "ace_revision": ACE_REVISION,
        "ace_onnx_path": "model/resnet20_cifar10_pre.onnx",
        "ace_onnx_sha256": ACE_ONNX_SHA256,
        "open64_model_source_sha256": file_sha256(args.model_source),
        "open64_checkpoint_sha256": file_sha256(args.checkpoint_output),
        "conv_count": len(conv_rows),
        "conv_mappings": conv_rows,
        "equivalence": equivalence,
        "provenance_limitations": [
            "ACE does not publish the original training recipe or source checkpoint",
            "the ONNX graph already contains folded Conv weight and bias tensors",
            "Open64 introduces synthetic identity BatchNorm modules solely to exercise and verify its reviewed BN-fold transaction",
            "no claim is made that the synthetic BatchNorm parameters were trained or published by ACE",
        ],
    }
    args.manifest_output.write_bytes(canonical_json_bytes(manifest))
    print(
        f"adapted {len(conv_rows)} ACE Conv nodes; "
        f"max_logit_diff={equivalence['maximum_logit_absolute_difference']:.9g}"
    )
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, ValueError) as error:
        print(f"CFHECNN-PREPROCESS-001: {error}", file=sys.stderr)
        raise SystemExit(1)
