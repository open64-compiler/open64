"""Optional ir_b2a smoke test for native Python-produced WHIRL artifacts."""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Optional

from open64_dsc import (
    WhirlExportOptions,
    export_to_whirl,
    load_builder,
    save_as_whirl,
)
from open64_dsc.builder import ProgramUnitHandle, ValueHandle


class DummyModel:
    pass


def _append_operator_probes(module) -> None:
    builder = load_builder("native")
    lhs = ValueHandle(module.values[0].handle)
    rhs = ValueHandle(module.values[1].handle)
    matmul = builder.common_matmul(lhs, rhs)
    residual_add = builder.common_residual_add(lhs, rhs)
    linear_weight = builder.external_tensor_constant(
        "linear_weight",
        "float32",
        2,
        "[1,1]",
        "weight",
        "safetensors",
        "resnet.safetensors",
        "fc.weight",
        0,
        4,
        "a" * 64,
        "OI",
    )
    linear_bias = builder.external_tensor_constant(
        "linear_bias",
        "float32",
        1,
        "[1]",
        "bias",
        "safetensors",
        "resnet.safetensors",
        "fc.bias",
        4,
        4,
        "b" * 64,
        "C",
    )
    linear = builder.common_linear(lhs, linear_weight, linear_bias)
    relu = builder.common_relu(lhs)
    flatten = builder.common_flatten(lhs)
    output_logits = builder.common_output_logits(lhs)
    max_pool2d = builder.cnn_max_pool2d(lhs)
    global_avg_pool2d = builder.cnn_global_avg_pool2d(lhs)
    conv_weight = builder.external_tensor_constant(
        "conv_weight",
        "float32",
        4,
        "[1,1,1,1]",
        "weight",
        "safetensors",
        "resnet.safetensors",
        "conv.weight",
        8,
        4,
        "c" * 64,
        "OIHW",
    )
    conv_bias = builder.external_tensor_constant(
        "conv_bias",
        "float32",
        1,
        "[1]",
        "bias",
        "safetensors",
        "resnet.safetensors",
        "conv.bias",
        12,
        4,
        "d" * 64,
        "C",
    )
    conv2d = builder.cnn_conv2d(
        lhs,
        conv_weight,
        conv_bias,
        {
            "attr.kernel_shape": "3,3",
            "attr.stride": "1,1",
            "attr.padding": "1,1",
            "attr.dilation": "1,1",
            "attr.groups": "1",
            "attr.input_layout": "NCHW",
            "attr.weight_layout": "OIHW",
            "attr.output_layout": "NCHW",
        },
    )
    bn_scale = builder.external_tensor_constant(
        "bn_scale",
        "float32",
        1,
        "[1]",
        "batchnorm_scale",
        "safetensors",
        "resnet.safetensors",
        "bn.weight",
        16,
        4,
        "e" * 64,
        "C",
    )
    bn_bias = builder.external_tensor_constant(
        "bn_bias",
        "float32",
        1,
        "[1]",
        "batchnorm_bias",
        "safetensors",
        "resnet.safetensors",
        "bn.bias",
        20,
        4,
        "f" * 64,
        "C",
    )
    bn_running_mean = builder.external_tensor_constant(
        "bn_running_mean",
        "float32",
        1,
        "[1]",
        "batchnorm_running_mean",
        "safetensors",
        "resnet.safetensors",
        "bn.running_mean",
        24,
        4,
        "1" * 64,
        "C",
    )
    bn_running_var = builder.external_tensor_constant(
        "bn_running_var",
        "float32",
        1,
        "[1]",
        "batchnorm_running_var",
        "safetensors",
        "resnet.safetensors",
        "bn.running_var",
        28,
        4,
        "2" * 64,
        "C",
    )
    batch_norm = builder.cnn_batch_norm_infer(
        lhs,
        bn_scale,
        bn_bias,
        bn_running_mean,
        bn_running_var,
    )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        matmul,
    )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        residual_add,
    )
    for parameter in (
        linear_weight,
        linear_bias,
        conv_weight,
        conv_bias,
        bn_scale,
        bn_bias,
        bn_running_mean,
        bn_running_var,
    ):
        builder.append_program_unit_marker(
            ProgramUnitHandle(module.entry_function.handle),
            parameter,
        )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        linear,
    )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        relu,
    )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        flatten,
    )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        output_logits,
    )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        max_pool2d,
    )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        global_avg_pool2d,
    )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        conv2d,
    )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        batch_norm,
    )


def _find_ir_b2a() -> Optional[Path]:
    configured = os.environ.get("OPEN64_IR_B2A", "")
    if configured:
        path = Path(configured)
        if path.is_file() and os.access(str(path), os.X_OK):
            return path
        print(f"skip: ir_b2a is not executable: {path}")
        return None

    found = shutil.which("ir_b2a")
    if found:
        return Path(found)

    print("skip: ir_b2a not found; set OPEN64_IR_B2A to enable smoke test")
    return None


def main() -> int:
    ir_b2a = _find_ir_b2a()
    if ir_b2a is None:
        return 0

    with tempfile.TemporaryDirectory() as work_dir_text:
        work_dir = Path(work_dir_text)
        artifact = work_dir / "python_native_model.B"
        text_dump = work_dir / "python_native_model.st.ir"

        try:
            module = export_to_whirl(
                DummyModel(),
                [object(), object()],
                WhirlExportOptions(backend="native", model_name="python_native"),
            )
            _append_operator_probes(module)
        except RuntimeError as exc:
            if "does not support" in str(exc):
                print(f"skip: native backend capability missing: {exc}")
                return 0
            raise
        save_as_whirl(module, str(artifact))
        if not artifact.exists() or artifact.stat().st_size == 0:
            print("native Python WHIRL artifact was not created", file=sys.stderr)
            return 1

        result = subprocess.run(
            [str(ir_b2a), "-st", str(artifact), str(text_dump)],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        if result.returncode != 0:
            print(result.stdout, file=sys.stderr)
            print(result.stderr, file=sys.stderr)
            return result.returncode

        text = text_dump.read_text(encoding="utf-8", errors="replace")
        required = [
            "FUNC_ENTRY",
            "common.tensor_const",
            "common.add",
            "common.matmul",
            "common.residual_add",
            "common.linear",
            "common.relu",
            "common.flatten",
            "common.output_logits",
            "cnn.max_pool2d",
            "cnn.global_avg_pool2d",
            "cnn.conv2d",
            "cnn.batch_norm_infer",
            "attr.broadcast_rule=none",
            "attr.start_dim=1",
            "attr.transpose_kid0=false",
            "attr.transpose_kid1=false",
            "attr.shape_check=exact",
            "attr.has_bias=true",
            "attr.weight_layout=OI",
            "value_kind=external_data",
            "attr.kernel_shape=3,3",
            "attr.output_size=1,1",
            "attr.groups=1",
            "attr.weight_layout=OIHW",
            "attr.epsilon=1e-05",
            "attr.training=false",
            "Symbols:",
            "Types:",
        ]
        missing = [needle for needle in required if needle not in text]
        if missing:
            print(
                "ir_b2a -st output missed expected text: " +
                ", ".join(missing),
                file=sys.stderr,
            )
            print(text, file=sys.stderr)
            return 1

    print("native Python ir_b2a smoke passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
