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


class InputTensor:
    shape = (1, 1, 4, 4)


def _append_operator_probes(module) -> None:
    builder = load_builder("native")
    lhs = ValueHandle(module.values[0].handle)
    rhs = ValueHandle(module.values[1].handle)
    matmul_lhs = builder.tensor_constant(
        "matmul_lhs",
        "float32",
        2,
        "[2,3]",
        "splat",
        "1.0",
    )
    matmul_rhs = builder.tensor_constant(
        "matmul_rhs",
        "float32",
        2,
        "[3,4]",
        "splat",
        "1.0",
    )
    common_input = builder.tensor_constant(
        "common_input",
        "float32",
        2,
        "[1,4]",
        "splat",
        "1.0",
    )
    linear_input = builder.tensor_constant(
        "linear_input",
        "float32",
        2,
        "[1,1]",
        "splat",
        "1.0",
    )
    cnn_input = builder.tensor_constant(
        "cnn_input",
        "float32",
        4,
        "[1,1,4,4]",
        "splat",
        "1.0",
    )
    matmul = builder.common_matmul(matmul_lhs, matmul_rhs)
    residual_add = builder.common_residual_add(lhs, rhs)
    linear_input = builder.tensor_constant(
        "linear_input", "float32", 2, "[1,1]", "splat", "1.0"
    )
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
    linear = builder.common_linear(linear_input, linear_weight, linear_bias)
    relu = builder.common_relu(common_input)
    flatten = builder.common_flatten(common_input)
    output_logits = builder.common_output_logits(common_input)
    max_pool2d = builder.cnn_max_pool2d(cnn_input)
    global_avg_pool2d = builder.cnn_global_avg_pool2d(cnn_input)
    conv_weight = builder.external_tensor_constant(
        "conv_weight",
        "float32",
        4,
        "[1,1,3,3]",
        "weight",
        "safetensors",
        "resnet.safetensors",
        "conv.weight",
        8,
        36,
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
        44,
        4,
        "d" * 64,
        "C",
    )
    conv2d = builder.cnn_conv2d(
        cnn_input,
        conv_weight,
        conv_bias,
        {
            "attr.kernel_shape": "1,1",
            "attr.stride": "1,1",
            "attr.padding": "0,0",
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
        48,
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
        52,
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
        56,
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
        60,
        4,
        "2" * 64,
        "C",
    )
    batch_norm = builder.cnn_batch_norm_infer(
        cnn_input,
        bn_scale,
        bn_bias,
        bn_running_mean,
        bn_running_var,
    )
    for parameter in (
        matmul_lhs,
        matmul_rhs,
        common_input,
        linear_input,
        cnn_input,
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
        matmul,
    )
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        residual_add,
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


def _run_smoke(ir_b2a: Path, work_dir: Path) -> int:
    work_dir.mkdir(parents=True, exist_ok=True)
    artifact = work_dir / "python_native_model.B"
    text_dump = work_dir / "python_native_model.T"
    side_file = work_dir / "python_native_model.safetensors"
    for path in (artifact, text_dump, side_file):
        if path.exists():
            path.unlink()

    module = export_to_whirl(
        DummyModel(),
        [InputTensor(), InputTensor()],
        WhirlExportOptions(backend="native", model_name="python_native"),
    )
    _append_operator_probes(module)
    save_as_whirl(module, str(artifact))
    if not artifact.exists() or artifact.stat().st_size == 0:
        print("native Python WHIRL artifact was not created", file=sys.stderr)
        return 1

    result = subprocess.run(
        [str(ir_b2a), "-st", "-src", str(artifact), str(text_dump)],
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
        "safetensors://resnet.safetensors",
        "attr.kernel_shape=3,3",
        "attr.output_size=1,1",
        "attr.groups=1",
        "attr.weight_layout=OIHW",
        "attr.epsilon=1e-05",
        "attr.training=false",
        "Symbols:",
        "Types:",
        "DSL IR Image: version=1",
        "DSL Opcode Descriptor Table:",
        "DSL Node Table:",
        "DSL Attribute Table:",
        "DSL Value Table:",
        "DSL Value Reference Table:",
        "operator=OPR_DSLADD stable_name=common.add version=1",
        "name=attr.broadcast_rule kind=string value=none",
        "ordinal=kid0",
        "kind=constant",
        "tensor_descriptor={kind=tensor",
        "no_alias=true",
        "source files:",
        "native_ir_tools_smoke.py",
    ]
    missing = [needle for needle in required if needle not in text]
    if missing:
        print(
            "ir_b2a -st -src output missed expected text: " +
            ", ".join(missing),
            file=sys.stderr,
        )
        print(text, file=sys.stderr)
        return 1

    forbidden = ["OPR_DSL ", "MDSL ", "OPC_MDSL"]
    exposed = [needle for needle in forbidden if needle in text]
    if exposed:
        print(
            "ir_b2a exposed physical DSL storage text: " +
            ", ".join(exposed),
            file=sys.stderr,
        )
        print(text, file=sys.stderr)
        return 1

    print(f"retained native artifacts: {work_dir}")
    return 0


def main() -> int:
    ir_b2a = _find_ir_b2a()
    if ir_b2a is None:
        return 0

    artifact_dir = os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR", "")
    if artifact_dir:
        status = _run_smoke(ir_b2a, Path(artifact_dir))
    else:
        with tempfile.TemporaryDirectory() as work_dir_text:
            status = _run_smoke(ir_b2a, Path(work_dir_text))
    if status != 0:
        return status

    print("native Python ir_b2a smoke passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
