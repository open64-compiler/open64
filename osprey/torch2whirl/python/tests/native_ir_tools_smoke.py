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
    relu = builder.common_relu(lhs)
    flatten = builder.common_flatten(lhs)
    output_logits = builder.common_output_logits(lhs)
    max_pool2d = builder.cnn_max_pool2d(lhs)
    global_avg_pool2d = builder.cnn_global_avg_pool2d(lhs)
    conv_bias = builder.tensor_constant(
        "conv_bias",
        "float32",
        1,
        "[1]",
        "splat",
        "0.0",
    )
    conv2d = builder.cnn_conv2d(
        lhs,
        rhs,
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
    builder.append_program_unit_marker(
        ProgramUnitHandle(module.entry_function.handle),
        matmul,
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

        module = export_to_whirl(
            DummyModel(),
            [object(), object()],
            WhirlExportOptions(backend="native", model_name="python_native"),
        )
        _append_operator_probes(module)
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
            "common.relu",
            "common.flatten",
            "common.output_logits",
            "cnn.max_pool2d",
            "cnn.global_avg_pool2d",
            "cnn.conv2d",
            "attr.start_dim=1",
            "attr.transpose_kid0=false",
            "attr.kernel_shape=3,3",
            "attr.output_size=1,1",
            "attr.groups=1",
            "attr.weight_layout=OIHW",
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
