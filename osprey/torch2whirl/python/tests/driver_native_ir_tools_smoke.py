"""ir_b2a smoke test for native artifacts produced through torch2whirl."""

from __future__ import annotations

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
from textwrap import dedent
from typing import Optional

from driver_torch_smoke import _write_resnet_model


def _require_torch() -> None:
    try:
        import torch  # noqa: F401
    except ImportError as exc:
        raise RuntimeError(
            "torch is required for driver_native_ir_tools_smoke.py"
        ) from exc


def _find_driver() -> Path:
    configured = os.environ.get("TORCH2WHIRL_DRIVER", "")
    if configured:
        path = Path(configured)
        if path.is_file() and os.access(str(path), os.X_OK):
            return path
        raise FileNotFoundError(f"torch2whirl driver is not executable: {path}")

    found = shutil.which("torch2whirl")
    if found:
        return Path(found)

    raise FileNotFoundError("torch2whirl driver not found")


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


def _write_model(path: Path) -> None:
    path.write_text(
        dedent(
            """
            import torch


            class AddMatmulModel(torch.nn.Module):
                def forward(self, add_lhs, add_rhs, matmul_lhs, matmul_rhs):
                    add_result = add_lhs + add_rhs
                    matmul_result = torch.matmul(matmul_lhs, matmul_rhs)
                    return add_result, matmul_result


            def create_model():
                return AddMatmulModel()
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


def _run_driver(
    driver: Path,
    model_path: Path,
    artifact: Path,
    sample_inputs: tuple[str, ...],
) -> subprocess.CompletedProcess[str]:
    command = [
        str(driver),
        str(model_path),
        "--entry",
        "forward",
        "--backend",
        "native",
    ]
    for sample_input in sample_inputs:
        command.extend(["--sample-input", sample_input])
    command.extend(["--output", str(artifact)])
    return subprocess.run(
        command,
        check=False,
        env=os.environ.copy(),
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    )


def _run_valid_driver(driver: Path, model_path: Path, artifact: Path) -> int:
    completed = _run_driver(
        driver,
        model_path,
        artifact,
        ("shape:1,3,64,64",),
    )
    if completed.returncode != 0:
        print(completed.stdout, file=sys.stderr)
        print(completed.stderr, file=sys.stderr)
    return completed.returncode


def _check_invalid_graph_rejected(driver: Path, tmpdir: Path) -> int:
    model_path = tmpdir / "invalid_model.py"
    artifact = tmpdir / "invalid_model.B"
    _write_invalid_add_model(model_path)

    completed = _run_driver(
        driver,
        model_path,
        artifact,
        ("shape:1,3", "shape:1,4"),
    )
    if completed.returncode == 0:
        print("driver accepted verifier-invalid add graph", file=sys.stderr)
        return 1
    error_text = completed.stdout + completed.stderr
    if "shape" not in error_text:
        print(
            "driver rejection did not report verifier shape context:",
            file=sys.stderr,
        )
        print(error_text, file=sys.stderr)
        return 1
    if artifact.exists():
        print("verifier-invalid driver run wrote an artifact", file=sys.stderr)
        return 1
    return 0


def _inspect_artifact(ir_b2a: Path, artifact: Path, text_dump: Path) -> int:
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
        "common.model_input",
        "common.tensor_const",
        "cnn.conv2d",
        "cnn.batch_norm_infer",
        "common.relu",
        "cnn.max_pool2d",
        "common.residual_add",
        "cnn.global_avg_pool2d",
        "common.flatten",
        "common.linear",
        "common.output_logits",
        "attr.kernel_shape=7,7",
        "attr.stride=2,2",
        "attr.padding=3,3",
        "attr.semantic=logits",
        "value=safetensors://driver_native_model.safetensors#conv1.weight",
        "value_kind=implicit_zero",
        "Symbols:",
        "Types:",
    ]
    missing = [needle for needle in required if needle not in text]
    if missing:
        print(
            "driver ir_b2a -st output missed expected text: " +
            ", ".join(missing),
            file=sys.stderr,
        )
        print(text, file=sys.stderr)
        return 1

    return 0


def main() -> int:
    ir_b2a = _find_ir_b2a()
    if ir_b2a is None:
        return 0
    _require_torch()
    driver = _find_driver()

    with tempfile.TemporaryDirectory(prefix="torch2whirl-driver-native-") as tmp:
        tmpdir = Path(tmp)
        model_path = tmpdir / "model.py"
        artifact = tmpdir / "driver_native_model.B"
        text_dump = tmpdir / "driver_native_model.st.ir"
        _write_resnet_model(model_path)

        driver_status = _run_valid_driver(driver, model_path, artifact)
        if driver_status != 0:
            completed = _run_driver(
                driver,
                model_path,
                artifact,
                ("shape:1,3,64,64",),
            )
            error_text = completed.stdout + completed.stderr
            if "does not support" in error_text:
                print(
                    "skip: native backend capability missing: " +
                    error_text.strip()
                )
                return 0
            return driver_status
        if not artifact.exists() or artifact.stat().st_size == 0:
            print("driver native WHIRL artifact was not created", file=sys.stderr)
            return 1

        inspect_status = _inspect_artifact(ir_b2a, artifact, text_dump)
        if inspect_status != 0:
            return inspect_status
        invalid_status = _check_invalid_graph_rejected(driver, tmpdir)
        if invalid_status != 0:
            return invalid_status

    print("driver native ir_b2a smoke passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
