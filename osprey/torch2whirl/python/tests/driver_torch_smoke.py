"""Smoke test for the C++ torch2whirl driver invoking open64_dsc.cli."""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import sys
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

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
