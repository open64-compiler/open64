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

        command = [
            str(driver),
            str(model_path),
            "--entry",
            "forward",
            "--sample-input",
            "shape:1,3",
            "--output",
            str(output_path),
        ]
        completed = subprocess.run(
            command,
            check=False,
            env=os.environ.copy(),
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
        )
        if completed.returncode != 0:
            sys.stderr.write(completed.stdout)
            sys.stderr.write(completed.stderr)
            return completed.returncode

        _check_artifact(output_path)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
