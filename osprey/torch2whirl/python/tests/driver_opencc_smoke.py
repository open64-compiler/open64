"""Smoke test for opencc consuming a torch2whirl native artifact."""

from __future__ import annotations

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
from textwrap import dedent
from typing import Optional


def _require_torch() -> None:
    try:
        import torch  # noqa: F401
    except ImportError as exc:
        raise RuntimeError("torch is required for driver_opencc_smoke.py") from exc


def _find_executable(env_name: str, tool_name: str) -> Optional[Path]:
    configured = os.environ.get(env_name, "")
    if configured:
        path = Path(configured)
        if path.is_file() and os.access(str(path), os.X_OK):
            return path
        print(f"skip: {tool_name} is not executable: {path}")
        return None

    found = shutil.which(tool_name)
    if found:
        return Path(found)

    print(f"skip: {tool_name} not found; set {env_name} to enable smoke test")
    return None


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


def _write_model(path: Path) -> None:
    path.write_text(
        dedent(
            """
            import torch


            class AddModel(torch.nn.Module):
                def forward(self, lhs, rhs):
                    return lhs + rhs


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
        ("shape:1,3", "shape:1,3"),
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


def _run_opencc(opencc: Path, artifact: Path, object_path: Path) -> int:
    command = [
        str(opencc),
        "-x",
        "whirl",
        "-c",
        str(artifact),
        "-o",
        str(object_path),
    ]
    completed = subprocess.run(
        command,
        check=False,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    )
    if completed.returncode != 0:
        print(completed.stdout, file=sys.stderr)
        print(completed.stderr, file=sys.stderr)
        return completed.returncode
    if not object_path.exists() or object_path.stat().st_size == 0:
        print("opencc did not create a non-empty object file", file=sys.stderr)
        return 1
    return 0


def main() -> int:
    opencc = _find_executable("OPEN64_OPENCC", "opencc")
    if opencc is None:
        return 0
    _require_torch()
    driver = _find_driver()

    with tempfile.TemporaryDirectory(prefix="torch2whirl-opencc-") as tmp:
        tmpdir = Path(tmp)
        model_path = tmpdir / "model.py"
        artifact = tmpdir / "driver_native_model.B"
        object_path = tmpdir / "driver_native_model.o"
        _write_model(model_path)

        invalid_status = _check_invalid_graph_rejected(driver, tmpdir)
        if invalid_status != 0:
            return invalid_status

        driver_status = _run_valid_driver(driver, model_path, artifact)
        if driver_status != 0:
            return driver_status
        if not artifact.exists() or artifact.stat().st_size == 0:
            print("driver native WHIRL artifact was not created", file=sys.stderr)
            return 1

        opencc_status = _run_opencc(opencc, artifact, object_path)
        if opencc_status != 0:
            return opencc_status

    print("driver opencc smoke passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
