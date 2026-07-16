"""Native process-boundary certification for tiny Llama 2 prefill."""

from __future__ import annotations

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
from typing import Optional


def _require_torch() -> None:
    try:
        import torch  # noqa: F401
    except ImportError as exc:
        raise RuntimeError(
            "torch is required for llama2_prefill_native_ir_tools_smoke.py"
        ) from exc


def _find_executable(variable: str, name: str) -> Optional[Path]:
    configured = os.environ.get(variable, "")
    if configured:
        path = Path(configured)
        if path.is_file() and os.access(str(path), os.X_OK):
            return path
        print(f"skip: {name} is not executable: {path}")
        return None

    found = shutil.which(name)
    if found:
        return Path(found)

    print(f"skip: {name} not found; set {variable} to enable smoke test")
    return None


def _test_root() -> Path:
    return Path(__file__).resolve().parent


def _run_driver(
    driver: Path,
    model_path: Path,
    artifact: Path,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            str(driver),
            str(model_path),
            "--entry",
            "forward",
            "--sample-input",
            "int-shape:1,8",
            "--backend",
            "native",
            "--output",
            str(artifact),
        ],
        check=False,
        env=os.environ.copy(),
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    )


def _inspect_artifact(ir_b2a: Path, artifact: Path, text_dump: Path) -> int:
    completed = subprocess.run(
        [str(ir_b2a), "-st", "-src", str(artifact), str(text_dump)],
        check=False,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    )
    if completed.returncode != 0:
        print(completed.stdout, file=sys.stderr)
        print(completed.stderr, file=sys.stderr)
        return completed.returncode

    text = text_dump.read_text(encoding="utf-8", errors="replace")
    required = [
        "llama2_model.py",
        "FUNC_ENTRY",
        "contract=transformer.prefill.v1",
        "contract=transformer.decoder_layer.v1",
        "transformer.token_embedding",
        "transformer.rms_norm",
        "transformer.rotary_embedding",
        "transformer.attention",
        "transformer.swiglu",
        "common.output_logits",
        "attr.execution_mode=full_sequence",
        "attr.mask_mode=causal",
        "attr.cache_mode=none",
        "DSL REGION TABLE:",
        "Symbols:",
        "Types:",
        "{line: 1/",
    ]
    missing = [needle for needle in required if needle not in text]
    if missing:
        print(
            "prefill ir_b2a -st -src output missed expected text: " +
            ", ".join(missing),
            file=sys.stderr,
        )
        return 1

    if text.count("contract=transformer.decoder_layer.v1") != 2:
        print("prefill trace did not expose two decoder layers", file=sys.stderr)
        return 1

    forbidden = ["OPR_DSL ", "MDSL ", "OPC_MDSL"]
    exposed = [needle for needle in forbidden if needle in text]
    if exposed:
        print(
            "prefill ir_b2a exposed physical DSL storage text: " +
            ", ".join(exposed),
            file=sys.stderr,
        )
        return 1

    return 0


def _run_smoke(ir_b2a: Path, driver: Path, work_dir: Path) -> int:
    work_dir.mkdir(parents=True, exist_ok=True)
    source_model = _test_root() / "models" / "llama2_model.py"
    model_path = work_dir / "llama2_model.py"
    retained_source_model = (
        work_dir / "source" / "models" / "llama2_model.py"
    )
    artifact = work_dir / "llama2.B"
    text_dump = work_dir / "llama2.T"
    side_file = work_dir / "llama2.safetensors"
    driver_log = work_dir / "llama2_driver.log"
    for path in (model_path, artifact, text_dump, side_file, driver_log):
        if path.exists():
            path.unlink()

    shutil.copy2(source_model, model_path)
    retained_source_model.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(source_model, retained_source_model)
    completed = _run_driver(driver, model_path, artifact)
    driver_log.write_text(
        f"exit_status={completed.returncode}\n" +
        completed.stdout + completed.stderr,
        encoding="utf-8",
    )
    if completed.returncode != 0:
        print(completed.stdout, file=sys.stderr)
        print(completed.stderr, file=sys.stderr)
        return completed.returncode
    if not artifact.is_file() or artifact.stat().st_size == 0:
        print("prefill WHIRL artifact was not created", file=sys.stderr)
        return 1
    if not side_file.is_file() or side_file.stat().st_size == 0:
        print("prefill safetensors side file was not created", file=sys.stderr)
        return 1

    inspect_status = _inspect_artifact(ir_b2a, artifact, text_dump)
    if inspect_status != 0:
        return inspect_status

    print(f"retained Llama prefill artifacts: {work_dir}")
    return 0


def main() -> int:
    ir_b2a = _find_executable("OPEN64_IR_B2A", "ir_b2a")
    if ir_b2a is None:
        return 0
    driver = _find_executable("TORCH2WHIRL_DRIVER", "torch2whirl")
    if driver is None:
        return 0
    _require_torch()

    artifact_dir = os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR", "")
    if artifact_dir:
        status = _run_smoke(ir_b2a, driver, Path(artifact_dir))
    else:
        with tempfile.TemporaryDirectory(
            prefix="torch2whirl-llama2-prefill-native-"
        ) as tmp:
            status = _run_smoke(ir_b2a, driver, Path(tmp))
    if status != 0:
        return status

    print("Llama prefill native ir_b2a smoke passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
