"""Native process-boundary certification for tiny Llama 2 multiple-PU mode."""

from __future__ import annotations

import os
from pathlib import Path
import re
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
            "torch is required for llama2_multi_pu_native_ir_tools_smoke.py"
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


def _test_root() -> Path:
    return Path(__file__).resolve().parent


def _pythonpath_env() -> dict[str, str]:
    env = os.environ.copy()
    current = env.get("PYTHONPATH", "")
    pieces = [
        current,
        str(_test_root()),
        str(_test_root().parent),
    ]
    env["PYTHONPATH"] = os.pathsep.join(piece for piece in pieces if piece)
    return env


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
            "--backend",
            "native",
            "--multiple-pu",
            "--sample-input",
            "int-shape:1,8",
            "--output",
            str(artifact),
        ],
        check=False,
        env=_pythonpath_env(),
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    )


def _inspect_artifact(ir_b2a: Path, artifact: Path, text_dump: Path) -> int:
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
        "llama2_model.py",
        "FUNC_ENTRY",
        "FUNC_ENTRY <1,50,TinyRMSNorm>",
        "FUNC_ENTRY <1,51,TinyLlama2ForCausalLM>",
        "IDNAME 0 <2,1,hidden_states>",
        "IDNAME 0 <2,2,rms_norm_scale>",
        "IDNAME 0 <2,3,normalized_result>",
        "IDNAME 0 <2,1,model_hidden>",
        "IDNAME 0 <2,2,model_norm_scale>",
        "IDNAME 0 <2,3,model_result>",
        "transformer.rms_norm",
        "VCALL",
        "TinyRMSNorm",
        "__WHIRL_DSL_CALL__",
        "instance=norm",
        "context=TinyLlama2ForCausalLM.norm",
        "read_only passed_not_saved",
        "out passed_not_saved",
        "metadata=owner_pu=TinyRMSNorm",
        "metadata=owner_pu=TinyLlama2ForCausalLM",
    ]
    missing = [needle for needle in required if needle not in text]
    if missing:
        print(
            "multiple-PU ir_b2a -st -src output missed expected text: " +
            ", ".join(missing),
            file=sys.stderr,
        )
        print(text, file=sys.stderr)
        return 1

    if len(re.findall(r"FUNC_ENTRY <[^>]*Tiny", text)) != 2:
        print("multiple-PU trace did not expose exactly two Tiny FUNC_ENTRYs",
              file=sys.stderr)
        return 1

    if text.count("__WHIRL_DSL_CALL__") < 1:
        print("multiple-PU trace missed logical call comment", file=sys.stderr)
        return 1

    if "OPR_DSL " in text or "MDSL " in text or "OPC_MDSL" in text:
        print("multiple-PU trace exposed private DSL storage text",
              file=sys.stderr)
        return 1

    return 0


def _run_smoke(ir_b2a: Path, driver: Path, work_dir: Path) -> int:
    work_dir.mkdir(parents=True, exist_ok=True)
    model_path = _test_root() / "models" / "llama2_model.py"
    artifact = work_dir / "llama2_multi_pu.B"
    text_dump = work_dir / "llama2_multi_pu.T"
    driver_log = work_dir / "llama2_multi_pu_driver.log"
    for path in (artifact, text_dump, driver_log):
        if path.exists():
            path.unlink()

    completed = _run_driver(driver, model_path, artifact)
    driver_log.write_text(
        completed.stdout + completed.stderr,
        encoding="utf-8",
    )
    if completed.returncode != 0:
        print(completed.stdout, file=sys.stderr)
        print(completed.stderr, file=sys.stderr)
        return completed.returncode
    if not artifact.exists() or artifact.stat().st_size == 0:
        print("multiple-PU WHIRL artifact was not created", file=sys.stderr)
        return 1

    inspect_status = _inspect_artifact(ir_b2a, artifact, text_dump)
    if inspect_status != 0:
        return inspect_status

    print(f"retained Llama multiple-PU artifacts: {work_dir}")
    return 0


def main() -> int:
    ir_b2a = _find_ir_b2a()
    if ir_b2a is None:
        return 0
    _require_torch()
    driver = _find_driver()

    artifact_dir = os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR", "")
    if artifact_dir:
        status = _run_smoke(ir_b2a, Path(driver), Path(artifact_dir))
    else:
        with tempfile.TemporaryDirectory(
            prefix="torch2whirl-llama2-multi-pu-native-"
        ) as tmp:
            status = _run_smoke(ir_b2a, Path(driver), Path(tmp))
    if status != 0:
        return status

    print("Llama multiple-PU native ir_b2a smoke passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
