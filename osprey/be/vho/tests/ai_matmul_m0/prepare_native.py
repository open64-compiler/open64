"""Package the compiled m0 PU for correctness and timing on native x86-64."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import shutil
import tempfile


HERE = Path(__file__).resolve().parent


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def verify_manifest(root: Path) -> None:
    lines = (root / "SHA256SUMS").read_text(encoding="utf-8").splitlines()
    if not lines:
        raise ValueError("emulated artifact has no checksum manifest")
    for line in lines:
        expected, relative = line.split("  ", 1)
        path = (root / relative).resolve()
        try:
            path.relative_to(root)
        except ValueError:
            raise ValueError(f"artifact path escapes root: {relative}")
        if not path.is_file() or sha256(path) != expected:
            raise ValueError(f"artifact checksum failed: {relative}")


def prepare(smoke: Path, output: Path) -> Path:
    smoke = smoke.resolve()
    output = output.resolve()
    if output.exists():
        raise FileExistsError(f"artifact already exists: {output}")
    verify_manifest(smoke)
    receipt_path = smoke / "receipt.json"
    receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
    if (receipt.get("status") != "functional_smoke_emulated" or
            receipt.get("numerical_check") !=
                "all 16384 elements exactly 128.0f on every run"):
        raise ValueError("input is not a verified compiled-PU smoke")
    output.parent.mkdir(parents=True, exist_ok=True)
    stage = Path(tempfile.mkdtemp(prefix=".ai-matmul-m0-native-",
                                  dir=output.parent))
    try:
        for name in ("binary", "source", "traces", "provenance"):
            (stage / name).mkdir()
        shutil.copy2(smoke / "binary/matmul-m0", stage / "binary/matmul-m0")
        for name in ("fixture.py", "runtime.c", "runtime.h", "benchmark.c"):
            shutil.copy2(smoke / "source" / name, stage / "source" / name)
        for name in ("matmul.B", "matmul.I"):
            shutil.copy2(smoke / "binary" / name, stage / "binary" / name)
        shutil.copy2(smoke / "traces/matmul.T", stage / "traces/matmul.T")
        shutil.copy2(receipt_path, stage / "provenance/emulated-receipt.json")
        shutil.copy2(HERE / "run_native.py", stage / "run_native.py")
        metadata = {
            "schema": "ai-matmul-m0-native-package-v1",
            "source_artifact": str(smoke),
            "source_receipt_sha256": sha256(receipt_path),
            "executable_sha256": sha256(stage / "binary/matmul-m0"),
            "input_whirl_sha256": sha256(stage / "binary/matmul.B"),
            "ir_trace_sha256": sha256(stage / "traces/matmul.T"),
            "native_verified": False,
        }
        (stage / "native.json").write_text(
            json.dumps(metadata, indent=2) + "\n", encoding="utf-8")
        stage.rename(output)
    except Exception:
        print(f"failed native package retained for diagnosis: {stage}")
        raise
    return output


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--smoke-artifact", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    print(prepare(args.smoke_artifact, args.output_dir))
