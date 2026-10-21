"""Package the exact compiler-selected TMA source for native GPU validation."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import shutil
import tempfile

from ai_opt_report import parse
from compile_source import validate_selected_source


HERE = Path(__file__).resolve().parent


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def verify_compiler_artifact(root: Path) -> None:
    lines = (root / "SHA256SUMS").read_text(encoding="utf-8").splitlines()
    if not lines:
        raise ValueError("compiler artifact has no checksum manifest")
    for line in lines:
        expected, relative = line.split("  ", 1)
        path = (root / relative).resolve()
        try:
            path.relative_to(root)
        except ValueError:
            raise ValueError(f"compiler artifact path escapes root: {relative}")
        if not path.is_file() or sha256(path) != expected:
            raise ValueError(f"compiler artifact checksum failed: {relative}")


def prepare(compiler_artifact: Path, output: Path) -> Path:
    compiler_artifact = compiler_artifact.resolve()
    output = output.resolve()
    if output.exists():
        raise FileExistsError(f"artifact already exists: {output}")
    verify_compiler_artifact(compiler_artifact)
    receipt_path = compiler_artifact / "receipt.json"
    receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
    source_path = compiler_artifact / receipt["source_path"]
    trace_path = compiler_artifact / receipt["decision_trace_path"]
    handoff_path = compiler_artifact / receipt["m2_handoff_path"]
    handoff = json.loads(handoff_path.read_text(encoding="utf-8"))
    if (not receipt.get("compiler_generated_from_whirl_analysis") or
            not receipt.get("p8_tma_requested") or
            sha256(source_path) != receipt["source_sha256"] or
            sha256(handoff_path) != receipt["m2_handoff_sha256"] or
            handoff.get("schema") != "ai-matmul-m2-handoff-v1" or
            handoff.get("whirl_rewrite_applied") is not False or
            handoff.get("source_sha256") != sha256(source_path) or
            handoff.get("trace_sha256") != sha256(trace_path)):
        raise ValueError("input is not a verified compiler-selected TMA source")
    candidates, selection, action = parse(trace_path.read_text(encoding="utf-8"))
    if (selection.get("version") != 7 or not selection.get("fetch") or
            selection["selected"] != receipt["selected_optimization_plan_id"] or
            selection["fetch"]["selection"]["selected"] != receipt["p8_selected_plan_id"] or
            handoff["selected_optimization_plan_id"] != selection["selected"] or
            handoff["fetch_plan_id"] !=
                selection["fetch"]["selection"]["selected"] or
            action["tile_rewrite"] != "none"):
        raise ValueError("input trace disagrees with the compiler receipt")
    validate_selected_source(source_path.read_text(encoding="utf-8"),
                             candidates, selection)
    output.parent.mkdir(parents=True, exist_ok=True)
    stage = Path(tempfile.mkdtemp(prefix=".ai-matmul-compiler-gpu-",
                                  dir=output.parent))
    try:
        (stage / "source").mkdir()
        (stage / "traces").mkdir()
        (stage / "plans").mkdir()
        (stage / "reports/phases").mkdir(parents=True)
        shutil.copy2(source_path, stage / "source/matmul.generated.cu")
        shutil.copy2(HERE / "compiler_harness.cu", stage / "source/benchmark.cu")
        shutil.copy2(HERE / "run_gpu.py", stage / "run_gpu.py")
        shutil.copy2(receipt_path, stage / "source/compiler-receipt.json")
        shutil.copy2(trace_path, stage / "traces/compile.t")
        shutil.copy2(handoff_path, stage / "plans/m2-selected-tile.json")
        for name in ("optimization-summary.log",):
            shutil.copy2(compiler_artifact / "reports" / name,
                         stage / "reports" / name)
        for name in ("tile.log", "fetch-pipeline.log"):
            shutil.copy2(compiler_artifact / "reports/phases" / name,
                         stage / "reports/phases" / name)
        (stage / "source.sha256").write_text(
            f"{sha256(stage / 'source/matmul.generated.cu')}  "
            "source/matmul.generated.cu\n", encoding="utf-8")
        metadata = {
            "schema": "ai-matmul-compiler-gpu-package-v1",
            "compiler_artifact": str(compiler_artifact),
            "compiler_receipt_sha256": sha256(receipt_path),
            "raw_trace_sha256": sha256(trace_path),
            "source_sha256": sha256(source_path),
            "harness_sha256": sha256(stage / "source/benchmark.cu"),
            "m2_handoff_sha256": sha256(stage / "plans/m2-selected-tile.json"),
            "tile_plan_id": selection["selected"],
            "fetch_plan_id": selection["fetch"]["selection"]["selected"],
            "whirl_rewrite_applied": False,
            "runtime_verified": False,
        }
        (stage / "compiler.json").write_text(
            json.dumps(metadata, indent=2) + "\n", encoding="utf-8")
        stage.rename(output)
    except Exception:
        print(f"failed package retained for diagnosis: {stage}")
        raise
    return output


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler-artifact", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    print(prepare(args.compiler_artifact, args.output_dir))
