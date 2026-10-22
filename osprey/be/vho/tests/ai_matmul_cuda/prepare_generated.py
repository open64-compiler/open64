"""Package an applied M2 object and its exact selected CUDA source for GPU validation."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import tempfile


HERE = Path(__file__).resolve().parent
M0 = HERE.parent / "ai_matmul_m0"
ABI = HERE.parents[3] / "include/open64_dsl_runtime_abi.h"


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler-artifact", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    compiler = args.compiler_artifact.resolve()
    output = args.output_dir.resolve()
    if output.exists():
        parser.error(f"artifact already exists: {output}")
    handoff_path = compiler / "plans/m2-selected-tile.json"
    handoff = json.loads(handoff_path.read_text(encoding="utf-8"))
    if (handoff.get("schema") != "ai-matmul-m2-handoff-v1" or
            not handoff.get("whirl_rewrite_applied") or
            handoff.get("status") != "generated_runtime_call_unverified"):
        parser.error("input is not an applied M2 compiler artifact")
    source = compiler / "source/matmul.generated.cu"
    obj = compiler / "binary/matmul.o"
    trace = compiler / "traces/compile.t"
    if (digest(source) != handoff["source_sha256"] or
            digest(obj) != handoff["post_lowering_artifact_sha256"] or
            digest(trace) != handoff["trace_sha256"]):
        parser.error("compiler source, object, or trace changed since handoff")
    contents = source.read_text(encoding="utf-8")
    for macro, expected in (
        ("AI_TILE_PLAN_ID", handoff["selected_tile_plan_id"]),
        ("AI_OPTIMIZATION_PLAN_ID", handoff["selected_optimization_plan_id"]),
        *((key, handoff["problem"][key]) for key in ("M", "N", "K")),
        *((key, handoff["tile"][key]) for key in ("BM", "BN", "BK", "TM", "TN")),
    ):
        match = re.search(rf"^#define {macro} (\d+)$", contents, re.MULTILINE)
        if match is None or int(match[1]) != expected:
            parser.error(f"selected source {macro} disagrees with handoff")

    output.parent.mkdir(parents=True, exist_ok=True)
    stage = Path(tempfile.mkdtemp(prefix=".m2-generated-", dir=output.parent))
    try:
        for name in ("source", "binary", "traces", "plans", "logs", "measurements"):
            (stage / name).mkdir()
        shutil.copy2(source, stage / "source/matmul.generated.cu")
        shutil.copy2(obj, stage / "binary/matmul.o")
        shutil.copy2(trace, stage / "traces/compile.t")
        shutil.copy2(handoff_path, stage / "plans/m2-selected-tile.json")
        for path in (HERE / "generated_runtime.cu", HERE / "generated_benchmark.c",
                     HERE / "kernel_control.cu",
                     M0 / "runtime.c", M0 / "runtime.h",
                     M0 / "generated_runtime.h", ABI):
            shutil.copy2(path, stage / "source" / path.name)
        shutil.copy2(HERE / "run_generated_gpu.py", stage / "run_generated_gpu.py")
        shutil.copy2(HERE / "remote_generated.py", stage / "remote_generated.py")
        receipt = {
            "schema": "ai-matmul-m2-generated-gpu-package-v1",
            "status": "native_gpu_execution_pending",
            "compiler_artifact": str(compiler),
            "selected_tile_plan_id": handoff["selected_tile_plan_id"],
            "selected_optimization_plan_id": handoff["selected_optimization_plan_id"],
            "runtime_verified": False,
            "closes_m2_gate": False,
        }
        (stage / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n",
                                             encoding="utf-8")
        hashes = sorted(path for path in stage.rglob("*") if path.is_file())
        (stage / "SHA256SUMS").write_text("".join(
            f"{digest(path)}  {path.relative_to(stage)}\n" for path in hashes),
            encoding="utf-8")
        stage.rename(output)
    except BaseException:
        shutil.rmtree(stage)
        raise
    print(output)


if __name__ == "__main__":
    main()
