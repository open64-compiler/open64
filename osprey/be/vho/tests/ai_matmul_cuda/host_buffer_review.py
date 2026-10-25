"""Retain the selected GEMM's host CUDA review adapter and buffer dependencies."""

import hashlib
import json
from pathlib import Path
import re
import shutil


HERE = Path(__file__).resolve().parent
OSPREY = HERE.parents[3]


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def publish_host_buffer_review(root):
    root = Path(root)
    handoff_path = root / "plans/m2-selected-tile.json"
    handoff = json.loads(handoff_path.read_text(encoding="utf-8"))
    if (handoff.get("schema") != "ai-matmul-m2-handoff-v1" or
            handoff.get("whirl_rewrite_applied") is not True or
            handoff.get("status") != "generated_runtime_call_unverified"):
        raise ValueError("host-buffer review requires an applied selected plan")
    kernel = root / "source/matmul.generated.cu"
    trace = root / "traces/compile.t"
    if (digest(kernel) != handoff["source_sha256"] or
            digest(trace) != handoff["trace_sha256"]):
        raise ValueError("host-buffer review source or trace disagrees with handoff")
    macros = dict(AI_TILE_PLAN_ID=handoff["selected_tile_plan_id"],
                  AI_OPTIMIZATION_PLAN_ID=handoff["selected_optimization_plan_id"],
                  **handoff["problem"], **handoff["tile"])
    contents = kernel.read_text(encoding="utf-8")
    for key, expected in macros.items():
        match = re.search(rf"^#define {key} (\d+)$", contents, re.MULTILINE)
        if match is None or int(match[1]) != expected:
            raise ValueError(f"host-buffer review {key} disagrees with handoff")
    m, n, k = (handoff["problem"][key] for key in ("M", "N", "K"))
    if min(m, n, k) <= 0:
        raise ValueError("host-buffer review needs positive matrix extents")

    files = {
        "source/matmul.host.cu": HERE / "generated_runtime.cu",
        "source/generated_runtime.h": HERE.parent / "ai_matmul_m0/generated_runtime.h",
        "source/open64_dsl_runtime_abi.h": OSPREY / "include/open64_dsl_runtime_abi.h",
        "source/open64_target_buffer_service.h": OSPREY / "include/open64_target_buffer_service.h",
        "source/target_buffer_service.c": OSPREY / "libopen64rt/target_buffer_service.c",
    }
    manifest_path = root / "plans/host-buffer-review.json"
    report_path = root / "reports/host-buffer-review.log"
    if any((root / path).exists() for path in files) or manifest_path.exists() or report_path.exists():
        raise ValueError("host-buffer review output already exists")
    for relative, source in files.items():
        shutil.copy2(source, root / relative)
    report_path.parent.mkdir(parents=True, exist_ok=True)
    requirements = {"A": m * k * 4, "B": k * n * 4, "C": m * n * 4}
    record = {
        "schema": "ai-matmul-host-buffer-review-v1",
        "origin": "fixed_shape_review_adapter",
        "compiler_generated_host_pu": False,
        "host_source_path": "source/matmul.host.cu",
        "kernel_source_path": "source/matmul.generated.cu",
        "device_review_source_path": handoff.get("device_cuda_path"),
        "selected_tile_plan_id": handoff["selected_tile_plan_id"],
        "selected_optimization_plan_id": handoff["selected_optimization_plan_id"],
        "handoff_sha256": digest(handoff_path),
        "kernel_sha256": digest(kernel),
        "trace_sha256": digest(trace),
        "buffer_bytes": requirements,
        "binding_policy": "host_inputs_and_host_output",
        "files": {path: digest(root / path) for path in files},
        "native_cuda_compiled": False,
        "gpu_execution_verified": False,
    }
    manifest_path.write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")
    report_path.write_text(
        "# What it summarizes: Preserved host CUDA adapter source, selected plan, "
        "and described allocation/transfer sequence; these are not observed runtime events.\n"
        f"host_source=source/matmul.host.cu origin={record['origin']} "
        f"plan={record['selected_optimization_plan_id']} "
        f"tile={record['selected_tile_plan_id']}\n"
        f"A bytes={requirements['A']} target=allocate upload=before_launch\n"
        f"B bytes={requirements['B']} target=allocate upload=before_launch\n"
        f"C bytes={requirements['C']} target=allocate upload=none download=after_completion\n"
        "sequence=bind_A,bind_B,bind_C,launch,wait,download_C,release_C_B_A\n"
        "status=source_review_only native_cuda_compiled=no gpu_execution_verified=no\n",
        encoding="utf-8")
    return record
