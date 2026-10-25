"""Compile a fixed F32 matmul with the opt-in Open64 CUDA tile-source switch."""

from __future__ import annotations

import argparse
import ast
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import subprocess
import sys
import tempfile

from ai_opt_report import PHASE_LOGS, parse, render


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[4]
FIXTURE = HERE.parent / "ai_matmul_m0/fixture.py"


def validate_selected_source(source: str, candidates: list[dict],
                             selection: dict) -> dict[str, int]:
    selected = next((row for row in candidates
                     if row["plan"] == selection["selected"]), None)
    if selected is None or selected["legality"] != "proven":
        raise RuntimeError("selected tile is missing or not proven")
    header = re.search(
        r"^// tile_plan=(\d+) optimization_plan=(\d+) "
        r"candidate=(\d+) family=(\w+)$", source, re.MULTILINE)
    if header is None or (int(header[1]), int(header[2]), int(header[3]),
                          header[4]) != (selected["tile"], selected["plan"],
                                         selected["candidate"], selected["family"]):
        raise RuntimeError("generated source plan identity disagrees with trace")
    dimensions = {}
    for name in ("M", "N", "K", "BM", "BN", "BK", "TM", "TN"):
        match = re.search(rf"^#define {name} (\d+)$", source, re.MULTILINE)
        if match is None:
            raise RuntimeError(f"generated source lacks numeric {name}")
        dimensions[name] = int(match[1])
    bm, bn, bk = (dimensions[name] for name in ("BM", "BN", "BK"))
    tm, tn = dimensions["TM"], dimensions["TN"]
    transpose_a = selected.get("layout", {}).get("a_layout") == "transposed_padded"
    a_elements = bk * (bm + 1) if transpose_a else bm * bk
    if ((bm, bn, bk) != selected["cta"] or not tm or not tn or
            bm % tm or bn % tn or
            bm // tm * (bn // tn) != selected["threads"] or
            (a_elements + bk * bn) * 4 != selected["shared"]):
        raise RuntimeError("generated tile geometry disagrees with selected plan")
    if selected.get("layout"):
        stride = selected["layout"]["a_stride"]
        if (f"// a_smem_layout={selected['layout']['a_layout']} a_stride={stride}"
                not in source or
                f"#define A_STRIDE {stride}" not in source or
                f"#define AI_A_SHARED_ELEMS {a_elements}" not in source):
            raise RuntimeError("generated A layout disagrees with selected plan")
    for name, expected in (("AI_TILE_PLAN_ID", selected["tile"]),
                           ("AI_OPTIMIZATION_PLAN_ID", selected["plan"])):
        match = re.search(rf"^#define {name} (\d+)$", source, re.MULTILINE)
        if match is None or int(match[1]) != expected:
            raise RuntimeError(f"generated source {name} disagrees with selected plan")
    if selection.get("version") == 7:
        shapes = [tuple(map(int, row["shape"].strip("[]").split(",")))
                  for row in selection["semantic"] if row["rank"] == 2]
        m, n, k = (dimensions[name] for name in ("M", "N", "K"))
        if ((m, k) not in shapes or (k, n) not in shapes or
                (m, n) not in shapes):
            raise RuntimeError("generated problem dimensions disagree with tensor facts")
    grid = selected.get("grid")
    if grid is not None:
        m, n, k = (dimensions[name] for name in ("M", "N", "K"))
        expected_grid = ((m + bm - 1) // bm, (n + bn - 1) // bn)
        if (tuple(grid["problem"]) != (m, n, k) or
                tuple(grid["grid"]) != expected_grid or
                grid["ctas"] != expected_grid[0] * expected_grid[1] or
                grid["k_iterations"] != (k + bk - 1) // bk):
            raise RuntimeError("generated problem grid disagrees with selected plan")
    loop = re.search(
        r"^// whirl_k_loop=DO_LOOP scope=detached_plan_only "
        r"start=(\d+) end=(\d+) step=(\d+)$", source, re.MULTILINE)
    traced_loop = selection.get("k_loop")
    if traced_loop is not None and (loop is None or
            tuple(map(int, loop.groups())) !=
            (traced_loop["start"], traced_loop["end"], traced_loop["step"])):
        raise RuntimeError("generated source WHIRL loop disagrees with trace")
    if loop is not None and tuple(map(int, loop.groups())) != (
            0, dimensions["K"], dimensions["BK"]):
        raise RuntimeError("generated source WHIRL loop disagrees with tile")
    if loop is not None:
        emitted = re.search(
            r"for \(int base = (\d+); base < (\d+); base \+= (\d+)\)",
            source)
        if emitted is None or emitted.groups() != loop.groups():
            raise RuntimeError("generated CUDA K loop disagrees with WHIRL loop")
    return dimensions


def validate_device_whirl_output(source: str, dump: str,
                                 selection: dict, selected: dict) -> None:
    kernel = selection.get("whirl_kernel")
    if kernel is None or (kernel["plan"], kernel["tile"]) != (
            selected["plan"], selected["tile"]):
        raise RuntimeError("device WHIRL image lacks selected plan identity")
    bm, bn, bk = selected["cta"]
    problem = selected.get("grid", {}).get("problem")
    if problem is None:
        raise RuntimeError("device WHIRL needs traced problem geometry")
    m, n, k = problem
    a_elements = (bk * (bm + 1) if selected.get("layout", {}).get(
        "a_layout") == "transposed_padded" else bm * bk)
    required_source = (
        r'extern\s+"C"\s+__global__\s+void\s+ai_matmul_tile_f32\s*\(',
        rf'__shared__\s+_IEEE32\s+as\[{a_elements}LL\]',
        rf'__shared__\s+_IEEE32\s+bs\[{bk * bn}LL\]',
        rf'for\s*\(base\s*=\s*0;\s*base\s*<\s*{k};\s*'
        rf'base\s*=\s*base\s*\+\s*{bk}\)',
        r'\(threadIdx\)\.x', r'\(blockIdx\)\.[xy]',
    )
    if any(re.search(pattern, source) is None for pattern in required_source):
        raise RuntimeError("whirl2c CUDA output disagrees with selected tile")
    if len(re.findall(r'__syncthreads\s*\(\s*\)', source)) != 2:
        raise RuntimeError("whirl2c CUDA output lacks two K-tile barriers")
    if (re.search(r'FUNC_ENTRY\s*<[^>]*ai_matmul_tile_f32>', dump) is None or
            "DO_LOOP {line:" not in dump or
            "(DEVICE_SYMBOL) 1" not in dump or
            dump.count("(DEVICE_SYMBOL) 4") < 2):
        raise RuntimeError("device WHIRL dump lacks kernel or shared staging")
    guard_m, guard_n, guard_k = (m % bm != 0, n % bn != 0, k % bk != 0)
    expected_guards = sum((guard_m or guard_k, guard_k or guard_n,
                           guard_m or guard_n))
    if (len(re.findall(r'\bif\s*\(', source)) < expected_guards or
            dump.count("IF {line:") < expected_guards):
        raise RuntimeError("predicated device WHIRL lacks required edge guards")


def validate_device_publication_order(trace: str, selection: dict) -> None:
    action = f"AI_OPT 7 TILE_ACTION 1 {selection['selected']} "
    kernel = selection["whirl_kernel"]
    publication = (f"AI_OPT 7 WHIRL_KERNEL 1 {kernel['plan']} "
                   f"{kernel['tile']} {kernel['image']}")
    lines = trace.splitlines()
    actions = [index for index, line in enumerate(lines)
               if line.startswith(action)]
    publications = [index for index, line in enumerate(lines)
                    if line == publication]
    if (len(actions) != 1 or len(publications) != 1 or
            actions[0] >= publications[0]):
        raise RuntimeError("device WHIRL publication did not follow tile action")


def m2_handoff(source: Path, trace: Path, input_whirl: Path,
               lowered_artifact: Path, candidates: list[dict],
               selection: dict, applied: bool = False) -> dict:
    dimensions = validate_selected_source(source.read_text(encoding="utf-8"),
                                          candidates, selection)
    selected = next(row for row in candidates
                    if row["plan"] == selection["selected"])
    return {
        "schema": "ai-matmul-m2-handoff-v1",
        "status": ("generated_runtime_call_unverified" if applied else
                   "selected_source_only"),
        "whirl_rewrite_applied": applied,
        "cuda_source_emitter": "legacy_template",
        "selected_k_loop_scope": selection.get("k_loop", {}).get("scope"),
        "selected_optimization_plan_id": selected["plan"],
        "selected_tile_plan_id": selected["tile"],
        "selected_candidate_id": selected["candidate"],
        "family": selected["family"],
        "selection_reason": selection["reason"],
        "problem": {name: dimensions[name] for name in ("M", "N", "K")},
        "tile": {name: dimensions[name] for name in
                 ("BM", "BN", "BK", "TM", "TN")},
        "resources": {
            "threads_per_cta": selected["threads"],
            "shared_bytes_per_stage": selected["shared"],
            "register_bytes_per_thread": selected["registers"],
        },
        "cost": {
            "unit": selected["unit"],
            "confidence": selected["confidence"],
            "total": selected["total"],
            "compute": selected["compute"],
            "memory_unhidden": selected["memory"],
            "communication_unhidden": selected["communication"],
            "sync": selected["sync"],
            "launch": selected["launch"],
            "runtime_selection": selected["runtime"],
        },
        "source_line": selected["source_line"],
        "fetch_plan_id": selection.get("fetch", {}).get("selection", {}).get("selected"),
        "source_sha256": sha256(source),
        "trace_sha256": sha256(trace),
        "input_whirl_sha256": sha256(input_whirl),
        "post_lowering_artifact_kind": "backend_object" if applied else "whirl",
        "post_lowering_artifact_sha256": sha256(lowered_artifact),
        "lowered_whirl_sha256": None if applied else sha256(lowered_artifact),
    }


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def review_record(stage: Path, selection: dict, candidates: list[dict],
                  applied: bool) -> dict:
    selected = next((row for row in candidates
                     if row["plan"] == selection["selected"]), None)
    if selected is None or selected["legality"] != "proven":
        raise RuntimeError("review source lacks a proven selected plan")
    source = stage / "source/matmul.whirl.cu"
    image = stage / "binary/matmul.device.B"
    trace = stage / "traces/compile.t"
    parity = stage / "binary/parity.log"
    if not all(path.is_file() for path in (source, image, trace, parity)):
        raise RuntimeError("review evidence is incomplete")
    parity_text = parity.read_text(encoding="utf-8")
    if "whirl_vs_reference=0" not in parity_text or "whirl_vs_legacy=0" not in parity_text:
        raise RuntimeError("review source failed host CTA parity")
    kernel = selection.get("whirl_kernel", {})
    if (kernel.get("plan"), kernel.get("tile"), kernel.get("image")) != (
            selected["plan"], selected["tile"], image.name):
        raise RuntimeError("review source and device image disagree with selection")
    return {
        "schema": "ai-matmul-cu-review-v1",
        "status": "compiler_only_ready_for_manual_review",
        "canonical_cuda_path": "source/matmul.whirl.cu",
        "canonical_cuda_sha256": sha256(source),
        "canonical_emitter": "whirl2c -cuda from selected device WHIRL",
        "legacy_comparison_path": "source/matmul.generated.cu",
        "device_whirl_path": "binary/matmul.device.B",
        "device_whirl_sha256": sha256(image),
        "decision_trace_path": "traces/compile.t",
        "decision_trace_sha256": sha256(trace),
        "tile_log_path": "reports/phases/tile.log",
        "summary_path": "reports/optimization-summary.log",
        "host_parity_path": "binary/parity.log",
        "selected_optimization_plan_id": selected["plan"],
        "selected_tile_plan_id": selected["tile"],
        "selected_candidate_id": selected["candidate"],
        "selection_reason": selection["reason"],
        "source_line": selected["source_line"],
        "problem": selected["grid"]["problem"],
        "cta_tile": selected["cta"],
        "threads_per_cta": selected["threads"],
        "gmem_copy_map": selected.get("layout", {}).get("copy_map", "linear"),
        "a_smem_layout": selected.get("layout", {}).get("a_layout", "compact"),
        "a_smem_stride": selected.get("layout", {}).get("a_stride", selected["cta"][2]),
        "b_smem_layout": "row_major",
        "shared_bytes_per_stage": selected["shared"],
        "movement": "one_slot_synchronous",
        "edge_plan": selection.get("edge_plan"),
        "relative_cost": selected["total"],
        "cost_confidence": selected["confidence"],
        "host_whirl_rewrite_applied": applied,
        "host_cta_parity_passed": True,
        "native_cuda_compiled": False,
        "gpu_correctness_verified": False,
        "gpu_performance_verified": False,
    }


def lowering_dump(raw_trace: Path, output: Path) -> str:
    marker = "========== WHIRL after VHO DSL Lowering =========="
    text = raw_trace.read_text(encoding="utf-8")
    if text.count(marker) != 1:
        raise RuntimeError("expected one post-DSL-lowering WHIRL dump")
    dump = text.split(marker, 1)[1].split("!!! DevWarn", 1)[0].strip() + "\n"
    if not dump.startswith("FUNC_ENTRY"):
        raise RuntimeError("post-DSL-lowering dump lacks a function body")
    output.write_text(dump, encoding="utf-8")
    return dump


def selected_k_loop_dump(raw_trace: Path, output: Path,
                         selection: dict) -> str:
    marker = "========== AI selected K-tile WHIRL plan loop =========="
    text = raw_trace.read_text(encoding="utf-8")
    if text.count(marker) != 1:
        raise RuntimeError("expected one selected K-tile WHIRL loop dump")
    dump = text.split(marker, 1)[1]
    dump = re.split(r"(?m)^(?:AI_OPT |========== |!!! DevWarn)", dump, 1)[0]
    dump = dump.strip() + "\n"
    loop = selection["k_loop"]
    header = (f"plan={loop['plan']} tile={loop['tile']} "
              "scope=detached_plan_only\n")
    if not dump.startswith(header) or not re.search(r"\bDO_LOOP\b", dump):
        raise RuntimeError("selected K-tile WHIRL loop dump is incomplete")
    output.write_text(dump, encoding="utf-8")
    return dump


def run(command: list[str], cwd: Path, log: Path,
        env: dict[str, str] | None = None) -> None:
    with log.open("w", encoding="utf-8") as stream:
        stream.write("command=" + json.dumps(command) + "\n")
        stream.flush()
        status = subprocess.run(command, cwd=cwd, env=env, stdout=stream,
                                stderr=subprocess.STDOUT, check=False)
        stream.write(f"exit_status={status.returncode}\n")
    if status.returncode:
        raise RuntimeError(f"compiler command failed: {log}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--tma", action="store_true",
                        help="request selected P8 multidimensional TMA source")
    parser.add_argument("--apply", action="store_true",
                        help="apply the selected synchronous tile to WHIRL at -O3")
    parser.add_argument("--device-whirl", action="store_true",
                        help="retain and check selected device WHIRL and whirl2c CUDA")
    parser.add_argument("--digest-output", action="store_true",
                        help="treat output-dir as a milestone root and publish by raw-trace digest")
    args = parser.parse_args()
    if args.apply and args.tma:
        parser.error("M2 generated-tile application requires synchronous movement")
    if args.device_whirl and args.tma:
        parser.error("selected device WHIRL currently requires synchronous movement")
    if sys.platform != "linux" or platform.machine() != "x86_64":
        parser.error("requires the Linux/x86-64 Open64 compiler build")
    build = args.build_dir.resolve()
    opencc = build / "osprey/targdir/driver/opencc"
    ir_b2a = build / "osprey/targdir/ir_tools/ir_b2a"
    backend = build / "osprey/targdir/be/be.so"
    whirl2c = build / "osprey/targdir/whirl2c/whirl2c"
    for tool in (opencc, ir_b2a, backend) + ((whirl2c,) if args.device_whirl else ()):
        if not tool.is_file() or not os.access(tool, os.X_OK):
            parser.error(f"required Open64 tool missing: {tool}")
    output = args.output_dir.resolve()
    if output.exists() and not args.digest_output:
        parser.error(f"artifact already exists: {output}")
    parent = output if args.digest_output else output.parent
    parent.mkdir(parents=True, exist_ok=True)
    stage = Path(tempfile.mkdtemp(prefix=".ai-cuda-source-", dir=parent))
    try:
        for name in ("source", "binary", "traces", "logs", "plans"):
            (stage / name).mkdir()
        shutil.copy2(FIXTURE, stage / "source/fixture.py")
        shutil.copy2(HERE / "README.md", stage / "source/README.md")
        env = dict(os.environ)
        env["PYTHONPATH"] = os.pathsep.join(filter(None, (
            str(build / "osprey/targdir/torch2whirl/python"),
            str(REPO / "osprey/torch2whirl/python"),
            env.get("PYTHONPATH", ""),
        )))
        binary = stage / "binary/matmul.B"
        run([sys.executable, str(FIXTURE), str(binary)], stage,
            stage / "logs/fixture.log", env)
        input_whirl_sha256 = sha256(binary)
        whirl_dump = stage / "traces/matmul.T"
        run([str(ir_b2a), "-st", "-src", str(binary), str(whirl_dump)], stage,
            stage / "logs/ir_b2a.log")
        cuda_source = stage / "source/matmul.generated.cu"
        device_image = stage / "binary/matmul.device.B"
        device_dump = stage / "traces/matmul.device.T"
        device_cuda = stage / "source/matmul.whirl.cu"
        optimization = "-O3" if args.apply else "-O0"
        inline_option = "-noinline" if args.apply else "-INLINE:none"
        baseline_command = [str(opencc), f"-run-build={build}", optimization,
                            inline_option, "-c", "-keep", "matmul.B"]
        if args.apply:
            baseline_command.insert(-1, "-Wb,-DSL:dump_after_lower=ON")
        run(baseline_command, stage / "binary", stage / "logs/option-off.log")
        lowered = stage / ("binary/matmul.o" if args.apply else "binary/matmul.I")
        if not lowered.is_file():
            raise RuntimeError("option-off compile did not produce backend output")
        option_off_output_sha256 = sha256(lowered)
        if not args.apply:
            run([str(ir_b2a), "-st", "-src", str(lowered),
                 str(stage / "traces/option-off-lowered.T")], stage,
                stage / "logs/option-off-ir_b2a.log")
        option_off_raw_trace = stage / "binary/matmul.t"
        if not option_off_raw_trace.is_file():
            raise RuntimeError("option-off compile did not emit phase coverage")
        option_off_trace = stage / "traces/option-off.t"
        option_off_raw_trace.rename(option_off_trace)
        if args.apply:
            off_text = lowering_dump(option_off_trace,
                                     stage / "traces/option-off-lowered.T")
            if ("__open64_dsl_matmul_v1" not in off_text or
                    "__open64_dsl_matmul_generated_v1" in off_text):
                raise RuntimeError("option-off lowered call is not the generic matmul")
        option_off_selection, _ = render(option_off_trace,
                                         stage / "reports/option-off")
        if (option_off_selection["phases"][7]["status"] != "disabled_by_option"
                or len(option_off_selection["phases"]) != 12):
            raise RuntimeError("option-off phase coverage is incomplete")
        if cuda_source.exists():
            raise RuntimeError("CUDA source appeared with the option disabled")
        command = [str(opencc), f"-run-build={build}", optimization, inline_option,
                   "-c", "-keep",
                   "-Wb,-DSL:cuda_tile_output=../source/matmul.generated.cu",
                   "matmul.B"]
        if args.device_whirl:
            command.insert(-1, "-Wb,-DSL:cuda_whirl_output=matmul.device.B")
        if args.apply:
            command.insert(-1, "-Wb,-DSL:dump_after_lower=ON")
        if args.tma:
            command.insert(-1, "-Wb,-DSL:cuda_tma_prefetch=ON")
        if args.apply:
            command.insert(-1, "-Wb,-DSL:cuda_tile_apply=ON")
        run(command, stage / "binary", stage / "logs/opencc.log")
        raw_trace = stage / "binary/matmul.t"
        trace = stage / "traces/compile.t"
        if not raw_trace.is_file():
            raise RuntimeError("AI optimization decision trace was not emitted")
        raw_trace.rename(trace)
        selection, action = render(trace, stage / "reports")
        candidate_rows, _, _ = parse(trace.read_text(encoding="utf-8"))
        if args.device_whirl:
            kernel = selection.get("whirl_kernel")
            if (kernel is None or
                    kernel["image"] != device_image.name or
                    not device_image.is_file()):
                raise RuntimeError("device WHIRL trace/image provenance mismatch")
            validate_device_publication_order(
                trace.read_text(encoding="utf-8"), selection)
            run([str(ir_b2a), "-st", "-src", "binary/matmul.device.B",
                 "traces/matmul.device.T"], stage,
                stage / "logs/device-ir_b2a.log")
            run([str(whirl2c), "-cuda",
                 "-CLIST:dotc_file=../source/matmul.whirl.cu",
                 "-fB,matmul.device.B", "matmul.cu"],
                stage / "binary", stage / "logs/whirl2c-cuda.log")
            selected = next(row for row in candidate_rows
                            if row["plan"] == selection["selected"])
            validate_device_whirl_output(
                device_cuda.read_text(encoding="utf-8"),
                device_dump.read_text(encoding="utf-8"), selection, selected)
            run(["bash", str(HERE / "test_whirl_cuda_parity.sh"),
                 "source/matmul.generated.cu", "source/matmul.whirl.cu",
                 "binary"],
                stage, stage / "logs/device-parity.log")
        if "k_loop" not in selection:
            raise RuntimeError("selected K-tile WHIRL loop trace is missing")
        selected_k_loop_dump(trace, stage / "traces/selected-k-loop.T",
                             selection)
        if (len(selection["phases"]) != 12 or
                selection["phases"][7]["selected"] != selection["selected"]):
            raise RuntimeError("selected phase coverage is incomplete")
        for report_root in (stage / "reports", stage / "reports/option-off"):
            full = (report_root / "optimization.log").read_text(encoding="utf-8")
            summary = (report_root / "optimization-summary.log").read_text(
                encoding="utf-8")
            if len([line for line in summary.splitlines() if line.startswith("P")]) != 12:
                raise RuntimeError("phase summary is incomplete")
            for phase, (filename, _) in enumerate(PHASE_LOGS):
                content = (report_root / "phases" / filename).read_text(
                    encoding="utf-8")
                if (not content.startswith("# What it summarizes:") or
                        full.count(f"## P{phase} {filename}\n{content}") != 1):
                    raise RuntimeError(f"phase report omitted or duplicated: {filename}")
        tile_report = (stage / "reports/phases/tile.log").read_text(
            encoding="utf-8")
        if "decision=selected" not in tile_report or "decision=not_selected_cost" not in tile_report:
            raise RuntimeError("selected and skipped tile candidates are not reported")
        for filename in ("semantic.log", "locality.log"):
            content = (stage / "reports/phases" / filename).read_text(
                encoding="utf-8")
            if "tensor=" not in content or "detail=not_recorded" in content:
                raise RuntimeError(f"active P0/P1 facts are missing: {filename}")
        source_tree = ast.parse((stage / "source/fixture.py").read_text(
            encoding="utf-8"))
        matmul_lines = [node.lineno for node in ast.walk(source_tree)
                        if isinstance(node, ast.Call)
                        and isinstance(node.func, ast.Attribute)
                        and node.func.attr == "common_matmul"]
        if selection["reason"] not in ("lowest_complete_relative_cost",
                                       "roofline_near_cost"):
            raise RuntimeError("tile selection reason was not recorded")
        if len(matmul_lines) != 1 or any(
                row["source_line"] != matmul_lines[0]
                for row in candidate_rows):
            raise RuntimeError("tile candidate source lines do not match input Python")
        if not cuda_source.is_file():
            raise RuntimeError("Open64 did not emit the selected CUDA tile")
        text = cuda_source.read_text(encoding="utf-8")
        required = ("tile_plan=", "cost=", "__global__ void ai_matmul_tile_f32",
                    "__shared__ float as", "__shared__ float bs",
                    "__syncthreads()", "#define BM", "#define BK")
        if any(token not in text for token in required):
            raise RuntimeError("compiler-emitted CUDA source is incomplete")
        handoff = m2_handoff(cuda_source, trace, binary, lowered,
                             candidate_rows, selection, args.apply)
        if args.device_whirl:
            handoff.update(device_whirl_path="binary/matmul.device.B",
                           device_whirl_sha256=sha256(device_image),
                           device_cuda_path="source/matmul.whirl.cu",
                           device_cuda_sha256=sha256(device_cuda),
                           device_dump_sha256=sha256(device_dump),
                           device_parity_log_sha256=sha256(stage / "binary/parity.log"))
            review = review_record(stage, selection, candidate_rows, args.apply)
            (stage / "plans/cu-review.json").write_text(
                json.dumps(review, indent=2) + "\n", encoding="utf-8")
        if args.tma:
            tma_required = ("cp_async_bulk_tensor", "cuTensorMapEncodeTiled",
                            "mbarrier_try_wait_parity", "P8_STAGES",
                            "cudaFuncAttributeMaxDynamicSharedMemorySize",
                            "ai_matmul_tile_f32_sync",
                            "ai_matmul_tile_f32_launch_ex", "*used_tma = true")
            if any(token not in text for token in tma_required):
                raise RuntimeError("selected TMA pipeline source is incomplete")
            if "fetch" not in selection or not (stage / "reports/phases/fetch-pipeline.log").is_file():
                raise RuntimeError("selected P8 trace/report is missing")
        expected_rewrite = "generated_runtime_call" if args.apply else "none"
        if (f"optimization_plan={selection['selected']}" not in text or
                action["tile_rewrite"] != expected_rewrite):
            raise RuntimeError("CUDA source and optimization action disagree")
        if sha256(binary) != input_whirl_sha256:
            raise RuntimeError("binary WHIRL changed after source emission")
        if args.apply:
            lowered_text = lowering_dump(trace, stage / "traces/lowered.T")
            if ("__open64_dsl_matmul_generated_v1" not in lowered_text or
                    ".dsl_generated_matmul_plan" not in lowered_text or
                    "__open64_dsl_matmul_v1" in lowered_text or
                    lowered_text == off_text):
                raise RuntimeError("generated tile runtime call missing from WHIRL")
            if sha256(lowered) == option_off_output_sha256:
                raise RuntimeError("backend object is unchanged with tile application")
        else:
            if sha256(lowered) != option_off_output_sha256:
                raise RuntimeError("lowered WHIRL changed in source-only mode")
            run([str(ir_b2a), "-st", "-src", str(lowered),
                 str(stage / "traces/lowered.T")], stage,
                stage / "logs/lowered-ir_b2a.log")
        (stage / "plans/m2-selected-tile.json").write_text(
            json.dumps(handoff, indent=2) + "\n", encoding="utf-8")
        receipt = {
            "schema": "ai-matmul-compiler-cuda-source-v1",
            "status": ("compiler_generated_tile_whirl_applied_unverified" if args.apply
                       else "compiler_selected_cuda_source_unverified"),
            "compiler_generated_from_whirl_analysis": True,
            "whirl_rewrite_applied": args.apply,
            "runtime_verified": False,
            "closes_m2_generated_tile_gate": False,
            "closes_m3_prefetch_pipeline_gate": False,
            "source_path": "source/matmul.generated.cu",
            "source_sha256": sha256(cuda_source),
            "review_cuda_path": "source/matmul.whirl.cu" if args.device_whirl else None,
            "review_cuda_sha256": sha256(device_cuda) if args.device_whirl else None,
            "review_record_path": "plans/cu-review.json" if args.device_whirl else None,
            "m2_handoff_path": "plans/m2-selected-tile.json",
            "m2_handoff_sha256": sha256(stage / "plans/m2-selected-tile.json"),
            "decision_trace_path": "traces/compile.t",
            "cuda_source_emitter": "legacy_template",
            "device_whirl_emitted": args.device_whirl,
            "device_whirl_path": "binary/matmul.device.B" if args.device_whirl else None,
            "device_whirl_sha256": sha256(device_image) if args.device_whirl else None,
            "device_whirl_dump_path": "traces/matmul.device.T" if args.device_whirl else None,
            "device_cuda_path": "source/matmul.whirl.cu" if args.device_whirl else None,
            "device_cuda_sha256": sha256(device_cuda) if args.device_whirl else None,
            "whirl2c_sha256": sha256(whirl2c) if args.device_whirl else None,
            "device_parity_passed": args.device_whirl,
            "device_parity_log_path": "binary/parity.log" if args.device_whirl else None,
            "cuda_legacy_emit_default": True,
            "selected_k_loop_dump_path": "traces/selected-k-loop.T",
            "selected_k_loop_scope": selection["k_loop"]["scope"],
            "option_off_trace_path": "traces/option-off.t",
            "optimization_summary_path": "reports/optimization-summary.log",
            "option_off_summary_path": "reports/option-off/optimization-summary.log",
            "tile_log_path": "reports/phases/tile.log",
            "phase_coverage_complete": True,
            "m1_trace_report_contract_verified": True,
            "m1_accepted": False,
            "m1_acceptance_blocker": "native_m0_baseline_receipt_pending",
            "selected_optimization_plan_id": selection["selected"],
            "selection_reason": selection["reason"],
            "p8_tma_requested": args.tma,
            "p8_selected_plan_id": selection.get("fetch", {}).get("selection", {}).get("selected"),
            "p8_movement": "tma_2d" if args.tma else "synchronous",
            "input_whirl_sha256": input_whirl_sha256,
            "lowered_whirl_sha256": None if args.apply else option_off_output_sha256,
            "lowered_whirl_byte_identical_with_option_off": None if args.apply else True,
            "backend_object_sha256": sha256(lowered) if args.apply else None,
            "backend_object_byte_identical_with_option_off":
                sha256(lowered) == option_off_output_sha256 if args.apply else None,
            "post_lowering_dump_sha256":
                sha256(stage / "traces/lowered.T") if args.apply else None,
            "target": "nvidia_hopper_analysis_profile",
            "host": platform.platform(),
            "opencc_sha256": sha256(opencc),
            "backend_sha256": sha256(backend),
            "option_off_command": baseline_command,
            "command": command,
        }
        (stage / "receipt.json").write_text(
            json.dumps(receipt, indent=2) + "\n", encoding="utf-8"
        )
        files = sorted(p for p in stage.rglob("*") if p.is_file())
        (stage / "SHA256SUMS").write_text(
            "".join(f"{sha256(p)}  {p.relative_to(stage)}\n" for p in files),
            encoding="utf-8",
        )
        if args.digest_output:
            output = parent / f"sha256-{sha256(trace)}"
            if output.exists():
                raise RuntimeError(f"digest artifact already exists: {output}")
        stage.rename(output)
    except Exception as error:
        reports = stage / "reports"
        reports.mkdir(exist_ok=True)
        (reports / "optimization-summary.log").write_text(
            "# What it summarizes: Failed matmul compiler-source run; "
            "no optimization decision is certified.\n"
            f"run=failed error={type(error).__name__} detail={error}\n",
            encoding="utf-8",
        )
        print(f"failed artifact retained for diagnosis: {stage}", file=sys.stderr)
        raise
    selected_source = ("source/matmul.whirl.cu" if args.device_whirl else
                       "source/matmul.generated.cu")
    print(f"compiler-selected CUDA source: {output / selected_source}")


if __name__ == "__main__":
    main()
