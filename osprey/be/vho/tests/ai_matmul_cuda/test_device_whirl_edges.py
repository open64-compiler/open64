"""Check predicated M/N/K edges in selected device WHIRL and emitted CUDA."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

from ai_opt_report import parse, render
from compile_source import validate_device_whirl_output, validate_selected_source


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[4]


def run(command, cwd, env):
    result = subprocess.run(command, cwd=cwd, env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if result.returncode:
        raise RuntimeError(f"command returned {result.returncode}: {command}\n"
                           f"{result.stdout}")
    return result.stdout


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args()
    build = args.build_dir.resolve()
    opencc = build / "osprey/targdir/driver/opencc"
    ir_b2a = build / "osprey/targdir/ir_tools/ir_b2a"
    whirl2c = build / "osprey/targdir/whirl2c/whirl2c"
    for tool in (opencc, ir_b2a, whirl2c):
        if not tool.is_file():
            parser.error(f"missing tool: {tool}")
    if args.output_dir is None:
        temporary = tempfile.TemporaryDirectory(prefix="ai-device-edges-")
        output = Path(temporary.name)
    else:
        output = args.output_dir.resolve()
        output.mkdir(parents=True, exist_ok=False)
        temporary = None
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join(filter(None, (
        str(build / "osprey/targdir/torch2whirl/python"),
        str(REPO / "osprey/torch2whirl/python"),
        env.get("PYTHONPATH", ""),
    )))
    run([sys.executable, str(HERE / "edge_matmul_fixture.py"), "edge.B"],
        output, env)
    def compile_tile(root, applied, require_all_edges):
        flags = ["-O3", "-noinline"] if applied else ["-O0", "-INLINE:none"]
        command = [str(opencc), f"-run-build={build}", *flags, "-c", "-keep",
                   "-Wb,-DSL:cuda_tile_output=edge.generated.cu",
                   "-Wb,-DSL:cuda_whirl_output=edge.device.B"]
        if applied:
            command.extend(("-Wb,-DSL:cuda_tile_apply=ON",
                            "-Wb,-DSL:dump_after_lower=ON"))
        command.append("edge.B")
        run(command, root, env)
        trace_path = root / "edge.t"
        candidates, selection, action = parse(trace_path.read_text(
            encoding="utf-8"))
        selected = next(row for row in candidates
                        if row["plan"] == selection["selected"])
        dimensions = validate_selected_source(
            (root / "edge.generated.cu").read_text(encoding="utf-8"),
            candidates, selection)
        m, n, k = (dimensions[key] for key in ("M", "N", "K"))
        bm, bn, bk = selected["cta"]
        guard_m, guard_n, guard_k = (m % bm != 0, n % bn != 0, k % bk != 0)
        if (require_all_edges and not all((guard_m, guard_n, guard_k))) or \
                (not require_all_edges and
                 (guard_m, guard_n, guard_k) != (False, False, True)):
            raise RuntimeError("edge fixture did not exercise expected tile axes")
        expected = ("zero_fill" if guard_m or guard_k else "direct",
                    "zero_fill" if guard_k or guard_n else "direct",
                    "guarded_store" if guard_m or guard_n else "direct")
        edge = selection.get("edge_plan")
        if edge is None or edge["policy"] != "predicated" or \
                tuple(edge[key] for key in ("a_load", "b_load", "c_store")) \
                != expected:
            raise RuntimeError("device edge guard plan is missing from trace")
        if action["tile_rewrite"] != (
                "generated_runtime_call" if applied else "none"):
            raise RuntimeError("host tile action disagrees with test mode")
        run([str(ir_b2a), "-st", "-src", "edge.device.B", "edge.dump.T"],
            root, env)
        run([str(whirl2c), "-cuda", "-CLIST:dotc_file=edge.whirl.cu",
             "-fB,edge.device.B", "edge.cu"], root, env)
        dump = (root / "edge.dump.T").read_text(encoding="utf-8")
        source = (root / "edge.whirl.cu").read_text(encoding="utf-8")
        validate_device_whirl_output(source, dump, selection, selected)
        expected_count = sum((guard_m or guard_k, guard_k or guard_n,
                              guard_m or guard_n))
        if dump.count("IF {line:") != expected_count:
            raise RuntimeError("device WHIRL did not prune redundant edge guards")
        parity_dir = root / "parity"
        parity = run(["bash", str(HERE / "test_whirl_cuda_parity.sh"),
                      "edge.generated.cu", "edge.whirl.cu", str(parity_dir)],
                     root, env)
        if "whirl_vs_reference=0" not in parity or \
                "whirl_vs_legacy=0" not in parity:
            raise RuntimeError("predicated device parity did not match reference")
        render(trace_path, root / "reports")
        summary = (root / "reports/optimization-summary.log").read_text(
            encoding="utf-8")
        if "edge_plan=predicated" not in summary:
            raise RuntimeError("optimization summary omitted edge guard plan")
        return dict(selected_plan=selected["plan"], tile=selected["cta"],
                    problem=(m, n, k), host_rewrite=action["tile_rewrite"],
                    parity=parity.strip())

    source_only = compile_tile(output, False, True)
    applied = output / "applied"
    applied.mkdir()
    shutil.copy2(output / "edge.B", applied / "edge.B")
    applied_result = compile_tile(applied, True, True)
    relocations = run(["readelf", "-rW", "edge.o"], applied, env)
    if "__open64_dsl_matmul_generated_v1" not in relocations or \
            "__open64_dsl_matmul_generated_v1" not in (
                applied / "edge.t").read_text(encoding="utf-8"):
        raise RuntimeError("applied host PU lacks generated-provider call")
    k_only = output / "k-only"
    k_only.mkdir()
    run([sys.executable, str(HERE / "edge_matmul_fixture.py"),
         "edge.B", "128", "128", "135"], k_only, env)
    k_only_result = compile_tile(k_only, False, False)
    overflow = output / "i4-overflow"
    overflow.mkdir()
    run([sys.executable, str(HERE / "edge_matmul_fixture.py"),
         "edge.B", "50000", "50000", "128"], overflow, env)
    rejected = subprocess.run(
        [str(opencc), f"-run-build={build}", "-O0", "-INLINE:none",
         "-c", "-keep", "-Wb,-DSL:cuda_legacy_emit=OFF",
         "-Wb,-DSL:cuda_whirl_output=edge.device.B", "edge.B"],
        cwd=overflow, env=env, text=True, stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT)
    (overflow / "compile.log").write_text(rejected.stdout, encoding="utf-8")
    if (rejected.returncode == 0 or (overflow / "edge.device.B").exists() or
            "DSL CUDA WHIRL requires a consistent synchronous edge policy"
            not in rejected.stdout):
        raise RuntimeError("I4-unsafe tile was not rejected before publication")
    receipt = dict(schema="ai-device-whirl-edges-v1",
                   source_only=source_only, applied=applied_result,
                   k_only=k_only_result, i4_overflow_rejected=True,
                   native_cuda_verified=False, gpu_correctness_verified=False,
                   gpu_performance_verified=False)
    (output / "receipt.json").write_text(
        json.dumps(receipt, indent=2) + "\n", encoding="utf-8")
    files = sorted(path for path in output.rglob("*") if path.is_file())
    (output / "SHA256SUMS").write_text(
        "".join(f"{hashlib.sha256(path.read_bytes()).hexdigest()}  "
                f"{path.relative_to(output)}\n" for path in files),
        encoding="utf-8")
    print(f"predicated WHIRL tile and host rewrite verified: {output}")
    if temporary is not None:
        temporary.cleanup()


if __name__ == "__main__":
    main()
