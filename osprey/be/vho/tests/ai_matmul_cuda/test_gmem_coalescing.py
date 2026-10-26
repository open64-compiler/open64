"""Exercise cost-gated GMEM lane maps and WHIRL-derived CUDA source."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

from ai_opt_report import parse, render
from compile_source import validate_device_whirl_output, validate_selected_source


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[4]


def run(command, cwd, env, log):
    result = subprocess.run(command, cwd=cwd, env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (cwd / log).write_text(result.stdout, encoding="utf-8")
    if result.returncode:
        raise RuntimeError(f"command failed: {cwd / log}")
    return result.stdout


def compile_case(root, build, env, k, expected_family, smem_search=True):
    root.mkdir()
    (root / "traces").mkdir()
    opencc = build / "osprey/targdir/driver/opencc"
    ir_b2a = build / "osprey/targdir/ir_tools/ir_b2a"
    whirl2c = build / "osprey/targdir/whirl2c/whirl2c"
    run([sys.executable, str(HERE / "edge_matmul_fixture.py"),
         "gemm.B", "128", "128", str(k)], root, env, "fixture.log")
    run([str(opencc), f"-run-build={build}", "-O3", "-noinline", "-c", "-keep",
         "-Wb,-DSL:cuda_tile_output=gemm.generated.cu",
         "-Wb,-DSL:cuda_whirl_output=gemm.device.B",
         f"-Wb,-DSL:cuda_smem_layout_search={'ON' if smem_search else 'OFF'}",
         "-Wb,-DSL:cuda_tile_apply=ON", "gemm.B"],
        root, env, "opencc.log")
    trace = root / "gemm.t"
    candidates, selection, action = parse(trace.read_text(encoding="utf-8"))
    selected_gmem_line = next(line for line in trace.read_text(
        encoding="utf-8").splitlines()
        if line.startswith(f"AI_OPT 7 TILE_GMEM 1 {selection['selected']} "))
    changed = selected_gmem_line.split()
    changed[7] = str(int(changed[7]) + 32)
    try:
        parse(trace.read_text(encoding="utf-8").replace(
            selected_gmem_line, " ".join(changed), 1))
    except ValueError as error:
        if "GMEM sectors disagree" not in str(error):
            raise
    else:
        raise RuntimeError("altered GMEM sector trace was accepted")
    if selection.get("cost_model") != "v5":
        raise RuntimeError("GMEM sector cost model was not selected")
    selected = next(row for row in candidates
                    if row["plan"] == selection["selected"])
    if (any(row["smem"]["b_layout"] == "xor_columns" for row in candidates)
            != smem_search or
            (selected["smem"]["b_layout"] == "xor_columns") != smem_search):
        raise RuntimeError("SMEM search option or selected B layout was lost")
    smem_line = next(line for line in trace.read_text().splitlines()
                     if line.startswith(f"AI_OPT 7 TILE_SMEM 1 {selected['tile']} "))
    changed = smem_line.split()
    changed[14] = str(int(changed[14]) + 1)
    try:
        parse(trace.read_text().replace(smem_line, " ".join(changed), 1))
    except ValueError as error:
        if "SMEM record disagrees" not in str(error):
            raise
    else:
        raise RuntimeError("altered SMEM bank trace was accepted")
    if (selected["family"] != expected_family or
            action["tile_rewrite"] != "generated_runtime_call"):
        raise RuntimeError("unexpected cost-selected GMEM lane map")
    validate_selected_source((root / "gemm.generated.cu").read_text(
        encoding="utf-8"), candidates, selection)
    run([str(ir_b2a), "-st", "-src", "gemm.device.B",
         "traces/gemm.device.T"],
        root, env, "ir_b2a.log")
    run([str(whirl2c), "-cuda", "-CLIST:dotc_file=gemm.whirl.cu",
         "-fB,gemm.device.B", "gemm.cu"], root, env, "whirl2c.log")
    validate_device_whirl_output(
        (root / "gemm.whirl.cu").read_text(encoding="utf-8"),
        (root / "traces/gemm.device.T").read_text(encoding="utf-8"),
        selection, selected)
    parity = run(["bash", str(HERE / "test_whirl_cuda_parity.sh"),
                  "gemm.generated.cu", "gemm.whirl.cu", "parity"],
                 root, env, "parity-run.log")
    if "whirl_vs_reference=0" not in parity or "whirl_vs_legacy=0" not in parity:
        raise RuntimeError("selected GMEM lane map failed host CTA parity")
    render(trace, root / "reports")
    by_family = {row["family"]: row for row in candidates}
    old = by_family["cuda_128"]
    coalesced = by_family["cuda_128_coalesced"]
    if (coalesced["gmem"]["c"] >= old["gmem"]["c"] or
            coalesced["gmem"]["a"] != old["gmem"]["a"] or
            coalesced["gmem"]["b"] != old["gmem"]["b"]):
        raise RuntimeError("coalesced map did not reduce C sectors")
    return dict(problem=(128, 128, k), selected_plan=selected["plan"],
                selected_family=selected["family"],
                selected_relative_cost=selected["total"],
                smem=selected["smem"], smem_search=smem_search,
                old_c_sector_bytes=old["gmem"]["c"],
                coalesced_c_sector_bytes=coalesced["gmem"]["c"],
                parity=parity.strip())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build = args.build_dir.resolve()
    output = args.output_dir.resolve()
    output.mkdir(parents=True, exist_ok=False)
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join(filter(None, (
        str(build / "osprey/targdir/torch2whirl/python"),
        str(REPO / "osprey/torch2whirl/python"),
        env.get("PYTHONPATH", ""),
    )))
    short = compile_case(output / "k32", build, env, 32,
                         "cuda_128_coalesced")
    long = compile_case(output / "k128", build, env, 128, "cuda_128")
    no_search = compile_case(output / "no-smem-search", build, env, 128,
                             "cuda_128_coalesced", smem_search=False)
    receipt = dict(schema="ai-gmem-coalescing-v1", short_k=short,
                   long_k=long, no_smem_search=no_search, host_cta_parity_passed=True,
                   native_cuda_compiled=False, gpu_performance_verified=False)
    (output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n",
                                           encoding="utf-8")
    files = sorted(path for path in output.rglob("*") if path.is_file())
    (output / "SHA256SUMS").write_text(
        "".join(f"{hashlib.sha256(path.read_bytes()).hexdigest()}  "
                f"{path.relative_to(output)}\n" for path in files),
        encoding="utf-8")
    print(f"GMEM coalescing choices and CUDA parity verified: {output}")


if __name__ == "__main__":
    main()
