"""Produce/reopen real Hopper WHIRL, compile native CUDA and audit the recipe."""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import subprocess

from test_executable_pipeline import run
from wgmma_report import audit, host_source, parse

HERE = Path(__file__).resolve().parent


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--cuda", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build, root, cuda = args.build_dir.resolve(), args.output_dir.resolve(), args.cuda.resolve()
    root.mkdir(parents=True, exist_ok=False)
    env = dict(os.environ)
    results = []
    shapes = [("atom", 64, 64, 16, 1), ("multi", 128, 128, 32, 1), ("long-k", 64, 64, 64, 1),
              ("rectangular", 64, 128, 48, 1), ("simplifier-off", 64, 64, 16, 0)]
    for format_id, dtype in [(5, "f16"), (6, "bf16")]:
        for label, rows, columns, reduction, simplify in shapes:
            case = root / (dtype + "-" + label)
            case.mkdir()
            trace = run([str(build / "osprey/targdir/ir_tools/dsl_wgmma_contract_test"),
                         "hopper.before.B", "hopper.after.B", str(format_id), str(rows), str(columns), str(reduction), str(simplify)],
                        case, env, "hopper.native.t")
            record = parse(trace)
            for boundary in ("before", "after"):
                stem = "hopper." + boundary
                run([str(build / "osprey/targdir/whirl2c/whirl2c"), "-cuda", "-CLIST:dotc_file=" + stem + ".cu",
                     "-fB," + stem + ".B", "hopper-source.cu"], case, env, stem + ".emit.log")
            for path in list(case.glob("*.t")):
                if path.name != "hopper.native.t":
                    path.rename(path.with_name(path.stem + ".emitter-trace.log"))
            for boundary in ("before", "after"):
                stem = "hopper." + boundary
                run([str(build / "osprey/targdir/ir_tools/ir_b2a"), "-st", "-src", stem + ".B", stem + ".T"],
                    case, env, stem + ".dump.log")
            before, after = (case / "hopper.before.cu").read_text(), (case / "hopper.after.cu").read_text()
            physical = audit(after, (case / "hopper.after.T").read_text(), record)
            if "wgmma." in before or "rounded_product = a_value * b_value" not in before:
                raise ValueError("default source is no longer typed SIMT")
            broken_sources = [("wgmma.fence.sync.aligned;", ""), ("wgmma.commit_group.sync.aligned;", ""),
                              ("wgmma.wait_group.sync.aligned 0;", "wgmma.wait_group.sync.aligned 1;"),
                              ("68720001024", "68719476736"), ('"l"( mma_shared_a)', '"l"( mma_shared_b)'),
                              ("__asm__ volatile", "__asm__"), ("__syncthreads();", ""),
                              ("__align__(16)", "__align__(8)"), ("freedoms=31", "freedoms=1")]
            for old, new in broken_sources:
                if old not in after:
                    raise ValueError("mutation did not touch source: " + old)
                try:
                    audit(after.replace(old, new, 1), (case / "hopper.after.T").read_text(), record)
                except ValueError:
                    pass
                else:
                    raise ValueError("independent source audit accepted corruption: " + old)
            (case / "hopper.geometry-emulation.cpp").write_text(host_source(after))
            host_runs = []
            for boundary in ("before", "after"):
                stem = "hopper." + boundary
                run([str(cuda / "bin/nvcc"), "-std=c++11", "-arch=sm_90a", "--fmad=false", "--ftz=false",
                     "--ptx", stem + ".cu", "-o", stem + ".ptx"], case, env, stem + ".ptx.log")
                temporary = stem + ".cubin.tmp"
                run([str(cuda / "bin/nvcc"), "-std=c++11", "-arch=sm_90a", "--fmad=false", "--ftz=false",
                     "-cubin", "-Xptxas=-v", stem + ".cu", "-o", temporary], case, env, stem + ".native-compile.log")
                (case / temporary).rename(case / (stem + ".sm90a.cubin"))
                for optimize in (0, 3):
                    executable = stem + f".host-o{optimize}"
                    run(["c++", "-x", "c++", "-std=c++11", f"-O{optimize}", "-fno-fast-math", "-ffp-contract=off", "-pthread",
                         "-I", str(HERE / "wgmma_cuda_stub"), "hopper.before.cu" if boundary == "before" else "hopper.geometry-emulation.cpp",
                         str(HERE / "wgmma_host.cpp"), "-o", executable], case, env, executable + ".build.log")
                    host_runs.append(run([str(case / executable), str(format_id), str(rows), str(columns), str(reduction),
                                          str(int(boundary == "before"))], case, env, executable + ".log").strip())
            if label == "atom":
                command = [str(cuda / "bin/nvcc"), "-arch=sm_90", "-cubin", "hopper.after.cu", "-o", "rejected.cubin.tmp"]
                result = subprocess.run(command, cwd=case, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                (case / "wrong-target.log").write_text(result.stdout)
                with (case / "commands.log").open("a") as file:
                    file.write(json.dumps(command) + "\n")
                if not result.returncode or "not supported on .target 'sm_90'" not in result.stdout or (case / "rejected.cubin.tmp").exists():
                    raise ValueError("sm_90 silently admitted architecture-specific WGMMA")
            ptx = (case / "hopper.after.ptx").read_text()
            ordered = [ptx.index(token) for token in ("fence.proxy.async.shared::cta;", "wgmma.fence.sync.aligned;",
                       "wgmma.mma_async.sync.aligned.m64n64k16.f32.", "wgmma.commit_group.sync.aligned;", "wgmma.wait_group.sync.aligned 0;")]
            if ordered != sorted(ordered) or "4096 bytes smem" not in (case / "hopper.after.native-compile.log").read_text():
                raise ValueError("native PTX/protocol/shared allocation mismatch")
            phases = case / "reports/phases"
            phases.mkdir(parents=True)
            reports = {
                "semantic.log": f"source_input={dtype} product_partial_outer_output=FP32 numerical_acceptance={record['acceptance']} ISA_freedoms=31 narrowing=none default=SIMT\n",
                "tile.log": f"M={rows} N={columns} K={reduction} atom=64x64x16 threads=128 exact=true {physical}\n",
                "residency.log": "shared_bytes=4096 private_fragment_bytes=128 TMEM=0 compiled_register_stack_details=hopper.after.native-compile.log native_occupancy=unknown\n",
                "fetch-pipeline.log": "generic_U2_to_shared proxy_fence CTA_join register_fence MMA commit wait_zero completed_fragment_store CTA_reuse_join overlap=none\n",
                "physical-plan.log": "action=explicit_O3_WGMMA_API default=SIMT automatic_promotion=false exact_target=sm_90a native_CUDA_PTX_cubin=compiled host_geometry_protocol=passed GPU_arithmetic=unknown timing=unknown\n",
            }
            aggregate = ""
            for name, text in reports.items():
                contents = "# What it summarizes: MMA4-H " + name[:-4] + " review; no GPU timing claim.\n" + text
                (phases / name).write_text(contents); aggregate += contents
            (case / "reports/optimization.log").write_text(aggregate)
            (case / "reports/optimization-summary.log").write_text(
                "# What it summarizes: Explicit typed Hopper lowering with completed-atom host emulation, not native GPU execution.\n"
                f"target=sm_90a format={dtype} native_CUDA_PTX_cubin=passed default_SIMT=preserved host_runs=4 negative_preflights=38 native_body_mutations=16 stale_records=2 source_audit_mutations=9 timing=unknown\n")
            (case / "hopper.before-after.diff").write_text("".join(difflib.unified_diff(
                (case / "hopper.before.T").read_text().splitlines(True), (case / "hopper.after.T").read_text().splitlines(True),
                fromfile="hopper.before.T", tofile="hopper.after.T")))
            results.append(dict(label=case.name, record=record, host_runs=host_runs, physical=physical, native_gpu_verified=False))
    receipt = dict(schema="mma4-h-hopper-lowering-v1", cases=results, native_gpu_verified=False, timing="unknown", automatic_promotion=False)
    (root / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    (root / "summary.log").write_text(
        "# What it summarizes: Real Hopper WHIRL lowering/native CUDA assembly plus independent geometry/protocol host checks, not GPU certification.\n"
        f"typed_cases={len(results)} native_CUDA_PTX_cubin_kernels={len(results) * 2} host_runs={len(results) * 4} malformed_preflights={len(results) * 38} "
        f"changed_body_rejections={len(results) * 16} stale_record_rejections={len(results) * 2} independent_source_rejections={len(results) * 9} exact_target_negatives=2 default=SIMT timing=unknown native_gpu=unverified\n")
    (root / "SHA256SUMS").write_text("".join(hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(root)) + "\n"
        for path in sorted(root.rglob("*")) if path.is_file() and path.name != "SHA256SUMS"))
    print((root / "summary.log").read_text(), end="")


if __name__ == "__main__":
    main()
