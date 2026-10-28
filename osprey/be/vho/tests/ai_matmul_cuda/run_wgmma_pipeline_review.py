"""Retain actual MMA5-H WHIRL/CUDA/PTX/cubin, host and independent model evidence."""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess

from test_executable_pipeline import run
from wgmma_pipeline_report import audit, host_source, model_oracle, parse

HERE = Path(__file__).resolve().parent


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--cuda", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--delivery", type=int, choices=(1, 2), nargs="+", default=(1, 2))
    args = parser.parse_args()
    build, cuda, root = args.build_dir.resolve(), args.cuda.resolve(), args.output_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    results = []
    shapes = [("one", 64, 64, 16, 1), ("two", 64, 64, 32, 1), ("odd", 64, 128, 48, 1),
              ("many", 128, 192, 80, 1), ("simplifier-off", 64, 64, 64, 0)]
    for delivery in args.delivery:
        for format_id in (5, 6):
            for label, rows, columns, reduction, simplify in shapes:
                case = root / f"delivery-{delivery}-format-{format_id}-{label}"
                case.mkdir()
                env = dict(os.environ)
                env.pop("OPEN64_WGMMA_TMA", None)
                if delivery == 2:
                    env["OPEN64_WGMMA_TMA"] = "1"
                trace = run([str(build / "osprey/targdir/ir_tools/dsl_wgmma_pipeline_contract_test"),
                             "hopper.before.B", "hopper.after.B", str(format_id), str(rows), str(columns), str(reduction), str(simplify)],
                            case, env, "hopper.native.t")
                record, model = parse(trace), model_oracle(trace)
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
                source, dump = (case / "hopper.after.cu").read_text(), (case / "hopper.after.T").read_text()
                physical = audit(source, dump, record)
                if "wgmma." in (case / "hopper.before.cu").read_text():
                    raise ValueError("SIMT source changed")
                (case / "hopper.host.cpp").write_text(host_source(source, record))
                mutations = [("wgmma.wait_group.sync.aligned 0;", "wgmma.wait_group.sync.aligned 1;"),
                             ("wgmma.commit_group.sync.aligned;", ""), ("wgmma.fence.sync.aligned;", ""),
                             ("add.u64 desc_a, desc_a, 128;", "add.u64 desc_a, desc_a, 64;"),
                             ('"l"( mma_shared_a)', '"l"( mma_shared_b)'), ("__asm__ volatile", "__asm__"),
                             ("__syncthreads();", ""), ("freedoms=31", "freedoms=1")]
                if delivery == 2:
                    mutations += [("expect_tx.shared::cta.b64 _, [ready], 4096", "expect_tx.shared::cta.b64 _, [ready], 2048"),
                                  ("mbarrier.try_wait.parity.acquire.cta.shared::cta.b64", "mbarrier.test_wait.parity.acquire.cta.shared::cta.b64"),
                                  ("mbarrier.try_wait.parity.acquire.cta.shared::cta.b64", "mbarrier.try_wait.parity.relaxed.cta.shared::cta.b64"),
                                  ("mbarrier.inval.shared::cta.b64 [ready];", ""),
                                  ("cp.async.bulk.shared::cta.global", "cp.async.bulk.shared::cluster.global")]
                for old, new in mutations:
                    mutation_source = re.sub(r" {2,}", " ", source)
                    if old not in mutation_source:
                        raise ValueError("source mutation did not apply: " + old)
                    try:
                        audit(mutation_source.replace(old, new, 1), dump, record)
                    except ValueError:
                        pass
                    else:
                        raise ValueError("changed executable source admitted: " + old)
                host_runs = []
                for boundary in ("before", "after"):
                    stem = "hopper." + boundary
                    for kind, suffix in (("--ptx", ".ptx"), ("-cubin", ".cubin")):
                        temporary = stem + suffix + ".tmp"
                        run([str(cuda / "bin/nvcc"), "-std=c++11", "-arch=sm_90a", "--fmad=false", "--ftz=false",
                             kind, "-Xptxas=-v", stem + ".cu", "-o", temporary], case, env, stem + suffix + ".log")
                        (case / temporary).rename(case / (stem + suffix))
                    for optimize in (0, 3):
                        executable = stem + f".host-o{optimize}"
                        host = "wgmma_host.cpp" if boundary == "before" else "wgmma_pipeline_host.cpp"
                        run(["c++", "-x", "c++", "-std=c++11", f"-O{optimize}", "-fno-fast-math", "-ffp-contract=off", "-pthread",
                             "-I", str(HERE / "wgmma_pipeline_cuda_stub"), stem + ".cu" if boundary == "before" else "hopper.host.cpp",
                             str(HERE / host), "-o", executable], case, env, executable + ".build.log")
                        host_runs.append(run([str(case / executable), str(format_id), str(rows), str(columns), str(reduction),
                                              str(int(boundary == "before"))], case, env, executable + ".log").strip())
                if label == "one":
                    command = [str(cuda / "bin/nvcc"), "-arch=sm_90", "-cubin", "hopper.after.cu", "-o", "rejected.cubin.tmp"]
                    result = subprocess.run(command, cwd=case, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                    (case / "wrong-target.log").write_text(result.stdout)
                    if not result.returncode or "not supported on .target 'sm_90'" not in result.stdout or (case / "rejected.cubin.tmp").exists():
                        raise ValueError("wrong target silently admitted")
                ptx, allocation = (case / "hopper.after.ptx").read_text(), (case / "hopper.after.cubin.log").read_text()
                if "wgmma.mma_async" not in ptx or f"{record['smem']} bytes smem" not in allocation:
                    raise ValueError("native operation/resource allocation mismatch")
                if delivery == 2 and ("CPASYNC" not in ptx.upper() and "cp.async.bulk" not in ptx):
                    raise ValueError("lost real bulk TMA")
                reports = case / "reports"
                reports.mkdir()
                contents = {
                    "semantic.log": f"input_format={format_id} output=FP32 freedoms=31 numerical_acceptance={record['acceptance']} fallback=MMA3_SIMT\n",
                    "tile.log": f"M={rows} N={columns} K={reduction} exact=64x64x16 slots=2 odd_tail=uniform_one_group\n",
                    "residency.log": f"{physical} private_fragment_bytes_per_thread=128 actual_allocation=../hopper.after.cubin.log native_occupancy=unknown\n",
                    "fetch-pipeline.log": ("raw_copy_issue slots=0,1; raw_ready_wait slots=0,1; CTA_ready_join; repack; " if delivery == 2 else "cooperative_repack slots=0,1; ") +
                        "proxy_fence; CTA_publish; register_fence; MMA0; commit0; MMA1_if_present; commit1; wait_zero; fragment_store; CTA_reader_reuse_join; next_batch overlap_assumption=none\n",
                    "physical-plan.log": f"{model} actual_request=explicit_API delivery={delivery} emitted_by_default=SIMT automatic_promotion=false GPU_arithmetic=unknown timing=unknown\n" +
                        "\n".join(line for line in trace.splitlines() if line.startswith("AI_MMA_PIPELINE_MODEL")) + "\n",
                }
                aggregate = ""
                for name, body in contents.items():
                    report = "# What it summarizes: MMA5-H " + name[:-4] + "; synthetic costs are not native timing.\n" + body
                    (reports / name).write_text(report)
                    aggregate += report
                (reports / "optimization.log").write_text(aggregate)
                (reports / "optimization-summary.log").write_text(
                    "# What it summarizes: Bounded explicit Hopper pipeline, not GPU certification.\n"
                    f"default=SIMT explicit_delivery={delivery} target=sm_90a native_compile=passed host_geometry_protocol=passed timing=unknown\n")
                (case / "hopper.before-after.diff").write_text("".join(difflib.unified_diff(
                    (case / "hopper.before.T").read_text().splitlines(True), dump.splitlines(True), fromfile="hopper.before.T", tofile="hopper.after.T")))
                summary = re.search(r"MMA5-H passed: negatives=(\d+) mutations=(\d+) stale=(\d+)", trace)
                if not summary:
                    raise ValueError("missing native negative evidence")
                results.append(dict(label=case.name, record=record, model=model, host_runs=host_runs, physical=physical,
                                    negatives=int(summary[1]), mutations=int(summary[2]), stale=int(summary[3]), source_mutations=len(mutations)))
    receipt = dict(schema="mma5-h-bounded-pipeline-v1", cases=results, default="SIMT", GPU_arithmetic="unknown", timing="unknown")
    (root / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    summary = dict(cases=len(results), native_kernels=2 * len(results), host_runs=4 * len(results),
                   negatives=sum(case["negatives"] for case in results), mutations=sum(case["mutations"] for case in results),
                   source_mutations=sum(case["source_mutations"] for case in results),
                   independent_model_corners=sum(case["model"]["independently_checked_cost_corners"] for case in results),
                   default="SIMT", GPU_arithmetic="unknown", timing="unknown")
    (root / "summary.log").write_text("# What it summarizes: Actual MMA5-H WHIRL/native assembly and host/oracle evidence, not native execution.\n" + json.dumps(summary, sort_keys=True) + "\n")
    (root / "SHA256SUMS").write_text("".join(hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(root)) + "\n"
        for path in sorted(root.rglob("*")) if path.is_file() and path.name != "SHA256SUMS"))
    print((root / "summary.log").read_text(), end="")


if __name__ == "__main__":
    main()
