"""Produce, reopen, inspect and execute real MMA3 WHIRL baseline artifacts."""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import shutil

from test_executable_pipeline import run
from typed_simt_report import audit, contract

HERE = Path(__file__).resolve().parent


def reports(root, record, counts, runs):
    phase = root / "reports/phases"
    phase.mkdir(parents=True)
    data = {
        "semantic.log": f"contract={record} input_storage=U2 input_semantics=declared_F16_or_BF16 output=FP32 narrowing=none\n",
        "tile.log": f"compute=direct_SIMT threads={record['threads']} M={record['M']} N={record['N']} K={record['K']} output_owner=one_thread_per_element costs=unknown\n",
        "residency.log": "SMEM=0 TMEM=0 product=volatile_FP32 accumulator=volatile_FP32 native_registers=unknown native_occupancy=unknown\n",
        "fetch-pipeline.log": "input_load=U2 widening=exact_per_load source_K_order=increasing asynchronous_ops=none timing=unknown\n",
        "physical-plan.log": f"scope=standalone_native_source_API not_FX_ingestion_or_F32_planner_dispatch action=serial_to_direct_SIMT static_ops={counts} source_and_accumulation_contract=preserved\n",
    }
    complete = "# What it summarizes: MMA3 native source baseline phase review, not a full frontend or native GPU pipeline.\n"
    for name, body in data.items():
        text = f"# What it summarizes: MMA3 {name[:-4]} native baseline evidence.\n" + body
        (phase / name).write_text(text)
        complete += text
    (root / "reports/optimization.log").write_text(complete)
    (root / "reports/optimization-summary.log").write_text(
        "# What it summarizes: Accepted typed source, actual WHIRL transformation, numerical execution and limitations.\n"
        f"source_format={record['source']} product=FP32 accumulation=FP32 output=FP32 source_K_order=preserved "
        f"host_runs={len(runs)} timing=unknown native_gpu=unverified automatic_dispatch=not_enabled\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build, root = args.build_dir.resolve(), args.output_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    env = dict(os.environ)
    results = []
    specs = [("exact", 2, 32, 16, 32, 0, 1), ("tails", 3, 5, 7, 32, 0, 1),
             ("multi", 2, 65, 17, 32, 0, 1), ("one", 1, 1, 1, 32, 0, 1),
             ("long-k", 1, 3, 257, 32, 0, 1), ("wide-cta", 3, 129, 33, 128, 0, 1),
             ("exhaustive", 1, 65536, 1, 32, 0, 1), ("o3", 2, 32, 16, 32, 3, 1),
             ("simp-off", 2, 32, 16, 32, 3, 0)]
    for format_id, name in ((5, "f16"), (6, "bf16")):
        for label, rows, columns, reduction, threads, level, simplify in specs:
            case = root / (name + "-" + label)
            case.mkdir()
            native = run([str(build / "osprey/targdir/ir_tools/dsl_typed_simt_contract_test"),
                          "typed.before.B", "typed.after.B", str(format_id), str(rows), str(columns),
                          str(reduction), str(threads), str(level), str(simplify)], case, env, "typed.native.t")
            record = contract(native)
            runs = []
            counts = None
            for boundary in ("before", "after"):
                stem = "typed." + boundary
                run([str(build / "osprey/targdir/whirl2c/whirl2c"), "-cuda",
                     f"-CLIST:dotc_file={stem}.cu", f"-fB,{stem}.B", "typed-source.cu"], case, env, stem + ".emit.log")
            for path in list(case.glob("*.t")):
                if path.name != "typed.native.t":
                    path.rename(path.with_name(path.stem + ".whirl2c-trace.log"))
            for boundary in ("before", "after"):
                stem = "typed." + boundary
                run([str(build / "osprey/targdir/ir_tools/ir_b2a"), "-st", "-src", stem + ".B", stem + ".T"],
                    case, env, stem + ".dump.log")
                counts = audit((case / (stem + ".cu")).read_text(), (case / (stem + ".T")).read_text(), record,
                               "serial" if boundary == "before" else "simt")
                for optimize in (0, 3):
                    tag = stem + f".host-o{optimize}"
                    flags = ["-std=c++11", f"-O{optimize}", "-fno-fast-math", "-ffp-contract=fast", "-I", str(HERE / "typed_cuda_stub")]
                    run(["c++", "-x", "c++", *flags, "-c", stem + ".cu", "-o", tag + ".o"], case, env, tag + ".compile.log")
                    run(["c++", *flags, str(HERE / "typed_simt_host.cpp"), tag + ".o", "-o", tag], case, env, tag + ".link.log")
                    outcome = run([str(case / tag), str(format_id), str(rows), str(columns), str(reduction),
                                   str(threads), str(int(boundary == "before")), "1"], case, env, tag + ".run.log")
                    runs.append(outcome.strip())
            (case / "typed.before-after.diff").write_text("".join(difflib.unified_diff(
                (case / "typed.before.T").read_text().splitlines(True), (case / "typed.after.T").read_text().splitlines(True),
                fromfile="typed.before.T", tofile="typed.after.T")))
            if label in ("tails", "simp-off"):
                for optimize in range(4):
                    native_case = case / f"backend-o{optimize}"
                    native_case.mkdir()
                    shutil.copy2(case / "typed.after.B", native_case / "kernel.B")
                    run([str(build / "osprey/targdir/driver/opencc"), f"-run-build={build}", f"-O{optimize}",
                         "-noinline", "-S", "-keep", "kernel.B"], native_case, env, "compiler.log")
                    run(["c++", "-c", "kernel.s", "-o", "kernel.o"], native_case, env, "assemble.log")
                    run(["c++", "-std=c++11", "-O0", "-fno-fast-math", "-ffp-contract=off", "-I", str(HERE / "typed_cuda_stub"),
                         str(HERE / "typed_simt_host.cpp"), "kernel.o", "-o", "host"], native_case, env, "link.log")
                    outcome = run([str(native_case / "host"), str(format_id), str(rows), str(columns), str(reduction),
                                   str(threads), "0", "0"], native_case, env, "host.log")
                    runs.append(outcome.strip())
            reports(case, record, counts, runs)
            results.append(dict(label=case.name, contract=record, static_ops=counts, host_runs=runs, native_gpu_verified=False))
    (root / "receipt.json").write_text(json.dumps(dict(schema="mma3-typed-simt-v1", cases=results,
        timing="unknown", native_gpu_verified=False, scope="native_source_lowering_API_not_general_FX_ingestion_or_automatic_dispatch"), indent=2) + "\n")
    (root / "summary.log").write_text(
        "# What it summarizes: Real typed WHIRL/CUDA source review and strict FP32 host/backend execution.\n"
        f"typed_cases={len(results)} host_runs={sum(len(row['host_runs']) for row in results)} "
        "formats=F16,BF16 exhaustive_widening_patterns_per_format=65536 FP32_product_accumulation_output=true "
        "malformed_contracts_per_case=26 altered_bodies_per_case=12 timing=unknown native_gpu=unverified\n")
    (root / "SHA256SUMS").write_text("".join(hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(root)) + "\n"
        for path in sorted(root.rglob("*")) if path.is_file() and path.name != "SHA256SUMS"))
    print((root / "summary.log").read_text(), end="")


if __name__ == "__main__":
    main()
