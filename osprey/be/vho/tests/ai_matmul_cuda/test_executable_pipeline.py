"""Retain executable P8 WHIRL/CUDA schedules and independently audit ordering."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

from ai_opt_report import parse, render, render_program

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[4]


def run(command, cwd, env, name):
    result = subprocess.run(command, cwd=cwd, env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            timeout=240)
    (cwd / name).write_text(result.stdout, encoding="utf-8")
    with (cwd / "commands.log").open("a", encoding="utf-8") as log:
        log.write(json.dumps(command) + "\n")
    if result.returncode:
        raise RuntimeError(f"command failed ({result.returncode}): {cwd / name}")
    return result.stdout


def audit_source(source, dump, tile, tma, direct=False):
    compact = re.sub(r"\s+", "", source)
    bm, bn, bk = tile["cta"]
    stages = 2 if tma else 1
    if direct:
        body = source[source.index("{", source.index('extern "C" __global__')):]
        columns = tile["layout"]["thread_columns"]
        tm, tn = bm * columns // tile["threads"], bn // columns
        k = tile["grid"]["problem"][2]
        if ("__shared__" in source or "__syncthreads" in body or "__open64_cuda_" in body
                or "IF" not in dump or not re.search(r"\*\s*\(a\s*\+", source)
                or not re.search(r"\*\s*\(b\s*\+", source)
                or "q=" not in compact or "acc[" not in source):
            raise ValueError("direct kernel lost guarded global/register contract")
        if any(fragment not in compact for fragment in (
                f"for(base=0;base<{k};base=base+{bk})",
                f"for(q=0;q<{bk};q=q+1)", f"if((base+q)<{k})",
                f"local_row=(tid/{columns})*{tm};", f"local_col=(tid%{columns})*{tn};")):
            raise ValueError("direct source disagrees with selected register map or reduction bounds")
        return
    a_size = stages * bk * (bm + int(tile["layout"]["a_layout"] == "transposed_padded"))
    for name, size in (("as", a_size), ("bs", stages * bk * bn)):
        if re.search(rf"__shared__\s+(?:__align__\(128\)\s+)?_IEEE32\s+{name}\[{size}LL\]", source) is None:
            raise ValueError("incorrect selected staging allocation")
    if not tma:
        if source.count("__syncthreads();") != 2 or "__open64_cuda_tma_" in source:
            raise ValueError("non-TMA publication/release ordering lost")
        return
    calls = re.findall(r"(__open64_cuda_\w+|__syncthreads)\s*\(", source)
    expected = ["barrier_init", "barrier_init", "proxy_publish", "sync",
                "tma_acquire", "tma_issue", "tma_issue", "tma_wait", "sync",
                "tma_acquire", "tma_issue", "tma_issue",
                "barrier_invalidate", "barrier_invalidate"]
    expected = ["__syncthreads" if name == "sync" else "__open64_cuda_" + name
                for name in expected]
    # Ignore function prototypes; only statements in the kernel body count.
    body = source[source.index("{", source.index('extern "C" __global__')):]
    calls = re.findall(r"(__open64_cuda_\w+|__syncthreads)\s*\(", body)
    if calls != expected:
        raise ValueError("acquire/issue/wait/release call order disagrees with schedule")
    if ("__align__(128)" not in source or "__shared__ _UINT64 ready[2LL]" not in source
            or "#include <open64_cuda_pipeline.cuh>" not in source
            or len(re.findall(r"CALL[^\n]*<[^>]*__open64_cuda_tma_issue>", dump)) != 4):
        raise ValueError("missing concrete WHIRL pipeline operation or alignment")
    # Inspect exact arithmetic after whitespace normalization, not trace claims.
    if (f"slot=(base/{bk})%2;" not in compact
            or f"((base/{bk})/2)%2" not in compact
            or f"base<{tile['grid']['problem'][2] - 2*bk}" not in compact
            or f"base+{2*bk}" not in compact):
        raise ValueError("slot, phase, or refill bound disagrees with selected tile")


def compile_case(root, build, env, k, tma, extra_options=(), dimensions=(128, 128)):
    root.mkdir()
    run([sys.executable, str(HERE / "edge_matmul_fixture.py"),
         "gemm.B", str(dimensions[0]), str(dimensions[1]), str(k)], root, env, "fixture.log")
    run([str(build / "osprey/targdir/ir_tools/ir_b2a"), "-st", "-src",
         "gemm.B", "gemm.input.T"], root, env, "input-dump.log")
    command = [str(build / "osprey/targdir/driver/opencc"),
               f"-run-build={build}", "-O3", "-noinline", "-c", "-keep",
               "-Wb,-DSL:cuda_legacy_emit=OFF",
               "-Wb,-DSL:cuda_whirl_output=gemm.device.B"]
    if tma:
        command.append("-Wb,-DSL:cuda_tma_prefetch=ON")
    command.extend(extra_options)
    run(command + ["gemm.B"], root, env, "compiler.log")
    candidates, selection, action = parse((root / "gemm.t").read_text())
    selected = next(row for row in candidates if row["plan"] == selection["selected"])
    proof = selection.get("pipeline_kernel")
    if proof is None or ("joint" not in selection and (proof["stages"] == 2) != tma):
        raise RuntimeError("selected executable P8 pipeline missing")
    tma = proof["stages"] == 2
    direct = proof["abi"] == "direct_ptr_v1"
    trace = (root / "gemm.t").read_text()
    for old, new in (("whirl_order=verified", "whirl_order=unverified"),
                     (proof["order"], "issue_compute_release")):
        try:
            parse(trace.replace(old, new, 1))
        except ValueError:
            pass
        else:
            raise RuntimeError("corrupt pipeline proof trace was accepted")
    run([str(build / "osprey/targdir/ir_tools/ir_b2a"), "-st", "-src",
         "gemm.device.B", "gemm.dump.T"], root, env, "device-dump.log")
    run([str(build / "osprey/targdir/whirl2c/whirl2c"), "-cuda",
         "-CLIST:dotc_file=gemm.whirl.cu", "-fB,gemm.device.B", "gemm.cu"],
        root, env, "whirl2c.log")
    source = (root / "gemm.whirl.cu").read_text()
    dump = (root / "gemm.dump.T").read_text()
    audit_source(source, dump, selected, tma, direct)
    if tma:
        for before, after in (("__open64_cuda_tma_wait(", "__open64_cuda_missing_wait("),
                               (" / 2) % 2", " / 2) % 3"),
                               (f"base < {k - 2*selected['cta'][2]}", "base < 999999")):
            changed = source.replace(before, after, 1)
            if changed == source:
                raise RuntimeError("negative source audit did not alter its target")
            try:
                audit_source(changed, dump, selected, tma)
            except ValueError:
                pass
            else:
                raise RuntimeError("corrupt pipeline source order was accepted")
    flags = [f"-DAI_TEST_K={k}", f"-DAI_TEST_TMA={int(tma)}",
             f"-DAI_TEST_M={dimensions[0]}", f"-DAI_TEST_N={dimensions[1]}",
             f"-DAI_TEST_THREADS={selected['threads']}"]
    flags += [f"-DAI_TEST_{name}={value}" for name, value in
              zip(("BM", "BN", "BK"), selected["cta"])]
    cxx = env.get("CXX", "c++")
    run([cxx, "-x", "c++", "-std=c++11", "-O0", "-pthread",
         "-I", str(HERE / "cuda_stub"), "-c", "gemm.whirl.cu", "-o", "gemm.host-sim.o"],
        root, env, "host-sim-compile.log")
    run([cxx, "-std=c++11", "-O0", "-pthread", "-I", str(HERE / "cuda_stub"),
         *flags, str(HERE / "executable_pipeline_sim.cpp"), "gemm.host-sim.o",
         "-o", "pipeline-sim"], root, env, "host-sim-link.log")
    simulation = run([str(root / "pipeline-sim")], root, env, "host-sim.log")
    render(root / "gemm.t", root / "reports")
    shutil.copy2(REPO / "osprey/include/open64_cuda_pipeline.cuh", root)
    return dict(problem=(*dimensions, k), selected_tile=selected["plan"],
                tile=selected["cta"], pipeline=proof,
                host_rewrite=action["tile_rewrite"], simulation=simulation.strip(),
                native_cuda_verified=False, joint=selection.get("joint"))


def negative_case(root, build, env, apply):
    root.mkdir()
    k = 128 if apply else 17
    run([sys.executable, str(HERE / "edge_matmul_fixture.py"),
         "gemm.B", "128", "128", str(k)], root, env, "fixture.log")
    command = [str(build / "osprey/targdir/driver/opencc"), f"-run-build={build}",
               "-O3", "-noinline", "-c", "-keep",
               "-Wb,-DSL:cuda_legacy_emit=OFF", "-Wb,-DSL:cuda_tma_prefetch=ON",
               "-Wb,-DSL:cuda_whirl_output=gemm.device.B"]
    if apply:
        command.append("-Wb,-DSL:cuda_tile_apply=ON")
    result = subprocess.run(command + ["gemm.B"], cwd=root, env=env,
                            text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (root / "rejection.log").write_text(result.stdout)
    expected = "M2 generated tile requires" if apply else "P8 selected no legal TMA plan"
    if result.returncode == 0 or (root / "gemm.device.B").exists() or expected not in result.stdout:
        raise RuntimeError("unsupported TMA contract was not rejected before publication")
    return dict(host_application=apply, input_k=k, rejected_before_publication=True)


def multi_case(root, build, env):
    root.mkdir()
    run([sys.executable, str(HERE / "two_matmul_fixture.py"), "two.B"],
        root, env, "fixture.log")
    run([str(build / "osprey/targdir/driver/opencc"), f"-run-build={build}",
         "-O0", "-INLINE:none", "-c", "-keep", "-Wb,-DSL:cuda_legacy_emit=OFF",
         "-Wb,-DSL:cuda_tma_prefetch=ON", "-Wb,-DSL:cuda_whirl_output=two.device.B",
         "two.B"], root, env, "compiler.log")
    run([str(build / "osprey/targdir/ir_tools/ir_b2a"), "-st", "-src",
         "two.device.B", "two.dump.T"], root, env, "device-dump.log")
    run([str(build / "osprey/targdir/whirl2c/whirl2c"), "-cuda",
         "-CLIST:dotc_file=two.whirl.cu", "-fB,two.device.B", "two.cu"],
        root, env, "whirl2c.log")
    source = (root / "two.whirl.cu").read_text()
    if (source.count('extern "C" __global__ void ai_matmul_tile_f32') != 2
            or "__open64_cuda_tma_issue_" in source):
        raise RuntimeError("multiple TMA PUs lost their shared operation ABI")
    run([env.get("CXX", "c++"), "-x", "c++", "-std=c++11", "-O0", "-pthread",
         "-I", str(HERE / "cuda_stub"), "-c", "two.whirl.cu", "-o", "two.host-sim.o"],
        root, env, "host-sim-compile.log")
    render_program(root / "two.t", root / "reports")
    return dict(device_pus=2, shared_operation_abi_preserved=True, source_compiled_on_host=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    root = args.output_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    build = args.build_dir.resolve()
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join((str(build / "osprey/targdir/torch2whirl/python"),
                                       str(REPO / "osprey/torch2whirl/python")))
    cases = [compile_case(root / "sync", build, env, 135, False)]
    for k in (16, 32, 48, 80, 144):
        cases.append(compile_case(root / f"tma-k{k}", build, env, k, True))
    rejected = [negative_case(root / "reject-edges", build, env, False),
                negative_case(root / "reject-host-abi", build, env, True)]
    multiple = multi_case(root / "multi-tma", build, env)
    (root / "receipt.json").write_text(json.dumps(dict(
        schema="ai-executable-p8-v1", cases=cases, rejected=rejected, multi_tma=multiple,
        native_cuda_verified=False, gpu_correctness_verified=False,
        gpu_performance_verified=False), indent=2) + "\n")
    files = sorted(path for path in root.rglob("*") if path.is_file())
    (root / "SHA256SUMS").write_text("".join(
        f"{hashlib.sha256(path.read_bytes()).hexdigest()}  {path.relative_to(root)}\n"
        for path in files))
    print(f"Executable P8 WHIRL source audit passed: {root}")


if __name__ == "__main__":
    main()
