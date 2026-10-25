"""Exercise program-boundary publication of two selected device PUs."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile

from ai_opt_report import PHASE_LOGS, render_program


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[4]


def run(command, cwd, env, expect_success=True):
    result = subprocess.run(command, cwd=cwd, env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if (result.returncode == 0) != expect_success:
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
        temporary = tempfile.TemporaryDirectory(prefix="ai-multi-device-")
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
    run([sys.executable, str(HERE / "two_matmul_fixture.py"), "two.B"],
        output, env)
    options = [str(opencc), f"-run-build={build}", "-O0", "-INLINE:none",
               "-c", "-keep", "-Wb,-DSL:cuda_legacy_emit=OFF",
               "-Wb,-DSL:cuda_whirl_output=two.device.B", "two.B"]
    run(options, output, env)
    image = output / "two.device.B"
    trace = (output / "two.t").read_text(encoding="utf-8")
    if not image.is_file():
        raise RuntimeError("two selected kernels did not publish one image")
    expected = (
        (0, "ai_matmul_duplicate_0", "ai_matmul_tile_f32"),
        (1, "ai_matmul_duplicate_1", "ai_matmul_tile_f32_1"),
    )
    bindings = [line.split() for line in trace.splitlines()
                if line.startswith("AI_OPT 7 DEVICE_BIND ")]
    if len(bindings) != 2:
        raise RuntimeError(f"expected two device bindings, got {bindings}")
    for fields, (ordinal, owner, entry) in zip(bindings, expected):
        if (len(fields) != 9 or fields[3:5] != [str(ordinal), owner] or
                fields[7] != entry or fields[8] != "two.device.B"):
            raise RuntimeError(f"invalid device binding: {fields}")
    run([str(whirl2c), "-cuda", "-CLIST:dotc_file=two.device.cu",
         "-fB,two.device.B", "two.cu"], output, env)
    run([str(ir_b2a), "-st", "-src", "two.device.B", "two.dump.T"],
        output, env)
    dump = (output / "two.dump.T").read_text(encoding="utf-8")
    source = (output / "two.device.cu").read_text(encoding="utf-8")
    for _, owner, entry in expected:
        if (re.search(rf"FUNC_ENTRY\s*<[^>]*{entry}>", dump) is None or
                re.search(rf'extern\s+"C"\s+__global__\s+void\s+{entry}\s*\(',
                          source) is None or owner in source):
            raise RuntimeError(f"missing or contaminated device entry: {entry}")
    if dump.count("DO_LOOP {line:") < 2:
        raise RuntimeError("device image did not retain both tiled loop nests")
    render_program(output / "two.t", output / "reports")
    with tempfile.TemporaryDirectory(prefix="ai-invalid-binding-") as invalid:
        invalid_root = Path(invalid)
        invalid_trace = invalid_root / "invalid.t"
        invalid_trace.write_text(trace.replace(
            "DEVICE_BIND 1 ai_matmul_duplicate_1",
            "DEVICE_BIND 0 ai_matmul_duplicate_1"), encoding="utf-8")
        try:
            render_program(invalid_trace, invalid_root / "reports")
        except ValueError:
            pass
        else:
            raise RuntimeError("duplicate device ordinal was accepted")

    applied = output / "applied"
    applied.mkdir()
    shutil.copy2(output / "two.B", applied / "two.B")
    run([str(opencc), f"-run-build={build}", "-O3", "-noinline",
         "-c", "-keep", "-Wb,-DSL:dump_after_lower=ON",
         "-Wb,-DSL:cuda_legacy_emit=OFF",
         "-Wb,-DSL:cuda_whirl_output=two.applied.device.B",
         "-Wb,-DSL:cuda_tile_apply=ON", "two.B"], applied, env)
    applied_trace = (applied / "two.t").read_text(encoding="utf-8")
    if (not (applied / "two.o").is_file() or
            not (applied / "two.applied.device.B").is_file() or
            applied_trace.count("__open64_dsl_matmul_generated_v1") != 1 or
            applied_trace.count("__open64_dsl_matmul_generated_v2") != 1):
        raise RuntimeError("host rewrite did not bind both provider versions")
    applied_bindings = [line.split() for line in applied_trace.splitlines()
                        if line.startswith("AI_OPT 7 DEVICE_BIND ")]
    if len(applied_bindings) != 2 or [row[7] for row in applied_bindings] != [
            entry for _, _, entry in expected]:
        raise RuntimeError("applied host/device entry mapping is incomplete")
    run([str(ir_b2a), "-st", "-src", "two.applied.device.B",
         "two.applied.dump.T"], applied, env)
    if (applied / "two.applied.dump.T").read_text(
            encoding="utf-8").count("FUNC_ENTRY <") != 2:
        raise RuntimeError("applied device image lost one kernel PU")
    symbols = run(["readelf", "-sW", "two.o"], applied, env)
    relocations = run(["readelf", "-rW", "two.o"], applied, env)
    if (re.search(r"\b64 OBJECT\b[^\n]*dsl_generated_matmul_plan", symbols)
            is None or
            re.search(r"\b72 OBJECT\b[^\n]*dsl_generated_matmul_plan", symbols)
            is None or
            "__open64_dsl_matmul_generated_v1" not in relocations or
            "__open64_dsl_matmul_generated_v2" not in relocations):
        raise RuntimeError("host object lost versioned plan/entry ABI")
    selected_plans = [int(line.split()[5]) for line in applied_trace.splitlines()
                      if line.startswith("AI_OPT 7 TILE_SELECT ")]
    if len(selected_plans) != 2 or selected_plans[0] != selected_plans[1]:
        raise RuntimeError("two-PU fixture has inconsistent selected plans")
    run(["objcopy", "--globalize-symbol=ai_matmul_duplicate_0",
         "--globalize-symbol=ai_matmul_duplicate_1", "two.o",
         "two.link.o"], applied, env)
    run(["cc", "-std=c11", "-O2", "-fno-fast-math",
         "-ffp-contract=off", "-DAI_MATMUL_M2_RUNTIME=1",
         f"-DAI_EXPECT_PLAN={selected_plans[0]}",
         "-I", str(REPO / "osprey/include"),
         "-I", str(HERE.parent / "ai_matmul_m0"),
         str(HERE.parent / "ai_matmul_m0/runtime.c"),
         str(HERE / "compiled_pu_multi_smoke.c"), "two.link.o",
         "-no-pie", "-o", "two-host-smoke"], applied, env)
    smoke = run([str(applied / "two-host-smoke")], applied, env)
    if "both host PUs used their versioned generated-plan binding" not in smoke:
        raise RuntimeError("compiled host PU binding smoke did not pass")
    render_program(applied / "two.t", applied / "reports")
    summary = (applied / "reports/optimization-summary.log").read_text(
        encoding="utf-8")
    if (summary.count("ordinal=") < 2 or
            summary.count("P7 status=rewritten") != 2 or
            "entry=ai_matmul_tile_f32_1" not in summary):
        raise RuntimeError("multi-PU optimization summary is incomplete")
    for filename, _ in PHASE_LOGS:
        content = (applied / "reports/phases" / filename).read_text(
            encoding="utf-8")
        if not content.startswith("# What it summarizes:") or \
                content.count("ordinal=") != 2:
            raise RuntimeError(f"multi-PU phase report is incomplete: {filename}")
    receipt = {
        "schema": "ai-multi-device-whirl-v1",
        "source_only_device_pus": 2,
        "applied_device_pus": 2,
        "entries": [entry for _, _, entry in expected],
        "host_provider_abis": [1, 2],
        "compiled_host_pu_smoke_passed": True,
        "multi_pu_phase_report_complete": True,
        "native_cuda_verified": False,
        "gpu_correctness_verified": False,
        "gpu_performance_verified": False,
    }
    (output / "receipt.json").write_text(
        json.dumps(receipt, indent=2) + "\n", encoding="utf-8")
    files = sorted(path for path in output.rglob("*") if path.is_file())
    (output / "SHA256SUMS").write_text(
        "".join(f"{hashlib.sha256(path.read_bytes()).hexdigest()}  "
                f"{path.relative_to(output)}\n" for path in files),
        encoding="utf-8")
    print(f"two-device image and versioned host bindings verified: {output}")
    if temporary is not None:
        temporary.cleanup()


if __name__ == "__main__":
    main()
