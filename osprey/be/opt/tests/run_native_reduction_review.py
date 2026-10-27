"""Retain reproducible native-contract/PREOPT evidence in a fresh directory.

Design: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md.
Run inside the configured Linux build environment; no Python ML dependencies.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
from reduction_trace_report import audit, write_reports


def run_review(build, output):
    """Invoke real native tools, retain raw outputs and audit intentional gates."""
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    for name in ("ascii", "trace", "logs"):
        (output / name).mkdir()
    commands = []
    env = dict(os.environ)
    env["LD_LIBRARY_PATH"] = str(build / "be") + ":" + str(build / "wopt")

    def run(label, command, expected_success=True):
        """Record command and both streams, treating expected rejection explicitly."""
        command = [str(part) for part in command]
        result = subprocess.run(command, env=env, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, text=True)
        (output / "logs" / (label + ".stdout")).write_text(result.stdout)
        (output / "logs" / (label + ".stderr")).write_text(result.stderr)
        commands.append(dict(label=label, argv=command, returncode=result.returncode))
        (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if (result.returncode == 0) != expected_success:
            raise RuntimeError(label + " unexpected result: " + result.stderr[-2000:])
        return result

    image = output / "reduction.B"
    run("cross-pu", [build / "ir_tools/dsl_reduce_sum_contract_test", image, "cross-pu"])
    run("native-contract", [build / "ir_tools/dsl_reduce_sum_contract_test", image])
    run("mapped-reopen", [build / "ir_tools/ir_b2a", "-st", "-src", image,
                          output / "ascii/reduction.T"])
    source = Path(__file__).resolve().with_name("dsl_reduce_sum_contract_test.cxx")
    for label, enabled, trace_enabled in (("preopt-on", "on", "on"),
                                          ("default-trace", "on", None),
                                          ("trace-off", "on", "off"),
                                          ("preopt-off", "off", "on")):
        trace = output / "trace" / (label + ".t")
        command = [build / "be/be", "-fB," + str(image),
            "-fo," + str(output / (label + ".obj")), "-ft," + str(trace),
            "-PHASE:w=on:c=off:wpath=" + str(build / "wopt"), "-O2",
            "-DSL:wopt=" + enabled, "-WOPT:cr_simp=on", "-tr25", source]
        if trace_enabled is not None:
            command.insert(-1, "-DSL:ai_opt_trace=" + trace_enabled)
        result = run(label, command, expected_success=False)
        if "OPR_DSLREDUCESUM.v1 has no executable VHO lowering route" not in result.stderr:
            raise RuntimeError(label + " failed before the expected lowering gate")
        if label in ("trace-off", "preopt-off") and "AI_REDUCE " in trace.read_text():
            raise RuntimeError(label + " unexpectedly emitted reduction events")
        if (output / (label + ".obj")).exists():
            raise RuntimeError("unsupported lowering published an object")
        # The backend opens its mapped output before VHO; failed output is not
        # a finalized image and must never be presented as PREOPT evidence.
        partial = output / (label + ".O")
        if partial.exists():
            partial.rename(output / (label + ".incomplete-mapped-output"))
    rows = write_reports(output / "trace/preopt-on.t", output / "reports")
    if audit((output / "trace/default-trace.t").read_text()) != rows:
        raise RuntimeError("default trace differs from explicit trace-on")
    ascii_text = (output / "ascii/reduction.T").read_text()
    if "OPR_DSLREDUCESUM" not in ascii_text or "attr.reassociate=false" not in ascii_text:
        raise RuntimeError("mapped reader lost the reduction contract")
    products = ("be/be", "be/be.so", "wopt/wopt.so", "ir_tools/ir_b2a",
                "ir_tools/dsl_reduce_sum_contract_test")
    receipt = dict(schema="open64-r1-native-reduction-v1", contracts=len(rows),
        native_preopt_verified=True, executable_reduction=False,
        default_trace_verified=True,
        frontend_ingestion=False, native_gpu_verified=False,
        products={name: hashlib.sha256((build / name).read_bytes()).hexdigest()
                  for name in products})
    (output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    (output / "SHA256SUMS").write_text("".join(
        hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(output)) + "\n"
        for path in sorted(output.rglob("*")) if path.is_file() and path.name != "SHA256SUMS"))
    print("native reduction/PREOPT review passed: " + str(output))


def main():
    """Choose an existing build and a never-before-used artifact directory."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    run_review(args.build_dir.resolve(), args.output_dir)


if __name__ == "__main__":
    main()
