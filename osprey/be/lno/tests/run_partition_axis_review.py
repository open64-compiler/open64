"""Retain ordinary-C -> mapped WHIRL -> PREOPT/LNO partition evidence.

Design: doc/AI-COMPILER-NATIVE-PARTITION-AXIS-v0.1.md.
Use a configured Linux build and installed C frontend. No GPU/performance work.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

from partition_axis_trace_report import audit, write_reports


def run_review(build, cc, output):
    """Publish fresh evidence, fail closed on missing proofs or changed WHIRL."""
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    for name in ("source", "logs", "trace", "ascii"):
        (output / name).mkdir()
    source = Path(__file__).resolve().with_name("dsl_partition_axis.c")
    shutil.copy2(source, output / "source" / source.name)
    env = dict(os.environ)
    env["LD_LIBRARY_PATH"] = ":".join(str(build / d) for d in ("be", "wopt", "lno"))
    commands = []

    def run(label, command):
        """Preserve the exact command and both streams before testing success."""
        command = [str(x) for x in command]
        result = subprocess.run(command, cwd=output, env=env, capture_output=True, text=True)
        for stream in ("stdout", "stderr"):
            (output / "logs" / (label + "." + stream)).write_text(getattr(result, stream))
        commands.append(dict(label=label, argv=command, returncode=result.returncode))
        (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if result.returncode:
            raise RuntimeError(label + ": " + result.stderr[-3000:])
        return result

    run("frontend", [cc, "-O0", "-keep", "-c", "-OPT:IEEE_arith=3:roundoff=0",
                     "-PHASE:c=off", source])
    image = output / "dsl_partition_axis.B"
    run("source-reopen", [build / "ir_tools/ir_b2a", "-st", "-src", image,
                           output / "ascii/source.T"])
    modes = (("enabled", "on", "on", 3), ("trace-off", "on", "off", 3),
             ("disabled", "off", "on", 3), ("default", None, "on", 3),
             ("O0", "on", "on", 0), ("O1", "on", "on", 1), ("O2", "on", "on", 2))
    for label, enabled, trace, level in modes:
        command = [build / "be/be", "-fB," + str(image),
            "-fo," + str(output / (label + ".o")),
            "-ft," + str(output / "trace" / (label + ".phase.t")),
            "-PHASE:l=on:w=off:c=off:lpath=" + str(build / "lno") +
            ":wpath=" + str(build / "wopt"), "-O" + str(level),
            "-DSL:ai_opt_trace=" + trace, "-tr25", "-tt25:1",
            "-OPT:IEEE_arith=3:roundoff=0", source]
        if enabled is not None:
            command.insert(-1, "-DSL:partition_axis=" + enabled)
        run(label, command)
        if level == 3:
            run(label + "-reopen", [build / "ir_tools/ir_b2a", "-st", "-src",
                output / (label + ".N"), output / "ascii" / (label + ".T")])
        raw = (output / "trace" / (label + ".phase.t")).read_text()
        if label != "enabled" and "AI_PARTITION " in raw:
            raise RuntimeError(label + " emitted unexpected partition trace")
        if label == "enabled" and "Pre_Optimizer phase=" not in raw:
            raise RuntimeError("missing actual PREOPT invocation evidence")
    rows = write_reports(output / "trace/enabled.phase.t", output / "reports")
    proved = [r for r in rows if r["status"] == "proved"]
    if (len(rows) != 13 or len(proved) != 5 or
            sorted((r["instance_axis"], r["contribution_axis"], r["instances"],
                    r["contributions"], r["recipe"]) for r in proved) != sorted([
                ("0", "1", "3", "17", "ordered_sum"),
                ("1", "0", "3", "17", "ordered_sum"),
                ("0", "1", "3", "17", "ordered_squared_sum"),
                ("0", "1", "3", "3", "ordered_sum"),
                ("0", "1", "3", "17", "ordered_sum")])):
        raise RuntimeError("native partition coverage differs: " + repr(rows))
    if not {"domain_unproved", "access_unproved", "alias_unproved", "recurrence_unproved"}.issubset(
            {r["status"] for r in rows}):
        raise RuntimeError("native refusal coverage missing")
    for label in ("trace-off", "disabled", "default"):
        before = (output / "ascii/disabled.T").read_text().splitlines(keepends=True)
        after = (output / "ascii" / (label + ".T")).read_text().splitlines(keepends=True)
        diff = "".join(difflib.unified_diff(before, after, fromfile="disabled.T", tofile=label + ".T"))
        (output / (label + ".whirl.diff")).write_text(diff)
        if diff or (output / "disabled.N").read_bytes() != (output / (label + ".N")).read_bytes():
            raise RuntimeError(label + " changed mapped WHIRL")
    if (output / "disabled.N").read_bytes() != (output / "enabled.N").read_bytes():
        raise RuntimeError("discovery changed mapped WHIRL")
    diff = "".join(difflib.unified_diff(
        (output / "ascii/disabled.T").read_text().splitlines(keepends=True),
        (output / "ascii/enabled.T").read_text().splitlines(keepends=True),
        fromfile="disabled.T", tofile="enabled.T"))
    (output / "analysis-before-after.diff").write_text(diff)
    if diff:
        raise RuntimeError("discovery changed logical mapped WHIRL")
    source_text = (output / "ascii/source.T").read_text()
    if ("DSLREDUCE" in source_text or "DSLMEANVARNORM" in source_text or
            "F4ADD" not in source_text or "ARRAY" not in source_text or
            source_text.count("FUNC_ENTRY") != 13):
        raise RuntimeError("fixture is not ordinary scalar loop WHIRL")
    if any(int(r["source_line"]) <= 0 for r in rows):
        raise RuntimeError("missing source lines")
    receipt = dict(schema="open64-native-partition-axis-v1", cases=len(rows),
        proved=len(proved), refused=len(rows) - len(proved),
        native_scalar_discovery=True, actual_preopt_verified=True,
        mapped_reopen_verified=True, trace_control_verified=True,
        option_default_off=True, minimum_optimization_level=3,
        analysis_has_no_whirl_changes=True, normalization_recognition=False,
        physical_partition_selection=False, native_gpu_verified=False,
        products={name: hashlib.sha256((build / name).read_bytes()).hexdigest()
            for name in ("be/be", "be/be.so", "wopt/wopt.so", "lno/lno.so", "ir_tools/ir_b2a")})
    (output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    (output / "SHA256SUMS").write_text("".join(
        hashlib.sha256(p.read_bytes()).hexdigest() + "  " + str(p.relative_to(output)) + "\n"
        for p in sorted(output.rglob("*")) if p.is_file() and p.name != "SHA256SUMS"))
    print(json.dumps(receipt, indent=2))


def main():
    """Require explicit existing tools and a fresh host-mounted evidence directory."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--cc", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    run_review(args.build_dir.resolve(), args.cc.resolve(), args.output_dir)


if __name__ == "__main__":
    main()
