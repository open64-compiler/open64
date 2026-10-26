"""Validate bounded-sum WHIRL and compact traces without claiming GPU FP8 support."""

import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import unittest

HERE = Path(__file__).resolve().parent


def summarize(trace):
    contracts, plans, segments, actions, rejections = {}, {}, {}, {}, []
    for line in trace.splitlines():
        if not line.startswith("AI_ACC "):
            continue
        row = line.split()
        if len(row) < 3 or row[1] != "1":
            raise ValueError("unsupported accumulation schema")
        kind, values = row[2], row[3:]
        if kind == "CONTRACT":
            if len(values) != 11:
                raise ValueError("invalid numerical contract")
            data = list(map(int, values))
            ident, source, inputs, product, partial, outer, output, permission, span, evidence, evidence_id = data
            if (ident <= 0 or ident in contracts or source not in range(1, 5) or
                    inputs not in range(1, 5) or product != 1 or partial != 1 or outer != 1 or
                    output not in range(1, 5) or not permission & 1 or permission & ~3 or
                    (((inputs != source and inputs != 1) or
                      (output != source and output != 1)) and not permission & 2) or
                    span <= 0 or evidence not in (1, 2, 3) or evidence_id <= 0):
                raise ValueError("invalid numerical permission/evidence")
            contracts[ident] = data
        elif kind == "PLAN":
            if len(values) != 16 or values[-1] != "planned":
                raise ValueError("invalid accumulation plan")
            data = list(map(int, values[:-1]))
            if data[0] in plans:
                raise ValueError("duplicate accumulation plan")
            plans[data[0]] = data
        elif kind == "SEGMENT":
            if len(values) != 5:
                raise ValueError("invalid segment")
            plan, ordinal, begin, valid, issued = map(int, values)
            segments.setdefault(plan, []).append((ordinal, begin, valid, issued))
        elif kind == "ACTION":
            if len(values) != 8 or values[4:] != ["synchronous", "verified", "scalar_f32", "no"]:
                raise ValueError("unsupported executable accumulation proof")
            ident, count, resets, merges = map(int, values[:4])
            if ident in actions or count != resets or count != merges:
                raise ValueError("missing or duplicate resets/merges")
            actions[ident] = count
        elif kind == "REJECT":
            rejections.append(" ".join(values))
        else:
            raise ValueError("unknown accumulation record")
    for ident, data in plans.items():
        _, contract_id, owner, target, instruction, inst, total, staging, requested, effective, scale, count, bits, evidence, evidence_id = data
        if (contract_id not in contracts or min(owner, target, instruction, inst, total, staging, requested, effective, count) <= 0 or
                requested > contracts[contract_id][8] or effective > requested or
                evidence not in (0, 1, 2, 3) or (evidence == 0) != (evidence_id == 0) or
                bits < 0 or (bits and evidence == 0)):
            raise ValueError("invalid accumulation plan contract")
        rows = segments.get(ident, [])
        end, maximum = 0, 0
        for expected, (ordinal, begin, valid, issued) in enumerate(rows):
            if (ordinal != expected or begin != end or valid <= 0 or issued < valid or
                    issued > requested or begin % inst or issued % inst or
                    (expected + 1 < count and issued != valid) or
                    (scale and begin // scale != (begin + valid - 1) // scale)):
                raise ValueError("invalid segment coverage/span")
            end += valid
            maximum = max(maximum, issued)
        if len(rows) != count or end != total or maximum != effective:
            raise ValueError("incomplete segment coverage")
        if ident in actions and actions[ident] != count:
            raise ValueError("action differs from selected plan")
        if ident in actions and (target != 1 or instruction != 1 or inst != 1 or
                                  contracts[contract_id][1:7] != [1] * 6 or scale):
            raise ValueError("native target masquerades as scalar lowering")
        if ident not in actions and not any(row.startswith(f"{ident} ") for row in rejections):
            raise ValueError("unexplained unlowered plan")
    if not plans or not actions or set(actions) - set(plans) or set(segments) - set(plans):
        raise ValueError("missing or orphan accumulation evidence")
    return plans, actions, rejections


def render_report(trace_path, output_dir, extra_lines=()):
    trace = trace_path.read_text()
    plans, actions, rejections = summarize(trace)
    lines = ["scope=standalone_contract_fixture not_full_matmul_pipeline",
             f"trace_sha256={hashlib.sha256(trace_path.read_bytes()).hexdigest()}"]
    for ident, data in sorted(plans.items()):
        status = "materialized_scalar_f32" if ident in actions else "planned_not_lowered"
        target = {1: "cpu_baseline", 2: "hopper", 3: "blackwell"}.get(data[3], str(data[3]))
        instruction = {1: "scalar_f32", 2: "hopper_wgmma_fp8", 3: "blackwell_scaled_fp4"}.get(data[4], str(data[4]))
        lines.append(f"plan={ident} target={target} instruction={instruction} "
                     f"K={data[6]} BK={data[7]} requested_span={data[8]} "
                     f"effective_span={data[9]} segments={data[11]} "
                     f"internal_bits={data[12] or 'unknown'} status={status} native_gpu=no")
        if ident in actions:
            lines.append(f"action={ident} resets={actions[ident]} merges={actions[ident]} "
                         "completion=synchronous proof=producer_verified")
    lines += [f"rejection={row}" for row in rejections]
    lines += list(extra_lines)
    summary = ("# What it summarizes: Bounded partial-sum plans, actual WHIRL resets/merges, "
               "and unsupported native target gates.\n" + "\n".join(lines) + "\n")
    output_dir.mkdir(parents=True, exist_ok=False)
    (output_dir / "optimization-summary.log").write_text(summary)
    phase = ("# What it summarizes: Numerical permission, planned reduction segments, "
             "materialization actions and rejected target lowering.\n" + trace)
    (output_dir / "accumulation.log").write_text(phase)
    (output_dir / "optimization.log").write_text(phase)


class AccumulationReportTest(unittest.TestCase):
    def fixture(self):
        return ("AI_ACC 1 CONTRACT 1 1 1 1 1 1 1 1 2 1 1\n"
                "AI_ACC 1 PLAN 1 1 50 1 1 1 3 64 2 2 0 2 24 1 1 planned\n"
                "AI_ACC 1 SEGMENT 1 0 0 2 2\nAI_ACC 1 SEGMENT 1 1 2 1 1\n"
                "AI_ACC 1 ACTION 1 2 2 2 synchronous verified scalar_f32 no\n")

    def test_complete(self):
        self.assertEqual(summarize(self.fixture())[1], {1: 2})

    def test_gap(self):
        with self.assertRaises(ValueError):
            summarize(self.fixture().replace("SEGMENT 1 1 2", "SEGMENT 1 1 3"))

    def test_missing_merge(self):
        with self.assertRaises(ValueError):
            summarize(self.fixture().replace("ACTION 1 2 2 2", "ACTION 1 2 2 1"))

    def test_missing_action(self):
        with self.assertRaises(ValueError):
            summarize("\n".join(row for row in self.fixture().splitlines() if "ACTION" not in row))

    def test_missing_permission(self):
        with self.assertRaises(ValueError):
            summarize(self.fixture().replace("1 1 2 1 1\n", "1 0 2 1 1\n"))

    def test_missing_contract(self):
        with self.assertRaises(ValueError):
            summarize("\n".join(row for row in self.fixture().splitlines() if "CONTRACT" not in row))

    def test_hopper_cannot_claim_scalar_action(self):
        with self.assertRaises(ValueError):
            summarize(self.fixture().replace("PLAN 1 1 50 1 1 1", "PLAN 1 1 50 2 2 1"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path)
    parser.add_argument("--trace", type=Path, help="Audit a retained AI_ACC trace and regenerate reports only")
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    if args.trace:
        render_report(args.trace.resolve(), args.output_dir.resolve())
        return
    if args.build_dir is None:
        parser.error("--build-dir is required unless --trace is specified")
    root, build = args.output_dir.resolve(), args.build_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    commands = []

    def run(command, cwd, log):
        commands.append(dict(argv=list(map(str, command)), cwd=str(cwd)))
        result = subprocess.run(list(map(str, command)), cwd=cwd, text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=240)
        (cwd / log).write_text(result.stdout)
        if result.returncode:
            raise RuntimeError(f"command failed ({result.returncode}): {command}; {result.stdout}")
        return result.stdout

    try:
        run(["env", "OPEN64_ACC_BEFORE_ONLY=1",
             build / "osprey/targdir/ir_tools/dsl_accumulation_contract_test",
             "accumulation.before.B", "accumulation.after.B", "accumulation.t"], root, "before-producer.log")
        run([build / "osprey/targdir/ir_tools/dsl_accumulation_contract_test",
             "accumulation.before.B", "accumulation.after.B", "accumulation.t"], root, "producer.log")
        errors = {}
        for label in ("before", "after"):
            run([build / "osprey/targdir/ir_tools/ir_b2a", "-st", "-src",
                 f"accumulation.{label}.B", f"accumulation.{label}.T"], root, f"{label}-dump.log")
            case = root / label
            case.mkdir()
            run([build / "osprey/targdir/whirl2c/whirl2c", "-cuda",
                 "-CLIST:dotc_file=accumulation.whirl.cu",
                 f"-fB,{root / f'accumulation.{label}.B'}", "accumulation.cu"], case, "whirl2c.log")
            shutil.copy2(HERE / "accumulation_host_sim.cpp", case)
            run([os.environ.get("CXX", "c++"), "-std=c++11", "-O2", "-ffp-contract=off",
                 "-fno-fast-math", "-I", HERE / "cuda_stub", "accumulation_host_sim.cpp",
                 "-o", "host-sim"], case, "compile.log")
            output = run([case / "host-sim"], case, "numerical.log")
            errors[label] = float(output.splitlines()[-1].split("abs_error=")[1])
        if not errors["after"] < errors["before"]:
            raise RuntimeError("absorption fixture did not exercise partial merging")
        (root / "accumulation.before-after.diff").write_text("".join(difflib.unified_diff(
            (root / "accumulation.before.T").read_text().splitlines(True),
            (root / "accumulation.after.T").read_text().splitlines(True),
            fromfile="accumulation.before.T", tofile="accumulation.after.T")))
        reports = root / "reports"
        render_report(root / "accumulation.t", reports, [
            f"absorption_fixture before_abs_error={errors['before']} after_abs_error={errors['after']} "
            "not_a_universal_accuracy_bound"])
        (root / "receipt.json").write_text(json.dumps(dict(schema="ai-accumulation-contract-v1",
            scope="same-PU before/after scalar dot", malformed_records=14,
            altered_whirl_rejected=4, planner_shapes=45, errors=errors,
            native_fp8_lowering=False, native_fp4_lowering=False, gpu_verified=False), indent=2) + "\n")
    finally:
        (root / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        f"{hashlib.sha256(path.read_bytes()).hexdigest()}  {path.relative_to(root)}\n"
        for path in sorted(root.rglob("*")) if path.is_file()))
    print(f"accumulation compiler contract passed: {root}")


if __name__ == "__main__":
    main()
