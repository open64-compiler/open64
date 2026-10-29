#!/usr/bin/env python3
"""Retain LN2 native planning, actual PREOPT controls and mapped-image evidence.

Design: doc/AI-COMPILER-LAYERNORM-STORAGE-EVENTS-v0.1.md.
This is not selected lowering, runtime enforcement or GPU verification.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

from normalization_plan_trace_report import write_reports, require
from normalization_ref_trace_report import audit as source_audit
from normalization_binding_report import validate_variant
from normalization_trace_report import audit as legacy_audit

ARCHES = ("sm_90", "sm_90a", "sm_100", "sm_100a")
VARIANTS = ("default", "unit", "wide", "epsilon2", "subnormal", "mixed", "shared-affine", "count-limit")
PRODUCTS = ("ir_tools/dsl_normalization_plan_test", "ir_tools/ir_b2a", "be/be", "be/be.so", "wopt/wopt.so")


def sha256(path):
    """Authenticate source/tools and retained evidence by bytes."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def check_source(text, variant):
    """Check actual semantic coverage, never infer it from a fixture name."""
    facts = source_audit(text)
    legacy = legacy_audit(text) if "AI_NORM 1 FORM " in text else []
    if variant == "count-limit":
        require(len(facts) == 1 and not legacy and facts[0]["rows"] == "1" and
                facts[0]["width"] == str(2**24) and facts[0]["epsilon_bits"] == "3727c5ac", "missing count boundary")
    else:
        validate_variant(facts, legacy, variant)
    return facts, legacy


def review(build, source, output):
    """Run native producer/readers/backend in fresh retained directories."""
    output.mkdir(parents=True, exist_ok=False)
    binaries = output / "binaries"; binaries.mkdir()
    for name in PRODUCTS:
        shutil.copy2(build / name, binaries / Path(name).name)
    env = dict(os.environ, LD_LIBRARY_PATH=str(binaries))
    commands = []

    def run(case, label, args, success=True):
        """Keep exact argv, return code and both streams including expected refusal."""
        args = [str(x) for x in args]
        process = subprocess.run(args, cwd=case, env=env, text=True, capture_output=True, check=False)
        for stream in ("stdout", "stderr"):
            (case / "logs" / (label+"."+stream)).write_text(getattr(process,stream))
        commands.append(dict(case=case.name, label=label, argv=args, returncode=process.returncode))
        (output / "commands.json").write_text(json.dumps(commands, indent=2)+"\n")
        require((process.returncode == 0) == success, label+": "+process.stderr[-2000:])
        return process

    for arch in ARCHES:
        for variant in VARIANTS:
            case = output / (arch+"-"+variant)
            for name in ("ascii", "trace", "logs"):
                (case / name).mkdir(parents=True)
            image = case / "ln_ref.B"
            run(case,"producer",[binaries / "dsl_normalization_plan_test",image,case / "plan.t",arch,variant])
            require(image.read_bytes() == (case / "ln_ref.B.preserved.B").read_bytes(), "plan query mutated source")
            for stem in ("ln_ref", "ln_ref.B.preserved"):
                run(case,"mapped-"+stem,[binaries / "ir_b2a","-st","-src",case / (stem+".B"),case / "ascii" / (stem+".T")])
            for label, enabled, tracing in (("preopt-on","on","on"),("default-trace","on",None),
                                             ("trace-off","on","off"),("preopt-off","off","on")):
                args = [binaries / "be","-fB,"+str(image),"-fo,"+str(case / (label+".obj")),
                        "-ft,"+str(case / "trace" / (label+".t")),"-PHASE:w=on:c=off:wpath="+str(binaries),
                        "-O2","-DSL:wopt="+enabled,"-WOPT:cr_simp=on","-tr25"]
                if tracing is not None: args.append("-DSL:ai_opt_trace="+tracing)
                process = run(case,label,args+[source / "osprey/be/opt/tests/dsl_normalization_ref_test.cxx"],False)
                require("OPR_DSLMEANVARNORM.v2 has no executable VHO lowering route" in process.stderr and
                        not (case / (label+".obj")).exists(), "unexpected executable publication")
                partial = case / (label+".O")
                if partial.exists(): partial.rename(case / (label+".incomplete-mapped-output"))
    return reaudit(output, output / "reports")


def reaudit(root, output):
    """Require complete native/source/control/resource/event coverage before logs."""
    output.mkdir(parents=True, exist_ok=False)
    records = []
    for arch in ARCHES:
        for variant in VARIANTS:
            name = arch+"-"+variant; case = root / name
            require((case / "ln_ref.B").read_bytes() == (case / "ln_ref.B.preserved.B").read_bytes(), "source image drift")
            require((case / "ascii/ln_ref.T").read_bytes() == (case / "ascii/ln_ref.B.preserved.T").read_bytes(), "source dump drift")
            source = (case / "trace/preopt-on.t").read_text()
            facts, legacy = check_source(source,variant)
            require(source_audit(source) == source_audit((case / "trace/default-trace.t").read_text()), "default trace drift")
            for label in ("trace-off","preopt-off"):
                require("AI_NORM" not in (case / ("trace/"+label+".t")).read_text(), "trace control ignored")
            rows = write_reports(case / "plan.t",case / "trace/preopt-on.t",output / name)
            expected = (len(facts)+len(legacy))*3 + (48 if variant == "default" else 0)
            require(len(rows) == expected, "query census mismatch")
            firsts = rows[:(len(facts)+len(legacy))*3] if variant != "default" else rows[:3]
            for fact in facts+legacy:
                matching = [r for r in firsts if all(r[k] == fact[k] for k in ("pu","node","result"))]
                require([r["kind"] for r in matching] == ["1","2","3"], "missing alternatives")
                expected_status = ["1"]*3 if "reference" not in fact else (["0","7","7"] if variant == "count-limit" else ["0"]*3)
                require([r["status"] for r in matching] == expected_status, "baseline legality mismatch")
            if variant == "default":
                require(set(r["status"] for r in rows) == set(str(i) for i in range(10)), "missing refusal gate")
            records.append(dict(name=name,queries=len(rows),bound=sum(r["status"] == "0" for r in rows),
                                refused=sum(r["status"] != "0" for r in rows),source_contracts=len(facts),v1_contracts=len(legacy)))
    summary = dict(schema="LN2-storage-review-v1",cases=records,queries=sum(r["queries"] for r in records),
                   bound=sum(r["bound"] for r in records),refused=sum(r["refused"] for r in records),
                   binaries={Path(p).name:sha256(root / "binaries" / Path(p).name) for p in PRODUCTS},
                   mapped_checkpoint_dumps=64,actual_PREOPT=True,source_v1_preserved=True,reduction_dependency=False,
                   runtime_enforcement=False,LayerNorm_lowering=False,GPU=False)
    (output / "audit.json").write_text(json.dumps(summary, indent=2, sort_keys=True)+"\n")
    (output / "optimization-summary.log").write_text("# What it summarizes: LN2 native plans/refusals, source preservation and open runtime/cost/lowering gates.\n"+
        f"cases={len(records)} queries={summary['queries']} conditional_bound={summary['bound']} refused={summary['refused']} mapped_dumps=64\n"+
        "C0=direct C1=retained_d C2=X_to_d same_numerical_contract=true v1=preserved_refused\n"+
        "runtime_enforcement=unproved grid_limit=unbound registers=unbound occupancy=unmodeled async=not_supported COST none DECISION none TRANSFORM none GPU=not_run\n")
    (output / "optimization.log").write_text("# What it summarizes: complete successful LN2 alternatives, residency and event report collection.\n"+
        "".join((output / r["name"] / "optimization.log").read_text() for r in records))
    return summary


def main():
    """Run native production or independently reaudit retained raw evidence."""
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build",type=Path);parser.add_argument("--source",type=Path)
    parser.add_argument("--root",required=True,type=Path);parser.add_argument("--audit-output",type=Path)
    args=parser.parse_args()
    if args.audit_output:
        result=reaudit(args.root.resolve(),args.audit_output.resolve())
    else:
        require(args.build is not None and args.source is not None,"build/source required")
        result=review(args.build.resolve(),args.source.resolve(),args.root.resolve())
    print(json.dumps({k:v for k,v in result.items() if k not in ("cases","binaries")},sort_keys=True))


if __name__ == "__main__":
    main()
