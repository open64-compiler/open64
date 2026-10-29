#!/usr/bin/env python3
"""Retain LN2-d native export, mapped preservation and independent host bridge.

Design: doc/AI-COMPILER-LAYERNORM-RUNTIME-GUARDS-v0.1.md.
Use native, host, then audit modes; no hardware or numerical kernel claim.
"""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

from run_normalization_plan_review import ARCHES, VARIANTS, sha256
from normalization_guard_trace_report import audit_exports, audit_runtime, decode, FIELDS
from normalization_plan_trace_report import require


def command(args,cwd,env,records,label):
    """Preserve every argv/exit/stream beside its immutable case evidence."""
    args=[str(x) for x in args]
    p=subprocess.run(args,cwd=cwd,env=env,text=True,capture_output=True,check=False)
    (cwd/(label+".stdout")).write_text(p.stdout);(cwd/(label+".stderr")).write_text(p.stderr)
    records.append(dict(argv=args,cwd=str(cwd),returncode=p.returncode))
    (cwd/"commands.json").write_text(json.dumps(records,indent=2)+"\n")
    require(p.returncode==0,label+": "+p.stderr[-3000:])


def native(build,baseline,root):
    """Run actual native compiler API and mapped reader on every alternative."""
    root.mkdir(parents=True,exist_ok=False)
    binaries=root/"binaries";binaries.mkdir()
    for file in (build/"ir_tools/dsl_normalization_guard_test",baseline/"binaries/ir_b2a"):
        shutil.copy2(file,binaries/file.name)
    env=dict(os.environ,LD_LIBRARY_PATH=str(baseline/"binaries"));records=[]
    for arch in ARCHES:
        for variant in VARIANTS:
            case=root/(arch+"-"+variant);case.mkdir();(case/"ascii").mkdir()
            image=case/"ln_ref.B"
            command([binaries/"dsl_normalization_guard_test",image,case/"guard.t",arch,variant],case,env,records,"producer")
            require(image.read_bytes()==(case/"ln_ref.B.preserved.B").read_bytes(),"export mutated mapped source")
            require(image.read_bytes()==(baseline/case.name/"ln_ref.B").read_bytes(),"source differs from independently planned source")
            for stem in ("ln_ref","ln_ref.B.preserved"):
                command([binaries/"ir_b2a","-st","-src",case/(stem+".B"),case/"ascii"/(stem+".T")],case,env,records,"mapped-"+stem)
    print(json.dumps(dict(cases=32,guards=len(list(root.glob("*/*.guard")))),sort_keys=True))


def host(binary,root,output):
    """Consume native byte guards in an independent runtime-only host process."""
    require(sys.byteorder=="little","host-native review guard requires little endian")
    output.mkdir(parents=True,exist_ok=False);records=[]
    shutil.copy2(binary,output/"normalization_runtime_test")
    for image in sorted(root.glob("*/*.guard")):
        case=output/image.parent.name;case.mkdir(exist_ok=True)
        command([output/"normalization_runtime_test",image,case/(image.name+".runtime.t")],case,os.environ,records,image.name)
    print(json.dumps(dict(guard_invocations=len(records),host_mock_only=True),sort_keys=True))


def audit(baseline,root,hostroot,output):
    """Recompute complete joins and reports from retained raw evidence only."""
    output.mkdir(parents=True,exist_ok=False);records=[];exports_total=refused_total=checks=events=0
    export_log="# What it summarizes: freshly rebound v2 guard exports/refusals joined to independent native source, target and storage/event plans.\n"
    runtime_log="# What it summarizes: observed host-mock admission/leases, gated phase work, failures, drain, publication and release; NOT internal GPU events.\n"
    export_log+="GUARD_COLUMNS "+" ".join(FIELDS)+"\n"
    for arch in ARCHES:
        for variant in VARIANTS:
            name=arch+"-"+variant;case=root/name;old=baseline/name
            require((case/"ln_ref.B").read_bytes()==(case/"ln_ref.B.preserved.B").read_bytes()==(old/"ln_ref.B").read_bytes(),"mapped-image drift")
            require((case/"ascii/ln_ref.T").read_bytes()==(case/"ascii/ln_ref.B.preserved.T").read_bytes()==(old/"ascii/ln_ref.T").read_bytes(),"mapped dump drift")
            images=sorted(case.glob("*.guard"))
            gs,refused=audit_exports((case/"guard.t").read_text(),(old/"plan.t").read_text(),(old/"trace/preopt-on.t").read_text(),images)
            exports_total+=len(gs);refused_total+=refused
            for g in gs:export_log+="EXPORT "+name+" "+" ".join(str(g[k]) for k in FIELDS)+"\n"
            export_log+=f"CENSUS {name} {len(gs)} {refused}\n"
            for image in images:
                g=decode(image.read_bytes());run=hostroot/name/(image.name+".runtime.t")
                result=audit_runtime(run.read_text(),g);checks+=result["checks"];events+=result["events"]
                records.append(dict(case=name,guard=image.name,guard_sha256=sha256(image),trace_sha256=sha256(run),**result))
                runtime_log+=f"HOST {name} {image.name} {result['checks']} {result['events']} {result['refusals']} exact_export_admissions={result['exact_export_admissions']} host_mutated_binding_admissions={result['host_mutated_binding_admissions']} {g['source_line']}\n"
    expected={str(p.relative_to(root))+".runtime.t" for p in root.glob("*/*.guard")}
    require(expected=={str(p.relative_to(hostroot)) for p in hostroot.glob("*/*.runtime.t")},"host raw census mismatch")
    require(exports_total==len(records) and exports_total>100 and refused_total>=8,"incomplete alternative coverage")
    summary=dict(schema="LN2d-runtime-bridge-v1",cases=32,guard_exports=exports_total,guard_refusals=refused_total,
                 host_checks=checks,host_events=events,mapped_dumps=64,v1_preserved=True,reduction_dependency=False,
                 production_provider=False,LayerNorm_lowering=False,profitability=False,GPU=False,records=records)
    (output/"audit.json").write_text(json.dumps(summary,indent=2,sort_keys=True)+"\n")
    (output/"guard-binding.log").write_text(export_log);(output/"runtime-guards.log").write_text(runtime_log)
    (output/"optimization.log").write_text("# What it summarizes: complete successful LN2-d guard-binding and host runtime enforcement report collection.\n"+export_log+runtime_log)
    (output/"optimization-summary.log").write_text("# What it summarizes: LN2-d native guard exports, mapped preservation, observed host enforcement and remaining executable/provider gates.\n"+
        f"cases=32 exports={exports_total} refused={refused_total} host_checks={checks} host_events={events} mapped_dumps=64 v1=preserved\n"+
        "guard=exported leases=host_mock_enforced host_order=observed internal_CTA_events=provider_declared_not_observed\n"+
        "COST none DECISION none TRANSFORM none production_provider=missing executable_WHIRL=missing GPU=not_run numerical_kernel=not_executed\n")
    return {k:v for k,v in summary.items() if k!="records"}


def main():
    """Keep generation, host observation and raw-evidence audit explicit."""
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument("mode",choices=("native","host","audit"));p.add_argument("--root",required=True,type=Path)
    for key in ("build","baseline","binary","output","hostroot"):p.add_argument("--"+key,type=Path)
    a=p.parse_args();root=a.root.resolve()
    if a.mode=="native":native(a.build.resolve(),a.baseline.resolve(),root)
    elif a.mode=="host":host(a.binary.resolve(),root,a.output.resolve())
    else:print(json.dumps(audit(a.baseline.resolve(),root,a.hostroot.resolve(),a.output.resolve()),sort_keys=True))


if __name__=="__main__":main()
