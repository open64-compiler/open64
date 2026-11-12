#!/usr/bin/env python3
"""Build the two implementations and write one result.json (Python stdlib only)."""
import argparse
import hashlib
import json
import math
import shutil
import subprocess
import sys
import time
import copy
from pathlib import Path

from data import ATOL, RTOL, REGISTRY, ROOT, digest, generate, verify
sys.path.insert(0,str(ROOT.parent))
sys.path.insert(0,str(ROOT))
from gpu_test_results import from_scaffold


def persist(out,result):
    """Keep native evidence and write the common result before any profiling."""
    out.with_name(out.stem+'.legacy.json').write_text(json.dumps(result,indent=2,allow_nan=False)+'\n')
    out.write_text(json.dumps(from_scaffold(result),indent=2,allow_nan=False)+'\n')


def content_id(paths):
    h=hashlib.sha256()
    for name,path in sorted(paths):
        h.update(name.encode()+b"\0"+Path(path).read_bytes()+b"\0")
    return h.hexdigest()


def read_submission(folder):
    folder=Path(folder).resolve()
    manifest=json.loads((folder/"manifest.json").read_text())
    required={"schema_version","workload_id","suite_id","target_sm","build","baseline"}
    if set(manifest)!=required or manifest["schema_version"]!=1:
        raise ValueError("manifest must contain exactly the version 1 fields")
    kind,suite=REGISTRY[manifest["workload_id"]]
    if manifest["suite_id"]!=suite:
        raise ValueError("workload/suite mismatch")
    if type(manifest["target_sm"]) is not int or not 80<=manifest["target_sm"]<=999:
        raise ValueError("BF16 examples require target_sm >= 80")
    build=manifest["build"]
    if set(build)!={"flags","libraries"}:
        raise ValueError("invalid build fields")
    if not isinstance(build["flags"],list) or any(
        f not in {"-O0","-O1","-O2","-O3","-std=c++17","-lineinfo"} for f in build["flags"]):
        raise ValueError("unsupported build flag")
    if build["libraries"]!=["cudart"]:
        raise ValueError("this minimal server supports cudart only")
    # Minimal implementation: package-local baseline; no remote registry required.
    baseline=manifest["baseline"]
    if set(baseline)!={"source"}:
        raise ValueError("this server requires baseline.source")
    for source in ["candidate.cu",baseline["source"]]:
        path=(folder/source).resolve()
        if not path.is_relative_to(folder) or not path.is_file():
            raise ValueError("missing or non-local CUDA source")
    files=[(str(p.relative_to(folder)),p) for p in folder.rglob("*") if p.is_file()]
    return folder,manifest,kind,content_id(files)


def build(folder,manifest,kind,submission_id,nvcc,build_root):
    header=ROOT/"server"/"include"/kind/"workload.h"
    harness=ROOT/"server"/"harness.cu"
    version=subprocess.run([nvcc,"--version"],capture_output=True,text=True,check=True).stdout
    key=hashlib.sha256((submission_id+digest(header)+digest(harness)+version).encode()).hexdigest()
    cache=build_root/key
    cache.mkdir(parents=True,exist_ok=True)
    exe=cache/"runner"
    if exe.exists() and (cache/"receipt.json").exists():
        receipt=json.loads((cache/"receipt.json").read_text())
        receipt["cache_hit"]=True
        return exe,receipt
    flags=manifest["build"]["flags"]
    base=[nvcc,*flags,"-arch=sm_"+str(manifest["target_sm"]),"-I",str(header.parent)]
    commands=[]
    elapsed={}
    with (cache/"build.log").open("w") as log:
        for name,source in [("baseline",manifest["baseline"]["source"]),("candidate","candidate.cu")]:
            command=[*base,"-Drun="+name+"_run","-Dworkspace_bytes="+name+"_workspace_bytes",
                     "-c",str(folder/source),"-o",str(cache/(name+".o"))]
            commands.append(command)
            started=time.monotonic()
            subprocess.run(command,stdout=log,stderr=log,check=True,timeout=180)
            elapsed[name+"_ms"]=(time.monotonic()-started)*1000
        command=[*base,"-DGEMM" if kind=="gemm" else "-DMLA",str(harness),
                 str(cache/"baseline.o"),str(cache/"candidate.o"),"-o",str(exe)]
        commands.append(command)
        subprocess.run(command,stdout=log,stderr=log,check=True,timeout=180)
    receipt=dict(cache_hit=False,nvcc_version=version.strip(),commands=commands,**elapsed)
    (cache/"receipt.json").write_text(json.dumps(receipt,indent=2)+"\n")
    return exe,receipt


def case_result(metadata,native):
    result=dict(case_id=metadata["case_id"],status=native.get("status","skipped"),
                useful_flops=metadata["useful_flops"],
                correctness=native.get("correctness",{"baseline_pass":None,"candidate_pass":None}),
                baseline=None,candidate=None,wall_time_ratio=None)
    if native.get("status")=="passed":
        for name in ["baseline","candidate"]:
            metrics=dict(native[name])
            for key in ["wall_us","device_us"]:
                if not math.isfinite(metrics[key]) or metrics[key]<=0:
                    raise ValueError("invalid native timing")
            metrics["effective_tflops"]=metadata["useful_flops"]/(metrics["device_us"]*1e6)
            result[name]=metrics
        result["wall_time_ratio"]=result["candidate"]["wall_us"]/result["baseline"]["wall_us"]
    if "reason" in native:
        result["reason"]=native["reason"]
    return result


def evaluate(options):
    folder,manifest,kind,submission_id=read_submission(options.submission)
    corpus=generate(kind)
    index=verify(corpus)
    out=Path(options.output or ROOT/"runs"/kind/"result.json").resolve()
    out.parent.mkdir(parents=True,exist_ok=True)
    metadata=[json.loads((corpus/c["case_id"]/"case.json").read_text()) for c in index["cases"]]
    result=dict(schema_version=1,submission_id=submission_id,
                workload_id=manifest["workload_id"],suite_id=manifest["suite_id"],
                corpus_sha256=digest(corpus/"index.json"),
                baseline_id=content_id([
                    (manifest["baseline"]["source"],folder/manifest["baseline"]["source"]),
                    ("server/workload.h",ROOT/"server"/"include"/kind/"workload.h"),
                    *[(str(p.relative_to(folder)),p) for p in folder.rglob("*")
                      if p.is_file() and p.suffix in {".h",".cuh"}]]),
                status="not_run",environment=None,build=None,session_id=None,
                policy=dict(samples=options.samples,warmup=options.warmup,
                            min_improvement=options.min_improvement,
                            noise_multiplier=3,atol=ATOL,rtol=RTOL,
                            wall_boundary="run plus stream completion; reset/allocation excluded",
                            flops=index["flops"]),
                cases=[],decision="inconclusive",reason=None)
    native=[]
    reason="check-only: corpus verified; CUDA compilation and execution not performed"
    if not options.check_only:
        nvcc=shutil.which(options.nvcc)
        if not nvcc:
            result["status"]="build_failed"
            reason="nvcc not found; run on a CUDA machine or use --check-only"
        else:
            try:
                exe,receipt=build(folder,manifest,kind,submission_id,nvcc,ROOT/".build")
                result["build"]=receipt
                command=[str(exe),str(options.samples),str(options.warmup),str(ATOL),str(RTOL),
                         *[str(corpus/c["case_id"]) for c in index["cases"]]]
                run=subprocess.run(command,capture_output=True,text=True,timeout=options.timeout)
                (out.parent/"runner.stdout.log").write_text(run.stdout)
                (out.parent/"runner.stderr.log").write_text(run.stderr)
                lines=[json.loads(line) for line in run.stdout.splitlines() if line.strip()]
                if lines and "environment" in lines[0]:
                    result["environment"]=lines.pop(0)["environment"]
                    result["environment"]["same_device_and_session"]=True
                    result['session_id']=result['environment'].get('session_id')
                native=lines
                result["status"]="completed" if run.returncode==0 else "execution_failed"
                reason=None if run.returncode==0 else "runner failed; inspect runner logs"
            except subprocess.TimeoutExpired:
                result["status"]="execution_failed"
                reason="build or execution timeout; no partial timing accepted"
            except subprocess.CalledProcessError:
                result["status"]="build_failed"
                reason="CUDA build failed; inspect .build/*/build.log"
            except (ValueError,OSError) as error:
                result["status"]="execution_failed"
                reason=str(error)
    for i,meta in enumerate(metadata):
        item=native[i] if i<len(native) else dict(status="skipped",reason=reason or "runner stopped")
        result["cases"].append(case_result(meta,item))
    if result["status"]=="completed":
        if any(c["status"]!="passed" for c in result["cases"]):
            result["decision"]="rejected"
            reason="correctness failure"
        else:
            # Conservative demonstration policy: all cases must improve beyond
            # both the requested floor and three normalized MADs.
            accepted=True
            for c in result["cases"]:
                b,o=c["baseline"],c["candidate"]
                noise=3*(b["wall_mad_us"]+o["wall_mad_us"])/b["wall_us"]
                threshold=max(options.min_improvement,noise)
                accepted &= c["wall_time_ratio"] < 1-threshold
            result["decision"]="accepted" if accepted else "inconclusive"
            reason=None if accepted else "not every case clears the improvement/noise threshold"
    result["reason"]=reason
    persist(out,result)
    # Persist authoritative unprofiled results BEFORE optional diagnostics. The
    # collector receives no benchmark result object and cannot replace timings.
    if options.profile_pm:
        if options.check_only or result["status"]!="completed":
            result["diagnostics"]=dict(status="skipped",reason="requires a completed GPU benchmark")
        else:
            passed=[c for c in result["cases"] if c["status"]=="passed"]
            selected=options.profile_case or ([max(passed,key=lambda c:c["useful_flops"])["case_id"]] if passed else [])
            try:
                if not selected or any(x not in {c["case_id"] for c in passed} for x in selected):
                    raise ValueError("profile cases must name successfully benchmarked datasets")
                from pm import collect
                identity=dict(submission_id=submission_id,workload_id=manifest["workload_id"],
                              suite_id=manifest["suite_id"],corpus_sha256=result["corpus_sha256"],
                              baseline_id=result["baseline_id"],environment=copy.deepcopy(result["environment"]),
                              atol=ATOL,rtol=RTOL)
                result["diagnostics"]=collect(exe,corpus,out.parent,list(dict.fromkeys(selected)),
                    result["environment"]["gpu_uuid"],options,identity)
            except Exception as error:
                # Profiling/tool incompatibility cannot invalidate completed tests.
                result["diagnostics"]=dict(status="failed",reason=str(error))
            for case in result["cases"]:
                diagnostic=result["diagnostics"].get("cases",{}).get(case["case_id"])
                if diagnostic:
                    case["diagnostics"]=diagnostic
        persist(out,result)
    print(out)
    return 0 if result["status"]=="completed" and all(c["status"]=="passed" for c in result["cases"]) or options.check_only else 1


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("submission",type=Path)
    parser.add_argument("--output",type=Path)
    parser.add_argument("--nvcc",default="nvcc")
    parser.add_argument("--samples",type=int,default=20)
    parser.add_argument("--warmup",type=int,default=3)
    parser.add_argument("--timeout",type=float,default=120)
    parser.add_argument("--min-improvement",type=float,default=0.02)
    parser.add_argument("--check-only",action="store_true")
    parser.add_argument("--profile-pm",action="store_true",help="optional separate single-pass PM diagnostics")
    parser.add_argument("--profile-case",action="append",help="case ID; repeat to select multiple (default: largest FLOP count)")
    parser.add_argument("--ncu",default="ncu")
    parser.add_argument("--ncu-python",default=sys.executable,help="Python compatible with NVIDIA ncu_report")
    parser.add_argument("--ncu-python-path",help="Nsight Compute extras/python directory")
    parser.add_argument("--pm-memory-metric",help="override default DRAM/L2 proxy with a supported metric")
    parser.add_argument("--pm-compute-metric",help="override FMA/SM default, e.g. a Tensor Core metric")
    parser.add_argument("--pm-interval",type=int,default=20000,help="sampling interval; cycles or ns depending on GPU (0=auto)")
    args=parser.parse_args()
    if args.samples<1 or args.warmup<0 or args.timeout<=0 or not 0<=args.min_improvement<1 or args.pm_interval<0:
        parser.error("invalid measurement policy")
    if args.profile_case and not args.profile_pm:
        parser.error("--profile-case requires --profile-pm")
    try:
        return evaluate(args)
    except (ValueError,KeyError,OSError) as error:
        parser.error(str(error))


if __name__=="__main__":
    sys.exit(main())
