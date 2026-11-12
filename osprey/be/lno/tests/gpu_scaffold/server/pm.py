"""Optional PM diagnostics. Never returns or alters benchmark measurements."""
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

from data import ROOT, digest
from pm_view import render

MEMORY_METRICS=("dram__throughput.avg.pct_of_peak_sustained_elapsed",
                "lts__throughput.avg.pct_of_peak_sustained_elapsed")
COMPUTE_METRICS=("sm__pipe_fma_cycles_active.avg.pct_of_peak_sustained_elapsed",
                 "sm__pipe_fp32_cycles_active.avg.pct_of_peak_sustained_elapsed",
                 "sm__throughput.avg.pct_of_peak_sustained_elapsed")
PROFILER_METRICS=("profiler__pmsampler_pass_groups","profiler__replayer_passes",
                  "profiler__timestamp_workload_start","profiler__timestamp_workload_end",
                  "profiler__pmsampler_interval_time","profiler__pmsampler_interval_cycles",
                  "profiler__pmsampler_merged_samples")


def call(command,folder,name,timeout,env=None):
    proc=subprocess.run([str(x) for x in command],capture_output=True,text=True,timeout=timeout,env=env)
    (folder/(name+".stdout.log")).write_text(proc.stdout)
    (folder/(name+".stderr.log")).write_text(proc.stderr)
    if proc.returncode:
        raise RuntimeError(f"{name} failed (exit {proc.returncode}); see {name}.stderr.log")
    return proc.stdout


def select_metrics(query,memory=None,compute=None):
    available=set(re.findall(r"[A-Za-z][A-Za-z0-9_]*__[A-Za-z0-9_.]+",query))
    selected=[]
    for role,requested,defaults in [("memory",memory,MEMORY_METRICS),("compute",compute,COMPUTE_METRICS)]:
        choices=[requested] if requested else defaults
        name=next((x for x in choices if x in available),None)
        if not name:
            raise ValueError(role+" metric unavailable to PM sampling; select a supported metric explicitly")
        selected.append(name)
    if selected[0]==selected[1]:
        raise ValueError("memory and compute metrics must differ")
    return selected


def ncu_command(ncu,report,memory,compute,interval,exe,role,warmup,case_dir,gpu_uuid,atol,rtol):
    metrics=["pmsampling:"+memory,"pmsampling:"+compute,*PROFILER_METRICS]
    return [ncu,"--profile-from-start","off","--replay-mode","kernel",
            "--cache-control","none","--clock-control","none",
            "--pm-sampling-max-passes","1","--disable-pm-warp-sampling",
            "--pm-sampling-interval",str(interval),"--metrics",",".join(metrics),
            "--export",str(report),"--force-overwrite",
            str(exe),"--profile",role,str(warmup),str(atol),str(rtol),str(case_dir),gpu_uuid]


def python_env(ncu,explicit):
    env=os.environ.copy()
    candidates=[Path(explicit)] if explicit else [
        Path(ncu).resolve().parent/"extras"/"python",
        Path(ncu).resolve().parent.parent/"extras"/"python"]
    paths=[str(p.resolve()) for p in candidates if p.is_dir()]
    if paths:
        env["PYTHONPATH"]=os.pathsep.join([*paths,env.get("PYTHONPATH","")])
    return env


def collect(exe,corpus,output_dir,case_ids,gpu_uuid,options,identity):
    """Return only diagnostics; case_ids and identity are copies of benchmark metadata."""
    root=Path(output_dir)/"diagnostics"
    root.mkdir(parents=True,exist_ok=True)
    summary=dict(status="unsupported",method="ncu_pm_sampling",cases={},reason=None)
    ncu=shutil.which(options.ncu)
    if not ncu:
        summary["reason"]="ncu not found; unprofiled results preserved"
        return summary
    try:
        version=call([ncu,"--version"],root,"ncu-version",options.timeout)
        help_text=call([ncu,"--help"],root,"ncu-help",options.timeout)
        for flag in ["--query-metrics-collection","--pm-sampling-max-passes","--disable-pm-warp-sampling"]:
            if flag.lstrip("-") not in help_text:
                raise ValueError("installed ncu does not advertise required control: "+flag)
        query=call([ncu,"--query-metrics","--query-metrics-collection","pmsampling",
                    "--query-metrics-mode","all","--csv"],root,"metric-query",options.timeout)
        memory,compute=select_metrics(query,options.pm_memory_metric,options.pm_compute_metric)
        env=python_env(ncu,options.ncu_python_path)
        call([options.ncu_python,"-c","import ncu_report"],root,"python-check",options.timeout,env)
    except (OSError,ValueError,RuntimeError,subprocess.TimeoutExpired) as error:
        summary["reason"]=str(error)
        return summary
    summary.update(status="collected",ncu_version=version.strip(),memory_metric=memory,
                   compute_metric=compute,reason=None)
    for case_id in case_ids:
        folder=root/case_id
        folder.mkdir(parents=True,exist_ok=True)
        record=dict(schema_version=1,case_id=case_id,method="ncu_pm_sampling",ncu_version=version.strip(),
                    requested_interval=options.pm_interval,
                    requested_interval_units="architecture-dependent: cycles or ns",
                    max_collection_passes_per_kernel=1,
                    interpretation="aggregate activity; not exact transfer/compute overlap",
                    benchmark_identity=dict(identity),runner_sha256=digest(exe),
                    memory_metric=memory,compute_metric=compute,implementations={})
        display={}
        for role in ["baseline","candidate"]:
            directory=folder/role
            directory.mkdir(exist_ok=True)
            native=directory/"profile.ncu-rep"
            samples=directory/"samples.json"
            command=ncu_command(ncu,native,memory,compute,options.pm_interval,exe,role,
                                options.warmup,Path(corpus)/case_id,gpu_uuid,
                                identity["atol"],identity["rtol"])
            item=dict(status="failed",command=[str(x) for x in command],reason=None)
            try:
                # Never accept stale artifacts after a failed collection/export.
                native.unlink(missing_ok=True)
                samples.unlink(missing_ok=True)
                stdout=call(command,directory,"profile",options.timeout)
                if '"profile_validation":"passed"' not in stdout:
                    raise RuntimeError("profile runner did not confirm output validation")
                if not native.exists():
                    raise RuntimeError("ncu did not create a native report")
                call([options.ncu_python,ROOT/"server"/"pm_report.py",native,samples,
                      "--memory",memory,"--compute",compute],directory,"export",options.timeout,env)
                data=json.loads(samples.read_text())
                if data.get("single_pass_verified") is not True:
                    raise ValueError("export did not verify single-pass collection")
                item.update(status="collected",native_report=str(native.relative_to(folder)),
                            samples=str(samples.relative_to(folder)))
                display[role]=dict(item,data=data)
            except (OSError,ValueError,RuntimeError,subprocess.TimeoutExpired) as error:
                item["reason"]=str(error)
                display[role]=dict(item)
            record["implementations"][role]=item
        statuses=[x["status"] for x in record["implementations"].values()]
        record["status"]="collected" if all(x=="collected" for x in statuses) else (
            "partial" if "collected" in statuses else "failed")
        render(case_id,display,folder/"timeline.html")
        (folder/"index.json").write_text(json.dumps(record,indent=2,allow_nan=False)+"\n")
        summary["cases"][case_id]=dict(status=record["status"],
            artifact=str((folder/"index.json").relative_to(output_dir)),
            timeline=str((folder/"timeline.html").relative_to(output_dir)))
    statuses=[x["status"] for x in summary["cases"].values()]
    if not statuses or any(x!="collected" for x in statuses):
        summary["status"]="partial" if any(x in {"collected","partial"} for x in statuses) else "failed"
    return summary
