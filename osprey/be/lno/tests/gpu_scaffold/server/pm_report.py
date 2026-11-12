"""Extract single-pass PM timelines through NVIDIA's optional ncu_report module.

Importing this module needs only Python stdlib. ncu_report is loaded exclusively
by the command-line exporter, in a separate compatible Python interpreter.
"""
import argparse
import json
import math
from pathlib import Path


class ReportError(ValueError):
    pass


def metric(action, name):
    names=set(action.metric_names())
    for key in (name,"pmsampling:"+name):
        if key in names:
            return action.metric_by_name(key)
    raise ReportError("required metric missing: "+name)


def integer(action,name,optional=False):
    try:
        value=metric(action,name).value()
    except ReportError:
        if optional:
            return None
        raise
    if isinstance(value,bool) or not isinstance(value,(int,float)) or not math.isfinite(value) or value!=int(value):
        raise ReportError("invalid integer metric: "+name)
    return int(value)


def export_action(action,memory,compute):
    groups=metric(action,"profiler__pmsampler_pass_groups")
    if groups.value()!=1 or groups.num_instances()!=1:
        raise ReportError("PM sampling is not verified as one pass group")
    members={x.strip().removeprefix("pmsampling:") for x in str(groups.value(0)).split(",")}
    if not {memory,compute}.issubset(members):
        raise ReportError("memory and compute metrics were not verified in the same pass")
    passes=integer(action,"profiler__replayer_passes")
    if passes!=1:
        raise ReportError("report requires more than one collection pass")
    start=integer(action,"profiler__timestamp_workload_start")
    end=integer(action,"profiler__timestamp_workload_end")
    if start<0 or end<=start:
        raise ReportError("invalid workload bounds")
    interval_ns=integer(action,"profiler__pmsampler_interval_time")
    interval_cycles=integer(action,"profiler__pmsampler_interval_cycles")
    if interval_ns<0 or interval_cycles<0 or not (interval_ns or interval_cycles):
        raise ReportError("missing actual sampling interval")
    channels={}
    warnings=[]
    for label,name in [("memory",memory),("compute",compute)]:
        m=metric(action,name)
        if not m.has_correlation_ids():
            raise ReportError("no sample timestamps: "+name)
        timestamps=m.correlation_ids()
        if timestamps.num_instances()!=m.num_instances():
            raise ReportError("sample/timestamp count mismatch")
        samples=[]
        for i in range(m.num_instances()):
            timestamp=timestamps.as_uint64(i)
            value=float(m.value(i))
            if not math.isfinite(value):
                raise ReportError("non-finite PM sample")
            # Raw timestamps remain in JSON. Display only points within measured
            # execution; do not turn pre/post-kernel samples into kernel activity.
            samples.append(dict(timestamp_ns=timestamp,value=value))
        samples.sort(key=lambda x:x["timestamp_ns"])
        visible=sum(start<=s["timestamp_ns"]<=end for s in samples)
        if visible<3:
            warnings.append(label+": insufficient temporal resolution (<3 in-kernel samples)")
        channels[label]=dict(metric=name,unit=m.unit(),description=m.description(),
                             samples=samples,in_kernel_samples=visible)
    merged=integer(action,"profiler__pmsampler_merged_samples",optional=True)
    if merged:
        warnings.append("PM samples were merged under back pressure")
    if merged is None:
        warnings.append("merged-sample count unavailable")
    return dict(kernel=action.name(),pass_groups=1,collection_passes=passes,
                pass_metrics=sorted(members),workload_start_ns=start,workload_end_ns=end,
                interval_ns=interval_ns,interval_cycles=interval_cycles,
                merged_samples=merged,dropped_samples=None,
                scope="device aggregate; clipped to workload timestamps; no per-SM/CTA attribution",
                context_filter="not applied to raw samples; use an isolated GPU worker",
                warnings=warnings,channels=channels)


def export_report(context,memory,compute):
    actions=[]
    for r in range(context.num_ranges()):
        region=context.range_by_idx(r)
        for i in range(region.num_actions()):
            actions.append(export_action(region.action_by_idx(i),memory,compute))
    if not actions:
        raise ReportError("no profiled kernels in report")
    return dict(schema_version=1,method="ncu_pm_sampling",single_pass_verified=True,
                interpretation="aggregate activity; not exact transfer/compute overlap",
                kernels=actions)


def main():
    parser=argparse.ArgumentParser()
    parser.add_argument("report",type=Path)
    parser.add_argument("output",type=Path)
    parser.add_argument("--memory",required=True)
    parser.add_argument("--compute",required=True)
    args=parser.parse_args()
    # Optional vendor extension, never imported by the benchmark process.
    import ncu_report
    result=export_report(ncu_report.load_report(str(args.report)),args.memory,args.compute)
    args.output.write_text(json.dumps(result,indent=2,allow_nan=False)+"\n")


if __name__=="__main__":
    main()
