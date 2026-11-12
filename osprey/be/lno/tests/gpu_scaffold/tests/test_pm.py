"""Host tests use synthetic NVIDIA-report objects, never claimed as GPU results."""
import copy
import io
import json
from pathlib import Path
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch
from contextlib import redirect_stdout

sys.path.insert(0,str(Path(__file__).resolve().parents[1]/"server"))
import data
import pm
import pm_report
import pm_view
import run

MEMORY=pm.MEMORY_METRICS[0]
COMPUTE=pm.COMPUTE_METRICS[0]


class Metric:
    def __init__(self,value,instances=(),ids=None,unit=""):
        self.aggregate=value; self.instances=list(instances); self.ids=ids; self.units=unit
    def value(self,i=None): return self.aggregate if i is None else self.instances[i]
    def num_instances(self): return len(self.instances)
    def has_correlation_ids(self): return self.ids is not None
    def correlation_ids(self): return Metric(0,self.ids)
    def as_uint64(self,i=None): return int(self.value(i))
    def unit(self): return self.units
    def description(self): return "Synthetic activity for host-side tests"


class Action:
    def __init__(self):
        self.metrics={
            "profiler__pmsampler_pass_groups":Metric(1,[MEMORY+","+COMPUTE],[0]),
            "profiler__replayer_passes":Metric(1),
            "profiler__timestamp_workload_start":Metric(100000),
            "profiler__timestamp_workload_end":Metric(220000),
            "profiler__pmsampler_interval_time":Metric(20000),
            "profiler__pmsampler_interval_cycles":Metric(0),
            "profiler__pmsampler_merged_samples":Metric(0),
            MEMORY:Metric(40,[12,30,70,55,20,10,3],[80000,100000,120000,140000,160000,180000,240000],"%"),
            COMPUTE:Metric(50,[0,10,45,80,90,70,0],[80000,100000,120000,140000,160000,180000,240000],"%")}
    def name(self): return "synthetic_tiled_kernel"
    def metric_names(self): return tuple(self.metrics)
    def metric_by_name(self,name): return self.metrics[name]


class Region:
    def __init__(self,action): self.action=action
    def num_actions(self): return 1
    def action_by_idx(self,i): return self.action


class Context:
    def __init__(self,action): self.action=action
    def num_ranges(self): return 1
    def range_by_idx(self,i): return Region(self.action)


def fixture():
    return pm_report.export_report(Context(Action()),MEMORY,COMPUTE)


class PMTests(unittest.TestCase):
    def test_extracts_one_pass_timestamps_and_keeps_raw_samples(self):
        result=fixture()
        self.assertTrue(result["single_pass_verified"])
        k=result["kernels"][0]
        self.assertEqual(k["channels"]["memory"]["in_kernel_samples"],5)
        self.assertEqual(len(k["channels"]["memory"]["samples"]),7)
        self.assertEqual(k["interval_ns"],20000)

    def test_rejects_multiple_groups(self):
        a=Action(); a.metrics["profiler__pmsampler_pass_groups"]=Metric(2,[MEMORY,COMPUTE],[0,1])
        with self.assertRaisesRegex(pm_report.ReportError,"one pass"):
            pm_report.export_report(Context(a),MEMORY,COMPUTE)

    def test_rejects_additional_replay(self):
        a=Action(); a.metrics["profiler__replayer_passes"]=Metric(2)
        with self.assertRaisesRegex(pm_report.ReportError,"collection pass"):
            pm_report.export_report(Context(a),MEMORY,COMPUTE)

    def test_rejects_unverified_membership(self):
        a=Action(); a.metrics["profiler__pmsampler_pass_groups"]=Metric(1,[MEMORY],[0])
        with self.assertRaisesRegex(pm_report.ReportError,"same pass"):
            pm_report.export_report(Context(a),MEMORY,COMPUTE)

    def test_rejects_missing_pass_metadata(self):
        a=Action(); del a.metrics["profiler__pmsampler_pass_groups"]
        with self.assertRaisesRegex(pm_report.ReportError,"missing"):
            pm_report.export_report(Context(a),MEMORY,COMPUTE)

    def test_insufficient_resolution_is_explicit(self):
        a=Action(); a.metrics["profiler__timestamp_workload_end"]=Metric(105000)
        k=pm_report.export_action(a,MEMORY,COMPUTE)
        self.assertTrue(any("insufficient" in s for s in k["warnings"]))

    def test_prefixed_report_metric_names_supported(self):
        a=Action(); a.metrics["pmsampling:"+MEMORY]=a.metrics.pop(MEMORY)
        self.assertEqual(pm_report.export_action(a,MEMORY,COMPUTE)["pass_groups"],1)

    def test_metrics_must_be_discovered(self):
        query=MEMORY+"\n"+COMPUTE
        self.assertEqual(pm.select_metrics(query),[MEMORY,COMPUTE])
        with self.assertRaisesRegex(ValueError,"unavailable"):
            pm.select_metrics(query,compute="not_a_supported_metric")

    def test_command_limits_collection_without_generic_sections(self):
        cmd=pm.ncu_command("ncu","report",MEMORY,COMPUTE,20000,"runner","candidate",3,"case","uuid",.003,.02)
        self.assertEqual(cmd[cmd.index("--pm-sampling-max-passes")+1],"1")
        self.assertIn("--disable-pm-warp-sampling",cmd)
        self.assertNotIn("--section",cmd)
        self.assertEqual(cmd[cmd.index("--profile-from-start")+1],"off")

    def test_view_escapes_names_and_clips_outside_samples(self):
        d=fixture(); d["kernels"][0]["kernel"]='<script>alert(1)</script>'
        with tempfile.TemporaryDirectory() as tmp:
            dest=Path(tmp)/"timeline.html"
            pm_view.render("test",dict(baseline=dict(status="collected",data=d)),dest,fixture=True)
            text=dest.read_text()
            self.assertNotIn("<script>",text)
            self.assertIn("SYNTHETIC TEST FIXTURE",text)
            self.assertEqual(text.count("<circle "),10)
            self.assertIn("not transfer or compute stage boundaries",text)

    def test_unavailable_ncu_is_nonfatal(self):
        with tempfile.TemporaryDirectory() as tmp,patch("pm.shutil.which",return_value=None):
            result=pm.collect("runner","corpus",Path(tmp),["case"],"uuid",SimpleNamespace(ncu="ncu"),{})
            self.assertEqual(result["status"],"unsupported")

    def collection(self,tmp,bad_report=False):
        folder=Path(tmp)
        exe=folder/"runner"; exe.write_bytes(b"synthetic executable identity")
        options=SimpleNamespace(ncu="ncu",timeout=10,pm_memory_metric=None,pm_compute_metric=None,
            ncu_python=sys.executable,ncu_python_path=None,pm_interval=20000,warmup=3)
        commands=[]
        def tool(command,where,name,timeout,env=None):
            commands.append((name,command))
            if name=="ncu-version": return "synthetic ncu version"
            if name=="ncu-help": return "query-metrics-collection pm-sampling-max-passes disable-pm-warp-sampling"
            if name=="metric-query": return MEMORY+"\n"+COMPUTE
            if name=="python-check": return ""
            if name=="profile":
                Path(command[command.index("--export")+1]).write_bytes(b"synthetic report")
                return '{"profile_validation":"passed"}'
            if name=="export":
                action=Action()
                if bad_report: action.metrics["profiler__replayer_passes"]=Metric(2)
                content=pm_report.export_report(Context(action),MEMORY,COMPUTE)
                Path(command[3]).write_text(json.dumps(content))
                return ""
            self.fail("unexpected tool call "+name)
        with patch("pm.shutil.which",return_value="/fake/ncu"),patch("pm.call",side_effect=tool):
            result=pm.collect(exe,folder,folder,["case"],"uuid",options,dict(atol=.003,rtol=.02))
        return result,commands

    def test_collection_writes_reports_samples_index_and_view(self):
        with tempfile.TemporaryDirectory() as tmp:
            result,commands=self.collection(tmp)
            self.assertEqual(result["status"],"collected")
            root=Path(tmp)/"diagnostics"/"case"
            index=json.loads((root/"index.json").read_text())
            for role in ["baseline","candidate"]:
                self.assertTrue((root/index["implementations"][role]["samples"]).exists())
                self.assertTrue((root/index["implementations"][role]["native_report"]).exists())
            self.assertIn("Memory / compute",(root/"timeline.html").read_text())
            self.assertEqual(sum(name=="profile" for name,cmd in commands),2)

    def test_collection_rejects_multipass_without_retry(self):
        with tempfile.TemporaryDirectory() as tmp:
            result,commands=self.collection(tmp,bad_report=True)
            self.assertEqual(result["status"],"failed")
            self.assertEqual(sum(name=="profile" for name,cmd in commands),2)
            index=json.loads((Path(tmp)/"diagnostics/case/index.json").read_text())
            self.assertTrue(all(x["status"]=="failed" for x in index["implementations"].values()))

    def evaluation(self,tmp,enabled,collector):
        index=data.verify(data.generate("gemm"))
        native=[dict(environment=dict(gpu_uuid="test-uuid",gpu="synthetic-test-environment"))]
        for case in index["cases"]:
            native.append(dict(status="passed",correctness=dict(baseline_pass=True,candidate_pass=True),
                               baseline=dict(wall_us=100,wall_mad_us=1,device_us=80,samples=20),
                               candidate=dict(wall_us=75,wall_mad_us=1,device_us=60,samples=20)))
        output=Path(tmp)/("on.json" if enabled else "off.json")
        options=SimpleNamespace(submission=data.ROOT/"submissions"/"gemm",output=output,
            samples=20,warmup=3,min_improvement=.02,check_only=False,nvcc="nvcc",timeout=120,
            profile_pm=enabled,profile_case=None)
        process=SimpleNamespace(stdout="\n".join(json.dumps(x) for x in native),stderr="",returncode=0)
        def collect(*args):
            # A full result is persisted before the optional path starts.
            saved=json.loads(output.read_text())['extensions']['scaffold_v1']
            self.assertTrue(all(c["wall_time_ratio"]==.75 for c in saved["cases"]))
            return collector(*args)
        with patch("run.shutil.which",return_value="/fake/nvcc"), \
             patch("run.build",return_value=(Path("/fake/runner"),{"cache_hit":False})), \
             patch("run.subprocess.run",return_value=process), \
             patch("pm.collect",side_effect=collect) as profile,redirect_stdout(io.StringIO()):
            self.assertEqual(run.evaluate(options),0)
            self.assertEqual(profile.call_count,1 if enabled else 0)
        return json.loads(output.read_text())['extensions']['scaffold_v1']

    def test_opt_in_preserves_all_unprofiled_metrics_and_decision(self):
        with tempfile.TemporaryDirectory() as tmp:
            before=self.evaluation(tmp,False,lambda *x: self.fail("profiling was invoked"))
            def collected(*args):
                return dict(status="collected",cases={"larger":dict(status="collected",timeline="test.html")})
            after=self.evaluation(tmp,True,collected)
            after.pop("diagnostics")
            for c in after["cases"]: c.pop("diagnostics",None)
            self.assertEqual(before,after)

    def test_profiler_exception_does_not_change_success_or_ratios(self):
        with tempfile.TemporaryDirectory() as tmp:
            def broken(*args): raise RuntimeError("incompatible tool")
            result=self.evaluation(tmp,True,broken)
            self.assertEqual(result["status"],"completed")
            self.assertEqual(result["decision"],"accepted")
            self.assertEqual(result["diagnostics"]["status"],"failed")
            self.assertTrue(all(c["wall_time_ratio"]==.75 for c in result["cases"]))


if __name__=="__main__":
    unittest.main()
