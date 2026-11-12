"""Q0 quantized/paged MLA source census and CPU correctness artifacts.

This runner performs source capture, not candidate exploration. Unsupported
native ingestion is a required refusal, never a fabricated WHIRL image.
"""
import argparse
from collections import Counter
import hashlib
import inspect
import json
import math
import operator
import os
from pathlib import Path
import shutil
import subprocess
import sys

import torch
from torch.fx import symbolic_trace
from torch.fx.passes.shape_prop import ShapeProp
from models.mla_quantized_paged import CASES, make_fixture
from models.mla_quantized_reference import reference
from mla_q0_contract import make_contract, validate_contract

# Exact Q0 surface. Unlisted FX/ATen targets fail census instead of disappearing.
FX_REQUIREMENTS = {
    "abs": ("common-substrate", "common.abs.v1", "new-contract"),
    "round": ("common-substrate", "common.round_even.v1", "new-contract"),
    "clamp": ("common-substrate", "common.clamp.v1", "new-contract"),
    "to": ("common-substrate", "common.convert.v1", "new-contract"),
    "bitwise_or": ("common-substrate", "common.pack_u4.v1", "composite-pattern"),
    "bitwise_left_shift": ("common-substrate", "common.pack_u4.v1", "composite-pattern"),
    "bitwise_and": ("common-substrate", "common.unpack_u4.v1", "composite-pattern"),
    "bitwise_right_shift": ("common-substrate", "common.unpack_u4.v1", "composite-pattern"),
    "stack": ("common-substrate", "common.unpack_u4.v1", "composite-pattern"),
    "flatten": ("common-substrate", "common.unpack_u4.v1", "composite-pattern"),
    "getitem": ("common-substrate", "common.pack_u4.v1", "composite-pattern"),
    "index_select": ("common-substrate", "common.gather.v1", "marker-needs-native-schema"),
    "index_copy": ("state-effect-functional", "common.scatter.v2", "new-version-pure-functional"),
    "reshape": ("common-substrate", "common.reshape.v1", "existing-native-frontend-extension"),
    "transpose": ("common-substrate", "common.transpose.v1", "existing-native"),
    "unsqueeze": ("common-substrate", "common.reshape.v1", "existing-native"),
    "squeeze": ("common-substrate", "common.reshape.v1", "existing-native"),
    "matmul": ("domain-expression-component", "common.matmul.v3", "existing-native"),
    "amax": ("common-substrate", "common.reduce_max.v1", "existing-native"),
    "sum": ("common-substrate", "common.reduce_sum.v3", "existing-native"),
    "add": ("common-substrate", "common.add.v1", "dtype-dependent"),
    "sub": ("common-substrate", "common.sub.v1", "existing-native"),
    "mul": ("common-substrate", "common.mul.v1", "dtype-dependent"),
    "truediv": ("common-substrate", "common.div.v1", "existing-native"),
    "div": ("common-substrate", "common.div.v1", "rounding-dependent"),
    "remainder": ("common-substrate", "common.rem.v2", "new-version-integer"),
    "invert": ("common-substrate", "common.logical_not.v1", "existing-native"),
    "masked_fill": ("common-substrate", "common.fill_like.v1+common.select.v1", "existing-native"),
    "isfinite": ("common-substrate", "common.isfinite.v1", "existing-native"),
    "where": ("common-substrate", "common.select.v1", "existing-native"),
    "zeros_like": ("common-substrate", "common.fill_like.v1", "existing-native"),
    "ones_like": ("common-substrate", "common.fill_like.v1", "existing-native"),
    "full_like": ("common-substrate", "common.fill_like.v1", "existing-native"),
    "exp": ("common-substrate", "common.exp.v1", "existing-native"),
    "log": ("common-substrate", "common.log.v1", "existing-native"),
    "gt": ("common-substrate", "common.greater.v1", "existing-native"),
}
ATEN_ROOTS = {"abs","amax","clamp","div","round","_to_copy","mul","add","sub","matmul",
              "unsqueeze","transpose","squeeze","bitwise_not","masked_fill","isfinite","zeros_like",
              "where","exp","sum","gt","ones_like","log","full_like","slice","bitwise_or",
              "bitwise_left_shift","bitwise_and","bitwise_right_shift","stack","view","index_select",
              "index_copy","remainder","clone","_unsafe_view","lift_fresh_copy"}


def target_name(node):
    if node.op == "call_function":
        if hasattr(node.target,"_schema"):
            return str(node.target)
        return node.target.__module__+"."+node.target.__name__
    return str(node.target)


def safe(value):
    if isinstance(value,torch.fx.Node):
        return {"value":value.name}
    if isinstance(value,torch.dtype):
        return str(value)
    if isinstance(value,slice):
        return {"slice":[safe(value.start),safe(value.stop),safe(value.step)]}
    if isinstance(value,(tuple,list)):
        return [safe(x) for x in value]
    if isinstance(value,dict):
        return {str(k):safe(v) for k,v in value.items()}
    if isinstance(value,float) and not math.isfinite(value):
        return "-inf" if value<0 else "+inf" if value>0 else "nan"
    if value is Ellipsis:
        return "..."
    if value is None or type(value) in (str,int,float,bool):
        return value
    raise ValueError("unrepresented graph argument "+str(type(value)))


def classify(node, normalized=False):
    if node.op in ("placeholder","output"):
        return dict(category="region-contract",native="input/result",status="boundary")
    if node.op=="get_attr":
        return dict(category="constant",native="typed-constant",status="explicit-constant")
    if normalized:
        root=str(node.target).split(".")[1] if str(node.target).startswith("aten.") else ""
        if root not in ATEN_ROOTS:
            raise ValueError("unclassified normalized target "+str(node.target))
        return dict(category="state-effect-functional" if root=="index_copy" else "common-substrate",
                    native="see-source-pattern-join",status="normalized-source-evidence")
    name=str(node.target) if node.op=="call_method" else node.target.__name__
    if name not in FX_REQUIREMENTS:
        raise ValueError("unclassified FX target "+name)
    category,native,status=FX_REQUIREMENTS[name]
    meta=node.meta.get("tensor_meta")
    if name in ("add","mul") and hasattr(meta,"dtype") and meta.dtype==torch.int64:
        native="common."+name+".v2";status="new-version-integer"
    if name=="div" and node.kwargs.get("rounding_mode")=="floor":
        native="common.div.v2";status="new-version-integer"
    return dict(category=category,native=native,status=status)


def structure(graph, normalized=False):
    nodes=[]
    for node in graph.graph.nodes:
        meta=node.meta.get("tensor_meta")
        if meta is None:
            meta=node.meta.get("val")
        record=dict(name=node.name,op=node.op,target=target_name(node),arguments=safe(node.args),
                    keyword_arguments=safe(node.kwargs),classification=classify(node,normalized),
                    dtype=str(meta.dtype) if hasattr(meta,"dtype") else None,
                    shape=list(meta.shape) if hasattr(meta,"shape") else None)
        nodes.append(record)
    counts=Counter(n["op"]+":"+n["target"] for n in nodes)
    return dict(nodes=nodes,operators=dict(sorted(counts.items())))


def capture(name):
    model,inputs,meta=make_fixture(name)
    graph=symbolic_trace(model)
    with torch.no_grad():
        ShapeProp(graph).propagate(*inputs)
    return model,inputs,meta,graph,structure(graph)


def census():
    return {name:dict(nodes=len(data["nodes"]),operators=data["operators"])
            for name in CASES for data in [capture(name)[4]]}


def check_case(name):
    model,inputs,meta,graph,data=capture(name)
    original=[x.clone() for x in inputs]
    with torch.no_grad():
        actual=model(*inputs);replay=graph(*inputs)
    expected=reference([x.tolist() for x in inputs],meta)
    errors=[]
    for i,(value,fx,oracle) in enumerate(zip(actual,replay,expected)):
        expected_tensor=torch.tensor(oracle,dtype=torch.float64 if i<2 else value.dtype)
        torch.testing.assert_close(value.double() if i<2 else value,expected_tensor,
                                   rtol=2e-5 if i<2 else 0,atol=2e-6 if i<2 else 0)
        torch.testing.assert_close(value,fx,rtol=0,atol=0)
        finite=torch.isfinite(value) if value.is_floating_point() else torch.ones_like(value,dtype=torch.bool)
        errors.append(float((value.double()[finite]-expected_tensor.double()[finite]).abs().max()) if finite.any() else 0.0)
    for a,b in zip(original,inputs):
        torch.testing.assert_close(a,b,rtol=0,atol=0)
    return model,inputs,meta,graph,data,actual,expected,errors


def probe_ingestion(model,inputs,native=False):
    from open64_dsc import export_to_whirl,WhirlExportOptions
    try:
        export_to_whirl(model,inputs,WhirlExportOptions(backend="native" if native else "mock"))
    except (NotImplementedError,ValueError) as error:
        message=str(error)
        if "unsupported FX graph node" not in message:
            raise
        return dict(status="expected-unsupported",diagnostic=message,artifact_created=False)
    raise AssertionError("Q0-only graph unexpectedly entered native ingestion")


def write_artifacts(destination,native=False,native_tools=None):
    root=Path(destination);root.mkdir(parents=True,exist_ok=False)
    report=dict(schema="open64.mla-q0-capture.v1",torch=torch.__version__,device="cpu",
                native_quantized_whirl=False,gpu_execution=False,search_invoked=False,cases={})
    for name in CASES:
        model,inputs,meta,graph,data,actual,expected,errors=check_case(name)
        case=root/name;case.mkdir()
        (case/"fx.graph.txt").write_text(str(graph.graph)+"\n")
        (case/"fx.forward.py").write_text(graph.code)
        (case/"fx.census.json").write_text(json.dumps(data,indent=2,allow_nan=False)+"\n")
        (case/"serving.v1.json").write_text(json.dumps(make_contract(meta,inputs),indent=2,allow_nan=False)+"\n")
        payload=dict(metadata=meta,inputs=[dict(dtype=str(x.dtype),shape=list(x.shape),values=safe(x.tolist())) for x in inputs],
                     expected=safe(expected),actual=[safe(x.tolist()) for x in actual])
        (case/"values.json").write_text(json.dumps(payload,indent=2,allow_nan=False)+"\n")
        exported=torch.export.export(model,inputs)
        aten=structure(exported.graph_module,normalized=True)
        (case/"aten.graph.txt").write_text(str(exported.graph_module.graph)+"\n")
        (case/"aten.forward.py").write_text(exported.graph_module.code)
        (case/"aten.census.json").write_text(json.dumps(aten,indent=2,allow_nan=False)+"\n")
        torch.export.save(exported,case/"source.pt2")
        loaded=torch.export.load(case/"source.pt2").module()
        with torch.no_grad():
            for a,b in zip(actual,loaded(*inputs)):torch.testing.assert_close(a,b,rtol=0,atol=0)
        refusals=dict(mock=probe_ingestion(model,inputs))
        if native:refusals["native"]=probe_ingestion(model,inputs,True)
        (case/"ingestion-refusal.json").write_text(json.dumps(refusals,indent=2)+"\n")
        report["cases"][name]=dict(fx_nodes=len(data["nodes"]),aten_nodes=len(aten["nodes"]),max_abs_errors=errors,
            storage_dtype=[str(x.dtype) for x in actual[2:]],phase=meta["phase"],append=meta["append"],
            capture_replay="exact",source_reference="PASS",input_immutable=True,refusals=refusals)
    if native_tools is not None:
        tools=Path(native_tools);baseline=root/"legacy-fp32";baseline.mkdir()
        env=dict(os.environ,LD_LIBRARY_PATH=":".join(str(tools/x) for x in ("be","wopt","lno")))
        records={}
        for context in ("short","long"):
            image=baseline/(context+".B")
            # Native WHIRL tables are process-owned. Isolate each producer,
            # then reopen in a different process; never reuse its global pool.
            producer=subprocess.run([sys.executable,str(Path(__file__).resolve()),
                "--legacy-context",context,"--artifact-dir",str(image)],
                capture_output=True,text=True)
            (baseline/(context+".producer.stdout")).write_text(producer.stdout)
            (baseline/(context+".producer.stderr")).write_text(producer.stderr)
            if producer.returncode:raise RuntimeError("legacy producer failed: "+str(producer.returncode)+producer.stderr)
            argv=[str(tools/"ir_tools/ir_b2a"),"-st","-src",str(image),str(image.with_suffix(".T"))]
            process=subprocess.run(argv,env=env,capture_output=True,text=True)
            (baseline/(context+".reader.stdout")).write_text(process.stdout)
            (baseline/(context+".reader.stderr")).write_text(process.stderr)
            if process.returncode:raise RuntimeError("legacy reader failed: "+str(process.returncode)+process.stderr)
            if "attention.latent_kv_cache.v1" not in image.with_suffix(".T").read_text():
                raise AssertionError("legacy cache role missing")
            records[context]=dict(argv=argv,returncode=process.returncode,
                sha256=hashlib.sha256(image.read_bytes()).hexdigest(),native_input="FP32-only")
        report["legacy_native_baseline"]=records
    source=root/"source";source.mkdir()
    for name in ("mla_q0_capture.py","mla_q0_contract.py","test_mla_q0_capture_optional.py"):
        shutil.copy2(Path(__file__).with_name(name),source/name)
    (source/"models").mkdir()
    for name in ("__init__.py","mla_quantized_paged.py","mla_quantized_reference.py","mla_reference.py"):
        shutil.copy2(Path(__file__).with_name("models")/name,source/"models"/name)
    (root/"report.json").write_text(json.dumps(report,indent=2)+"\n")
    (root/"SHA256SUMS").write_text("".join(hashlib.sha256(p.read_bytes()).hexdigest()+"  "+str(p.relative_to(root))+"\n"
        for p in sorted(root.rglob("*")) if p.is_file()))
    return report


def replay_artifacts(root):
    """Load saved ATen programs and payloads in a separate interpreter process."""
    root=Path(root)
    for line in (root/"SHA256SUMS").read_text().splitlines():
        digest,name=line.split("  ",1)
        if hashlib.sha256((root/name).read_bytes()).hexdigest()!=digest:raise ValueError("receipt mismatch "+name)
    for name in CASES:
        payload=json.loads((root/name/"values.json").read_text())
        def restore(value):
            if isinstance(value,list):return [restore(x) for x in value]
            return {"-inf":-math.inf,"+inf":math.inf,"nan":math.nan}.get(value,value) if isinstance(value,str) else value
        inputs=[torch.tensor(restore(x["values"]),dtype=getattr(torch,x["dtype"].split(".")[-1])) for x in payload["inputs"]]
        contract=json.loads((root/name/"serving.v1.json").read_text());validate_contract(contract)
        module=torch.export.load(root/name/"source.pt2").module()
        with torch.no_grad():actual=module(*inputs)
        for value,expected in zip(actual,payload["actual"]):
            torch.testing.assert_close(value,torch.tensor(restore(expected),dtype=value.dtype),rtol=0,atol=0)
    return dict(status="PASS",cases=len(CASES),torch=torch.__version__,native_quantized_whirl=False,gpu_execution=False)


if __name__=="__main__":
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact-dir",type=Path)
    parser.add_argument("--write-golden",type=Path)
    parser.add_argument("--native-refusals",action="store_true")
    parser.add_argument("--replay",type=Path)
    parser.add_argument("--native-tools",type=Path)
    parser.add_argument("--legacy-context",choices=("short","long"),help=argparse.SUPPRESS)
    args=parser.parse_args()
    if args.legacy_context:
        from open64_dsc import export_to_whirl,save_as_whirl,WhirlExportOptions
        from models.absorbed_mla import make_fixture as dense_fp32
        if args.artifact_dir is None or args.artifact_dir.exists():parser.error("fresh legacy image required")
        model,inputs=dense_fp32(args.legacy_context)
        save_as_whirl(export_to_whirl(model,inputs,WhirlExportOptions(backend="native")),str(args.artifact_dir))
    elif args.replay:print(json.dumps(replay_artifacts(args.replay),indent=2))
    elif args.write_golden:args.write_golden.write_text(json.dumps(census(),indent=2,sort_keys=True)+"\n")
    else:
        destination=args.artifact_dir
        if destination is None and os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR"):
            destination=Path(os.environ["OPEN64_DSL_TEST_ARTIFACT_DIR"])/"mla-q0"
        if destination is None:parser.error("fresh --artifact-dir or OPEN64_DSL_TEST_ARTIFACT_DIR required")
        print(json.dumps(write_artifacts(destination,args.native_refusals,args.native_tools),indent=2))
