"""Q1 genuine native source .B/.T, mapped semantics and cache inference review.

The numerical interpreter executes independently reopened native operations for
ingestion parity; it is not compiled CPU/GPU lowering or a search cost provider.
"""
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import struct
import subprocess
import sys

import torch
from models.mla_quantized_paged import CASES


def bits(text):
    return struct.unpack(">f", bytes.fromhex(text))[0]


def require(ok, message):
    if not ok:
        raise ValueError("Q1 mapped semantic guard: " + message)


def evaluate_mapped(report, inputs):
    """Execute the actual independently reopened typed source graph, no FX.

    Explicit runtime checks implement source guard obligations in this review
    interpreter. Production executable guards/lowering remain Q3/Q5 work.
    """
    values, ordinal = {}, 0
    for record in report["values"]:
        name, opcode, a = record["name"], record["opcode"], record["attrs"]
        if opcode == "formal":
            require(ordinal < len(inputs), "formal count")
            result = inputs[ordinal]; ordinal += 1
        else:
            x = [values[k] for k in record["operands"]]
            version = record["version"]
            if opcode in ("common.add", "common.mul", "common.div", "common.rem") and version == 2:
                require(all(t.dtype == torch.int64 and bool((t >= 0).all()) for t in x), "nonnegative int64")
                if opcode in ("common.div", "common.rem"):
                    require(bool((x[1] > 0).all()), "positive divisor")
                left, right = torch.broadcast_tensors(*x)
                pairs = zip(left.reshape(-1).tolist(), right.reshape(-1).tolist())
                fn = {"common.add": lambda n,d:n+d, "common.mul": lambda n,d:n*d,
                      "common.div": lambda n,d:n//d, "common.rem": lambda n,d:n%d}[opcode]
                raw = [fn(n,d) for n,d in pairs]
                require(all(0 <= n < 2**63 for n in raw), "integer overflow")
                result = torch.tensor(raw, dtype=torch.int64).reshape(left.shape)
            elif opcode == "common.abs":
                result = torch.abs(x[0])
            elif opcode == "common.round_even":
                result = torch.round(x[0])
            elif opcode == "common.clamp":
                result = torch.clamp(x[0], min=bits(a["attr.min_bits"]) if a["attr.has_min"] == "true" else None,
                                     max=bits(a["attr.max_bits"]) if a["attr.has_max"] == "true" else None)
            elif opcode == "common.convert":
                if a["attr.mode"] == "checked_integral":
                    low, high = (-128,127) if a["attr.dst_dtype"] == "int8" else (0,255)
                    require(bool((torch.isfinite(x[0]) & (x[0] == torch.round(x[0])) &
                                  (x[0] >= low) & (x[0] <= high)).all()), "finite integral conversion range")
                result = x[0].to(getattr(torch,a["attr.dst_dtype"]))
            elif opcode == "common.pack_u4":
                require(bool((x[0] <= 15).all()), "U4 codes")
                result = x[0][...,0::2] | (x[0][...,1::2] << 4)
            elif opcode == "common.unpack_u4":
                result = torch.stack((x[0] & 15,x[0] >> 4),dim=-1).flatten(-2)
            elif opcode in ("common.gather", "common.scatter"):
                axis = int(a["attr.axis"])
                require(bool(((x[1] >= 0) & (x[1] < x[0].shape[axis])).all()), "index bounds")
                if opcode == "common.gather":
                    result = torch.index_select(x[0],axis,x[1])
                else:
                    require(len(set(x[1].tolist())) == x[1].numel(), "unique functional updates")
                    result = torch.index_copy(x[0],axis,x[1],x[2])
                    require(result.data_ptr() != x[0].data_ptr(), "fresh scatter result")
            elif opcode == "common.fill_like":
                result = torch.full_like(x[0],int(a["attr.value_i64"]) if version == 2 else bits(a["attr.value_bits"]))
            elif opcode == "common.reshape":
                result = x[0].reshape(tuple(map(int,a["attr.target_shape"].split(","))))
            elif opcode == "common.transpose":
                result = x[0].permute(tuple(map(int,a["attr.permutation"].split(","))))
            elif opcode in ("common.add", "common.sub", "common.mul", "common.div", "common.greater", "common.matmul"):
                fn = {"common.add":torch.add,"common.sub":torch.sub,"common.mul":torch.mul,
                      "common.div":torch.div,"common.greater":torch.gt,"common.matmul":torch.matmul}[opcode]
                result = fn(*x)
            elif opcode == "common.select":
                result = torch.where(*x)
            elif opcode in ("common.exp", "common.log", "common.isfinite", "common.logical_not"):
                result = getattr(torch,opcode.split(".")[1])(x[0])
            elif opcode in ("common.reduce_max", "common.reduce_sum"):
                result = (torch.amax if opcode.endswith("max") else torch.sum)(
                    x[0],dim=int(a["attr.axis"]),keepdim=a["attr.keepdim"] == "true")
            else:
                raise ValueError("unimplemented mapped review operation " + opcode)
        require(str(result.dtype) == "torch."+record["dtype"] and list(result.shape) == record["shape"], "result dtype/shape")
        values[name] = result
    require(ordinal == len(inputs), "unused formal boundary")
    return tuple(values[name] for name in report["results"])


def produce(case, destination):
    from mla_q0_capture import check_case, safe
    from open64_dsc import export_to_whirl,save_as_whirl,WhirlExportOptions
    root = Path(destination);root.mkdir(parents=True,exist_ok=False)
    model,inputs,meta,graph,census,actual,reference,errors = check_case(case)
    module = export_to_whirl(model,inputs,WhirlExportOptions(backend="native",model_name="mla_q1_"+case))
    save_as_whirl(module,str(root/"source.B"))
    require((root/"source.B").read_bytes().startswith(b"\x7fELF"), "genuine WHIRL ELF")
    torch.save(dict(inputs=inputs,source=actual),root/"inputs-and-source.pt")
    (root/"module.json").write_text(json.dumps(module.to_manifest(),indent=2)+"\n")
    (root/"source.fx.txt").write_text(str(graph.graph)+"\n")
    (root/"source.census.json").write_text(json.dumps(census,indent=2)+"\n")
    (root/"reference.json").write_text(json.dumps(dict(metadata=meta,expected=safe(reference),
                  max_abs_errors=errors,native_quantized_whirl=True,compiled_execution=False,gpu_execution=False),indent=2)+"\n")


def review(destination, ir_b2a, observer):
    root = Path(destination);root.mkdir(parents=True,exist_ok=False)
    logs=root/"logs";logs.mkdir();commands=[];cases={}
    def run(label,argv):
        p=subprocess.run(list(map(str,argv)),capture_output=True,text=True)
        for stream in ("stdout","stderr"):(logs/(label+"."+stream)).write_text(getattr(p,stream))
        commands.append(dict(label=label,argv=list(map(str,argv)),returncode=p.returncode))
        (root/"commands.json").write_text(json.dumps(commands,indent=2)+"\n")
        require(p.returncode == 0,label+": "+p.stderr[-2400:])
    for name in CASES:
        case=root/name
        run(name+"-produce",[sys.executable,__file__,"--produce",name,"--artifact-dir",case])
        run(name+"-reopen",[ir_b2a,"-st","-src",case/"source.B",case/"source.T"])
        run(name+"-inspect",[observer,"q1-inspect",case/"source.B",case/"source.native.json"])
        mapped=json.loads((case/"source.native.json").read_text())
        payload=torch.load(case/"inputs-and-source.pt",weights_only=True)
        original=[x.clone() for x in payload["inputs"]]
        with torch.no_grad():actual=evaluate_mapped(mapped,payload["inputs"])
        require(len(actual)==6,"six source results")
        for a,b in zip(actual,payload["source"]):torch.testing.assert_close(a,b,rtol=0,atol=0)
        for a,b in zip(original,payload["inputs"]):torch.testing.assert_close(a,b,rtol=0,atol=0)
        decoder_boundaries=[]
        if name in ("paged_s8_decode", "paged_u4_prefill"):
            from models.mla_quantized_paged import make_fixture
            from models.mla_quantized_reference import reference as scalar_reference
            from mla_q0_capture import safe
            model,_,meta=make_fixture(name)
            boundary=[x.clone() for x in payload["inputs"]]
            pattern=([i | ((15-i)<<4) for i in range(16)] if "u4" in name else [-128,-127,0,127])
            for index in (2,4):
                flat=boundary[index].reshape(-1)
                flat.copy_(torch.tensor([pattern[i%len(pattern)] for i in range(flat.numel())],dtype=flat.dtype))
            with torch.no_grad():decoded=evaluate_mapped(mapped,boundary);source=model(*boundary)
            oracle=scalar_reference([x.tolist() for x in boundary],meta)
            for i,(value,expected,ref) in enumerate(zip(decoded,source,oracle)):
                torch.testing.assert_close(value,expected,rtol=0,atol=0)
                reference_tensor=torch.tensor(ref,dtype=torch.float64 if i<2 else value.dtype)
                torch.testing.assert_close(value.double() if i<2 else value,reference_tensor,
                    rtol=2e-5 if i<2 else 0,atol=2e-6 if i<2 else 0)
            torch.save(dict(inputs=boundary,source=source),case/"decoder-boundary.pt")
            (case/"decoder-boundary-reference.json").write_text(json.dumps(safe(oracle),indent=2)+"\n")
            decoder_boundaries=["all sixteen low/high nibble codes" if "u4" in name else "signed -128,-127,0,127"]
        guards=[]
        if name.startswith(("paged", "append")):
            for label,value in (("page bounds",999),("integer overflow",2**63-1)):
                invalid=[x.clone() for x in payload["inputs"]];invalid[6][0,0]=value
                try:
                    evaluate_mapped(mapped,invalid)
                except ValueError:
                    guards.append(label)
                else:
                    raise AssertionError("mapped review guard did not refuse "+label)
        if name.startswith("append"):
            invalid=[x.clone() for x in payload["inputs"]]
            invalid[12][1,0]=invalid[12][0,0]
            try:
                evaluate_mapped(mapped,invalid)
            except ValueError:
                guards.append("unique functional writes")
            else:
                raise AssertionError("mapped review duplicate-write guard did not refuse")
        require(mapped["inferred_values"]>0 and mapped["native_state_unchanged"],"native inferred roles/read-only observation")
        trace=(case/"source.T").read_text()
        require("attention.latent_kv_cache.v1" in trace and "attention.rotary_key_cache.v1" in trace,"mapped cache traits")
        if "u4" in name:require("OPR_DSLUNPACKU4" in trace,"native U4 unpack")
        if name.startswith("append"):require("OPR_DSLSCATTER" in trace and "version=2" in trace,"functional native scatter")
        cases[name]=dict(native_nodes=mapped["native_nodes"],inferred_values=mapped["inferred_values"],
                         mapped_semantics="PASS exact",reference="PASS Q0 independent oracle",input_immutable=True,
                         review_interpreter_guards=guards,decoder_boundaries=decoder_boundaries)
    report=dict(schema="open64.mla-q1-native-validation.v1",status="PASS",cases=cases,
                independent_native_reopens=10,native_quantized_whirl=True,compiled_execution=False,
                gpu_execution=False,search_invoked=False,production_quantized_lowering=False)
    (root/"report.json").write_text(json.dumps(report,indent=2)+"\n")
    (root/"SHA256SUMS").write_text("".join(hashlib.sha256(p.read_bytes()).hexdigest()+"  "+str(p.relative_to(root))+"\n"
        for p in sorted(root.rglob("*")) if p.is_file()))
    print(json.dumps(report,indent=2))


if __name__ == "__main__":
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument("--produce",choices=CASES);p.add_argument("--artifact-dir",type=Path)
    p.add_argument("--ir-b2a",type=Path);p.add_argument("--observer",type=Path)
    a=p.parse_args();destination=a.artifact_dir
    if destination is None and os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR"):
        destination=Path(os.environ["OPEN64_DSL_TEST_ARTIFACT_DIR"])/"mla-q1"
    if destination is None:p.error("fresh artifact directory required")
    if a.produce:produce(a.produce,destination)
    elif a.ir_b2a and a.observer:review(destination,a.ir_b2a,a.observer)
    else:p.error("--ir-b2a and --observer required")
