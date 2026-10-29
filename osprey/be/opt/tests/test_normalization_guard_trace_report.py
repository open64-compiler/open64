"""Synthetic LN2-d evidence-tamper tests, not native/runtime execution evidence.
Design: doc/AI-COMPILER-LAYERNORM-RUNTIME-GUARDS-v0.1.md.
"""
import struct
import unittest

from normalization_guard_trace_report import FIELDS, audit_exports, audit_runtime, decode
from normalization_plan_trace_report import audit as plan_audit
from test_normalization_plan_trace_report import fixture, serialize, source_trace


class Image:
    """Supply synthetic guard bytes without conflating them with retained files."""
    def __init__(self,data):
        """Freeze one exact test image."""
        self.data=data

    def read_bytes(self):
        """Expose immutable synthetic bytes to the production auditor."""
        return self.data


def export_fixture(kind=3):
    """Independent small source+plan and exact homogeneous ABI serialization."""
    source=source_trace();plan=serialize(*fixture(kind));p=plan_audit(plan,source)[0]
    g=dict.fromkeys(FIELDS,0)
    g.update(abi_version=1,size=664,source_version=2,source_flags=29687,pu=11,node=7,x=1,gamma=2,beta=3,result=5,
             x_type=13568,result_type=13568,gamma_type=13824,beta_type=13824,rows=3,width=17,epsilon_bits=0x3727c5ac,
             source_line=211,architecture=1,route=1,toolkit=12080,ptx=807,environment=1)
    mapping=dict(contract="runtime",reserved_shared="reserved",shared_limit="limit",shared_quantum="quantum",bank_conflict="conflict")
    for k in ("kind","layout","contract","context","ownership","threads","ctas","tail","stride","shared","reserved_shared",
              "shared_limit","shared_quantum","participants","barriers","banks","bank_word","warp","bank_conflict","obligations"):
        g[k]=int(p[mapping.get(k,k)])
    for i in range(4):
        for k in ("role","capacity","offset","alignment","lease","bytes"):
            g[f"range{i}_{k}"]=p["ranges"][i]["allocation" if k=="role" else k]
    for i in range(15):g[f"before{i}"]=p["edges"][i]
    prefix="\n".join(plan.splitlines()[3:])+"\n"
    return g,prefix,plan,source


def image_trace(g,prefix):
    """Serialize positional synthetic columns, with a duplicated native line last."""
    values=[g[k] for k in FIELDS]
    return prefix+"AI_NORM_GUARD 1 EXPORT 83 "+" ".join(map(str,values))+" 211\n",Image(struct.pack("<83Q",*values))


def runtime_fixture():
    """A complete minimal synthetic host-only successful ticket lifecycle."""
    rows=["AI_NORM_RUN 1 HEADER seq ticket action status host_done leases host_test source_line"]
    n=0;done=0;leases=15

    def event(id,action,status,mask,pins,line=211):
        """Append one exactly sequenced synthetic host observation."""
        nonlocal n
        n+=1;rows.append(f"AI_NORM_RUN 1 EVENT {n} {id} {action} {status} {mask} {pins} 1 {line}")

    for _ in range(100):event(0,"register",1,0,0,0)
    g=export_fixture()[0]
    rows.append("AI_NORM_RUN 1 BIND 1 83 "+" ".join(str(g[k]) for k in FIELDS)+" 211")
    rows.append(f"AI_NORM_RUN 1 LIMIT 1 {g['ctas']} {g['threads']} {g['reserved_shared']} 1 211")
    for i in range(4):
        r=[g[f"range{i}_{k}"] for k in ("role","capacity","offset","alignment","bytes","lease")]
        rows.append(f"AI_NORM_RUN 1 RANGE 1 {i} {r[0]} {i+1} {i+1} "+" ".join(map(str,r[1:]))+" 211")
    event(1,"admit",0,done,leases)
    for name,index in (("ready",0),("launch",1),("drain",11),("publish",12),("input_release",13),("output_release",14)):
        if index<13:event(1,"begin_"+name,0,done,leases)
        done|=1<<index
        if index==13:leases=8
        if index==14:leases=0
        event(1,name,0,done,leases)
    event(1,"close",0,done,leases)
    rows += ["AI_NORM_TEST 1 CHECK synthetic 1 211"]*200
    rows += ["AI_NORM_TEST 1 SUMMARY 200 host_mock_only 211"]
    return "\n".join(rows)+"\n"


class NormalizationGuardTests(unittest.TestCase):
    """Keep evidence rejection independent from native implementation tests."""
    def test_all_alternatives(self):
        """All bounded plans share the same explicit source-v2 contract."""
        for kind in (1,2,3):
            g,prefix,plan,source=export_fixture(kind);text,image=image_trace(g,prefix)
            self.assertEqual(audit_exports(text,plan,source,[image]),([g],0))

    def test_every_export_field(self):
        """A trace+byte-consistent mutation still must agree with native facts."""
        g,prefix,plan,source=export_fixture()
        for field in FIELDS:
            bad=dict(g);bad[field]^=1;text,image=image_trace(bad,prefix)
            with self.subTest(field=field),self.assertRaises(ValueError):audit_exports(text,plan,source,[image])

    def test_bytes_and_census(self):
        """Refuse mismatched bytes, duplicates, unknown/truncated schemas."""
        g,prefix,plan,source=export_fixture();text,image=image_trace(g,prefix)
        for raw in (image.data[:-1],image.data+b"\0",b"\0"*664):
            with self.assertRaises(ValueError):audit_exports(text,plan,source,[Image(raw)])
        for bad in (text+text,text[:-5],text.replace("EXPORT 83","EXPORT 84"),text.replace(" 29687 "," -1 ")):
            with self.assertRaises(ValueError):audit_exports(bad,plan,source,[image])
        with self.assertRaises(ValueError):audit_exports(text,plan,source,[])

    def test_runtime_protocol(self):
        """Host observations are separate from internal GPU completion."""
        g=export_fixture()[0];result=audit_runtime(runtime_fixture(),g)
        self.assertEqual(result["checks"],200);self.assertFalse(result["internal_GPU_events_observed"])

    def test_runtime_tampering(self):
        """Catch early work, invented internal completion and missing work begin."""
        g=export_fixture()[0];text=runtime_fixture()
        bads=[text.replace("begin_launch 0 1 15","begin_launch 0 0 15"),
              text.replace("drain 0 2051 15","drain 0 4095 15"),
              text.replace("publish 0 6147 15","publish 0 6147 0"),
              text.replace("begin_publish","begin_unknown"),
              text.replace("CHECK synthetic 1 211","CHECK synthetic 0 211",1),
              text+"AI_NORM_TEST 1 CHECK after_summary 1 211\n",
              text.replace("host_mock_only","GPU_verified"),
              text.replace("AI_NORM_RUN 1 LIMIT 1 3 128 256","AI_NORM_RUN 1 LIMIT 1 2 128 256"),
              text.replace("AI_NORM_RUN 1 RANGE 1 3 14 4 4","AI_NORM_RUN 1 RANGE 1 3 14 1 1"),
              text.replace("AI_NORM_RUN 1 BIND 1 83","AI_NORM_RUN 1 BIND 1 84"),
              "\n".join(line for line in text.splitlines() if "begin_drain" not in line)+"\n"]
        for bad in bads:
            with self.subTest(bad=bad[-90:]),self.assertRaises(ValueError):audit_runtime(bad,g)


if __name__=="__main__":unittest.main()
