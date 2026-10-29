"""Synthetic LN2 range/event/report tests, separate from native evidence.

Design: doc/AI-COMPILER-LAYERNORM-STORAGE-EVENTS-v0.1.md.
"""
from pathlib import Path
import tempfile
import unittest

from normalization_plan_trace_report import (COLS, HEADER, HW, RANGES, REASONS, EVENTS,
                                             evaluate, ordered, audit, write_reports)
from normalization_ref_trace_report import audit as source_audit
from test_normalization_ref_trace_report import trace as source_trace
from test_normalization_target_trace_report import fixture as target_trace


def fixture(kind=3, source=None):
    """Construct a small typed request and serialize exact synthetic evidence."""
    source = source_trace() if source is None else source
    fact = source_audit(source)[0]
    hw = dict(zip(HW, ["sm_90", 1, 2, 2, 232448, 65536, 256, 16, 32, 4, 32, 1024, 123]))
    r = dict.fromkeys([c for c in COLS if c != "reason"], 0)
    r.update(pu=11, node=7, result=5, kind=kind, layout=1, runtime=1, context=1,
             ownership=1, threads=128, ctas=int(fact["rows"]), tail=1, source_line=211)
    n, rows = int(fact["width"]), int(fact["rows"])
    if kind != 1:
        r.update(stride=((n*4+15)//16)*16, participants=128,
                 barriers=sum(1 << x for x in (4,5,7,9)) | (1 << 2 if kind == 3 else 0))
        r["shared"] = r["stride"]+8
    ranges = [dict(index=i,allocation=11+i,capacity=(rows*n*4 if i in (0,3) else n*4),
                   offset=0,alignment=16,lease=2 if i==3 else 1,bytes=0,source_line=211) for i in range(4)]
    edges = [1 << (i+1) if i+1<len(EVENTS) else 0 for i in range(len(EVENTS))]
    return r, ranges, edges, fact, hw


def serialize(r, ranges, edges, fact, hw):
    """Use synthetic expected results only to test parser mutation checks."""
    status, derived, sizes = evaluate(r,ranges,edges,fact,hw,0)
    r = dict(r,**derived,status=status)
    words = {k:str(v) for k,v in r.items()};words["reason"] = REASONS[status]
    lines = ["AI_NORM_HW 1 MODEL " + " ".join(str(hw[k]) for k in HW),target_trace().rstrip(),HEADER,
             "AI_NORM_PLAN 1 BIND " + " ".join(words[k] for k in COLS)]
    for i, a in enumerate(ranges):
        a = dict(a,bytes=sizes[i])
        lines.append("AI_NORM_RANGE 1 RANGE " + " ".join(str(a[k]) for k in RANGES))
    lines += [f"AI_NORM_EVENT 1 EDGE {i} {mask} 211" for i,mask in enumerate(edges)]
    return "\n".join(lines)+"\n"


class NormalizationPlanTests(unittest.TestCase):
    """Exercise conservative planning without interpreting it as kernel execution."""

    def test_three_plans(self):
        """Direct has no scratch; retained plans charge row plus distinct mu/q slots."""
        for kind in (1,2,3):
            data = fixture(kind);row = audit(serialize(*data),source_trace())[0]
            self.assertEqual((row["status"],row["obligations"]),("0","63"))
            self.assertEqual(row["required"],"0" if kind==1 else "88")
            self.assertEqual(row["reserved"],"0" if kind==1 else "256")
            self.assertEqual(row["conflict"],"0" if kind==1 else "1")

    def test_count_limit_fallback(self):
        """Large legal N retains direct storage while full-row plans fail budget."""
        source = source_trace(rows="1",width="16777216",count_bits="4b800000")
        for kind, wanted in ((1,0),(2,7),(3,7)):
            r,a,e,f,h = fixture(kind,source)
            self.assertEqual(evaluate(r,a,e,f,h,0)[0],wanted)

    def test_range_boundaries(self):
        """Reject wrap, weak/misaligned base, undersized backing and wrong leases."""
        for field,value in (("allocation",0),("capacity",203),("offset",2),("offset",2**64-4),
                            ("alignment",2),("alignment",3),("lease",2)):
            r,a,e,f,h = fixture();a[0][field]=value
            self.assertEqual(evaluate(r,a,e,f,h,0)[0],4)

    def test_alias_and_adjacent_ranges(self):
        """Read aliases are legal; output overlaps are not; adjacent subranges are."""
        r,a,e,f,h = fixture()
        a[1].update(allocation=a[0]["allocation"],capacity=a[0]["capacity"])
        self.assertEqual(evaluate(r,a,e,f,h,0)[0],0)
        a[3].update(allocation=a[0]["allocation"])
        self.assertEqual(evaluate(r,a,e,f,h,0)[0],5)
        a[0]["capacity"]=a[1]["capacity"]=a[3]["capacity"]=408
        a[3]["offset"]=204
        self.assertEqual(evaluate(r,a,e,f,h,0)[0],0)
        a[1]["alignment"]=32
        self.assertEqual(evaluate(r,a,e,f,h,0)[0],5)

    def test_shared_affine_identity(self):
        """A single value ID cannot acquire two distinct physical bindings."""
        r,a,e,f,h = fixture();f=dict(f,beta=f["gamma"])
        self.assertEqual(evaluate(r,a,e,f,h,0)[0],5)
        a[2]=dict(a[1],index=2)
        self.assertEqual(evaluate(r,a,e,f,h,0)[0],0)

    def test_event_reachability(self):
        """Each missing boundary, a cycle, self edge and out-of-domain bit refuse."""
        edges=fixture()[2]
        self.assertTrue(ordered(edges))
        for i in range(len(EVENTS)-1):
            bad=list(edges);bad[i]=0
            self.assertFalse(ordered(bad))
        for i,mask in ((0,1),(14,1),(0,1<<31)):
            bad=list(edges);bad[i]|=mask
            self.assertFalse(ordered(bad))
        extra=list(edges);extra[0]|=1<<3
        self.assertTrue(ordered(extra))

    def test_uniform_tail_barriers(self):
        """Inactive lanes cannot leave a retained plan's required CTA barrier."""
        for name,value,status in (("participants",127,8),("barriers",0,8),("tail",0,6),("threads",33,6),("ctas",4,6)):
            r,a,e,f,h=fixture();r[name]=value
            self.assertEqual(evaluate(r,a,e,f,h,0)[0],status)

    def test_capacity_and_overflow(self):
        """No free scalar slots, unrounded reservation or wrapped stride."""
        for name,value in (("stride",16),("stride",81),("stride",2**64-16),("shared",80)):
            r,a,e,f,h=fixture();r[name]=value
            self.assertEqual(evaluate(r,a,e,f,h,0)[0],7)
        r,a,e,f,h=fixture();h["budget"]=255
        self.assertEqual(evaluate(r,a,e,f,h,0)[0],7)
        h["budget"]=256
        self.assertEqual(evaluate(r,a,e,f,h,0)[0],0)

    def test_target_and_version_precedence(self):
        """V1, stale/unsafe target and invalid layout never gain plan obligations."""
        r,a,e,f,h=fixture()
        for target_status in (1,2,6):
            status,out,sizes=evaluate(r,a,e,f,h,target_status)
            self.assertEqual((status,out["obligations"],sizes),(1,0,[0]*4))
        for field,value,status in (("kind",4,2),("runtime",0,3),("context",0,3),("layout",0,9)):
            bad=dict(r);bad[field]=value
            self.assertEqual(evaluate(bad,a,e,f,h,0)[0],status)

    def test_trace_mutations(self):
        """Refuse missing/extra/schema/source/derived-proof drift and integer narrowing."""
        text=serialize(*fixture())
        for before,after in (("AI_NORM_PLAN 1 BIND","AI_NORM_PLAN 2 BIND"),("conditional_storage_events 211","conditional_storage_events 212"),
                             (" 31 63 "," 31 0 "),("AI_NORM_EVENT 1 EDGE 0 2 ","AI_NORM_EVENT 1 EDGE 0 2147483650 "),
                             (" 128 3 1 "," 4294967424 3 1 ")):
            with self.subTest(before=before),self.assertRaises(ValueError):
                audit(text.replace(before,after),source_trace())
        for bad in ("\n".join(text.splitlines()[:-1]),text+"unknown\n"):
            with self.assertRaises(ValueError):audit(bad,source_trace())

    def test_report_conventions(self):
        """Describe each log, retain open obligations, and never overwrite reports."""
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);(root/"plan.t").write_text(serialize(*fixture()));(root/"source.t").write_text(source_trace())
            write_reports(root/"plan.t",root/"source.t",root/"reports")
            for path in (root/"reports").glob("*.log"):self.assertTrue(path.read_text().startswith("# What it summarizes:"))
            summary=(root/"reports/optimization-summary.log").read_text()
            self.assertIn("TRANSFORM none",summary);self.assertIn("runtime_enforcement=unproved",summary)
            with self.assertRaises(FileExistsError):write_reports(root/"plan.t",root/"source.t",root/"reports")


if __name__ == "__main__":
    unittest.main()
