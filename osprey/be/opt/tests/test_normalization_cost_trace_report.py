"""Synthetic LN3 cost/oracle/schema tests; native evidence is a separate suite.
Design: doc/AI-COMPILER-LAYERNORM-PROFITABILITY-v0.1.md.
"""
import random
import unittest

from normalization_cost_oracle import FIELDS,recipe,evaluate,choose,residents,profile_status
from normalization_cost_trace_report import RESOURCE,MODEL,COST,COST_HEADER,SELECT,SCENARIOS,expected_cost,audit
from normalization_guard_trace_report import FIELDS as GUARD_FIELDS
from test_normalization_guard_trace_report import Image,export_fixture,image_trace
from test_normalization_plan_trace_report import fixture,serialize,source_trace


def synthetic():
    """Complete synthetic comparison transcript; never a native producer claim."""
    texts=[serialize(*fixture(kind)) for kind in (1,2,3)]
    plan=texts[0]+''.join('\n'.join(t.splitlines()[1:])+'\n' for t in texts[1:])
    from normalization_plan_trace_report import audit as plans
    ps=plans(plan,source_trace());images=[];guards={}
    for kind in (1,2,3):
        g,prefix,_,_=export_fixture(kind);_,image=image_trace(g,prefix);images.append(image);guards[kind]=g
    hw=dict(zip(RESOURCE,[1,2,4,32,64,2048,1024,65536,255,256,4,233472,65536,128,1024,211]))
    lines=[]
    base=[32,128,128,100,2,120,4,4,32,32,1,8,16,32,0,8,32,16,16]
    for i,name in enumerate(SCENARIOS):
        model=dict(version=1,arch=1,revision=2,model=1 if i==0 else 1000+i,regs0=32,regs1=32,regs2=32,max_jobs=500000,margin=0,source_line=211)
        values=[[v]*3 for v in base];provenance=[3]*19
        if i==0:values=[[0]*3 for _ in FIELDS];provenance=[0]*19
        if i==2:values[3]=[100000]*3;values[5]=[2]*3
        if i==3:values[3]=[1]*3;values[5]=[100000]*3
        if i==4:
            for f in range(16):values[f]=[1 if f<3 else 0,values[f][1],100000]
        if i==5:values[8]=[0]*3;provenance[8]=0
        if i==6:model['revision']=3
        if i==7:model['max_jobs']=1
        if i==8:model['regs1']=0
        if i==9:model['regs1']=256
        if i==10:values[3]=[2**64-1]*3
        if i==11:model['margin']=2**64-1
        if i==12:
            values=[[1000000 if f<3 else 32 if f==16 else 0]*3 for f in range(19)];values[3]=[3]*3;values[11]=[1]*3
        if i==13:model['regs1']=model['regs2']=200
        lines += [f'AI_NORM_EXPERIMENT 1 CASE {name} 211','AI_NORM_RESOURCE 1 BIND '+' '.join(str(hw[k]) for k in RESOURCE),
                  'AI_NORM_RESOURCE 1 ORIGIN sm90_estimated_v2 reference_cycles estimated 211',
                  'AI_NORM_MODEL 1 HEADER '+' '.join(MODEL),'AI_NORM_MODEL 1 BIND '+' '.join(str(model[k]) for k in MODEL)]
        lines += [f'AI_NORM_MODEL 1 FIELD {f} {provenance[f]} '+' '.join(map(str,values[f]))+' 211' for f in range(19)]
        costs=[expected_cost(p,hw,model,values,provenance) for p in ps]
        nominal,selected,fallback,reason=choose(costs,model['margin']);next_id=0
        for c in costs:
            if selected and c['status']==0:next_id+=1;c['candidate_id']=c['plan_id']=next_id
        lines.append(COST_HEADER)
        lines += ['AI_NORM_COST 1 COST '+' '.join(str(c[k]) for k in COST) for c in costs]
        choice=dict(pu=11,node=7,result=5,nominal=nominal,selected=selected,fallback=fallback,reason=reason,
                    common_plan=costs[selected-1]['plan_id'] if selected else 0,analysis_only=1,source_line=211)
        lines += ['AI_NORM_SELECT 1 HEADER '+' '.join(SELECT),'AI_NORM_SELECT 1 SELECT '+' '.join(str(choice[k]) for k in SELECT)]
        if selected:lines.append('AI_NORM_CHOICE_GUARD 1 EXPORT 83 '+' '.join(str(guards[selected][k]) for k in GUARD_FIELDS)+' 211')
    return '\n'.join(lines)+'\n',plan,source_trace(),images


class NormalizationCostTests(unittest.TestCase):
    """Test semantic accounting, resource limits, interval policy and strict joins."""
    def test_complete_comparison(self):
        """C1/C2/C0 nominal outcomes and conservative fallback are distinct."""
        rows=audit(*synthetic());by={r['experiment']:r['selection'] for r in rows}
        self.assertEqual((by['balanced']['nominal'],by['balanced']['selected']),(2,1))
        self.assertEqual((by['global_slow']['nominal'],by['global_slow']['selected']),(3,3))
        self.assertEqual(by['shared_slow']['selected'],1)
        for name in ('builtin','missing','wrong_revision','budget','rf_unknown','overflow'):self.assertEqual(by[name]['selected'],0)

    def test_traffic_arithmetic_and_barriers(self):
        """Exact logical bytes, fold/epilogue operations and synchronization counts."""
        for kind in (1,2,3):
            jobs,c=recipe(3,17,kind,128,32,4,16,32,80 if kind>1 else 0,(0,)*4,(16,)*4)
            self.assertEqual(c['global_logical'],(24,20,16)[kind-1]*3*17)
            self.assertEqual(c['scalar_lanes'],((8 if kind==1 else 7)*17+5)*3)
            self.assertEqual(c['barriers'],(0,4,5)[kind-1]*3)
            self.assertEqual(c['shared'],0 if kind==1 else (12 if kind==2 else 24)*3*17+16*3)
            self.assertGreater(c['carrier_sectors'],0);self.assertGreater(len(jobs),c['scalar_insts'])

    def test_bounds_and_reconciliation(self):
        """Random explicit envelopes reconcile, even when instruction timing changes."""
        rng=random.Random(103)
        for kind in (1,2,3):
            for _ in range(8):
                values=[]
                for f in range(19):
                    mid=rng.randint(1,20);values.append((1,mid,100) if f<3 else (0,mid,100))
                values[16]=(32,32,32)
                shape=(2,7,kind,64,32,2,1,32,32 if kind>1 else 0,(0,)*4,(16,)*4)
                r=evaluate(shape,tuple(values),0 if kind==1 else 1)
                self.assertLessEqual(r['lower'],r['total']);self.assertLessEqual(r['total'],r['upper'])
                self.assertEqual(sum(r['terms']),r['total']);self.assertEqual(r['terms'][2],0)

    def test_profile_missing_and_invalid(self):
        """Every field is required; known zero duration is not an unknown field."""
        values=[(1,1,1)]*19;values[16]=(32,32,32);origin=[3]*19;hw=dict(arch=1,revision=2)
        self.assertEqual(profile_status(values,origin,(1,1,2,1),hw),0)
        for f in range(19):
            v=list(values);o=list(origin);v[f]=(0,0,0);o[f]=0
            self.assertEqual(profile_status(v,o,(1,1,2,1),hw),2)
            o[f]=4;self.assertEqual(profile_status(v,o,(1,1,2,1),hw),3)
        self.assertEqual(profile_status(values,origin,(1,1,3,1),hw),3)

    def test_waves_and_resource_pressure(self):
        """Finite SM/CTA slots add actual row-completion dependencies."""
        hw=dict(max_regs=255,reg_quantum=256,reg_warps=4,blocks=32,threads=2048,warps=64,registers=65536,
                shared_cap=233472,shared_budget=65536,shared_quantum=128,shared_reserved=1024)
        self.assertEqual(residents(hw,128,32,256,32),16);self.assertEqual(residents(hw,128,32,256,200),2)
        self.assertEqual(residents(hw,128,32,256,0),0);self.assertEqual(residents(hw,128,32,256,256),0)
        self.assertEqual(residents(hw,128,32,4096,32),12)
        a=recipe(9,1,1,128,32,4,16,32,0,(0,)*4,(16,)*4)[0]
        b=recipe(9,1,1,128,32,4,1,32,0,(0,)*4,(16,)*4)[0]
        self.assertNotEqual(a,b)

    def test_ties_uncertainty_and_overflow_margin(self):
        """Ties favor C0; unknown is not high cost; upper+margin never wraps."""
        costs=[dict(plan_status=0,status=0,total=100,upper=100,lower=100) for _ in range(3)]
        self.assertEqual(choose(costs,0),(1,1,1,2))
        costs[1].update(total=50,upper=100);self.assertEqual(choose(costs,0),(2,1,1,3))
        costs[1]['upper']=99;self.assertEqual(choose(costs,0),(2,2,1,4))
        self.assertEqual(choose(costs,2**64-1),(2,1,1,3))
        costs[1]['upper']=150;costs[2].update(total=75,upper=90)
        self.assertEqual(choose(costs,0),(2,3,1,4))
        costs[1]['status']=2;self.assertEqual(choose(costs,0),(0,0,1,1))

    def test_trace_tampering(self):
        """Source/schema/cost/components/guard/selection drift and missing cases fail."""
        text,plan,source,images=synthetic()
        for old,new in (('AI_NORM_COST 1 COST','AI_NORM_COST 2 COST'),('AI_NORM_RESOURCE 1 BIND 1 2 4','AI_NORM_RESOURCE 1 BIND 2 2 4'),
                        ('AI_NORM_SELECT 1 SELECT 11 7 5 2 1 1 3','AI_NORM_SELECT 1 SELECT 11 7 5 2 2 1 4'),
                        ('AI_NORM_CHOICE_GUARD 1 EXPORT 83','AI_NORM_CHOICE_GUARD 1 EXPORT 84'),('FIELD 18 3 16 16 16','FIELD 18 3 16 17 16')):
            with self.subTest(old=old),self.assertRaises(ValueError):audit(text.replace(old,new),plan,source,images)
        for bad in (text+text,'\n'.join(text.splitlines()[:-1]),text.replace('CASE balanced','CASE not_a_case')):
            with self.assertRaises(ValueError):audit(bad,plan,source,images)


if __name__=='__main__':unittest.main()
