"""MR2 fixed-fixture review: estimation isolation and selected-only correctness.

All timings/traffic are modeled under synthetic assumptions. No GPU is used.
The retained previous MR1 receipt must authenticate before reuse.
"""
import argparse
import copy
import difflib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
from run_common_high_review import execute, check_output
from run_high_reuse_review import authenticate
from run_high_observer_review import audit_objects

def hierarchy():
    """Explicit scenario inputs; these are not inferred Hopper measurements."""
    return dict(schema='open64.mla-hierarchy-profile.v1',unit='reference_cycles',
        id='mr2-synthetic-cold-warm',architecture='sm_90',provenance='synthetic',objective='worst',
        capabilities='mla-materializers-v1:G1=verified-v8;G2,G4,G8=analysis-only',
        hbm_bytes_per_cycle=64,l2_bytes_per_cycle=256,sector_bytes=32,
        request_amplification_ppm=1000000,shared_conflict_ppm=1000000,
        scenarios=[dict(id='cold',weight=1,cache_l2_hit_ppm=0,other_l2_hit_ppm=0),
                   dict(id='warm',weight=1,cache_l2_hit_ppm=900000,other_l2_hit_ppm=0)])

def version3(value):
    """Keep old inputs intact; create a separately versioned full binding."""
    value=copy.deepcopy(value);value['schema']='open64.lno-high-search.v3';value['hierarchy']=hierarchy()
    c=value['search_controls'];c.update(schema='open64.mla-search-controls.v3',
        space='mla-movement-space-v1-36',global_proposal_limit=108,global_unique_limit=36,patience=36)
    for run in c['runs']:run['proposal_limit']=36
    return value

def review(a):
    authenticate(a.previous)
    out=a.output.resolve();out.mkdir(parents=True,exist_ok=False);(out/'logs').mkdir()
    env=dict(os.environ,LD_LIBRARY_PATH=':'.join(str(a.tools/p) for p in ('be','wopt','lno')))
    for name in ('OPEN64_HIGH_SEARCH_BINDINGS','OPEN64_HIGH_REUSE_TEST','OPEN64_HIGH_APPLY_TEST'):env.pop(name,None)
    commands=[]
    def run(label,argv,success=True,environment=None):
        p=subprocess.run(list(map(str,argv)),cwd=out,env=environment or env,capture_output=True,text=True,timeout=600)
        for stream in ('stdout','stderr'):(out/'logs'/(label+'.'+stream)).write_text(getattr(p,stream))
        commands.append(dict(label=label,argv=list(map(str,argv)),returncode=p.returncode,expected_success=success))
        (out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        assert (p.returncode==0)==success,(label,p.stderr[-3500:])
        return p
    legacy=out/'legacy'
    run('MR1-compatibility',[sys.executable,Path(__file__).with_name('run_high_reuse_review.py'),
        '--previous',a.previous/'baseline','--tools',a.tools,'--objects',a.objects,
        '--test-lno',a.test_lno,'--output',legacy])
    native=legacy/'native';reader=a.tools/'ir_tools/ir_b2a';w2c=a.tools/'whirl2c/whirl2c'
    source=Path(__file__).parents[3]/'torch2whirl/python/tests/models/absorbed_mla.py'
    def save(name,value):
        path=out/(name+'.json');path.write_text(json.dumps(value,indent=2)+'\n');return path
    def reopen(label,image):
        run(label,[reader,'-st','-src',image,image.with_suffix('.T')]);return image.with_suffix('.T').read_text()
    def backend(label,image,binding,apply=False,disabled=False,fault=None,checkpoint=False,device=False,level=3):
        prefix=out/label;lno=a.test_lno.parent if fault else a.tools/'lno'
        argv=[a.tools/'be/be','-fB,'+str(image),'-fo,'+str(prefix)+'.o','-ft,'+str(prefix)+'.phase.t',
            '-PHASE:l=on:w=off:c=off:lpath='+str(lno)+':wpath='+str(a.tools/'wopt'),'-O'+str(level),
            '-DSL:partition_axis=on:ai_opt_trace=on','-tr25','-tt25:1','-OPT:IEEE_arith=3:roundoff=2',
            '-LNO:high_search='+('off' if disabled else 'on')+':high_apply='+('on' if apply else 'off')+
            ':high_search_bindings='+str(binding)]
        if checkpoint:argv+=['-LNO:high_apply_checkpoint='+str(prefix)]
        if device:argv+=['-LNO:high_device_output='+str(out/(label+'-device'))+':high_device_tool='+str(a.tools/'ir_tools/tensor_selected_bridge')]
        local=dict(env)
        if fault:local['OPEN64_HIGH_APPLY_TEST']=str(fault)
        run(label,[*argv,source],environment=local)
        return prefix.with_suffix('.N'),prefix.with_suffix('.phase.t').read_text()
    report=dict(status='PASS',gpu_execution=False,contexts={},legacy_reference_executions=30,
        MR2_selected_reference_executions=14,object_isolation=audit_objects(a.objects,out/'logs'))
    for ctx in ('short','long'):
        image=native/(ctx+'.high.B');manifest=json.loads((native/(ctx+'.high.json')).read_text())
        bound=version3(json.loads((native/(ctx+'.binding.json')).read_text()));bp=save(ctx+'.v3',bound)
        observed,trace=backend(ctx+'-observe',image,bp)
        assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in trace
        assert 'AI_HIGH_MOVEMENT 1 SCOPE modeled=true measured=false' in trace
        rows=[]
        for line in trace.splitlines():
            if line.startswith('AI_HIGH_MOVEMENT 1 CANDIDATE '):
                fields,reason=line.rsplit(' reason=',1)
                row=dict(x.split('=',1) for x in fields.split()[3:]);row['reason']=reason;rows.append(row)
        assert len(rows)==36 and any(x['G']=='2' and x['modeled']=='true' for x in rows)
        assert all(x['eligible']=='false' for x in rows if x['G']!='1')
        winner=re.search(r'AI_HIGH_SEARCH 2 OBSERVED .*winner=(\d+)',trace)
        assert winner and int(winner[1])<12 and 'estimates=36' in trace
        ordinary,_=backend(ctx+'-disabled',image,bp,disabled=True)
        ordinary_text=reopen(ctx+'-disabled-reopen',ordinary)
        assert reopen(ctx+'-observe-reopen',observed)==ordinary_text
        selected,applied=backend(ctx+'-selected',image,bp,apply=True,checkpoint=True,device=True)
        assert 'AI_HIGH_APPLY 1 COMMITTED' in applied and 'candidates_materialized=1' in applied
        before=out/(ctx+'-selected.before.B');after=out/(ctx+'-selected.after.B')
        before_text=reopen(ctx+'-before-reopen',before);after_text=reopen(ctx+'-after-reopen',after)
        assert 'lno-selected-mla-v2' in after_text and 'mla-movement-selected-v1' in after_text
        assert 'MR2 selected an implemented G=1 plan' in after_text
        (out/(ctx+'-selected.diff')).write_text(''.join(difflib.unified_diff(before_text.splitlines(True),after_text.splitlines(True))))
        reopen(ctx+'-selected-reopen',selected)
        c=out/(ctx+'-selected.c')
        run(ctx+'-w2c',[w2c,'-TARG:abi=n64','-O0','-CLIST:dotc_file='+str(c),
            '-CLIST:doth_file='+str(c.with_suffix('.h')),'-ft,'+str(out/(ctx+'-w2c.phase.t')),selected])
        assert 'MR2 selected an implemented G=1 plan' in c.read_text()
        lib=out/(ctx+'-selected.so')
        run(ctx+'-cc',['gcc','-std=c99','-O2','-shared','-fPIC','-ffp-contract=off','-fno-fast-math',
            '-I'+str(a.tools/'include'),'-include',Path(__file__).with_name('common_high_host.h'),c,'-lm','-o',lib])
        checks={}
        fixtures=a.previous/'baseline/baseline/m3/fixtures/selected'/ctx
        for reference in sorted(fixtures.glob('*.reference.json')):
            binary=out/(ctx+'-'+reference.stem+'.outputs.bin')
            binary.write_bytes(execute(lib,manifest,reference.with_name(reference.name.replace('.reference.json','.inputs.bin')).read_bytes()))
            checks[reference.name]=check_output(reference,binary)
        assert len(checks)==7
        device=out/(ctx+'-selected-device/pu-0')
        assert (device/'selected.complete.json').is_file() and (device/'selected.ptx').is_file()
        for phase in ('high','mid','low'):
            text=reopen(ctx+'-'+phase+'-reopen',device/('selected.'+phase+'.B'))
            assert 'MR2 selected an implemented G=1 plan' in text
        assert any('MR2 selected an implemented G=1 plan' in p.read_text() for p in device.glob('*.cu'))
        negatives=[]
        mutations=[
            ('missing-rate',lambda x:x['hierarchy'].pop('hbm_bytes_per_cycle')),
            ('zero-rate',lambda x:x['hierarchy'].update(hbm_bytes_per_cycle=0)),
            ('capability-injection',lambda x:x['hierarchy'].update(capabilities='G8=verified')),
            ('hit-range',lambda x:x['hierarchy']['scenarios'][0].update(cache_l2_hit_ppm=1000001)),
            ('wrong-arch',lambda x:x['hierarchy'].update(architecture='sm_90a')),
            ('old-schema-extension',lambda x:x.update(schema='open64.lno-high-search.v2')),
            ('unproved-certificate',lambda x:x['search_controls']['stopping'].append('relative-gap'))]
        for name,change in mutations:
            bad=copy.deepcopy(bound);change(bad);path=save(ctx+'.bad-'+name,bad)
            rejected,rt=backend(ctx+'-bad-'+name,image,path,apply=True)
            assert 'fallback=ordinary_lno' in rt and 'AI_HIGH_APPLY 1 COMMITTED' not in rt
            assert reopen(ctx+'-'+name+'-reopen',rejected)==ordinary_text
            negatives.append(name)
        for fault in (19,20,21,22,3):
            rejected,rt=backend(ctx+'-fault-'+str(fault),image,bp,apply=True,fault=fault)
            assert 'fallback=ordinary_lno' in rt and 'AI_HIGH_APPLY 1 COMMITTED' not in rt
            assert reopen(ctx+'-fault-'+str(fault)+'-reopen',rejected)==ordinary_text
        changed=copy.deepcopy(bound);changed['hierarchy']['scenarios'][0]['cache_l2_hit_ppm']=1
        path=save(ctx+'.rebound-profile',changed)
        run(ctx+'-bridge-profile-rebind',[a.tools/'ir_tools/tensor_selected_bridge','bridge-production',after,path,out/(ctx+'-bad-bridge')],success=False)
        for fault in ('head_group','movement'):
            run(ctx+'-bridge-'+fault,[a.tools/'ir_tools/tensor_selected_bridge_test','bridge-production',after,bp,
                out/(ctx+'-bridge-'+fault),'--test-fault',fault],success=False)
        small=copy.deepcopy(bound);small['search_controls'].update(global_proposal_limit=1,global_unique_limit=1)
        _,limited=backend(ctx+'-one-budget',image,save(ctx+'.one-budget',small))
        assert 'proposals=1 estimates=1' in limited
        for level in (0,2):
            _,low=backend(ctx+'-O'+str(level),image,bp,level=level)
            assert 'AI_HIGH_MOVEMENT ' not in low
        report['contexts'][ctx]=dict(winner=int(winner[1]),candidates=rows,reference_checks=checks,
            isolation=True,read_only_T_equal=True,selected_bridge=True,refusals=negatives,
            native_stale_or_rollback_faults=[19,20,21,22,3],bridge_profile_and_metadata_refusals=3)
    small=legacy/'multi-store';v3=version3(json.loads((small/'binding.json').read_text()));bp=save('multi-store.v3',v3)
    _,trace=backend('multi-store-unproved',small/'high.B',bp,apply=True)
    assert 'movement_reuse_unproved' in trace and 'fallback=ordinary_lno' in trace
    assert 'AI_HIGH_MOVEMENT 1 CANDIDATE' not in trace
    report['missing_proof_fallback']='ordinary_lno; v1/v2 G1 lane unchanged'
    (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    print('PASS: MR2 hierarchy/reuse costing, analysis-only capability gate, native isolation, stale/rollback refusals; 44 host reference executions')

if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    for name in ('previous','tools','objects','test-lno','output'):p.add_argument('--'+name,type=Path,required=True)
    review(p.parse_args())
