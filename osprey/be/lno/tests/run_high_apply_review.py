"""M4 selected-only High application, legality, rollback and native acceptance.

Design: doc/AI-COMPILER-SEARCH-M4-ATOMIC-HIGH-v1.0.md. Uses retained actual High
images, complete v2 bindings, separate test LNO and native whirl2c output.
Candidate exploration never invokes this test harness.
"""
import argparse
import copy
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
from run_common_high_review import execute, check_output
from run_high_search_control_review import audit_result
from high_observer_image_parity import compare_images


def review(args):
    """Retain exact native invocations and independent mapped/referenced views."""
    out=args.output.resolve();out.mkdir(parents=True,exist_ok=False);(out/'logs').mkdir()
    commands=[];env=dict(os.environ);env.pop('OPEN64_HIGH_SEARCH_BINDINGS',None);env.pop('OPEN64_HIGH_APPLY_TEST',None)
    report=dict(status='PASS',contexts={},rollback={},refused={},variants={},gpu_execution=False)

    def run(label,argv,fault=None,lno=None):
        """Record each command before checking its status; never skip a failure."""
        local=dict(env,LD_LIBRARY_PATH=':'.join(map(str,(args.tools/'be',args.tools/'wopt',(lno or args.lno).parent))))
        if fault is not None:local['OPEN64_HIGH_APPLY_TEST']=str(fault)
        p=subprocess.run(list(map(str,argv)),cwd=out,env=local,capture_output=True,text=True,timeout=600)
        for name in ['stdout','stderr']:(out/'logs'/(label+'.'+name)).write_text(getattr(p,name))
        commands.append(dict(label=label,argv=list(map(str,argv)),fault=fault,returncode=p.returncode))
        (out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        assert p.returncode==0,(label,p.stderr[-4000:])
        assert 'ambiguous in group' not in p.stderr
        return p

    def backend(label,context,apply='on',binding=None,checkpoint=True,fault=None,lno=None,level=3,extra=(),tracing='on'):
        """One backend invocation per selected materialization, never per estimate."""
        library=lno or args.lno;base=out/label
        argv=[args.tools/'be/be','-fB,'+str(args.previous/'controls'/(context+'.input.B')),
            '-fo,'+str(base)+'.o','-ft,'+str(base)+'.phase.t',
            '-PHASE:l=on:w=off:c=off:lpath='+str(library.parent)+':wpath='+str(args.tools/'wopt'),
            '-O'+str(level),'-DSL:ai_opt_trace='+tracing,'-tr25','-tt25:1','-OPT:IEEE_arith=3:roundoff=2',
            '-LNO:high_search=on:high_search_bindings='+str(binding or args.previous/'controls'/(context+'.v2.json'))]
        if apply is not None:argv+=['-LNO:high_apply='+apply]
        if checkpoint:argv+=['-LNO:high_apply_checkpoint='+str(base)]
        run(label,[*argv,*extra,args.source],fault,library)
        return base.with_suffix('.N'),base.with_suffix('.phase.t').read_text()

    def reopen(label,image):
        """Map actual B/N images independently, with matching source/symbol stem."""
        run(label,[args.b2a,'-st','-src',image,image.with_suffix('.T')])

    def compile_view(label,image):
        """Generate readable executable host CUDA review source from native WHIRL."""
        c=out/(label+'.cu')
        run(label+'-w2c',[args.w2c,'-TARG:abi=n64','-O0','-CLIST:dotc_file='+str(c),
            '-CLIST:doth_file='+str(c.with_suffix('.h')),'-ft,'+str(out/(label+'.w2c.phase.t')),image])
        library=out/(label+'.so')
        run(label+'-cc',['gcc','-x','c','-std=c99','-O2','-shared','-fPIC','-ffp-contract=off','-fno-fast-math',
            '-I'+str(args.include),'-include',Path(__file__).with_name('common_high_host.h'),c,'-lm','-o',library])
        return library,c

    def references(label,context,library,all_cases=True):
        """Execute guarded ABI buffers, proving input/guard preservation and saved FP64 tolerances."""
        manifest=json.loads((args.previous/'fixtures/m2c/observer'/(context+'.high.json')).read_text())
        references=sorted((args.previous/'fixtures/selected'/context).glob('*.reference.json'))
        if not all_cases:references=[x for x in references if x.name in ['all-masked.reference.json','empty-first-chunk.reference.json','mixed.reference.json']]
        if not references:references=sorted((args.previous/'fixtures/selected'/context).glob('*.reference.json'))[:2]
        results={}
        for reference in references:
            variant=reference.name[:-len('.reference.json')]
            data=(reference.parent/(variant+'.inputs.bin')).read_bytes()
            binary=out/(label+'.'+variant+'.bin');binary.write_bytes(execute(library,manifest,data))
            results[variant]=check_output(reference,binary)
        assert results
        return results

    def parity(image,baseline,trace):
        """Compare all meaningful bytes; retain raw CG padding offsets and values."""
        abi={k:int(v) for k,v in re.findall(r'(\w+)=(\d+)',re.search(r'AI_HIGH_SEARCH 1 IMAGE_ABI (.*)',trace)[1])}
        return compare_images(image,baseline,abi)

    baselines={}
    for context in ['short','long']:
        base,base_trace=backend(context+'-estimate',context,apply=None,checkpoint=False)
        assert 'AI_HIGH_APPLY' not in base_trace;baselines[context]=(base,base_trace)
        reopen(context+'-estimate',base)
        controlled= audit_result(base_trace,json.loads((args.previous/'controls'/(context+'.v2.json')).read_text())['search_controls'])
        normal,trace=backend(context+'-selected',context)
        assert 'COMMITTED ' in trace and 'legacy_LNO=bypassed applied=true' in trace
        assert 'INVALIDATION ' not in trace.split('AI_HIGH_APPLY 1 COMMITTED',1)[1]
        assert trace.count('AI_HIGH_APPLY 1 COMMITTED')==1 and 'attempts=1 candidates_materialized=1' in trace
        parsed=audit_result(trace,json.loads((args.previous/'controls'/(context+'.v2.json')).read_text())['search_controls'])
        assert parsed['events']==controlled['events'] and parsed['predicates']==controlled['predicates']
        before=out/(context+'-selected.before.B');after=out/(context+'-selected.after.B')
        for boundary in [before,after,normal]:reopen(context+'-'+boundary.name,boundary)
        difference=''.join(difflib.unified_diff(before.with_suffix('.T').read_text().splitlines(True),after.with_suffix('.T').read_text().splitlines(True)))
        (out/(context+'.before-after.diff')).write_text(difference)
        assert difference and 'DO_LOOP' in difference and 'ARRAY' in difference and 'Selected High MLA' in difference
        assert 'tensor.high.selected.schema' in after.with_suffix('.T').read_text()
        pre_lib,_=compile_view(context+'-before',before)
        post_lib,source=compile_view(context+'-after',after)
        output_lib,_=compile_view(context+'-output',normal)
        assert 'Selected High MLA' in source.read_text() and 'No global score/probability vectors' in source.read_text()
        checks={key:references(context+'-'+key,context,lib) for key,lib in [('before',pre_lib),('after',post_lib),('output',output_lib)]}
        repeated,repeat=backend(context+'-repeat',context,checkpoint=False)
        assert 'COMMITTED ' in repeat
        reopen(context+'-repeat',repeated)
        assert normal.with_suffix('.T').read_text()==repeated.with_suffix('.T').read_text()
        # Environment fault requests cannot affect the production build.
        ignored,ignored_trace=backend(context+'-production-fault-ignored',context,checkpoint=False,fault=3)
        assert 'COMMITTED ' in ignored_trace
        quiet,quiet_trace=backend(context+'-quiet',context,checkpoint=False,tracing='off')
        assert 'AI_HIGH_APPLY' not in quiet_trace
        reopen(context+'-quiet',quiet)
        assert quiet.with_suffix('.T').read_text()==normal.with_suffix('.T').read_text()
        report['contexts'][context]=dict(search=parsed['summary'],selected_once=True,analyses_rebuilt=True,
            comments_retained=True,reference_checks=checks,post_LNO_host_execution='PASS',production_fault_hook_absent=True)
        for mode in ['off',None,'invalid']:
            image,text=backend(context+'-apply-'+str(mode),context,apply=mode,checkpoint=False)
            if mode=='invalid':assert 'REFUSED reason=application_controls attempts=0' in text
            report['refused'][context+'-'+str(mode)]=parity(image,base,text)
        for fault in [1,2,3,4]:
            image,text=backend(context+'-rollback-'+str(fault),context,lno=args.test_lno,checkpoint=False,fault=fault)
            assert 'ROLLED_BACK ' in text and 'source_equal=true ST_TY_restored=true analyses_equal=true' in text
            report['rollback'][context+'-'+str(fault)]=parity(image,base,text)
            reopen(context+'-rollback-'+str(fault),image)
            assert image.with_suffix('.T').read_text()==base.with_suffix('.T').read_text()
        # Existing valid checkpoint artifacts must never be replaced. A late
        # after-checkpoint refusal must restore the same native fallback.
        before_collision=out/(context+'-before-collision.before.B');shutil.copy2(before,before_collision)
        old=before_collision.read_bytes()
        image,text=backend(context+'-before-collision',context)
        assert 'REFUSED reason=before_checkpoint attempts=0' in text and before_collision.read_bytes()==old
        report['refused'][context+'-before-collision']=parity(image,base,text)
        after_collision=out/(context+'-after-collision.after.B');shutil.copy2(after,after_collision)
        old=after_collision.read_bytes()
        image,text=backend(context+'-after-collision',context)
        assert 'ROLLED_BACK reason=after_checkpoint' in text and after_collision.read_bytes()==old
        report['rollback'][context+'-after-collision']=parity(image,base,text)
        for fault in [10,11,12,13,14,15,16,17,18]:
            image,text=backend(context+'-stale-'+str(fault),context,lno=args.test_lno,checkpoint=False,fault=fault)
            assert 'REFUSED ' in text and 'attempts=0' in text and 'COMMITTED ' not in text
            report['refused'][context+'-stale-'+str(fault)]=parity(image,base,text)
        for level in [0,1,2]:
            enabled,text=backend(context+'-O'+str(level)+'-on',context,level=level,checkpoint=False)
            disabled,_=backend(context+'-O'+str(level)+'-off',context,apply='off',level=level,checkpoint=False)
            assert 'AI_HIGH_APPLY' not in text and enabled.read_bytes()==disabled.read_bytes()
        disabled,text=backend(context+'-lno-off',context,checkpoint=False,extra=['-LNO:opt=0'])
        assert 'AI_HIGH_APPLY' not in text
    # First-proposal random seeds exercise fixed narrow/family plans through the
    # same estimator. This does not inject a winner or lower trial candidates.
    for ordinal in [1,2,9,10,11]:
        seeds=[]
        for candidate_seed in range(1000):
            state=(candidate_seed*1664525+1013904223)&0xffffffff
            state=(state*1664525+1013904223)&0xffffffff
            if state%12==ordinal:seeds.append(candidate_seed)
        assert seeds
        seed=seeds[0]
        value=json.loads((args.previous/'controls/long.v2.json').read_text())
        value['search_controls']['runs']=[dict(id='single',strategy='random',seed=seed,proposal_limit=1)]
        value['search_controls']['global_proposal_limit']=1
        path=out/('variant-'+str(ordinal)+'.json');path.write_text(json.dumps(value,indent=2)+'\n')
        image,text=backend('variant-'+str(ordinal),'long',binding=path)
        assert re.search(r'COMMITTED winner='+str(ordinal)+r' ',text),text[-1000:]
        selected=out/('variant-'+str(ordinal)+'.after.B');reopen('variant-'+str(ordinal),selected)
        library,_=compile_view('variant-'+str(ordinal),selected)
        report['variants'][str(ordinal)]=references('variant-'+str(ordinal),'long',library,False)
    value=json.loads((args.previous/'controls/long.v2.json').read_text())
    value['search_controls']['runs']=[dict(id='direct',strategy='exhaustive',seed=17,proposal_limit=1)]
    value['search_controls']['global_proposal_limit']=1
    path=out/'no-incumbent.json';path.write_text(json.dumps(value,indent=2)+'\n')
    image,text=backend('no-incumbent','long',binding=path,checkpoint=False)
    assert 'REFUSED reason=no_selected_incumbent attempts=0' in text
    report['refused']['no-incumbent']=parity(image,baselines['long'][0],text)
    report['reference_executions']=sum(len(v) for c in report['contexts'].values() for v in c['reference_checks'].values())+sum(len(v) for v in report['variants'].values())
    report['rollback_cases']=len(report['rollback']);report['refusal_cases']=len(report['refused'])
    (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    (out/'SHA256SUMS').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+str(p.relative_to(out))+'\n' for p in sorted(out.rglob('*')) if p.is_file()))
    print(json.dumps(dict(status='PASS',references=report['reference_executions'],rollback=report['rollback_cases'],
        refusals=report['refusal_cases'],fixed_variants=len(report['variants']),gpu_execution=False),indent=2))


def main():
    """Require every source/tool/evidence dependency explicitly for replay."""
    parser=argparse.ArgumentParser(description=__doc__)
    for name in ['tools','lno','test-lno','b2a','w2c','include','previous','source','output']:
        parser.add_argument('--'+name,type=Path,required=True)
    review(parser.parse_args())


if __name__=='__main__':
    main()
