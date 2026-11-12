"""Native LN4 mapped/trace/CUDA/offline-assembler and host numerical review.

Design: doc/AI-COMPILER-LAYERNORM-EXECUTABLE-LOWERING-v0.1.md.
No GPU is required or claimed. Retain all generated CUDA, PTX and cubins.
"""
import argparse
from collections import Counter
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess

from normalization_lower_trace_report import audit, comparison, write_reports
from normalization_lower_host import adapter, datasets, protocol, audit_output
from normalization_cost_trace_report import audit as cost_audit, SCENARIOS
from normalization_plan_trace_report import require
from run_normalization_cost_review import verify_receipt

PRODUCTS=('ir_tools/dsl_normalization_lower_test','ir_tools/dsl_normalization_plan_test','ir_tools/dsl_normalization_guard_test',
          'ir_tools/ir_b2a','be/be','be/be.so','wopt/wopt.so','whirl2c/whirl2c','whirl2c/whirl2c_be','whirl2c/whirl2c.so','whirl2c/be.so')
CASES=(('single-row',1),('single-row',2),('single-row',3),('feature-tail',2),('default',2),('subnormal',2),
       ('shared-affine',2),('legacy',1),('single-row',0),('single-row',5),('single-row',8),('unit',1),
       ('many-rows',2),('epsilon2',2),('wide',2))


def finish_reports(root):
    """Join all successful cost/lowering/compile/numerical phases, without rerunning native producers."""
    from normalization_cost_trace_report import write_reports as cost_reports
    result=root/'reports-final';result.mkdir(exist_ok=False);records=json.loads((root/'audit.json').read_text())['records']
    for record in records:
        case=root/record['case'];text=(case/'lower.t').read_text();r=audit(text)
        cost_trace=result/(record['case']+'.cost.t');cost_trace.write_text(comparison(text))
        experiment=text.splitlines()[0].split()[3];key=tuple(r['selection'][k] for k in ('pu','node','result'))
        temporary=result/(record['case']+'-cost')
        cost_reports(cost_trace,case/'plan/plan.t',case/'preopt-on.t',case/'guards',temporary,
                     scenarios=(experiment,),source_keys=(key,))
        reports=result/record['case'];shutil.copytree(case/'reports',reports)
        for name in ('physical-plan.log','bubble.log','roofline.log'):shutil.copy2(temporary/name,reports/name)
        phase_names=('physical-plan.log','bubble.log','roofline.log','lowering.log','compile.log','numerical.log')
        (reports/'optimization.log').write_text('# What it summarizes: full successful LN3 cost and LN4 transformation/compile/numerical phase collection.\n'+
            ''.join((reports/name).read_text() for name in phase_names if (reports/name).exists()))
        with (reports/'optimization-summary.log').open('a') as f:f.write('costs=independently_recomputed physical_plan_and_exposure_reports=retained\n')
    (result/'optimization.log').write_text('# What it summarizes: full successful cost, selection, actual device transformation, offline compile and numerical phase collection.\n'+
        ''.join((result/r['case']/'optimization.log').read_text() for r in records))
    shutil.copy2(root/'optimization-summary.log',result/'optimization-summary.log')
    return result


def review(build,source,cuda,parent,root,probe=False):
    """Fresh native production, independent source/cost audit and actual emission."""
    verify_receipt(parent);root.mkdir(parents=True,exist_ok=False);commands=[];records=[]
    binaries=root/'binaries'
    for relative in PRODUCTS:
        target=binaries/relative;target.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(build/relative,target)
    env=dict(os.environ,LD_LIBRARY_PATH=str(binaries/'be')+':'+str(binaries/'wopt'))

    def run(case,label,args,success=True,input_text=None):
        """Keep exact vectors, inputs and streams before checking the outcome."""
        args=list(map(str,args));p=subprocess.run(args,cwd=case,env=env,input=input_text,text=True,capture_output=True)
        log=case/'logs';log.mkdir(exist_ok=True)
        for name in ('stdout','stderr'):(log/(label+'.'+name)).write_text(getattr(p,name))
        if input_text is not None:(log/(label+'.stdin')).write_text(input_text)
        commands.append(dict(argv=args,cwd=str(case),label=label,returncode=p.returncode))
        (root/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        require((p.returncode==0)==success,label+': '+p.stderr[-3000:]);return p

    run(root,'nvcc-version',[cuda/'bin/nvcc','--version'])
    for arch in (('sm_90',) if probe else ('sm_90','sm_90a','sm_100','sm_100a')):
        cases=CASES[:3] if probe else CASES+((('single-row',6),('single-row',7),('single-row',10),('single-row',11),('count-limit',7)) if arch=='sm_90' else ())
        for variant,scenario in cases:
            name=arch+'-'+variant+'-'+SCENARIOS[scenario];case=root/name;case.mkdir();base=case/'kernel'
            source_variant='mixed' if variant=='legacy' else variant
            run(case,'native',[binaries/'ir_tools/dsl_normalization_lower_test',case/'source.B',base,case/'lower.t',arch,variant,scenario])
            require((case/'source.B').read_bytes()==(case/'kernel.source.after.B').read_bytes(),'source rewrite during materialization')
            old=parent/'native'/(arch+'-'+source_variant)/'ln_ref.B'
            if old.exists():require(old.read_bytes()==(case/'source.B').read_bytes(),'earlier v1/v2 mapped image drift')
            for producer,folder,trace in (('dsl_normalization_plan_test','plan','plan.t'),('dsl_normalization_guard_test','guards','guards.t')):
                target=case/folder;target.mkdir()
                run(case,folder,[binaries/'ir_tools'/producer,target/'ln_ref.B',target/trace,arch,source_variant])
                require((target/'ln_ref.B').read_bytes()==(case/'source.B').read_bytes(),'source evidence join drift')
            for stem in ('source','kernel.source.after'):
                run(case,'dump-'+stem,[binaries/'ir_tools/ir_b2a','-st','-src',case/(stem+'.B'),case/(stem+'.T')])
            require((case/'source.T').read_bytes()==(case/'kernel.source.after.T').read_bytes(),'full source dump changed')
            for label,enabled,tracing in (('preopt-on','on','on'),('default-trace','on',None),('trace-off','on','off'),('preopt-off','off','on')):
                args=[binaries/'be/be','-fB,'+str(case/'source.B'),'-fo,'+str(case/(label+'.obj')),'-ft,'+str(case/(label+'.t')),
                      '-PHASE:w=on:c=off:wpath='+str(binaries/'wopt'),'-O2','-DSL:wopt='+enabled,'-WOPT:cr_simp=on','-tr25']
                if tracing is not None:args+=['-DSL:ai_opt_trace='+tracing]
                p=run(case,label,args+[source/'osprey/be/opt/tests/dsl_normalization_ref_test.cxx'],False)
                require('OPR_DSLMEANVARNORM.v2 has no executable VHO lowering route' in p.stderr,'unexpected default-driver route')
                require(not (case/(label+'.obj')).exists(),'default driver silently published normalization')
                if label in ('trace-off','preopt-off'):require('AI_NORM' not in (case/(label+'.t')).read_text(),'trace control ignored')
            from normalization_ref_trace_report import audit as source_audit
            require(source_audit((case/'preopt-on.t').read_text())==source_audit((case/'default-trace.t').read_text()),'default PREOPT semantic drift')
            text=(case/'lower.t').read_text();r=audit(text)
            key=tuple(r['selection'][k] for k in ('pu','node','result'))
            cost_audit(comparison(text),(case/'plan/plan.t').read_text(),(case/'preopt-on.t').read_text(),
                       list((case/'guards').glob('*.guard')),scenarios=(SCENARIOS[scenario],),source_keys=(key,))
            record=dict(case=name,selected=r['selection']['selected'],materialized=r['materialized'],prior_source_preserved=old.exists())
            if r['materialized']:
                run(case,'reopen',[binaries/'ir_tools/dsl_normalization_lower_test','reopen',case/'kernel.device.after.B',case/'kernel.plan',case/'kernel.product',case/'reopen.t'])
                quiet=case/'quiet';quiet.mkdir()
                run(case,'quiet-materialize',[binaries/'ir_tools/dsl_normalization_lower_test','device-quiet',quiet/'kernel',case/'kernel.plan',quiet/'quiet.t'])
                require(not (quiet/'quiet.t').read_bytes(),'quiet materialization emitted actions')
                for suffix in ('device.before.B','device.after.B','product'):
                    require((case/('kernel.'+suffix)).read_bytes()==(quiet/('kernel.'+suffix)).read_bytes(),'quiet device image/product drift')
                for boundary in ('before','after'):
                    stem='kernel.device.'+boundary
                    run(case,'dump-'+stem,[binaries/'ir_tools/ir_b2a','-st','-src',case/(stem+'.B'),case/(stem+'.T')])
                a=(case/'kernel.device.before.T').read_text();b=(case/'kernel.device.after.T').read_text()
                (case/'kernel.before-after.diff').write_text(''.join(difflib.unified_diff(a.splitlines(True),b.splitlines(True),fromfile='before.T',tofile='after.T')))
                run(case,'whirl2c',[binaries/'whirl2c/whirl2c','-cuda','-CLIST:dotc_file='+str(case/'kernel.cu'),'-fB,'+str(case/'kernel.device.after.B'),'kernel-source.cu'])
                emitted=(case/'kernel.cu').read_text();r=write_reports(text,(case/'reopen.t').read_text(),emitted,case/'reports')
                flags=['-std=c++11','-arch='+arch,'--fmad=false','--ftz=false','--prec-div=true','--prec-sqrt=true']
                run(case,'ptx',[cuda/'bin/nvcc']+flags+['-ptx','kernel.cu','-o','kernel.ptx'])
                ptx=(case/'kernel.ptx').read_text()
                require(all(op+'.rn.f32' in ptx for op in ('add','mul','div','sqrt')),'target instruction rounding lost')
                require('fma.rn.f32' not in ptx and 'rsqrt.approx' not in ptx,'unauthorized target arithmetic')
                result=run(case,'cubin',[cuda/'bin/nvcc']+flags+['-cubin','kernel.cu','-o','kernel.cubin','-Xptxas=-v'])
                regs=re.search(r'Used (\d+) registers',result.stderr);require(regs is not None,'missing compiled register evidence')
                shared=re.search(r'(\d+) bytes smem',result.stderr);smem=int(shared[1]) if shared else 0
                require(smem==r['guard']['reserved_shared'],'compiled shared reservation differs from plan')
                model=next(s.split() for s in text.splitlines() if s.startswith('AI_NORM_MODEL 1 BIND '))
                estimate=int(model[6+r['selection']['selected']]);actual=int(regs[1])
                require(actual<=estimate,'compiled registers exceed estimate; recost required')
                (case/'entry-manifest.json').write_text(json.dumps(dict(schema='LN4-review-entry-v1',
                    kernel='ln_ref_c'+str(r['selection']['selected']-1),architecture=arch,
                    arguments=['X_subrange','gamma_subrange','beta_subrange','Y_subrange'],
                    grid=[r['guard']['ctas'],1,1],block=[r['guard']['threads'],1,1],dynamic_shared=0,
                    static_shared=smem,compiled_registers=actual,register_estimate=estimate,guard=r['guard'],
                    authoritative_image_sha256=hashlib.sha256((case/'kernel.device.after.B').read_bytes()).hexdigest(),
                    constructed_tree_seal=r['facts'][4],runtime_leases_required=True,production_provider=False,GPU=False),indent=2,sort_keys=True)+'\n')
                (case/'host.cxx').write_text(adapter(emitted,r));sets=datasets(r)
                run(case,'host-build',['g++','-std=c++11','-O2','-Wall','-Wextra','-fno-fast-math','-ffp-contract=off','-frounding-math','host.cxx','-o','host'])
                host=run(case,'host-execute',[case/'host'],input_text=protocol(r,sets));checks=audit_output(r,sets,host.stdout)
                compile_log=('# What it summarizes: actual WHIRL-derived CUDA/PTX/cubin acceptance and compiled resources, not GPU execution.\n'+
                    f'arch={arch} flags={" ".join(flags)} registers={actual} estimate={estimate} shared={smem} resource_gate=passed GPU=not_run\n')
                numerical=('# What it summarizes: actual emitted control/access code under explicit host scalar/CTA substitutions versus exact-rational LN-REF-1.\n'+
                    f'cases={len(sets)} output_checks={checks} inputs_and_output_canaries=preserved host_adapter=phase_serial GPU=not_run\n')
                (case/'reports/compile.log').write_text(compile_log);(case/'reports/numerical.log').write_text(numerical)
                with (case/'reports/optimization.log').open('a') as f:f.write(compile_log+numerical)
                record.update(host_cases=len(sets),output_checks=checks,registers=actual,register_estimate=estimate,shared=smem)
            else:
                require(not (case/'kernel.device.after.B').exists(),'refused plan has device image')
                write_reports(text,None,None,case/'reports')
            records.append(record);(root/'progress.json').write_text(json.dumps(records,indent=2)+'\n')
    selected=Counter(r['selected'] for r in records)
    require(set(selected)>={1,2,3},'C0/C1/C2 executable coverage missing')
    summary=dict(cases=len(records),selected=dict(selected),materialized=sum(r['materialized'] for r in records),
        output_checks=sum(r.get('output_checks',0) for r in records),host_cases=sum(r.get('host_cases',0) for r in records),
        full_mapped_dumps=2*len(records)+2*sum(r['materialized'] for r in records),PREOPT_controls=4*len(records),
        source_v1_preserved=True,reduction_dependency=False,default_driver_dispatch=False,GPU=False,records=records)
    (root/'audit.json').write_text(json.dumps(summary,indent=2,sort_keys=True)+'\n')
    (root/'optimization.log').write_text('# What it summarizes: full successful native LN4 lowering, compile and numerical report collection.\n'+
        ''.join((root/r['case']/'reports/optimization.log').read_text() for r in records))
    (root/'optimization-summary.log').write_text('# What it summarizes: complete LN4 selected/refused, actual device materialization, offline compile and host numerical census with remaining gates.\n'+
        ' '.join(k+'='+str(v) for k,v in summary.items() if k!='records')+'\n'+
        'WHIRL=authoritative v1=preserved runtime_leases=required production_CUDA_adapter=missing GPU=not_run calibration=open\n')
    finish_reports(root)
    return summary


def main():
    """Explicit paths and fresh outputs make native evidence replayable."""
    p=argparse.ArgumentParser(description=__doc__)
    for name in ('build','source','cuda','parent','root'):p.add_argument('--'+name,type=Path,required=True)
    p.add_argument('--probe',action='store_true');a=p.parse_args()
    result=review(*(getattr(a,k).resolve() for k in ('build','source','cuda','parent','root')),probe=a.probe)
    print(json.dumps({k:v for k,v in result.items() if k!='records'},sort_keys=True))


if __name__=='__main__':main()
