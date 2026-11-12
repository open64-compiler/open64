#!/usr/bin/env python3
"""LN3 native complete-cost/selection and unchanged mapped-source review.
Design: doc/AI-COMPILER-LAYERNORM-PROFITABILITY-v0.1.md.
"""
import argparse
from collections import Counter
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

from run_normalization_plan_review import ARCHES,VARIANTS,sha256
from normalization_cost_trace_report import write_reports,SCENARIOS
from normalization_plan_trace_report import require
from normalization_ref_trace_report import audit as source_audit
from normalization_trace_report import audit as legacy_audit

SOURCES=VARIANTS+('single-row','many-rows')
PRODUCTS=('ir_tools/dsl_normalization_cost_test','ir_tools/dsl_normalization_plan_test','ir_tools/dsl_normalization_guard_test',
          'ir_tools/ir_b2a','be/be','be/be.so','wopt/wopt.so')


def verify_receipt(root):
    """Authenticate immutable cached evidence before compatibility comparisons."""
    for line in (root/'SHA256SUMS').read_text().splitlines():
        digest,name=line.split('  ',1);require(sha256(root/name)==digest,'prior receipt changed: '+name)


def review(build,source,parent,root):
    """Run native cost, plan and guard producers; real PREOPT and reader checks."""
    verify_receipt(parent);root.mkdir(parents=True,exist_ok=False)
    binaries=root/'binaries';binaries.mkdir()
    for product in PRODUCTS:shutil.copy2(build/product,binaries/Path(product).name)
    env=dict(os.environ,LD_LIBRARY_PATH=str(binaries));commands=[]

    def run(case,label,args,success=True):
        """Retain exact options, streams and expected unsupported-lowering failures."""
        args=list(map(str,args));p=subprocess.run(args,cwd=case,env=env,text=True,capture_output=True)
        for stream in ('stdout','stderr'):(case/'logs'/(label+'.'+stream)).write_text(getattr(p,stream))
        commands.append(dict(case=case.name,label=label,argv=args,returncode=p.returncode))
        (root/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        require((p.returncode==0)==success,label+': '+p.stderr[-2000:]);return p

    for arch in ARCHES:
        for variant in SOURCES:
            case=root/(arch+'-'+variant)
            for directory in ('ascii','trace','logs','plan','guards'):(case/directory).mkdir(parents=True)
            run(case,'cost',[binaries/'dsl_normalization_cost_test',case/'ln_ref.B',case/'cost.t',arch,variant])
            for kind in ('plan','guards'):
                binary='dsl_normalization_plan_test' if kind=='plan' else 'dsl_normalization_guard_test'
                run(case,kind,[binaries/binary,case/kind/'ln_ref.B',case/kind/(kind+'.t'),arch,variant])
            original=(case/'ln_ref.B').read_bytes()
            for image in (case/'ln_ref.B.preserved.B',case/'plan/ln_ref.B',case/'plan/ln_ref.B.preserved.B',
                          case/'guards/ln_ref.B',case/'guards/ln_ref.B.preserved.B'):
                require(image.read_bytes()==original,'native query mutated source')
            for stem in ('ln_ref','ln_ref.B.preserved'):
                run(case,'mapped-'+stem,[binaries/'ir_b2a','-st','-src',case/(stem+'.B'),case/'ascii'/(stem+'.T')])
            for label,enabled,tracing in (('preopt-on','on','on'),('default-trace','on',None),('trace-off','on','off'),('preopt-off','off','on')):
                args=[binaries/'be','-fB,'+str(case/'ln_ref.B'),'-fo,'+str(case/(label+'.obj')),'-ft,'+str(case/'trace'/(label+'.t')),
                      '-PHASE:w=on:c=off:wpath='+str(binaries),'-O2','-DSL:wopt='+enabled,'-WOPT:cr_simp=on','-tr25']
                if tracing is not None:args.append('-DSL:ai_opt_trace='+tracing)
                result=run(case,label,args+[source/'osprey/be/opt/tests/dsl_normalization_ref_test.cxx'],False)
                require('OPR_DSLMEANVARNORM.v2 has no executable VHO lowering route' in result.stderr and
                        not (case/(label+'.obj')).exists(),'unexpected executable lowering')
                partial=case/(label+'.O')
                if partial.exists():partial.rename(case/(label+'.incomplete-mapped-output'))
    return reaudit(root,parent,root/'reports')


def reaudit(root,parent,output):
    """Recompute every component/corner/selection from raw source/plan/model data."""
    verify_receipt(parent);output.mkdir(parents=True,exist_ok=False);records=[];selections=Counter();nominals=Counter();statuses=Counter();compat=0
    for arch in ARCHES:
        for variant in SOURCES:
            name=arch+'-'+variant;case=root/name
            require((case/'ln_ref.B').read_bytes()==(case/'ln_ref.B.preserved.B').read_bytes(),'mapped source drift')
            require((case/'ascii/ln_ref.T').read_bytes()==(case/'ascii/ln_ref.B.preserved.T').read_bytes(),'mapped dump drift')
            if variant in VARIANTS:
                require((case/'ln_ref.B').read_bytes()==(parent/'native'/name/'ln_ref.B').read_bytes() and
                        (case/'ascii/ln_ref.T').read_bytes()==(parent/'native'/name/'ascii/ln_ref.T').read_bytes(),'prior source image/dump drift')
                compat+=1
            source=(case/'trace/preopt-on.t').read_text()
            facts=source_audit(source);default=(case/'trace/default-trace.t').read_text()
            require(facts==source_audit(default),'default trace semantic drift')
            if variant in VARIANTS:
                old=(parent/'native'/name/'trace/preopt-on.t').read_text()
                require(facts==source_audit(old),'prior PREOPT semantic drift')
                if 'AI_NORM 1 FORM ' in source:require(legacy_audit(source)==legacy_audit(old),'prior v1 semantic drift')
            for label in ('trace-off','preopt-off'):
                require('AI_NORM' not in (case/'trace'/(label+'.t')).read_text(),'trace controls ignored')
            rows=write_reports(case/'cost.t',case/'plan/plan.t',case/'trace/preopt-on.t',case/'guards',output/name)
            for r in rows:
                selections[r['selection']['selected']]+=1;nominals[r['selection']['nominal']]+=1
                statuses.update(c['status'] for c in r['costs'])
            records.append(dict(case=name,comparisons=len(rows),cost_trace_sha256=sha256(case/'cost.t')))
    require(set(nominals)=={0,1,2,3} and set(selections)=={0,1,2,3},'missing winner/fallback/no-choice coverage')
    summary=dict(schema='LN3-native-cost-review-v1',cases=40,comparisons=sum(r['comparisons'] for r in records),
                 candidate_costs=sum(statuses.values()),cost_statuses=dict(statuses),nominal_kinds=dict(nominals),selected_kinds=dict(selections),
                 full_mapped_dumps=80,actual_PREOPT_controls=160,prior_cases_preserved=compat,records=records,
                 analysis_only=True,source_v1_preserved=True,reduction_dependency=False,executable_LayerNorm=False,GPU=False)
    (output/'audit.json').write_text(json.dumps(summary,indent=2,sort_keys=True)+'\n')
    (output/'optimization-summary.log').write_text('# What it summarizes: LN3 native complete/incomplete comparisons, model-only gated choices, source preservation and remaining execution gates.\n'+
        f"cases=40 comparisons={summary['comparisons']} candidate_costs={summary['candidate_costs']} complete={statuses[0]} mapped_dumps=80 PREOPT_controls=160 prior_cases_preserved={compat}\n"+
        'NOMINAL '+json.dumps(dict(nominals),sort_keys=True)+'\nSELECTED '+json.dumps(dict(selections),sort_keys=True)+'\n'+
        'unit=assumed_reference_cycles builtin_timing=incomplete selection=analysis_only fallback=C0_conditional v1=preserved\n'+
        'TRANSFORM none executable_WHIRL=not_lowered production_provider=missing GPU=not_run reduction_dependency=false\n')
    (output/'optimization.log').write_text('# What it summarizes: full collection of successful LN3 physical-plan/exposure reports for every native case.\n'+
        ''.join((output/r['case']/'optimization.log').read_text() for r in records))
    return summary


def main():
    """Separate native production from repeatable strict cached-evidence audit."""
    p=argparse.ArgumentParser(description=__doc__)
    for option in ('build','source','parent','root','audit_output'):p.add_argument('--'+option.replace('_','-'),type=Path,required=option in ('parent','root'))
    a=p.parse_args()
    result=reaudit(a.root.resolve(),a.parent.resolve(),a.audit_output.resolve()) if a.audit_output else review(a.build.resolve(),a.source.resolve(),a.parent.resolve(),a.root.resolve())
    print(json.dumps({k:v for k,v in result.items() if k!='records'},sort_keys=True))


if __name__=='__main__':main()
