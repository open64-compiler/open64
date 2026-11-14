"""Derived LN4 compiled-entry/CUDA-mock review over authenticated mapped evidence.
Design: doc/AI-COMPILER-LAYERNORM-COMPILED-ENTRY-v0.1.md.
No compiler producer changes or GPU execution; output is always a fresh directory.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time

from package_normalization_entry import authenticate, package
from normalization_plan_trace_report import require
from normalization_guard_trace_report import audit_runtime, FIELDS


def audit_trace(text, expected):
    """Reconcile actual adapter observations, module pins and test acceptance."""
    seq={};events=[];checks=0;summary=None;faults=[]
    for line in text.splitlines():
        words=line.split()
        if words[:2] in (['AI_NORM_CUDA','1'],['AI_NORM_CUDA_TEST','1']):
            require(summary is None,'provider rows after terminal summary')
        if words[:3]==['AI_NORM_CUDA','1','EVENT']:
            require(len(words)==10 and int(words[-1])==expected['source_line'],'invalid provider columns')
            owner,n=int(words[3]),int(words[4]);require(owner>0 and n==seq.get(owner,0)+1,'provider sequence drift');seq[owner]=n
            events.append(dict(owner=owner,action=words[5],status=int(words[6]),a=int(words[7]),b=int(words[8])))
        elif words[:3]==['AI_NORM_CUDA_TEST','1','CHECK']:
            require(len(words)==6 and words[4]=='1' and int(words[5])==expected['source_line'],'failed provider check');checks+=1
        elif words[:3]==['AI_NORM_CUDA_TEST','1','SUMMARY']:
            require(len(words)==11 and summary is None and words[-2]=='host_mock_only' and int(words[-1])==expected['source_line'],'invalid provider summary')
            summary=list(map(int,words[3:-2]))
        elif words[:3]==['AI_NORM_CUDA_TEST','1','FAULT']:
            require(len(words)==5 and int(words[-1])==expected['source_line'],'invalid fault columns');faults.append(int(words[3]))
    require(summary and summary[0]==checks and checks>100,'missing provider acceptance')
    require(faults==list(range(1,32)),'missing/duplicate refusal fault coverage')
    opened=[e for e in events if e['action']=='open' and e['status']==0]
    require(len(opened)==1 and opened[0]['a']==expected['plan_seal'] and opened[0]['b']==expected['tree_seal'],'foreign entry open')
    observed=[e for e in events if e['owner']==opened[0]['owner']]
    require(any(e['action']=='pinned' and e['status']==4 for e in observed) and
            observed[-1]['action']=='unload' and observed[-1]['status']==0,'module lifetime not verified')
    require(any(e['action']=='resources' and e['status']==0 and e['a']==expected['compiled_registers'] and
                e['b']==expected['static_shared'] for e in observed),'missing exact loaded resources')
    attributes=[e for e in observed if e['action']=='attribute'];g=dict(zip(FIELDS,expected['guard_words']))
    # CUDA 12.8 CUfunction_attribute IDs; these are decoder IDs, not timing constants.
    values={e['a']:e['b'] for e in attributes}
    require(len(attributes)==6 and set(values)=={0,1,4,5,6,10} and all(e['status']==0 for e in attributes) and
            values[4]==expected['compiled_registers'] and values[1]==expected['static_shared'] and
            values[0]>=g['threads'] and values[5]>=(g['ptx']//100)*10+g['ptx']%100 and
            values[6]==(90 if g['architecture']<=2 else 100) and values[10]==0,'entry attribute observation drift')
    require(any(e['action']=='resident' and e['status']==0 and e['a']>=expected['resident_floor'] and
                e['b']==expected['resident_floor'] for e in observed),'missing resident-floor gate')
    require(any(e['action']=='perform' and e['a']==1 and e['status']==0 for e in observed) and
            any(e['action']=='perform' and e['a']==11 and e['status']!=0 for e in observed),'launch/failure-drain coverage missing')
    return dict(checks=checks,providers=len(seq),events=len(events),loads=summary[1],unloads=summary[2],
                mock_launches=summary[3],mock_drains=summary[4],hook_calls=summary[5],GPU=False)


def review(source, receipt, cuda, output, sanitize=False):
    """Reuse authenticated immutable producer; compile/run only affected runtime boundary."""
    receipt=authenticate(receipt);output.mkdir(parents=True,exist_ok=False);commands=[]
    env=dict(os.environ,PYTHONDONTWRITEBYTECODE='1')
    if sanitize:env.update(ASAN_OPTIONS='detect_leaks=1',UBSAN_OPTIONS='halt_on_error=1')
    def run(label,args,cwd=output):
        """Retain exact invocation, status and complete streams before assertions."""
        start=time.monotonic();p=subprocess.run(list(map(str,args)),cwd=cwd,env=env,text=True,capture_output=True)
        (output/(label+'.stdout')).write_text(p.stdout);(output/(label+'.stderr')).write_text(p.stderr)
        commands.append(dict(argv=list(map(str,args)),cwd=str(cwd),returncode=p.returncode,seconds=time.monotonic()-start))
        (output/'commands.json').write_text(json.dumps(commands,indent=2)+'\n');require(p.returncode==0,label+': '+p.stderr[-3000:])
    records=json.loads((receipt/'native/audit.json').read_text())['records'];packages=[]
    for record in records:
        name=record['case'];packages.append(package(receipt/'native'/name,output/'packages'/name,str(receipt)))
    require(len(packages)==36,'incomplete inherited producer census')
    header=''.join('#include "packages/'+r['case']+'/entry.h"\n' for r in records)
    header+='static OPEN64_NORMALIZATION_COMPILED_ENTRY entries[]={'+','.join(p['header_symbol']+'()' for p in packages)+'};\n'
    (output/'entries.h').write_text(header)
    flags=['-std=c++11','-O1','-g','-Wall','-Wextra','-Werror','-pthread','-I'+str(source/'osprey/include'),'-I'+str(cuda/'include'),'-I'+str(output)]
    if sanitize:flags+=['-fsanitize=address,undefined','-fno-omit-frame-pointer','-no-pie']
    run('compile',['g++']+flags+[source/'osprey/libopen64rt/normalization_cuda.cxx',
        source/'osprey/libopen64rt/normalization_runtime.cxx',source/'osprey/libopen64rt/tests/normalization_cuda_test.cxx',
        '-o',output/'normalization_cuda_test'])
    # Existing runtime-only regression consumes the same actual selected guards.
    run('compile-base',['g++']+flags+[source/'osprey/libopen64rt/normalization_runtime.cxx',
        source/'osprey/libopen64rt/tests/normalization_runtime_test.cxx','-o',output/'normalization_runtime_test'])
    results=[]
    for index,(record,p) in enumerate(zip(records,packages)):
        name=record['case'];case=output/'packages'/name;trace=case/'provider.t'
        run(name,[output/'normalization_cuda_test',index,case/'kernel.cubin',trace])
        result=audit_trace(trace.read_text(),p)
        run(name+'-base',[output/'normalization_runtime_test',case/'selected.guard',case/'runtime.t'])
        result['base_runtime']=audit_runtime((case/'runtime.t').read_text(),dict(zip(FIELDS,p['guard_words'])))
        results.append(dict(case=name,**result))
        (case/'provider.log').write_text('# What it summarizes: observed CUDA-API mock resources, admission, launch/drain failures and live-module pins; not GPU execution.\n'+
            'EVENT_COLUMNS provider seq action status a b source_line\n'+
            '\n'.join(s for s in trace.read_text().splitlines() if s.startswith('AI_NORM_CUDA '))+'\n')
    summary=dict(schema='LN4-compiled-entry-review-v1',cases=len(results),checks=sum(r['checks'] for r in results),
        base_runtime_checks=sum(r['base_runtime']['checks'] for r in results),
        API_mock_launches=sum(r['mock_launches'] for r in results),GPU=False,sanitized=sanitize,
        parent_receipt=str(receipt),parent_receipt_sha256=hashlib.sha256((receipt/'receipt.json').read_bytes()).hexdigest(),
        v1_preserved=True,reduction_dependency=False,compiler_producer_reused=True,records=results)
    (output/'audit.json').write_text(json.dumps(summary,indent=2,sort_keys=True)+'\n')
    (output/'optimization-summary.log').write_text('# What it summarizes: exact compiled-entry packages, real adapter/mock-API enforcement, unchanged mapped producer and remaining GPU/service gates.\n'+
        f"packages={len(results)} provider_checks={summary['checks']} inherited_full_mapped_dumps=193 source_v1=preserved reduction_dependency=false\n"+
        'CUDA_adapter=implemented API_execution=mock_only GPU=not_run calibrated_profiles=missing general_service_session=missing\n')
    (output/'optimization.log').write_text('# What it summarizes: full compiled-entry and successful provider enforcement report collection.\n'+
        ''.join((output/'packages'/r['case']/name).read_text() for r in results for name in ('compiled-entry.log','provider.log')))
    print(json.dumps({k:v for k,v in summary.items() if k!='records'},sort_keys=True));return summary


def main():
    """Explicit offline inputs and fresh artifact destination; never probe for a GPU."""
    p=argparse.ArgumentParser(description=__doc__)
    for key in ('source','receipt','cuda','output'):p.add_argument('--'+key,type=Path,required=True)
    p.add_argument('--sanitize',action='store_true');a=p.parse_args();review(a.source,a.receipt,a.cuda,a.output,a.sanitize)


if __name__=='__main__':main()
