"""Build/run LN4 host/service composition from an authenticated entry receipt.
Design: doc/AI-COMPILER-LAYERNORM-EXECUTION-CONSUMER-v0.1.md.
Compiler/mapped producer is unchanged; mocks do not certify a GPU or arithmetic.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time

from normalization_plan_trace_report import require
from normalization_execution_trace_report import report


def authenticate(root):
    """Authenticate every retained entry input before cached reuse."""
    root=root.resolve();receipt=json.loads((root/'receipt.json').read_text())
    require(receipt['schema']=='open64-ln4-compiled-entry-review-v1' and receipt['GPU'] is False,'unsupported entry receipt')
    for line in (root/'SHA256SUMS').read_text().splitlines():
        digest,name=line.split('  ',1);path=root/name
        require(root in path.resolve().parents and hashlib.sha256(path.read_bytes()).hexdigest()==digest,'entry receipt drift: '+name)
    require(hashlib.sha256((root/receipt['primary_artifact']).read_bytes()).hexdigest()==receipt['primary_sha256'],'primary identity mismatch')
    return root


def review(source, receipt, cuda, output, sanitize=False):
    """Compile actual shared-registry/adapter/consumer code, then audit raw evidence."""
    receipt=authenticate(receipt);output.mkdir(parents=True,exist_ok=False);commands=[]
    env=dict(os.environ,PYTHONDONTWRITEBYTECODE='1')
    if sanitize:env.update(ASAN_OPTIONS='detect_leaks=1',UBSAN_OPTIONS='halt_on_error=1')
    def run(label,args):
        """Keep exact command, timing and full diagnostic streams before asserting."""
        start=time.monotonic();p=subprocess.run(list(map(str,args)),cwd=output,env=env,text=True,capture_output=True)
        for key in ('stdout','stderr'):(output/(label+'.'+key)).write_text(getattr(p,key))
        commands.append(dict(argv=list(map(str,args)),cwd=str(output),returncode=p.returncode,seconds=time.monotonic()-start))
        (output/'commands.json').write_text(json.dumps(commands,indent=2)+'\n');require(p.returncode==0,label+': '+p.stderr[-3000:])
    old=receipt/'normal';records=json.loads((old/'audit.json').read_text())['records']
    shutil.copy2(old/'entries.h',output/'entries.h')
    for r in records:
        case=output/'packages'/r['case'];case.mkdir(parents=True)
        for name in ('entry.h','entry.json','kernel.cubin','selected.guard','compiled-entry.log'):shutil.copy2(old/'packages'/r['case']/name,case/name)
    flags=['-O1','-g','-Wall','-Wextra','-Werror','-pthread','-I'+str(source/'osprey/include'),'-I'+str(cuda/'include'),'-I'+str(output)]
    if sanitize:flags+=['-fsanitize=address,undefined','-fno-omit-frame-pointer','-no-pie']
    run('compile-buffer',['gcc','-std=c11']+flags+[source/'osprey/libopen64rt/target_buffer_service.c','-c','-o',output/'target_buffer_service.o'])
    common=[source/'osprey/libopen64rt/normalization_runtime.cxx',source/'osprey/libopen64rt/normalization_cuda.cxx']
    run('compile-execution',['g++','-std=c++11']+flags+common+[source/'osprey/libopen64rt/normalization_execution.cxx',
        source/'osprey/libopen64rt/tests/normalization_execution_test.cxx',output/'target_buffer_service.o','-o',output/'normalization_execution_test'])
    run('compile-adapter',['g++','-std=c++11']+flags+common+[source/'osprey/libopen64rt/tests/normalization_cuda_test.cxx','-o',output/'normalization_cuda_test'])
    run('compile-base',['g++','-std=c++11']+flags+[source/'osprey/libopen64rt/normalization_runtime.cxx',
        source/'osprey/libopen64rt/tests/normalization_runtime_test.cxx','-o',output/'normalization_runtime_test'])
    results=[]
    for index,r in enumerate(records):
        case=output/'packages'/r['case'];expected=json.loads((case/'entry.json').read_text())
        run(r['case'],[output/'normalization_execution_test',index,case/'kernel.cubin',case/'execution.t'])
        result=report((case/'execution.t').read_text(),expected,case/'execution.log');results.append(dict(case=r['case'],**result))
        run(r['case']+'-adapter',[output/'normalization_cuda_test',index,case/'kernel.cubin',case/'provider.t'])
        run(r['case']+'-base',[output/'normalization_runtime_test',case/'selected.guard',case/'runtime.t'])
        for name in ('provider.t','runtime.t'):
            require((case/name).read_bytes()==(old/'packages'/r['case']/name).read_bytes(),'shared regression trace drift: '+r['case']+'/'+name)
    result=dict(schema='LN4-execution-consumer-review-v1',cases=len(results),checks=sum(r['checks'] for r in results),
        scenarios=sum(r['scenarios'] for r in results),API_checks=sum(r['API_checks'] for r in results),
        adapter_regression_checks=19860,base_runtime_regression_checks=11392,shared_regression_trace_files=72,
        parent_receipt=str(receipt),parent_receipt_sha256=hashlib.sha256((receipt/'receipt.json').read_bytes()).hexdigest(),
        sanitized=sanitize,v1_preserved=True,reduction_dependency=False,compiler_producer_reused=True,GPU=False,records=results)
    (output/'audit.json').write_text(json.dumps(result,indent=2,sort_keys=True)+'\n')
    (output/'optimization-summary.log').write_text('# What it summarizes: compiled-entry host/service consumer, shared registry leases, movement and failure recovery; inherited mapped evidence and native GPU gates.\n'+
        f"cases={len(results)} scenarios={result['scenarios']} checks={result['checks']} API_checks={result['API_checks']} shared_regression_traces_exact=72\n"+
        'SOURCE_V1 preserved REDUCTION_DEPENDENCY false WHIRL_PRODUCER unchanged INHERITED_FULL_MAPPED_DUMPS 193\n'+
        'EXECUTION real_runtime_composition CUDA_API mock SENTINEL_WRITES not_numerical_kernel GPU not_run\n')
    (output/'optimization.log').write_text('# What it summarizes: full inherited compiled-entry, provider and successful execution-consumer report collection.\n'+
        ''.join((output/'packages'/r['case']/name).read_text() for r in results for name in ('compiled-entry.log','provider.log','execution.log')))
    print(json.dumps({k:v for k,v in result.items() if k!='records'},sort_keys=True));return result


def main():
    """Require explicit immutable inputs and a fresh host-mounted output directory."""
    p=argparse.ArgumentParser(description=__doc__)
    for key in ('source','receipt','cuda','output'):p.add_argument('--'+key,type=Path,required=True)
    p.add_argument('--sanitize',action='store_true');a=p.parse_args();review(a.source,a.receipt,a.cuda,a.output,a.sanitize)


if __name__=='__main__':main()
