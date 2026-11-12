"""LN4 actual-driver host/service closure, mapped ABI and real consumer handoff.

Design: doc/AI-COMPILER-LAYERNORM-HOST-DISPATCH-v0.1.md.
Fresh outputs, authenticated source fixtures, offline CUDA and API mocks only.
"""
import argparse
import difflib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess

from package_normalization_entry import authenticate, package
from normalization_request_trace_report import reports
from normalization_execution_trace_report import report
from normalization_plan_trace_report import require
from normalization_host_trace_report import audit as host_audit
from normalization_host_mode_evidence import complete_modes


def review(build, source, cuda, parent, output, probe=False):
    """Run actual compiler/writer/emitter; never fabricate host or device bodies."""
    parent=authenticate(parent);output.mkdir(exist_ok=False,parents=True);commands=[];records=[]
    binaries=output/'binaries'
    for relative in ('ir_tools/dsl_normalization_request_test','ir_tools/ir_b2a','be/be','be/be.so','wopt/wopt.so',
                     'whirl2c/whirl2c','whirl2c/whirl2c.so'):
        target=binaries/relative;target.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(build/relative,target)
    (binaries/'whirl2c/whirl2c_be').symlink_to('../be/be')
    (binaries/'whirl2c/be.so').symlink_to('../be/be.so')
    build=binaries
    env=dict(os.environ,PYTHONDONTWRITEBYTECODE='1',SOURCE_DATE_EPOCH='1793131200',
             LD_LIBRARY_PATH=str(build/'be')+':'+str(build/'wopt'))
    def run(case,label,args,success=True,extra=None):
        """Retain complete streams and exact commands, including negative cases."""
        p=subprocess.run(list(map(str,args)),cwd=case,env=dict(env,**(extra or {})),text=True,capture_output=True)
        log=case/'logs';log.mkdir(exist_ok=True)
        for field in ('stdout','stderr'):(log/(label+'.'+field)).write_text(getattr(p,field))
        commands.append(dict(cwd=str(case),label=label,argv=list(map(str,args)),returncode=p.returncode))
        (output/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        require((p.returncode==0)==success,label+': '+p.stderr[-4000:]);return p
    def backend(case,label,input_case,mode,trace=True,success=True):
        """Mode controls only driver-owned output packaging, not LN3 selection."""
        return run(case,label,[build/'be/be','-fB,'+str(input_case/'source.B'),'-ft,'+str(case/(label+'.t')),
            '-O3','-TARG:abi=n64','-OPT:roundoff=0:IEEE_arith=1:fast_math=off',
            '-PHASE:wpath='+str(build/'wopt'),'-DSL:gpu_arch='+input_case.name.split('-')[0],
            '-DSL:normalization_request='+str(input_case/'request.json'),
            '-DSL:normalization_output='+str(case/(label+'.after.B')),'-DSL:normalization_mode='+mode,
            '-DSL:ai_opt_trace='+('on' if trace else 'off'),source/'osprey/be/opt/tests/dsl_normalization_ref_test.cxx'],success)
    def emit_host(case,label,image,name,success=True):
        """Use ordinary whirl2c with exact-PU admission, not source extraction."""
        return run(case,label,[build/'whirl2c/whirl2c','-O0','-ft,'+str(case/(label+'.t')),
            '-DSL:normalization_host_entry='+name,'-CLIST:dotc_file='+label+'.c',
            '-CLIST:doth_file='+label+'.h','-fB,'+str(image),
            source/'osprey/be/opt/tests/dsl_normalization_ref_test.cxx'],success)
    parent_records=json.loads((parent/'native/audit.json').read_text())['records']
    if probe:parent_records=parent_records[:3]
    for record in parent_records:
        original=parent/'native'/record['case'];case=output/record['case'];case.mkdir()
        shutil.copy2(original/'request.json',case/'request.json')
        backend(case,'selected',original,'standalone')
        image=case/'selected.after.B';manifest_path=case/'selected.after.B.manifest.json'
        m=json.loads(manifest_path.read_text());host=m['host_dispatch']
        require(m['execution_mode']=='standalone' and host,'missing standalone dispatch')
        reopen=[build/'ir_tools/dsl_normalization_request_test','reopen',image,manifest_path,case/'reopen.t']
        run(case,'reopen',reopen)
        for mutation in ('plan','tree','root'):
            run(case,'mutated-'+mutation,reopen,False,{'OPEN64_LN_HOST_MUTATION':mutation})
        for stem in ('selected.before','selected.after'):
            run(case,'dump-'+stem,[build/'ir_tools/ir_b2a','-st','-src',case/(stem+'.B'),case/(stem+'.T')])
        before=(case/'selected.before.T').read_text();after=(case/'selected.after.T').read_text()
        (case/'selected.before-after.diff').write_text(''.join(difflib.unified_diff(before.splitlines(True),after.splitlines(True),fromfile='before.T',tofile='after.T')))
        require('OPR_DSLMEANVARNORM' in after and host in after and 'open64_normalization_execution_run_selected' in after,'mapped host/source evidence absent')
        emit_host(case,'host',image,host)
        host_source=(case/'host.c').read_text()
        host_audit((case/'selected.t').read_text(),m,host_source)
        require(host_source.count('open64_normalization_execution_run_selected(')==1 and
            str(m['plan_seal'])+'ULL' in host_source and str(m['tree_seal'])+'ULL' in host_source,'emitted host identity missing')
        require('__global__' not in host_source and m['kernel']+'(' not in host_source,'host source embeds kernel')
        run(case,'host-cc',['gcc','-std=c11','-Werror=implicit-function-declaration','-I'+str(source/'osprey/include'),
            '-fPIC','-shared','host.c','-o','host.so'])
        run(case,'cuda',[build/'whirl2c/whirl2c','-cuda','-ft,'+str(case/'cuda.t'),'-CLIST:dotc_file=kernel.cu',
            '-fB,'+str(image),source/'osprey/be/opt/tests/dsl_normalization_ref_test.cxx'])
        # Use the existing audited compiler contract, then exact assembler gates.
        text=(case/'selected.t').read_text();(case/'driver.t').write_text(text)
        result=reports(text,m,(case/'reopen.t').read_text(),(case/'kernel.cu').read_text(),case/'reports')
        arch=record['case'].split('-')[0]
        flags=['-std=c++11','-arch='+arch,'--fmad=false','--ftz=false','--prec-div=true','--prec-sqrt=true']
        run(case,'ptx',[cuda/'bin/nvcc']+flags+['-ptx','kernel.cu','-o','kernel.ptx'])
        run(case,'cubin',[cuda/'bin/nvcc']+flags+['-cubin','kernel.cu','-o','kernel.cubin','-Xptxas=-v'])
        entry=package(case,case/'package',str(parent))
        backend(case,'quiet',original,'standalone',False)
        require('AI_NORM' not in (case/'quiet.t').read_text(),'trace-off leaked optimization records')
        for suffix in ('.after.B','.before.B','.after.B.manifest.json'):
            require((case/('selected'+suffix)).read_bytes()==(case/('quiet'+suffix)).read_bytes(),'trace-off changed host/device plan')
        backend(case,'service',original,'service')
        sm=json.loads((case/'service.after.B.manifest.json').read_text())
        host_audit((case/'service.t').read_text(),sm)
        require(sm['host_dispatch']=='' and sm['execution_mode']=='service' and sm['plan_seal']==m['plan_seal'], 'service packaging changes choice')
        run(case,'dump-service',[build/'ir_tools/ir_b2a','-st','-src',case/'service.after.B',case/'service.after.T'])
        require(host not in (case/'service.after.T').read_text(),'service image contains generated host')
        emit_host(case,'service-no-host',case/'service.after.B',host,False)
        row=dict(case=record['case'],kind=m['selected_kind'],host=host,plan=m['plan_seal'],tree=m['tree_seal'],source_line=entry['source_line'])
        records.append(row)
        (case/'host-dispatch.log').write_text('# What it summarizes: actual standalone/service PU packaging, mapped host ABI/constants, emitted host C and cost-selected device assembly; no GPU run.\n'+
            '\n'.join(s for s in text.splitlines() if s.startswith('AI_NORM_HOST '))+'\n'+
            'MAPPED native_reopen_passed MUTATIONS plan_tree_root_refused HOST_C compiled SERVICE_HOST absent TRACE_OFF exact GPU not_run\n')
    # Every case contributes its native entry descriptor to the actual mock fixture.
    includes=[];symbols=[]
    for row in records:
        entry=json.loads((output/row['case']/'package/entry.json').read_text())
        includes.append('#include "'+row['case']+'/package/entry.h"');symbols.append(entry['header_symbol']+'()')
    (output/'entries.h').write_text('\n'.join(includes)+'\nstatic OPEN64_NORMALIZATION_COMPILED_ENTRY entries[]={'+','.join(symbols)+'};\n')
    flags=['-O1','-g','-pthread','-Wall','-Wextra','-Werror','-I'+str(source/'osprey/include'),'-I'+str(cuda/'include'),'-I'+str(output)]
    run(output,'buffer',['gcc','-std=c11']+flags+[source/'osprey/libopen64rt/target_buffer_service.c','-c','-o','buffer.o'])
    for mode,sanitize in (('normal',False),('sanitizer',True)):
        extra=['-fsanitize=address,undefined','-fno-omit-frame-pointer','-no-pie'] if sanitize else []
        if sanitize:run(output,'buffer-sanitizer',['gcc','-std=c11']+flags+extra+
            [source/'osprey/libopen64rt/target_buffer_service.c','-c','-o','buffer-sanitizer.o'])
        run(output,'compile-'+mode,['g++','-std=c++11']+flags+extra+['-DOPEN64_LN_DISPATCH_FIXTURE','-rdynamic',
            source/'osprey/libopen64rt/normalization_runtime.cxx',source/'osprey/libopen64rt/normalization_cuda.cxx',
            source/'osprey/libopen64rt/normalization_execution.cxx',source/'osprey/libopen64rt/tests/normalization_execution_test.cxx',
            output/('buffer-sanitizer.o' if sanitize else 'buffer.o'),'-ldl','-o',output/('execution-'+mode)])
        for index,row in enumerate(records):
            case=output/row['case'];trace=case/(mode+'.execution.t')
            if sanitize:run(case,'host-cc-sanitizer',['gcc','-std=c11','-I'+str(source/'osprey/include'),
                '-fPIC','-shared','-fsanitize=address,undefined','host.c','-o','host-sanitizer.so'])
            run(case,'run-'+mode,[output/('execution-'+mode),index,case/'package/kernel.cubin',trace,
                case/('host-sanitizer.so' if sanitize else 'host.so'),row['host']],
                extra={'ASAN_OPTIONS':'detect_leaks=1','UBSAN_OPTIONS':'halt_on_error=1'})
            expected=json.loads((case/'package/entry.json').read_text())
            measured=report(trace.read_text(),expected,case/(mode+'.execution.log'))
            row[mode]=measured
            if sanitize:require(trace.read_bytes()==(case/'normal.execution.t').read_bytes(),'sanitizer execution trace drift')
    complete_modes(build,source,output,records,run,parent)
    for row in records:
        case=output/row['case']
        (case/'optimization.log').write_text('# What it summarizes: complete selection, WHIRL lowering, host dispatch and mock runtime/provider evidence.\n'+
            (case/'reports/optimization.log').read_text()+(case/'host-dispatch.log').read_text()+
            (case/'package/compiled-entry.log').read_text()+(case/'provider.log').read_text()+(case/'normal.execution.log').read_text())
    refusals=output/'refusals';refusals.mkdir();original=parent/'native'/parent_records[0]['case']
    for mode in ('','performance','unknown'):
        name='bad-'+(mode or 'empty');backend(refusals,name,original,mode,success=False)
        require(not (refusals/(name+'.after.B')).exists(),'bad mode published image')
    first=output/records[0]['case']
    emit_host(first,'wrong-name',first/'selected.after.B','missing_dispatch',False)
    total=dict(schema='LN4-host-dispatch-review-v1',records=records,cases=len(records),mapped_images=4*len(records),
        native_host_mutation_refusals=3*len(records),focused_mode_refusals=4+len(records),
        normal_checks=sum(r['normal']['checks'] for r in records),scenarios=sum(r['normal']['scenarios'] for r in records),
        sanitizer_trace_parity=True,v1_preserved=True,reduction_dependency=False,GPU=False)
    (output/'audit.json').write_text(json.dumps(total,indent=2,sort_keys=True)+'\n')
    (output/'optimization-summary.log').write_text('# What it summarizes: LN4 compiler-side host/service closure, actual WHIRL/host C, complete mock handoff and remaining native GPU/deployment gates.\n'+
        ''.join(f"{r['case']} C{r['kind']-1} host={r['host']} mapped=verified assembly=passed runtime=mock_checked scenarios={r['normal']['scenarios']} GPU=not_run\n" for r in records)+
        f"cases={len(records)} checks={total['normal_checks']} v1=preserved reduction_dependency=false standalone_main=deployment_owned performance_harness=opt_in GPU=not_run\n")
    print(json.dumps({k:v for k,v in total.items() if k!='records'},sort_keys=True));return total


def main():
    """Require explicit offline toolchain and a fresh output destination."""
    p=argparse.ArgumentParser(description=__doc__)
    for name in ('build','source','cuda','parent','output'):p.add_argument('--'+name,type=Path,required=True)
    p.add_argument('--probe',action='store_true');a=p.parse_args()
    review(a.build,a.source,a.cuda,a.parent,a.output,a.probe)


if __name__=='__main__':main()
