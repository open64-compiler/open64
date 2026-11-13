"""Relocatable Q7 compile commands; all outputs go to a fresh directory.

Host binding rebuild reuses retained pure compiler contract object/be.so.
Generated CUDA compile checks and native PTX reconstruction do not change the
authorized retained cubins. See doc/AI-COMPILER-MLA-Q7-QUALIFICATION-v1.0.md.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
from mla_q7_common import authenticate


def main(argv=None):
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--packet',type=Path,required=True); p.add_argument('--expected-sha256',required=True)
    p.add_argument('--output',type=Path,required=True); p.add_argument('--cuda-home',type=Path,default=Path(os.environ.get('CUDA_HOME','/usr/local/cuda')))
    p.add_argument('--mode',choices=('host','generated'),required=True); p.add_argument('--case',action='append')
    a=p.parse_args(argv); root=a.packet.resolve(); out=a.output.resolve(); out.mkdir(parents=True,exist_ok=False)
    commands=[]; report=dict(status='FAIL',gpu_execution=False,aot_selection='deferred',search_invocations=0)
    def run(label,args):
        args=list(map(str,args)); result=subprocess.run(args,capture_output=True,text=True,timeout=600)
        (out/(label+'.stdout')).write_text(result.stdout); (out/(label+'.stderr')).write_text(result.stderr)
        commands.append(dict(label=label,argv=args,returncode=result.returncode)); (out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        if result.returncode: raise RuntimeError(label+' failed; see stderr')
        return result.stdout
    try:
        authenticate(root,a.expected_sha256)
        if a.mode=='host':
            objects=[]
            for i,args in enumerate(json.loads((root/'host-build-commands.json').read_text())[:3]):
                def relocate(value):
                    for old,new in (('/src',root/'source'),('/build',root/'build-input'),('/cuda',a.cuda_home.resolve())):
                        value=value.replace(old+'/',str(new)+'/')
                        if value==old: value=str(new)
                        if value=='-I'+old: value='-I'+str(new)
                    return value
                args=list(map(relocate,args)); index=args.index('-o')+1; obj=out/Path(args[index]).name; args[index]=str(obj)
                args+=['-MF',str(obj.with_suffix('.d'))]; run('host-'+str(i),args); objects.append(obj)
            # The captured source order is runtime, registry, private decoder.
            library=out/'libmla_serving_cuda_q7.so'
            run('host-link',['g++','-shared','-pthread','-o',library]+objects+
                [root/'build-input/osprey/targdir/ir_tools/tensor_attention_search.o','-L'+str(root/'runtime/be'),'-l:be.so',
                 '-L'+str(a.cuda_home/'lib64/stubs'),'-L'+str(a.cuda_home/'lib/stubs'),'-lcuda',
                 '-Wl,-rpath,'+str(root/'runtime/be')])
            symbols=run('host-symbols',['nm','-DC','--defined-only',library])
            if 'q6d_mock_' in symbols or 'VHO_tensor_serving_counter_test' in symbols: raise ValueError('model/test symbols in hardware binding')
            report.update(status='PASS',packet_sha256=a.expected_sha256,library_sha256=hashlib.sha256(library.read_bytes()).hexdigest(),
                          source_objects_rebuilt=3,retained_link_dependencies=['tensor_attention_search.o','be.so'],SDK_stub_link_only=True)
        else:
            packet=json.loads((root/'packet.json').read_text()); matches={}
            selected=[x for x in packet['cases'] if not a.case or x['id'] in a.case]
            if not selected or a.case and set(a.case)-{x['id'] for x in selected}: raise ValueError('unknown case')
            run('nvcc-version',[a.cuda_home/'bin/nvcc','--version'])
            for case in selected:
                src=root/'cases'/case['id']; dst=out/case['id']; dst.mkdir()
                for stage in ('prepare','attention','finalize'):
                    run(case['id']+'-'+stage+'-cuda',[a.cuda_home/'bin/nvcc','-arch=sm_90','--fmad=false','--ftz=false','--prec-div=true','--prec-sqrt=true',
                        '-I'+str(root/'source/osprey/include'),'-c',src/(stage+'.cu'),'-o',dst/(stage+'.cuda.o')])
                    run(case['id']+'-'+stage+'-ptx',[a.cuda_home/'bin/ptxas','-arch=sm_90','-c',src/(stage+'.ptx'),'-o',dst/(stage+'.ptx.o')])
                run(case['id']+'-math',[a.cuda_home/'bin/nvcc','-arch=sm_90','--fmad=false','--ftz=false','-dc',src/'math.cu','-o',dst/'math.o'])
                run(case['id']+'-link',[a.cuda_home/'bin/nvlink','-arch=sm_90','-o',dst/'native.cubin']+
                    [dst/(s+'.ptx.o') for s in ('prepare','attention','finalize')]+[dst/'math.o'])
                matches[case['id']]=(dst/'native.cubin').read_bytes()==(src/'native.cubin').read_bytes()
            report.update(status='PASS',scope='offline source compile/link only',cases=len(selected),exact_native_cubin_matches=matches,
                          rebuilt_images_approved=False,execution_uses_retained_fixed_images=True)
    except Exception as error:
        report['reason']=str(error)
    (out/'build-report.json').write_text(json.dumps(report,indent=2)+'\n'); print(json.dumps(report,indent=2)); return 0 if report['status']=='PASS' else 1


if __name__=='__main__': raise SystemExit(main())
