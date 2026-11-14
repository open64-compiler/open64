"""Actual standalone-mode level/ABI/FP/v1/publication refusal regression.
Design: doc/AI-COMPILER-LAYERNORM-HOST-DISPATCH-v0.1.md.
Fresh output directory only; every compiler command/diagnostic is retained.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess

from package_normalization_entry import authenticate
from normalization_plan_trace_report import require


def review(build, source, parent, output):
    """Prove host packaging cannot bypass existing semantic and optimization gates."""
    parent=authenticate(parent);output.mkdir(exist_ok=False,parents=True);records=[]
    env=dict(os.environ,LD_LIBRARY_PATH=str(build/'be')+':'+str(build/'wopt'))
    normal=parent/'native/sm_90-single-row-2'
    cases=[('O'+str(i),normal,['-O'+str(i)]) for i in range(3)]
    cases += [('n32',normal,['-TARG:abi=n32']),('fast-math',normal,['-OPT:fast_math=on']),
              ('roundoff',normal,['-OPT:roundoff=2']),('wrong-target',normal,['-DSL:gpu_arch=sm_100']),
              ('v1',parent/'native/refuse-legacy',[]),('existing',normal,[])]
    for name,case,extra in cases:
        image=output/(name+'.after.B')
        if name=='existing':image.write_bytes(b'preserve occupied output\n')
        args=[build/'be/be','-fB,'+str(case/'source.B'),'-ft,'+str(output/(name+'.t')),'-O3','-TARG:abi=n64',
            '-OPT:roundoff=0:IEEE_arith=1:fast_math=off','-PHASE:wpath='+str(build/'wopt'),'-DSL:gpu_arch=sm_90',
            '-DSL:normalization_mode=standalone','-DSL:normalization_request='+str(case/'request.json'),
            '-DSL:normalization_output='+str(image)]+extra+[source/'osprey/be/opt/tests/dsl_normalization_ref_test.cxx']
        p=subprocess.run(list(map(str,args)),cwd=output,env=env,text=True,capture_output=True)
        for field in ('stdout','stderr'):(output/(name+'.'+field)).write_text(getattr(p,field))
        require(p.returncode!=0,'standalone bypassed '+name)
        require(not image.exists() if name!='existing' else image.read_bytes()==b'preserve occupied output\n','refusal replaced/published image')
        require(not Path(str(image)+'.manifest.json').exists(),'refusal published manifest')
        require(not (output/(name+'.before.B')).exists(),'refusal published before image')
        records.append(dict(case=name,argv=list(map(str,args)),returncode=p.returncode))
    (output/'commands.json').write_text(json.dumps(records,indent=2)+'\n')
    (output/'host-refusals.log').write_text('# What it summarizes: standalone host packaging retains O3/n64/strict-FP, source-v1, exact-target and no-replace publication gates.\n'+
        ''.join(r['case']+' refused host_materialized=false published=false GPU=not_run\n' for r in records))
    return len(records)


def main():
    """Use authenticated fixtures and the caller's frozen compiler snapshot."""
    p=argparse.ArgumentParser(description=__doc__)
    for name in ('build','source','parent','output'):p.add_argument('--'+name,type=Path,required=True)
    a=p.parse_args();print('standalone_refusals='+str(review(a.build,a.source,a.parent,a.output)))


if __name__=='__main__':main()
