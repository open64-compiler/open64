"""Actual be request/checkpoint integration, mapped/CUDA/host review and refusals.

Design: doc/AI-COMPILER-LAYERNORM-DRIVER-REQUEST-v0.1.md.
Fresh outputs only, offline toolchain only, no GPU execution claim.
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

from normalization_plan_trace_report import require
from normalization_request_trace_report import reports, refusal_reports, audit as request_audit
from normalization_lower_host import adapter, datasets, protocol, audit_output

PRODUCTS=('ir_tools/dsl_normalization_request_test','ir_tools/ir_b2a','be/be','be/be.so','wopt/wopt.so',
          'whirl2c/whirl2c','whirl2c/whirl2c.so')


def review(build, source, cuda, root, probe=False):
    """Exercise real backend dispatch, not a fixture imitation of driver policy."""
    root.mkdir(parents=True,exist_ok=False);commands=[];records=[]
    binaries=root/'binaries'
    for relative in PRODUCTS:
        target=binaries/relative;target.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(build/relative,target)
    (binaries/'whirl2c/whirl2c_be').symlink_to('../be/be')
    (binaries/'whirl2c/be.so').symlink_to('../be/be.so')
    build=binaries
    env=dict(os.environ,LD_LIBRARY_PATH=str(build/'be')+':'+str(build/'wopt'),PYTHONDONTWRITEBYTECODE='1',SOURCE_DATE_EPOCH='1793131200')

    def run(case,label,args,success=True,input_text=None):
        """Retain exact command, return code and streams before making assertions."""
        args=list(map(str,args));p=subprocess.run(args,cwd=case,env=env,input=input_text,text=True,capture_output=True)
        log=case/'logs';log.mkdir(exist_ok=True)
        for field in ('stdout','stderr'):(log/(label+'.'+field)).write_text(getattr(p,field))
        if input_text is not None:(log/(label+'.stdin')).write_text(input_text)
        commands.append(dict(argv=args,cwd=str(case),label=label,returncode=p.returncode))
        (root/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        require((p.returncode==0)==success,label+': '+p.stderr[-3000:]);return p

    def backend(case,label,request,output,arch='sm_90',level=3,extra=(),success=True):
        """Explicit options establish compiler scope; no default profile is supplied."""
        return run(case,label,[build/'be/be','-fB,'+str(case/'source.B'),'-ft,'+str(case/(label+'.t')),
            '-O'+str(level),'-TARG:abi=n64','-OPT:roundoff=0:IEEE_arith=1:fast_math=off',
            '-PHASE:wpath='+str(build/'wopt'),'-DSL:gpu_arch='+arch,
            '-DSL:normalization_request='+str(request),'-DSL:normalization_output='+str(output)]+list(extra)+
            [source/'osprey/be/opt/tests/dsl_normalization_ref_test.cxx'],success)

    cases=(('single-row',1),('single-row',2),('single-row',3),('default',2),('mixed',2),
           ('shared-affine',2),('subnormal',2),('feature-tail',2),('multiple-pu',2))
    for arch in (('sm_90',) if probe else ('sm_90','sm_90a','sm_100','sm_100a')):
        for variant,scenario in (cases[:3] if probe else cases):
            case=root/f'{arch}-{variant}-{scenario}';case.mkdir();request=case/'request.json';output=case/'selected.after.B'
            producer=run(case,'producer',[build/'ir_tools/dsl_normalization_request_test',case/'source.B',request,arch,variant,scenario])
            original=(case/'source.B').read_bytes();backend(case,'driver',request,output,arch)
            manifest=json.loads((case/'selected.after.B.manifest.json').read_text())
            run(case,'reopen',[build/'ir_tools/dsl_normalization_request_test','reopen',output,case/'selected.after.B.manifest.json',case/'reopen.t'])
            for stem in ('source','selected.before','selected.after'):
                run(case,'dump-'+stem,[build/'ir_tools/ir_b2a','-st','-src',case/(stem+'.B'),case/(stem+'.T')])
            before=(case/'selected.before.T').read_text();after=(case/'selected.after.T').read_text()
            (case/'selected.before-after.diff').write_text(''.join(difflib.unified_diff(before.splitlines(True),after.splitlines(True),fromfile='before.T',tofile='after.T')))
            require('OPR_DSLMEANVARNORM' in after and '__global__' not in after,'analysis source provenance lost')
            # Every pre-kernel analysis PU is byte-for-byte identical in both full dumps.
            boundary=re.search(r'^FUNC_ENTRY <[^>]*,'+re.escape(manifest['kernel'])+r'>',before,re.M)
            require(boundary is not None,'empty driver kernel absent from before image')
            require(before[:boundary.start()] in after,'source PU changed during device materialization')
            run(case,'cuda',[build/'whirl2c/whirl2c','-cuda','-CLIST:dotc_file='+str(case/'kernel.cu'),'-fB,'+str(output),
                source/'osprey/be/opt/tests/dsl_normalization_ref_test.cxx'])
            record=reports((case/'driver.t').read_text(),manifest,(case/'reopen.t').read_text(),(case/'kernel.cu').read_text(),case/'reports')
            flags=['-std=c++11','-arch='+arch,'--fmad=false','--ftz=false','--prec-div=true','--prec-sqrt=true']
            run(case,'ptx',[cuda/'bin/nvcc']+flags+['-ptx','kernel.cu','-o','kernel.ptx'])
            p=run(case,'cubin',[cuda/'bin/nvcc']+flags+['-cubin','kernel.cu','-o','kernel.cubin','-Xptxas=-v'])
            registers=re.search(r'Used (\d+) registers',p.stderr);shared=re.search(r'(\d+) bytes smem',p.stderr)
            require(registers is not None and int(registers[1])<=manifest['request']['registers_per_thread'][manifest['selected_kind']-1],
                    'compiled RF exceeds planning reservation; recost required')
            require((int(shared[1]) if shared else 0)==record['guard']['reserved_shared'],'compiled shared differs from selected plan')
            (case/'host.cxx').write_text(adapter((case/'kernel.cu').read_text(),record));sets=datasets(record)
            run(case,'host-build',['g++','-std=c++11','-O2','-fno-fast-math','-ffp-contract=off','-frounding-math','host.cxx','-o','host'])
            host=run(case,'host',[case/'host'],input_text=protocol(record,sets));checks=audit_output(record,sets,host.stdout)
            quiet=case/'quiet.after.B';backend(case,'quiet',request,quiet,arch,extra=('-DSL:ai_opt_trace=off',))
            require('AI_NORM' not in (case/'quiet.t').read_text(),'trace-off emitted AI trace')
            for a,b in ((output,quiet),(case/'selected.before.B',case/'quiet.before.B'),
                        (case/'selected.after.B.manifest.json',case/'quiet.after.B.manifest.json')):
                require(a.read_bytes()==b.read_bytes(),'trace-off changed code or selected binding')
            require((case/'source.B').read_bytes()==original,'driver overwrote input image')
            if arch=='sm_90' and variant=='single-row' and scenario==2:
                space=case/'space dir';space.mkdir();spaced=space/'selected.after.B'
                backend(case,'space-path',request,spaced,arch)
                require(spaced.read_bytes()==output.read_bytes(),'path whitespace changed mapped code')
                request_audit((case/'space-path.t').read_text(),manifest,(case/'reopen.t').read_text(),(case/'kernel.cu').read_text())
                require('%20' in (case/'space-path.t').read_text(),'publication path breaks positional trace columns')
            compile_log='# What it summarizes: offline CUDA/PTX/cubin acceptance and compiled resource admission, not GPU execution.\n'
            compile_log+=f'arch={arch} RF={registers[1]} shared={int(shared[1]) if shared else 0} flags={" ".join(flags)} GPU=not_run\n'
            numerical='# What it summarizes: emitted-code host adapter versus independent exact-rational LN-REF-1 oracle.\n'
            numerical+=f'datasets={len(sets)} output_checks={checks} canaries=preserved GPU=not_run\n'
            for name,value in (('compile.log',compile_log),('numerical.log',numerical)):
                (case/'reports'/name).write_text(value)
                with (case/'reports/optimization.log').open('a') as f:f.write(value)
            records.append(dict(case=case.name,selected=manifest['selected_kind'],host_cases=len(sets),output_checks=checks,
                                host_trap_negative='unsupported_emulator_readback' if 'sse_trap_mask_change UNSUPPORTED' in producer.stdout else 'refused',
                                image_sha256=hashlib.sha256(output.read_bytes()).hexdigest()))

    # Driver refusals use an otherwise complete source/request and fresh names.
    base=root/'sm_90-single-row-2';template=json.loads((base/'request.json').read_text());negative=root/'refusals';negative.mkdir()
    (negative/'source.B').write_bytes((base/'source.B').read_bytes());rejects=[]
    mutations={
        'stale-source':lambda r:r['source'].__setitem__('epsilon_bits',r['source']['epsilon_bits']+1),
        'wrong-profile-arch':lambda r:r['profile'].__setitem__('arch_id',0),
        'stale-revision':lambda r:r['profile'].__setitem__('description_revision',999),
        'unknown-model':lambda r:r['profile'].__setitem__('model_id',0),
        'budget':lambda r:r.__setitem__('max_jobs',1),
        'alias-output':lambda r:r['invocation']['ranges'][3].__setitem__('allocation_id',r['invocation']['ranges'][0]['allocation_id']),
        'capacity':lambda r:r['invocation']['ranges'][0].__setitem__('allocation_bytes',1),
        'context':lambda r:r['invocation'].__setitem__('context_id',0),
        'unaligned':lambda r:r['invocation']['ranges'][0].__setitem__('offset_bytes',1),
        'bad-threads':lambda r:r['invocation'].__setitem__('threads_per_cta',33),
        'toolkit':lambda r:r['target'].__setitem__('toolkit',1),
        'route':lambda r:r['target'].__setitem__('route',99),
        'fp-environment':lambda r:r['target'].__setitem__('environment',0),
        'fp-options':lambda r:r['target'].__setitem__('fp_options',1),
        'rf-zero':lambda r:r.__setitem__('registers_per_thread',[32,0,32]),
        'missing-key':lambda r:r.pop('margin'),
        'unknown-key':lambda r:r.__setitem__('unknown',0),
        'nested-unknown-key':lambda r:r['target'].__setitem__('unknown',0),
        'fraction':lambda r:r.__setitem__('max_jobs',1.5),
        'boolean':lambda r:r.__setitem__('max_jobs',True),
        'negative':lambda r:r.__setitem__('max_jobs',-1),
        'uint64-overflow':lambda r:r.__setitem__('max_jobs',2**64),
        'uint32-overflow':lambda r:r['source'].__setitem__('node_id',2**32),
        'extra-range':lambda r:r['invocation']['ranges'].append(r['invocation']['ranges'][0]),
    }
    for field in range(19):
        def missing(r,f=field):
            """Unknown is not zero-cost; remove one complete profile fact."""
            r['profile']['fields'][f].update(provenance=0,bounds=[0,0,0])
        mutations['unknown-field-'+str(field)]=missing
    for name,change in mutations.items():
        request=copy.deepcopy(template);change(request);path=negative/(name+'.json');path.write_text(json.dumps(request))
        output=negative/(name+'.B');backend(negative,name,path,output,success=False)
        require(not any(p.exists() for p in (output,negative/(name+'.before.B'),Path(str(output)+'.manifest.json'))),'refusal published artifacts')
        rejects.append(name)
    malformed={'truncated':b'{','duplicate':b'{"schema":"open64.layernorm.request.v1",'+(base/'request.json').read_bytes()[1:],
               'nul':(base/'request.json').read_bytes()+b'\0','utf8':b'{"x":"\xff"}',
               'deep':b'['*64+b'0'+b']'*64,'oversized':b' '*1048577}
    for name,payload in malformed.items():
        path=negative/(name+'.json');path.write_bytes(payload);backend(negative,name,path,negative/(name+'.B'),success=False);rejects.append(name)
    for level in (0,1,2):
        name='O'+str(level);backend(negative,name,base/'request.json',negative/(name+'.B'),level=level,success=False);rejects.append(name)
    for name,options in (('wrong-gpu',('-DSL:gpu_arch=sm_100',)),('wrong-abi',('-TARG:abi=n32',)),
                         ('unsafe-math',('-OPT:fast_math=on',)),('unsafe-roundoff',('-OPT:roundoff=2',)),
                         ('unsafe-ieee',('-OPT:IEEE_arith=3',)),('conflict',('-DSL:cuda_whirl_output=/out/unwanted.B',))):
        backend(negative,name,base/'request.json',negative/(name+'.B'),extra=options,success=False);rejects.append(name)
    original=(base/'selected.after.B').read_bytes();backend(base,'existing-output',base/'request.json',base/'selected.after.B',success=False)
    require((base/'selected.after.B').read_bytes()==original,'existing image replaced');rejects.append('existing-output')
    for name,suffix in (('existing-before','.before.B'),('existing-manifest','.after.B.manifest.json')):
        output=negative/(name+'.after.B');occupied=negative/(name+suffix);occupied.symlink_to('missing-target')
        backend(negative,name,base/'request.json',output,success=False)
        require(occupied.is_symlink() and not output.exists(),'dangling symlink replaced during publication');rejects.append(name)
    selected_args=[build/'be/be','-fB,'+str(base/'selected.after.B'),'-ft,'+str(base/'selected-input.t'),'-O3','-TARG:abi=n64',
        '-OPT:roundoff=0:IEEE_arith=1:fast_math=off','-PHASE:wpath='+str(build/'wopt'),'-DSL:gpu_arch=sm_90',
        '-DSL:normalization_request='+str(base/'request.json'),'-DSL:normalization_output='+str(base/'unwanted.after.B'),
        source/'osprey/be/opt/tests/dsl_normalization_ref_test.cxx']
    run(base,'selected-input',selected_args,False);require(not (base/'unwanted.after.B').exists(),'already-selected input reoptimized')
    rejects.append('selected-input')
    for variant,scenario in (('legacy',1),('single-row',0),('unmanaged-formals',2),('unsupported-aux',2)):
        case=root/('refuse-'+variant);case.mkdir();request=case/'request.json'
        run(case,'producer',[build/'ir_tools/dsl_normalization_request_test',case/'source.B',request,'sm_90',variant,scenario])
        backend(case,'driver',request,case/'selected.B',success=False);rejects.append(variant)
        if variant in ('unmanaged-formals','unsupported-aux'):
            diagnostic=(case/'logs/driver.stderr').read_text()
            require(('formal coverage' if variant=='unmanaged-formals' else 'auxiliary PU subsection') in diagnostic,
                    'input boundary refused for an unintended reason')
    (root/'audit.json').write_text(json.dumps(dict(records=records,refusals=rejects,GPU=False),indent=2)+'\n')
    (root/'optimization-summary.log').write_text('# What it summarizes: actual backend request binding, selected kernels, mapped verification, offline resources, host numerical checks and refusal coverage.\n'+
        ''.join(f"{r['case']} C{r['selected']-1} mapped=verified offline_compile=passed host_checks={r['output_checks']} GPU=not_run\n" for r in records)+
        ''.join(f'{name} request=refused selected=false materialized=false\n' for name in rejects)+
        f'refusals={len(rejects)} v1=preserved reduction_dependency=false production_runtime_adapter=missing\n')
    (root/'optimization.log').write_text('# What it summarizes: full successful request/cost/transformation/compile/host phase report collection.\n'+
        ''.join((root/r['case']/'reports/optimization.log').read_text() for r in records))
    require(refusal_reports(root,commands)==len(rejects),'refusal report coverage differs from actual failed commands')
    return records


if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('--build',type=Path,required=True);p.add_argument('--source',type=Path,required=True)
    p.add_argument('--cuda',type=Path,required=True);p.add_argument('--output',type=Path,required=True);p.add_argument('--probe',action='store_true')
    a=p.parse_args();review(a.build,a.source,a.cuda,a.output,a.probe)
