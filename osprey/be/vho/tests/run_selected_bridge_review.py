"""M5 selected High -> Mid -> Low -> native PTX acceptance.

Uses actual M4 selected images and saved expanded FP64 oracles. CUDA emitted
from Low is executed with the existing host collective simulator. PTX assembly
and device linking are separate evidence; neither is GPU numerical execution.
Contract: doc/AI-COMPILER-SEARCH-M5-DEVICE-BRIDGE-v1.0.md.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
from mla_kernel_review import runner
from tensor_schedule_review import explanation_blocks
from validate_mla_gpu import check_output
sys.path.insert(0,str(Path(__file__).parents[2]/'lno/tests'))
from high_observer_image_parity import compare_images


def review(args):
    """Retain every command, mapped boundary, raw diff and oracle result."""
    out=args.output.resolve();out.mkdir(parents=True,exist_ok=False);(out/'logs').mkdir()
    tool_root=args.be.parent.parent
    env=dict(os.environ,LD_LIBRARY_PATH=':'.join(map(str,[args.be.parent,tool_root/'wopt',tool_root/'lno'])))
    commands=[];report=dict(status='RUNNING',cases={},source_refusals=[],worker_refusals=[],stage_refusals=[],gpu_execution=False)
    retained=None
    if args.reuse_execution:
        root=args.reuse_execution.resolve()
        for line in (root/'SHA256SUMS').read_text().splitlines():
            expected,name=line.split('  ',1);path=(root/name).resolve();path.relative_to(root)
            assert hashlib.sha256(path.read_bytes()).hexdigest()==expected,name
        retained=json.loads((root/'report.json').read_text());assert retained['status']=='PASS'
        report['retained_execution_manifest_sha256']=hashlib.sha256((root/'SHA256SUMS').read_bytes()).hexdigest()

    def run(label,argv,success=True,timeout=600):
        """Check status only after preserving diagnostics and exact invocation."""
        p=subprocess.run(list(map(str,argv)),cwd=out,env=env,capture_output=True,text=True,timeout=timeout)
        for field in ['stdout','stderr']:(out/'logs'/(label+'.'+field)).write_text(getattr(p,field))
        commands.append(dict(label=label,argv=list(map(str,argv)),returncode=p.returncode))
        (out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        assert (p.returncode==0)==success,(label,p.returncode,p.stderr[-4000:]);return p

    def digest(path):
        """Bind source preservation to complete binary bytes."""
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def tokens(path):
        """Compare C/C++ lexical tokens, preserving strings and multi-char operators.

        This permits comment-only provenance additions, never arithmetic or
        launch changes, when reusing expensive already-certified host execution.
        """
        pattern=r'/\*[\s\S]*?\*/|//[^\n]*|"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'|\w+|>>=|<<=|->\*|\.\.\.|::|==|!=|<=|>=|&&|\|\||<<|>>|\+\+|--|->|\+=|-=|\*=|/=|%=|&=|\|=|\^=|##|[^\s]'
        return [token for token in re.findall(pattern,path.read_text()) if not token.startswith(('/*','//'))]

    # The final ordinary driver/LNO must preserve the already-certified M4
    # selected and estimation outputs. Reuse the established native ABI-aware
    # comparator; retain its raw differences and explicit padding classification.
    report['backend_parity']={};tools=args.be.parent.parent
    for context in ['short','long']:
        for mode in ['estimate','selected']:
            label='current-'+context+'-'+mode;base=out/label
            argv=[args.be,'-fB,'+str(args.previous/'controls'/(context+'.input.B')),
                '-fo,'+str(base)+'.o','-ft,'+str(base)+'.phase.t',
                '-PHASE:l=on:w=off:c=off:lpath='+str(tools/'lno')+':wpath='+str(tools/'wopt'),
                '-O3','-DSL:ai_opt_trace=on','-tr25','-tt25:1','-OPT:IEEE_arith=3:roundoff=2',
                '-LNO:high_search=on:high_search_bindings='+str(args.previous/'controls'/(context+'.v2.json'))]
            if mode=='selected':argv+=['-LNO:high_apply=on']
            argv+=[Path('/src/osprey/torch2whirl/python/tests/models/absorbed_mla.py')]
            run(label,argv)
            trace=base.with_suffix('.phase.t').read_text()
            assert ('AI_HIGH_APPLY 1 COMMITTED' in trace)==(mode=='selected')
            abi={k:int(v) for k,v in re.findall(r'(\w+)=(\d+)',re.search(r'AI_HIGH_SEARCH 1 IMAGE_ABI (.*)',trace)[1])}
            comparison=compare_images(base.with_suffix('.N'),args.application/(context+'-'+mode+'.N'),abi)
            (out/(label+'.parity.json')).write_text(json.dumps(comparison,indent=2)+'\n');report['backend_parity'][label]=comparison
            run(label+'-reopen',[args.b2a,'-st','-src',base.with_suffix('.N'),base.with_suffix('.T')])

    cases=[('short','short-selected','short'),('long','long-selected','long')]
    cases += [('variant-'+str(i),'variant-'+str(i),'long') for i in [1,2,9,10,11]]
    for label,source_name,context in cases:
        case=out/label;case.mkdir()
        source=out/('current-'+context+'-selected.N') if label in ['short','long'] else args.application/(source_name+'.after.B')
        before=digest(source)
        prefix=case/'selected';recipe=args.previous/'observer'/(context+'.recipe.json')
        p=run(label+'-bridge',[args.bridge,'bridge',source,recipe,prefix])
        assert 'search_invocations=0 source_unchanged=true' in p.stdout
        assert digest(source)==before
        selected=json.loads(prefix.with_suffix('.selected.json').read_text())
        admission=json.loads(prefix.with_suffix('.admission.json').read_text())
        assert admission['source_unchanged'] and admission['search_invocations']==0
        boundaries={}
        for level in ['high','mid','low']:
            image=case/('selected.'+level+'.B');trace=image.with_suffix('.T')
            run(label+'-'+level+'-audit',[args.bridge,'bridge-audit',image,level])
            run(label+'-'+level+'-reopen',[args.b2a,'-st','-src',image,trace]);boundaries[level]=trace
        for first,last in [('high','mid'),('mid','low')]:
            diff=''.join(difflib.unified_diff(boundaries[first].read_text().splitlines(True),boundaries[last].read_text().splitlines(True)))
            (case/(first+'-'+last+'.diff')).write_text(diff);assert diff
        high=boundaries['high'].read_text();mid=boundaries['mid'].read_text();low=boundaries['low'].read_text()
        assert 'DO_LOOP' in high and 'ARRAY' in high and 'selected_thread_owner' in high
        assert re.search(r'^\s*(?:U8|I8)?ARRAY\b',mid,re.M) is None
        assert re.search(r'^\s*DO_LOOP\b',mid,re.M) is None
        assert 'selected_thread_owner' in mid and 'threadIdx' in low and 'TRUEBR' in mid and 'LABEL' in mid
        replay=case/'reopened.ptx'
        run(label+'-ptx-reopen',[args.bridge,'bridge-ptx',case/'selected.low.B',replay])
        assert replay.read_bytes()==(case/'selected.ptx').read_bytes()
        ptx=replay.read_text();assert 'shfl.sync.down.b32' in ptx and 'ex2.approx' not in ptx and 'lg2.approx' not in ptx
        assert '// ' in ptx and '.target sm_90' in ptx
        assert admission['source_comment'] in ptx
        flags=['-arch=sm_90','--fmad=false']
        run(label+'-assemble',[args.cuda/'bin/ptxas',*flags,'-v','-c',replay,'-o',case/'native.o'])
        run(label+'-math',[args.cuda/'bin/nvcc','-std=c++17',*flags,'--ftz=false','--prec-div=true','--prec-sqrt=true',
            '-dc',case/'selected.math.cu','-o',case/'math.o'])
        run(label+'-device-link',[args.cuda/'bin/nvlink','-arch=sm_90',case/'native.o',case/'math.o','-o',case/'native.cubin'])
        manifest=case/'launch.json'
        run(label+'-launch',[args.bridge,'bridge-audit',case/'selected.low.B','low',manifest])
        launch=json.loads(manifest.read_text());expected=5 if context=='short' else 6
        assert len(launch['kernels'])==expected
        run(label+'-cuda',[args.w2c,'-cuda','-TARG:abi=n64','-O0','-CLIST:dotc_file='+str(case/'kernels.cu'),
            '-CLIST:doth_file='+str(case/'kernels.h'),'-ft,'+str(case/'kernels.w2c.phase.t'),case/'selected.low.B'])
        assert admission['source_comment'] in (case/'kernels.cu').read_text()
        run(label+'-readable',[args.lowerer,'schedule-source',case/'selected.selected.json',case/'optimized.cu'])
        assert explanation_blocks((case/'kernels.cu').read_text())==explanation_blocks((case/'optimized.cu').read_text())
        run(label+'-cuda-parity-compile',[args.cuda/'bin/nvcc','-std=c++17',*flags,'--ftz=false','--prec-div=true','--prec-sqrt=true',
            '-dc',case/'kernels.cu','-o',case/'cuda-parity.o'])
        shutil.copyfile(Path(__file__).parent/'tensor_collective_stub/cuda_runtime.h',case/'cuda_runtime.h')
        checks={};references=sorted((args.previous/'fixtures/selected'/context).glob('*.reference.json'))
        if label.startswith('variant-'):
            references=[x for x in references if x.name in ['all-masked.reference.json','tail-only.reference.json']]
        assert references
        if retained is not None:
            prior=args.reuse_execution/label
            assert json.loads((prior/'launch.json').read_text())==launch
            for source_file in ['kernels.cu','optimized.cu']:
                assert tokens(prior/source_file)==tokens(case/source_file),(label,source_file)
            # Independently map both native trees; comparison mode is not an
            # admission bypass. Current Low already passed its complete key.
            for name,image in [('prior',prior/'selected.low.B'),('current',case/'selected.low.B')]:
                run(label+'-'+name+'-executable-key',[args.bridge,'bridge-code-key',image,case/(name+'.code-key.json')])
            assert (case/'prior.code-key.json').read_bytes()==(case/'current.code-key.json').read_bytes(),label
            for name in ['host','reverse']:
                for reference in references:
                    variant=reference.name[:-len('.reference.json')];binary=prior/(variant+'.'+name+'.bin')
                    checks[variant+'-'+name]=check_output(reference,binary)
                    if name=='reverse':assert binary.read_bytes()==(prior/(variant+'.host.bin')).read_bytes()
            assert checks==retained['cases'][label]['host_oracles']
            (case/'execution-reuse.json').write_text(json.dumps(dict(status='PASS',
                native_executable_key_equal=True,cuda_tokens_equal=True,launch_manifest_equal=True,
                retained_case=str(prior),oracle_results=checks),indent=2)+'\n')
        else:
            for name,reverse in [('host',False),('reverse',True)]:
                (case/(name+'.cxx')).write_text(runner(launch,False,reverse=reverse))
                run(label+'-'+name+'-compile',['g++','-std=c++17','-O1','-pthread','-ffp-contract=off','-fno-fast-math',
                    '-I'+str(case),case/(name+'.cxx'),'-o',case/name])
                for reference in references:
                    variant=reference.name[:-len('.reference.json')];binary=case/(variant+'.'+name+'.bin')
                    run(label+'-'+name+'-'+variant,[case/name,reference.parent/(variant+'.inputs.bin'),binary])
                    checks[variant+'-'+name]=check_output(reference,binary)
                    if reverse:assert binary.read_bytes()==(case/(variant+'.host.bin')).read_bytes()
        report['cases'][label]=dict(status='PASS',width=selected['split_width'],policy=selected.get('fixed_chunk_policy',1),
            blocks=[k['block_x'] for k in launch['kernels']],source_sha256=before,
            native_ptx_assembled=True,native_ptx_linked=True,cuda_parity_compiled=True,host_oracles=checks,
            host_execution='retained under exact native/code/launch equality' if retained is not None else 'fresh')
        (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')

    # Mutation controls are absent from production. The test executable changes
    # actual native nodes/tables after mapped reopen, before the real verifier.
    short=args.application/'short-selected.after.B';recipe=args.previous/'observer/short.recipe.json'
    source_digest=digest(short)
    for gate in ['mid_gate','low_gate','ptx_gate']:
        prefix=out/('refused-stage-'+gate)
        p=run('stage-refusal-'+gate,[args.test_bridge,'bridge',short,recipe,prefix,'--test-fault',gate],False)
        assert 'bridge_stale_level_key' in p.stderr
        assert prefix.with_suffix('.high.B').is_file() and not prefix.with_suffix('.low.B').exists() and not prefix.with_suffix('.ptx').exists()
        if gate=='mid_gate':assert not prefix.with_suffix('.mid.B').exists()
        else:assert prefix.with_suffix('.mid.B').is_file()
        assert digest(short)==source_digest;report['stage_refusals'].append(gate)
    for fault in ['tree','comment','abi','width','policy','resources','model','controls','numerical','scratch','source','facts']:
        prefix=out/('refused-source-'+fault)
        p=run('source-refusal-'+fault,[args.test_bridge,'bridge',short,recipe,prefix,'--test-fault',fault],False)
        assert 'stale selected source/key/COMMENT' in p.stderr
        assert not list(out.glob(prefix.name+'.*'));report['source_refusals'].append(fault)
    for level in ['high','mid','low']:
        faults=['comment','abi','barrier','block_x','grid_x','shared_bytes','attention_layout','split_width',
            'mask','collective_abi','argument','shared_bound','coordinate_type','extra_search']
        if level=='high':faults+=['tree']
        for fault in faults:
            p=run(level+'-refusal-'+fault,[args.test_bridge,'bridge-audit',out/'short'/('selected.'+level+'.B'),level,'--test-fault',fault],False)
            assert 'bridge_stale_level_key' in p.stderr
            report['worker_refusals'].append(level+'/'+fault)
        wrong='mid' if level!='mid' else 'high'
        run(level+'-wrong-level',[args.bridge,'bridge-audit',out/'short'/('selected.'+level+'.B'),wrong],False)
    for level in ['high','mid']:
        p=run('cuda-refusal-'+level,[args.w2c,'-cuda','-TARG:abi=n64','-O0',
            '-CLIST:dotc_file='+str(out/(level+'.rejected.cu')),'-CLIST:doth_file='+str(out/(level+'.rejected.h')),
            out/'short'/('selected.'+level+'.B')],False)
        assert 'CUDA-only emission of verified Low WHIRL' in p.stderr
    run('unsealed-source',[args.bridge,'bridge',args.application/'short-selected.before.B',recipe,out/'unsealed'],False)
    run('wrong-recipe',[args.bridge,'bridge',short,args.previous/'observer/long.recipe.json',out/'wrong-recipe'],False)
    run('production-no-faults',[args.bridge,'bridge',short,recipe,out/'production-fault','--test-fault','tree'],False)
    report.update(status='PASS',host_reference_executions=sum(len(x['host_oracles']) for x in report['cases'].values()),
        native_configurations=len(cases),source_preserved=True,estimator_invocations=0,
        reused_reference_executions=0 if retained is None else sum(len(x['host_oracles']) for x in report['cases'].values()))
    (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    (out/'SHA256SUMS').write_text(''.join(digest(p)+'  '+str(p.relative_to(out))+'\n' for p in sorted(out.rglob('*')) if p.is_file()))
    print(json.dumps(report,indent=2))


def main():
    """Require explicit tool, source, oracle and publication roots."""
    parser=argparse.ArgumentParser(description=__doc__)
    for name in ['bridge','test-bridge','be','b2a','w2c','lowerer','cuda','application','previous','output']:
        parser.add_argument('--'+name,type=Path,required=True)
    parser.add_argument('--reuse-execution',type=Path)
    review(parser.parse_args())


if __name__=='__main__':main()
