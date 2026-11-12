"""MR1 owned reuse proofs, isolated estimation and retained G=1 correctness.

Run only in a fresh artifact directory with explicit native tools, the prior
cache-trait receipt and an isolated test-only LNO. No GPU work is performed.
"""
import argparse
import difflib
import hashlib
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys

from run_high_observer_review import binding, ledger, audit_objects
from run_common_high_review import execute, check_output


def authenticate(root):
    """Verify the retained receipt and all cached artifacts before use."""
    for line in (root/'SHA256SUMS').read_text().splitlines():
        digest,name=line.split('  ',1)
        assert hashlib.sha256((root/name).read_bytes()).hexdigest()==digest,name
    receipt=json.loads((root/'receipt.json').read_text())
    assert hashlib.sha256((root/receipt['primary_artifact']).read_bytes()).hexdigest()==receipt['primary_sha256']


def produce_extra(root,length):
    """Capture B=2/H=4 with unchanged source contracts and unequal head inputs."""
    import torch
    from open64_dsc import export_to_whirl, save_as_whirl, WhirlExportOptions
    from models.absorbed_mla import make_fixture
    from models.mla_reference import expanded_reference
    root.mkdir(parents=True,exist_ok=False)
    model,original=make_fixture('short',length=length)
    heads=torch.tensor([0,1,2,0])
    inputs=(original[0].index_select(1,heads).repeat(2,1,1),
            original[1].index_select(1,heads).repeat(2,1,1),
            original[2].repeat(2,1,1),original[3].repeat(2,1,1),
            original[4].index_select(0,heads),original[5].index_select(0,heads),
            torch.ones(2,4,length,dtype=torch.bool))
    inputs[-1][1,3,:]=False
    module=export_to_whirl(model,inputs,WhirlExportOptions(backend='native'))
    save_as_whirl(module,str(root/'source.B'))
    assert len(module.graph_operators)==46
    (root/'census.json').write_text(json.dumps(dict(batch=2,heads=4,length=length,latent=16,rotary=4,operators=46,
        scope='existing admitted tensor-expression contracts; correctness fixture'),indent=2)+'\n')
    (root/'inputs.bin').write_bytes(b''.join(x.numpy().tobytes() for x in inputs))
    expected=expanded_reference(*(x.tolist() for x in inputs),model.alpha)
    values=[]
    for value in expected:
        flat=torch.tensor(value,dtype=torch.float64).flatten().tolist()
        values.append([x if math.isfinite(x) else ('-inf' if x<0 else '+inf') for x in flat])
    reference=root/'reference.json'
    reference.write_text(json.dumps(dict(schema='open64.mla-output-oracle.v1',values=values,rtol=2e-5,atol=2e-6),indent=2)+'\n')
    with torch.no_grad(): actual=model(*inputs)
    binary=root/'source.outputs.bin';binary.write_bytes(b''.join(x.numpy().tobytes() for x in actual))
    check_output(reference,binary)


def complete_extra_artifacts(out,tools):
    """Preserve every extra source/High boundary and raw diff without rewriting B."""
    commands=[]
    env=dict(os.environ,LD_LIBRARY_PATH=':'.join(str(tools/x) for x in ('be','wopt','lno')))
    for case in ('extra','multi-store'):
        root=out/case
        for name in ('source.B','before.B','high.B'):
            image=root/name;before=hashlib.sha256(image.read_bytes()).hexdigest()
            argv=[str(tools/'ir_tools/ir_b2a'),'-st','-src',str(image),str(image.with_suffix('.T'))]
            p=subprocess.run(argv,env=env,capture_output=True,text=True)
            (root/(name+'.reopen.stdout')).write_text(p.stdout)
            (root/(name+'.reopen.stderr')).write_text(p.stderr)
            assert p.returncode==0,p.stderr
            assert hashlib.sha256(image.read_bytes()).hexdigest()==before
            commands.append(dict(argv=argv,returncode=p.returncode,input_sha256=before))
        (root/'lowering.diff').write_text(''.join(difflib.unified_diff(
            (root/'before.T').read_text().splitlines(True),(root/'high.T').read_text().splitlines(True))))
        assert (root/'lowering.diff').stat().st_size>0
    (out/'extra-reopen-commands.json').write_text(json.dumps(commands,indent=2)+'\n')


def review(args):
    """Retain native proofs, typed records, raw IR equality and independent oracles."""
    authenticate(args.previous)
    out=args.output.resolve();out.mkdir(parents=True,exist_ok=False);(out/'logs').mkdir()
    env=dict(os.environ,LD_LIBRARY_PATH=':'.join(str(args.tools/x) for x in ('be','wopt','lno')))
    env.pop('OPEN64_HIGH_SEARCH_BINDINGS',None);env.pop('OPEN64_HIGH_REUSE_TEST',None)
    commands=[]
    def run(label,argv,fault=None):
        """Record exact command/results; faults exist only in the test component."""
        local=dict(env)
        if fault:local['OPEN64_HIGH_REUSE_TEST']=fault
        p=subprocess.run(list(map(str,argv)),env=local,cwd=out,capture_output=True,text=True,timeout=600)
        for stream in ('stdout','stderr'):(out/'logs'/(label+'.'+stream)).write_text(getattr(p,stream))
        commands.append(dict(label=label,argv=list(map(str,argv)),fault=fault,returncode=p.returncode))
        (out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        assert p.returncode==0,(label,p.stderr[-4000:])
        return p
    def backend(label,image,bindings,lno=None,fault=None,apply=False):
        """Execute the same active-PU boundary with production or test LNO."""
        prefix=out/label
        argv=[args.tools/'be/be','-fB,'+str(image),'-fo,'+str(prefix)+'.o','-ft,'+str(prefix)+'.phase.t',
              '-PHASE:l=on:w=off:c=off:lpath='+str((lno or args.tools/'lno/lno.so').parent)+
              ':wpath='+str(args.tools/'wopt'),'-O3','-DSL:partition_axis=on:ai_opt_trace=on',
              '-tr25','-tt25:1','-OPT:IEEE_arith=3:roundoff=2',
              '-LNO:high_search=on:high_apply='+('on' if apply else 'off')+':high_search_bindings='+str(bindings),
              Path(__file__).parents[3]/'torch2whirl/python/tests/models/absorbed_mla.py']
        run(label,argv,fault=fault)
        return prefix.with_suffix('.N'),prefix.with_suffix('.phase.t').read_text()
    def reopen(label,image):
        """Use a matching output stem and source/symbol interleave."""
        run(label,[args.tools/'ir_tools/ir_b2a','-st','-src',image,image.with_suffix('.T')])
        return image.with_suffix('.T').read_text()
    def rows(trace,kind):
        """Decode review records only; this is not a compiler input protocol."""
        prefix='AI_HIGH_REUSE 1 '+kind+' '
        return [dict(item.split('=',1) for item in line[len(prefix):].split()) for line in trace.splitlines() if line.startswith(prefix)]
    def assert_proof(trace,live_stage=True):
        """Require complete records and expiry, never silently accept unavailable facts."""
        assert 'AI_HIGH_REUSE 1 COLLECTED live=true' in trace
        assert 'AI_HIGH_REUSE 1 UNAVAILABLE' not in trace
        assert 'AI_HIGH_REUSE 1 OWNED_AFTER_CLEANUP live=false' in trace
        if live_stage:assert 'AI_HIGH_REUSE 1 EXPIRE_BEFORE_HOIST live=false' in trace
        assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in trace
        caches=rows(trace,'CACHE');assert len(caches)==2
        for row in caches:
            assert int(row['source_read_bytes'])==int(row['fused_logical_request_bytes'])+int(row['payload_bytes'])
            assert int(row['source_write_bytes'])==int(row['payload_bytes'])
        return dict(caches=caches,storages=rows(trace,'STORAGE'),reads=rows(trace,'READ'))
    # The previous review already owns the two canonical annotated High images.
    # Re-run its correctness/bridge suite with the current production components.
    native=out/'native'
    run('correctness-and-legacy',[sys.executable,Path(__file__).with_name('run_cache_trait_review.py'),
        '--baseline',args.previous/'baseline','--tools',args.tools,'--output',native])
    acceptance=json.loads((native/'report.json').read_text())
    assert sum(len(x['reference_checks'])+len(x['selected_reference_checks']) for x in acceptance['contexts'].values())==28
    report=dict(status='PASS',contexts={},gpu_execution=False,selected_host_reference_executions=28,
                object_isolation=audit_objects(args.objects,out/'logs'),extra={})
    expected_tests={'head_dependent_address','strided_address','offset_view','wrong_allocation_extent',
        'conditional_read_lifetime','symbolic_read_footprint','opaque_base','input_cache_write',
        'wrong_access_encoding','wrong_storage_encoding','reassigned_base_version','escaped_base_lifetime',
        'missing_storage_contract','possibly_aliased_write','missing_alias_answers','read_before_complete_producer',
        'unproved_transpose_value_flow','multiple_storage_versions','missing_unrolled_reduction_slice',
        'misaligned_unrolled_slice','missing_trait_graph_inference','equal_names_do_not_merge_storage',
        'traffic_overflow_clears_output','activation_ABA_rejected','binding_rebind_rejected',
        'manifest_rebind_rejected','producer_rebind_clears_output','all_owned_edits_isolated'}
    for ctx in ('short','long'):
        original_trace=(native/(ctx+'-observe.phase.t')).read_text()
        data=assert_proof(original_trace)
        _,tested=backend(ctx+'-owned-tests',native/(ctx+'.high.B'),native/(ctx+'.binding.json'),args.test_lno)
        tests=set(re.findall(r'AI_HIGH_REUSE_TEST PASS (\S+)',tested))
        assert expected_tests<=tests,(ctx,expected_tests-tests)
        assert_proof(tested)
        normal_image,normal=backend(ctx+'-normal',native/(ctx+'.high.B'),native/(ctx+'.binding.json'))
        declined_image,declined=backend(ctx+'-decline',native/(ctx+'.high.B'),native/(ctx+'.binding.json'),
                                         args.test_lno,fault='decline')
        assert 'AI_HIGH_REUSE 1 UNAVAILABLE reason=test_unproved_reuse fallback=existing_G1_search' in declined
        assert ledger(normal)==ledger(declined)
        assert reopen(ctx+'-normal-reopen',normal_image)==reopen(ctx+'-decline-reopen',declined_image)
        # Reuse facts do not enter old production seals or change the chosen G=1 body.
        _,selected=backend(ctx+'-selected-decline',native/(ctx+'.high.B'),native/(ctx+'.production.json'),
                           args.test_lno,fault='decline',apply=True)
        assert 'AI_HIGH_APPLY 1 COMMITTED' in selected
        legacy_trace=(native/(ctx+'-retained-legacy-observe.phase.t')).read_text()
        legacy=assert_proof(legacy_trace)
        assert all(c['provenance']=='closed-graph-derived' for c in legacy['caches'])
        assert ledger(original_trace)==ledger((args.previous/'native'/(ctx+'-observe.phase.t')).read_text())
        assert_proof((native/(ctx+'-selected.phase.t')).read_text(),live_stage=False)
        for suffix in ('.source.T','.high.T'):
            assert (native/(ctx+suffix)).read_bytes()==(args.previous/'native'/(ctx+suffix)).read_bytes()
        for phase in ('high.T','mid.T','low.T','ptx'):
            rel=Path(ctx+'-selected-device/pu-0/selected.'+phase)
            assert (native/rel).read_bytes()==(args.previous/'native'/rel).read_bytes()
        report['contexts'][ctx]=dict(data,owned_tests=sorted(tests),legacy_provenance=legacy['caches'],
            ledger_unchanged=True,declined_proof_native_T_equal=True,selected_G1_fallback=True,
            prior_T_and_PTX_equal=True)
    extra=out/'extra'
    run('extra-produce',[sys.executable,__file__,'--produce-extra',extra])
    lower=args.tools/'ir_tools/tensor_kernel_lower'
    run('extra-prepare',[lower,'prepare',extra/'source.B',extra/'recipe.json','--verify','3'])
    run('extra-high',[lower,'high',extra/'recipe.json',extra/'before.B',extra/'high.B',extra/'high.json'])
    reopen('extra-before-reopen',extra/'before.B');reopen('extra-high-reopen',extra/'high.B')
    _,probe=backend('extra-probe',extra/'high.B',native/'short.binding.json')
    proto=json.loads((native/'short.binding.json').read_text())
    manifest=json.loads((extra/'high.json').read_text())
    value=binding(manifest,probe,proto['gpu'],proto['model'])
    value['schema']='open64.lno-high-search.v2';value['search_controls']=proto['search_controls']
    bp=extra/'binding.json';bp.write_text(json.dumps(value,indent=2)+'\n')
    _,extra_trace=backend('extra-owned-tests',extra/'high.B',bp,args.test_lno)
    data=assert_proof(extra_trace)
    assert expected_tests<=set(re.findall(r'AI_HIGH_REUSE_TEST PASS (\S+)',extra_trace))
    c=extra/'high.c'
    run('extra-w2c',[args.tools/'whirl2c/whirl2c','-TARG:abi=n64','-O0',
        '-CLIST:dotc_file='+str(c),'-CLIST:doth_file='+str(extra/'high.h'),
        '-ft,'+str(extra/'w2c.phase.t'),extra/'high.B'])
    library=extra/'high.so'
    run('extra-cc',['gcc','-std=c99','-O2','-shared','-fPIC','-ffp-contract=off','-fno-fast-math',
        '-I'+str(args.tools/'include'),'-include',Path(__file__).with_name('common_high_host.h'),c,'-lm','-o',library])
    output=extra/'native.outputs.bin';output.write_bytes(execute(library,manifest,(extra/'inputs.bin').read_bytes()))
    data['reference_check']=check_output(extra/'reference.json',output)
    report['extra']=data
    # A benign unrolled multi-store producer is deliberately not a proved
    # single definition in v1. It must still compile and execute via G=1.
    small=out/'multi-store'
    run('small-produce',[sys.executable,__file__,'--produce-extra',small,'--extra-length','16'])
    run('small-prepare',[lower,'prepare',small/'source.B',small/'recipe.json','--verify','3'])
    run('small-high',[lower,'high',small/'recipe.json',small/'before.B',small/'high.B',small/'high.json'])
    _,probe=backend('small-probe',small/'high.B',native/'short.binding.json')
    sm=json.loads((small/'high.json').read_text());value=binding(sm,probe,proto['gpu'],proto['model'])
    value['schema']='open64.lno-high-search.v2';value['search_controls']=proto['search_controls']
    sb=small/'binding.json';sb.write_text(json.dumps(value,indent=2)+'\n')
    si,st=backend('small-decline',small/'high.B',sb,apply=True)
    assert 'AI_HIGH_REUSE 1 UNAVAILABLE reason=reuse_multi_store_producer_unproved fallback=existing_G1_search' in st
    assert 'AI_HIGH_APPLY 1 COMMITTED' in st and ledger(st)
    c=small/'selected.c'
    run('small-w2c',[args.tools/'whirl2c/whirl2c','-TARG:abi=n64','-O0',
        '-CLIST:dotc_file='+str(c),'-CLIST:doth_file='+str(small/'selected.h'),
        '-ft,'+str(small/'w2c.phase.t'),si])
    library=small/'selected.so'
    run('small-cc',['gcc','-std=c99','-O2','-shared','-fPIC','-ffp-contract=off','-fno-fast-math',
        '-I'+str(args.tools/'include'),'-include',Path(__file__).with_name('common_high_host.h'),c,'-lm','-o',library])
    binary=small/'native.outputs.bin';binary.write_bytes(execute(library,sm,(small/'inputs.bin').read_bytes()))
    report['multi_store_fallback']=dict(reason='reuse_multi_store_producer_unproved',selected_G1=True,
        reference_check=check_output(small/'reference.json',binary))
    complete_extra_artifacts(out,args.tools)
    (out/'reuse-summary.json').write_text(json.dumps(dict(schema='open64.mr1.reuse-review.v1',
        contexts={k:{field:v[field] for field in ('caches','storages','reads')} for k,v in report['contexts'].items()},
        extra=data,hardware_metrics='unknown; no GPU measurements'),indent=2)+'\n')
    (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    print('PASS: MR1 owned reuse, live invalidation/refusals, G=1 parity, 30 native host reference executions')


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--produce-extra',type=Path)
    parser.add_argument('--complete-artifacts',action='store_true')
    parser.add_argument('--extra-length',type=int,default=127)
    for name in ('previous','tools','objects','test-lno','output'):parser.add_argument('--'+name,type=Path)
    args=parser.parse_args()
    if args.produce_extra:produce_extra(args.produce_extra,args.extra_length)
    elif args.complete_artifacts:
        if not args.output or not args.tools:parser.error('artifact completion requires --output --tools')
        complete_extra_artifacts(args.output,args.tools)
    else:
        if not all((args.previous,args.tools,args.objects,args.test_lno,args.output)):
            parser.error('review requires --previous --tools --objects --test-lno --output')
        review(args)
