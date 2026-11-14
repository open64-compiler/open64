"""Focused native cache-trait ingestion, High lowering and fallback review.

Uses fixed retained reference inputs; no GPU measurements or candidate kernels
are executed. Only the selected production plan is materialized by that lane.
Artifacts are retained in a fresh output directory.
"""
import argparse
import copy
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys

from run_common_high_review import execute, check_output
from run_high_observer_review import binding


def produce(context, mode, image):
    """Export a fresh declared, undeclared or contradictory real FX model."""
    from open64_dsc import export_to_whirl, save_as_whirl, WhirlExportOptions
    from open64_dsc.tensor_traits import LATENT_KV_CACHE, ROTARY_KEY_CACHE
    from models.absorbed_mla import make_fixture
    model, inputs = make_fixture(context)
    if mode == 'legacy':
        model.__open64_tensor_traits__ = {}
    if mode == 'contradictory':
        model.__open64_tensor_traits__ = {'cache': ROTARY_KEY_CACHE, 'rope_cache': LATENT_KV_CACHE}
    module = export_to_whirl(model, inputs, WhirlExportOptions(backend='native'))
    save_as_whirl(module, str(image))


def review(args):
    """Exercise process boundaries and retain exact commands and comparisons."""
    # Authenticate every cached input before parsing it or executing an oracle
    # check. A partial/mutated retained baseline must fail closed.
    digests=dict(line.split('  ',1)[::-1] for line in
                 (args.baseline/'SHA256SUMS').read_text().splitlines())
    required=[args.baseline/'receipt.json',args.baseline/'validation.json']
    for ctx in ('short','long'):
        required += [args.baseline/'m3/controls'/(ctx+suffix) for suffix in ('.input.B','.v2.json')]
        required += [args.baseline/'m3/fixtures/selected'/ctx/'mla.B',
                     args.baseline/'previous/fixtures/observer'/(ctx+'.recipe.json')]
        fixtures=args.baseline/'m3/fixtures/selected'/ctx
        required += list(fixtures.glob('*.reference.json')) + list(fixtures.glob('*.inputs.bin'))
    for path in required:
        assert hashlib.sha256(path.read_bytes()).hexdigest()==digests[str(path.relative_to(args.baseline))],path
    receipt=json.loads((args.baseline/'receipt.json').read_text())
    assert receipt['primary_artifact']=='validation.json'
    assert hashlib.sha256((args.baseline/'validation.json').read_bytes()).hexdigest()==receipt['primary_sha256']
    out = args.output.resolve(); out.mkdir(parents=True, exist_ok=False)
    (out/'logs').mkdir()
    env = dict(os.environ, LD_LIBRARY_PATH=':'.join(str(args.tools/p) for p in ('be','wopt','lno')))
    env.pop('OPEN64_HIGH_SEARCH_BINDINGS', None)
    commands=[]
    def run(label, argv, success=True):
        """Retain exact diagnostics before requiring the expected exit class."""
        p=subprocess.run(list(map(str,argv)),cwd=out,env=env,capture_output=True,text=True,timeout=600)
        for stream in ('stdout','stderr'):(out/'logs'/(label+'.'+stream)).write_text(getattr(p,stream))
        commands.append(dict(label=label,argv=list(map(str,argv)),returncode=p.returncode))
        (out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        assert (p.returncode==0)==success,(label,p.stderr[-3500:])
        return p
    lower=args.tools/'ir_tools/tensor_kernel_lower'; reader=args.tools/'ir_tools/ir_b2a'
    w2c=args.tools/'whirl2c/whirl2c'
    source=Path(__file__).parents[3]/'torch2whirl/python/tests/models/absorbed_mla.py'
    prototype=json.loads((args.baseline/'m3/controls/short.v2.json').read_text())
    report=dict(status='PASS',contexts={},negative_cases=[],gpu_execution=False,
                authenticated_baseline_files=len(required))
    def reopen(label, image):
        """Independently inspect the mapped image with a matching trace stem."""
        run(label,[reader,'-st','-src',image,image.with_suffix('.T')])
        return image.with_suffix('.T').read_text()
    def backend(label, image, bindings=None, apply=False, enabled=True):
        """Run one ordinary driver invocation, optionally applying its winner."""
        base=out/label
        command=[args.tools/'be/be','-fB,'+str(image),'-fo,'+str(base)+'.o',
                 '-ft,'+str(base)+'.phase.t','-PHASE:l=on:w=off:c=off:lpath='+str(args.tools/'lno')+
                 ':wpath='+str(args.tools/'wopt'),'-O3','-DSL:partition_axis='+('on' if enabled else 'off')+
                 ':ai_opt_trace=on','-tr25','-tt25:1','-OPT:IEEE_arith=3:roundoff=2']
        if bindings:
            option='-LNO:high_search='+('on' if enabled else 'off')+':high_apply='+('on' if apply else 'off')+':high_search_bindings='+str(bindings)
            if apply:
                option+=':high_device_output='+str(out/(label+'-device'))+':high_device_tool='+str(args.tools/'ir_tools/tensor_selected_bridge')
            command.append(option)
        run(label,[*command,source])
        return base.with_suffix('.N'),base.with_suffix('.phase.t').read_text()
    def compile_high(label,image):
        """Compile real whirl2c High output with strict scalar FP32 semantics."""
        saved_trace=image.with_suffix('.T')
        trace_hash=hashlib.sha256(saved_trace.read_bytes()).hexdigest() if saved_trace.exists() else None
        c=out/(label+'.c')
        run(label+'-w2c',[w2c,'-TARG:abi=n64','-O0','-CLIST:dotc_file='+str(c),
                         '-CLIST:doth_file='+str(c.with_suffix('.h')),
                         '-ft,'+str(out/(label+'.w2c.phase.t')),image])
        if trace_hash:
            assert hashlib.sha256(saved_trace.read_bytes()).hexdigest()==trace_hash,'whirl2c changed retained .T'
        lib=out/(label+'.so')
        run(label+'-cc',['gcc','-std=c99','-O2','-shared','-fPIC','-ffp-contract=off','-fno-fast-math',
                        '-I'+str(args.tools/'include'),'-include',Path(__file__).with_name('common_high_host.h'),c,'-lm','-o',lib])
        return lib
    for ctx in ('short','long'):
        image=out/(ctx+'.source.B')
        run(ctx+'-produce',[sys.executable,__file__,'--produce',ctx,'--mode','declared','--image',image])
        original=hashlib.sha256(image.read_bytes()).hexdigest()
        text=reopen(ctx+'-source-reopen',image)
        assert 'traits = attention.latent_kv_cache.v1' in text and 'traits = attention.rotary_key_cache.v1' in text
        recipe=out/(ctx+'.recipe.json')
        run(ctx+'-prepare',[lower,'prepare',image,recipe,'--verify','3'])
        assert original==hashlib.sha256(image.read_bytes()).hexdigest()
        data=json.loads(recipe.read_text()); extension=data['tensor_cache_traits']
        roles={v['value']:v['trait'] for v in extension['values']}
        assert roles[2]=='attention.latent_kv_cache.v1' and roles[3]=='attention.rotary_key_cache.v1'
        for op in data['operations']:
            if op['name']=='common.reshape':assert roles.get(op['output'])==roles.get(op['inputs'][0])
            else:assert op['output'] not in roles
        before,high,mp=(out/(ctx+suffix) for suffix in ('.before.B','.high.B','.high.json'))
        run(ctx+'-high',[lower,'high',recipe,before,high,mp]);reopen(ctx+'-before-reopen',before)
        high_text=reopen(ctx+'-high-reopen',high)
        (out/(ctx+'.lowering.diff')).write_text(''.join(difflib.unified_diff(
            before.with_suffix('.T').read_text().splitlines(True),high_text.splitlines(True))))
        assert high_text.count('tensor.cache.trait = attention.')==2
        assert 'DO_LOOP' in high_text and 'ARRAY' in high_text and 'COMMENT' in high_text
        manifest=json.loads(mp.read_text()); assert manifest['recipe']['tensor_cache_traits']==extension
        library=compile_high(ctx+'-high',high)
        checks={}
        fixtures=args.baseline/'m3/fixtures/selected'/ctx
        for reference in sorted(fixtures.glob('*.reference.json')):
            binary=out/(ctx+'-'+reference.stem+'.outputs.bin')
            binary.write_bytes(execute(library,manifest,reference.with_name(reference.name.replace('.reference.json','.inputs.bin')).read_bytes()))
            checks[reference.name]=check_output(reference,binary)
        assert len(checks)==7
        probed,trace=backend(ctx+'-probe',high)
        assert 'AI_HIGH_SNAPSHOT SOURCE ' in trace
        after_text=reopen(ctx+'-probe-reopen',probed)
        assert after_text.count('tensor.cache.trait = attention.')==2
        assert 'cache-trait-v1' in next(x for x in trace.splitlines() if x.startswith('AI_HIGH_SNAPSHOT SOURCE '))
        bound=binding(manifest,trace,prototype['gpu'],prototype['model'])
        bound['schema']='open64.lno-high-search.v2'
        bound['search_controls']=copy.deepcopy(prototype['search_controls'])
        bp=out/(ctx+'.binding.json');bp.write_text(json.dumps(bound,indent=2)+'\n')
        _,observed=backend(ctx+'-observe',high,bp)
        assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in observed
        assert 'applied=false schedule_legality=not_proved' in observed
        production=copy.deepcopy(bound); production['schema']='open64.lno-high-search.v2'
        production['search_controls']=prototype['search_controls']
        prod=out/(ctx+'.production.json');prod.write_text(json.dumps(production,indent=2)+'\n')
        selected_host,applied=backend(ctx+'-selected',high,prod,apply=True)
        assert 'fallback=ordinary_lno' not in applied
        selected_library=compile_high(ctx+'-selected-host',selected_host)
        selected_checks={}
        for reference in sorted(fixtures.glob('*.reference.json')):
            binary=out/(ctx+'-selected-'+reference.stem+'.outputs.bin')
            binary.write_bytes(execute(selected_library,manifest,
                reference.with_name(reference.name.replace('.reference.json','.inputs.bin')).read_bytes()))
            selected_checks[reference.name]=check_output(reference,binary)
        device=out/(ctx+'-selected-device')/'pu-0'
        complete=json.loads((device/'selected.complete.json').read_text())
        assert (device/'selected.ptx').is_file() and complete
        for phase in ('high','mid','low'):
            generated=reopen(ctx+'-selected-'+phase,device/('selected.'+phase+'.B'))
            assert '.cache_trait = attention.latent_kv_cache.v1' in generated
            assert '.cache_trait = attention.rotary_key_cache.v1' in generated
        # Trait removal/changes cannot match a producer sealed with declarations.
        stale=copy.deepcopy(bound);stale['algorithm'].pop('tensor_cache_traits')
        sp=out/(ctx+'.stripped-binding.json');sp.write_text(json.dumps(stale,indent=2)+'\n')
        _,refused=backend(ctx+'-stripped-binding',high,sp)
        assert 'fallback=ordinary_lno' in refused and 'native_producer_manifest_mismatch' in refused
        report['negative_cases'].append(ctx+'-stripped-binding')
        legacy=out/(ctx+'.legacy.B')
        run(ctx+'-legacy-produce',[sys.executable,__file__,'--produce',ctx,'--mode','legacy','--image',legacy])
        lr=out/(ctx+'.legacy.json');run(ctx+'-legacy-prepare',[lower,'prepare',legacy,lr,'--verify','3'])
        assert 'tensor_cache_traits' not in json.loads(lr.read_text())
        lb,lh,lm=(out/(ctx+suffix) for suffix in ('.legacy-before.B','.legacy-high.B','.legacy-high.json'))
        run(ctx+'-legacy-high',[lower,'high',lr,lb,lh,lm])
        assert 'tensor.cache.trait' not in reopen(ctx+'-legacy-reopen',lh)
        retained=out/(ctx+'.retained-legacy.json')
        run(ctx+'-retained-legacy-prepare',[lower,'prepare',args.baseline/'m3/fixtures/selected'/ctx/'mla.B',retained,'--verify','3'])
        assert json.loads(retained.read_text())==json.loads((args.baseline/'previous/fixtures/observer'/(ctx+'.recipe.json')).read_text())
        _,legacy_observed=backend(ctx+'-retained-legacy-observe',args.baseline/'m3/controls'/(ctx+'.input.B'),
                                  args.baseline/'m3/controls'/(ctx+'.v2.json'))
        assert 'applied=false schedule_legality=not_proved' in legacy_observed
        assert 'tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in legacy_observed
        _,fallback=backend(ctx+'-disabled',high,bp,enabled=False)
        assert 'AI_HIGH_SEARCH ' not in fallback
        for label,change in [('unknown-version',lambda x:x['tensor_cache_traits'].update(schema='unsupported')),
                             ('duplicate',lambda x:x['tensor_cache_traits']['values'].append(x['tensor_cache_traits']['values'][0])),
                             ('unknown-role',lambda x:x['tensor_cache_traits']['values'][0].update(trait='unsupported')),
                             ('nul-role',lambda x:x['tensor_cache_traits']['values'][0].update(trait='attention.latent_kv_cache.v1\x00ignored')),
                             ('stripped-recipe',lambda x:x.pop('tensor_cache_traits'))]:
            bad=copy.deepcopy(data);change(bad);path=out/(ctx+'.bad-'+label+'.json');path.write_text(json.dumps(bad))
            dest=[out/(ctx+'.bad-'+label+s) for s in ('.before.B','.after.B','.high.json')]
            run(ctx+'-bad-'+label,[lower,'high',path,*dest],success=False)
            assert not any(p.exists() for p in dest)
            report['negative_cases'].append(ctx+'-'+label)
        report['contexts'][ctx]=dict(source_sha256=original,cache_trait_records=len(roles),
            reference_checks=checks,selected_reference_checks=selected_checks,
            high_formal_traits=2,estimation_isolation=True,selected_bridge=True,
            missing_annotations='legacy path retained',disabled_search='ordinary LNO retained')
    # A valid declaration with the wrong attention purpose is rejected by the
    # graph recognizer rather than silently reclassified or used for fusion.
    image=out/'contradictory.B';run('contradictory-produce',[sys.executable,__file__,'--produce','short','--mode','contradictory','--image',image])
    previous=image
    for command,options in [('prepare',['--verify','3']),('optimize',['--fuse']),('cooperate',['--allow-reassociate'])]:
        dest=out/('contradictory-'+command+'.json');run('contradictory-'+command,[lower,command,previous,dest,*options]);previous=dest
    dest=out/'contradictory-fused.json'
    p=run('contradictory-fuse',[lower,'fuse-attention',previous,dest,'--allow-reassociate'],success=False)
    assert 'contradicts absorbed attention dataflow' in p.stderr and not dest.exists()
    report['negative_cases'].append('contradictory-attention-role')
    (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    print('PASS: 2 native contexts, 28 reference executions, read-only observation, selected bridge and trait refusals')


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--produce',choices=('short','long'))
    parser.add_argument('--mode',choices=('declared','legacy','contradictory'),default='declared')
    parser.add_argument('--image',type=Path)
    for name in ('baseline','tools','output'):
        retained=os.environ.get('OPEN64_DSL_TEST_ARTIFACT_DIR')
        default=Path(retained)/'mla-cache-traits' if name=='output' and retained else None
        parser.add_argument('--'+name,type=Path,default=default)
    args=parser.parse_args()
    if args.produce:
        if not args.image:parser.error('--produce requires --image')
        produce(args.produce,args.mode,args.image)
    else:
        if not all((args.baseline,args.tools,args.output)):
            parser.error('review requires --baseline, --tools and --output (or OPEN64_DSL_TEST_ARTIFACT_DIR)')
        review(args)
