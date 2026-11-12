"""M6 production driver and exact retained dependence-image regression.

Contract: doc/AI-COMPILER-SEARCH-M6-PRODUCTION-ROUTE-v1.0.md. Real compiler
processes, mapped native artifacts, reference execution and refusal evidence.
No candidate search iteration invokes this development harness.
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
import struct
import subprocess
import sys

sys.path.insert(0,str(Path(__file__).resolve().parents[2]/'lno/tests'))
from run_common_high_review import execute,check_output


def graph_image(path):
    """Read the established ELF/PU dependence layout without changing an image.
    Return semantic tuples, retaining node offsets and excluding DEP padding.
    """
    data=path.read_bytes();assert data[:6]==b'\x7fELF\x02\x01'
    h=struct.unpack_from('<16sHHIQQQIHHHHHH',data)
    sections=[struct.unpack_from('<IIQQQQIIQQ',data,h[6]+i*h[11]) for i in range(h[12])]
    names=sections[h[13]];names=data[names[4]:names[4]+names[5]]
    section=next(s for s in sections if names[s[0]:].split(b'\0',1)[0]==b'.WHIRL.pu_section')
    base=section[4];p=base+struct.unpack_from('<I',data,base)[0]
    version,pus=struct.unpack_from('<II',data,p);assert version==1 and pus==1;p+=8+pus*20
    kinds=struct.unpack_from('<I',data,p)[0];p+=4;assert kinds==11
    subs={};descriptor=0
    for kind in range(kinds):
        assert struct.unpack_from('<I',data,p)[0]==kind;p+=4
        subs[kind]=struct.unpack_from('<II',data,p);p+=8
        if kind==2:descriptor=p-8
    tree,tree_size=subs[1];offset,size=subs[2];cursor=base+offset;end=cursor+size
    vertices,capacity=struct.unpack_from('<HH',data,cursor);cursor+=4
    offsets=list(struct.unpack_from('<'+'I'*vertices,data,cursor));cursor+=vertices*4
    edges=[]
    for source in range(vertices):
        while True:
            assert cursor+2<=end;sink=struct.unpack_from('<H',data,cursor)[0];cursor+=2
            if not sink:break
            assert sink<=vertices;cursor=(cursor+3)//4*4
            dep,must=struct.unpack_from('<HB',data,cursor);cursor+=4
            edges.append((offsets[source],offsets[sink-1],dep,must))
    assert cursor==end and len(edges)<=capacity
    return dict(offsets=offsets,edges=edges,capacity=capacity,
        graph_descriptor=descriptor,graph_file_offset=base+offset,graph_size=size,
        tree_sha256=hashlib.sha256(data[base+tree:base+tree+tree_size]).hexdigest())


def review(args):
    """Exercise automatic selected routing, refusal isolation and graph repair.
    Retain every command/diagnostic, original image and full native trace diff.
    """
    out=args.output.resolve();out.mkdir(parents=True,exist_ok=False);(out/'logs').mkdir()
    tools=args.tools.resolve();previous=args.previous.resolve();source=Path('/src/osprey/torch2whirl/python/tests/models/absorbed_mla.py')
    env=dict(os.environ,LD_LIBRARY_PATH=':'.join(map(str,[tools/'be',tools/'wopt',tools/'lno'])))
    env.pop('OPEN64_HIGH_SEARCH_BINDINGS',None);env.pop('OPEN64_HIGH_APPLY_TEST',None)
    commands=[];report=dict(status='RUNNING',production={},fallback={},manifest_refusals={},dependence={},gpu_execution=False)

    def run(label,argv,expected=0,baseline=False):
        """Capture the exact process result before checking it; no shell calls."""
        local=dict(env)
        if baseline:local['LD_LIBRARY_PATH']=':'.join(map(str,[previous/'tools/be',previous/'tools/wopt',previous/'tools/lno']))
        p=subprocess.run(list(map(str,argv)),cwd=out,env=local,capture_output=True,text=True,timeout=600)
        for name in ('stdout','stderr'):(out/'logs'/(label+'.'+name)).write_text(getattr(p,name))
        commands.append(dict(label=label,argv=list(map(str,argv)),returncode=p.returncode,baseline_reader=baseline))
        (out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
        if expected is not None:assert p.returncode==expected,(label,p.returncode,p.stderr[-5000:])
        return p

    def reopen(image):
        """Native symbol/source inspection uses the exact image stem."""
        trace=image.with_suffix('.T');run(image.stem+'-reopen',[tools/'ir_tools/ir_b2a','-st','-src',image,trace]);return trace

    def backend(label,context,device=False,helper=None,level=3,apply='on',search=True,image=None,extra=(),binding=None):
        """Only be_driver orchestrates selected construction in this lane."""
        base=out/label;directory=out/(label+'-device')
        argv=[tools/'be/be','-fB,'+str(image or previous/'fixtures/controls'/(context+'.input.B')),
            '-fo,'+str(base)+'.o','-ft,'+str(base)+'.phase.t',
            '-PHASE:l=on:w=off:c=off:lpath='+str(tools/'lno')+':wpath='+str(tools/'wopt'),'-O'+str(level),
            '-DSL:ai_opt_trace=on','-tr25','-tt25:1','-OPT:IEEE_arith=3:roundoff='+('2' if search else '0')]
        if search:argv+=['-LNO:high_search=on:high_search_bindings='+str(binding or previous/'fixtures/controls'/(context+'.v2.json'))]
        if apply is not None:argv+=['-LNO:high_apply='+apply]
        if device:argv+=['-LNO:high_device_output='+str(directory)+':high_device_tool='+str(helper or tools/'ir_tools/tensor_selected_bridge')]
        p=run(label,[*argv,*extra,source]);image=base.with_suffix('.N');trace=reopen(image)
        return image,trace,directory,p

    def host_view(label,image):
        """Compile native scalar WHIRL output with the existing math adapter."""
        c=out/(label+'.c');run(label+'-w2c',[tools/'whirl2c/whirl2c','-TARG:abi=n64','-O0',
            '-CLIST:dotc_file='+str(c),'-CLIST:doth_file='+str(c.with_suffix('.h')),image])
        library=out/(label+'.so');run(label+'-cc',['gcc','-std=c99','-O2','-shared','-fPIC','-ffp-contract=off','-fno-fast-math',
            '-I'+str(args.include),'-include',Path(__file__).with_name('selected_production_host.h'),c,
            out/'vexpf.o',out/'vexpftab.o','-lm','-o',library])
        return library

    # LNO's scalar host path can select its established strided vector-math
    # call even with SIMD disabled. Link the actual repository implementation.
    for name in ('vexpf','vexpftab'):
        run(name+'-runtime',['gcc','-std=gnu99','-O2','-fPIC','-fno-strict-aliasing','-D_LITTLE_ENDIAN','-c',
            Path('/src/osprey/libm')/(name+'.c'),'-o',out/(name+'.o')])

    # Reproduce the immutable legacy failure, then regenerate from the exact
    # High producer image/options. Existing bad images are never byte-edited.
    old=run('legacy-long-reopen',[previous/'tools/whirl2c/whirl2c','-TARG:abi=n64','-O0',
        '-CLIST:dotc_file='+str(out/'legacy.failed.c'),'-CLIST:doth_file='+str(out/'legacy.failed.h'),args.baseline/'long-portable.N'],expected=None,baseline=True)
    assert old.returncode and 'Depgraph_Read' in old.stderr and 'OPCODE_make_op' in old.stderr
    for context in ('short','long'):
        old_image=out/(context+'-legacy.before.B')
        shutil.copy2(args.baseline/(context+'-portable.N'),old_image)
        old_trace=reopen(old_image)
        image,trace,_,_=backend(context+'-scalar',context,search=False,apply=None,image=args.baseline/(context+'.after.B'),extra=['-DSL:partition_axis=on','-LNO:simd=0'])
        old_graph,new_graph=graph_image(old_image),graph_image(image)
        assert new_graph['offsets']==[v for v in old_graph['offsets'] if v]
        assert new_graph['edges']==[edge for edge in old_graph['edges'] if edge[0] and edge[1]]
        assert new_graph['capacity']==old_graph['capacity'] and new_graph['tree_sha256']==old_graph['tree_sha256']
        assert trace.read_bytes()==old_trace.read_bytes()
        for suffix,path in [('before',old_image),('after',image)]:
            copied=out/(context+'-dependence.'+suffix+'.B');shutil.copy2(path,copied);reopen(copied)
        native=[]
        for label,path in [('legacy',old_image),('corrected',image)]:
            audit=out/(context+'-'+label+'-graph.json')
            run(context+'-'+label+'-graph',[tools/'ir_tools/tensor_selected_bridge_test','bridge-test-graph',path,audit])
            native.append(json.loads(audit.read_text()))
        assert native[0]==native[1] and len(native[0]['vertices'])==len(new_graph['offsets']) and native[0]['edges']
        diff=out/(context+'-scalar.before-after.diff');diff.write_text(''.join(difflib.unified_diff(old_trace.read_text().splitlines(True),trace.read_text().splitlines(True),fromfile=str(old_trace),tofile=str(trace))))
        # Valid corrected images also reopen under the retained old reader.
        run(context+'-old-reader',[previous/'tools/whirl2c/whirl2c','-TARG:abi=n64','-O0',
            '-CLIST:dotc_file='+str(out/(context+'-legacy-reader.c')),'-CLIST:doth_file='+str(out/(context+'-legacy-reader.h')),image],baseline=True)
        library=host_view(context+'-scalar',image);legacy_library=host_view(context+'-legacy',old_image)
        manifest=json.loads((args.baseline/(context+'.high.json')).read_text());checks={}
        for reference in sorted((previous/'fixtures/fixtures/selected'/context).glob('*.reference.json')):
            variant=reference.name[:-len('.reference.json')];binary=out/(context+'-scalar-'+variant+'.bin')
            binary.write_bytes(execute(library,manifest,(reference.parent/(variant+'.inputs.bin')).read_bytes()))
            checks[variant]=check_output(reference,binary)
            legacy_binary=out/(context+'-legacy-'+variant+'.bin')
            legacy_binary.write_bytes(execute(legacy_library,manifest,(reference.parent/(variant+'.inputs.bin')).read_bytes()))
            checks['legacy-'+variant]=check_output(reference,legacy_binary)
            assert binary.read_bytes()==legacy_binary.read_bytes()
        assert checks
        report['dependence'][context]=dict(old_vertices=len(old_graph['offsets']),new_vertices=len(new_graph['offsets']),
            removed_zero_vertices=old_graph['offsets'].count(0),surviving_nodes_and_tuples_equal=True,tree_bytes_equal=True,full_T_equal=True,
            old_reader_accepts_corrected_image=True,current_reader_accepts_immutable_legacy=True,native_reader_graph_equal=True,reference_checks=checks)

    # Truncated vertex/edge payloads and invalid sinks must refuse cleanly,
    # without dereferencing an unavailable WN or dependence tuple.
    valid=out/'long-scalar.N';graph=graph_image(valid);raw=valid.read_bytes()
    for label,size in [('vertices',4),('edges',4+4*len(graph['offsets'])+1)]:
        changed=bytearray(raw);struct.pack_into('<I',changed,graph['graph_descriptor']+4,size)
        broken=out/('truncated-'+label+'.B');broken.write_bytes(changed)
        result=run('truncated-'+label,[tools/'ir_tools/tensor_selected_bridge_test','bridge-test-graph',broken,out/('truncated-'+label+'.json')],expected=None)
        assert result.returncode>0 and 'Unable to read the dependence graph' in result.stderr
        assert 'OPCODE_make_op' not in result.stderr
    report['reader_truncation_refusals']=2

    for context in ('short','long'):
        base,base_trace,_,_=backend(context+'-selected-baseline',context)
        image,trace,directory,p=backend(context+'-production',context,device=True)
        assert 'AI_HIGH_DEVICE 1 PUBLISHED' in p.stderr and trace.read_bytes()==base_trace.read_bytes()
        phase=image.with_suffix('.phase.t').read_text()
        assert 'ISOLATION tree_equal=true ST_equal=true TY_equal=true analyses_equal=true' in phase
        assert phase.count('AI_HIGH_APPLY 1 COMMITTED')==1 and phase.count('candidates_materialized=1')==2
        case=directory/'pu-0';complete=json.loads((case/'selected.complete.json').read_text());assert complete['status']=='PASS' and complete['search_invocations']==0
        for file in complete['files']:assert (case/file).is_file()
        for level in ('high','mid','low'):
            boundary=case/('selected.'+level+'.B');reopen(boundary)
            run(context+'-'+level+'-audit',[tools/'ir_tools/tensor_selected_bridge','bridge-audit',boundary,level])
        for suffix,path in [('before',case/'source.B'),('after',case/'selected.high.B')]:
            copied=case/('translation.'+suffix+'.B');shutil.copy2(path,copied);reopen(copied)
        before=(case/'translation.before.T').read_text();after=(case/'translation.after.T').read_text()
        (case/'translation.before-after.diff').write_text(''.join(difflib.unified_diff(before.splitlines(True),after.splitlines(True),fromfile='translation.before.T',tofile='translation.after.T')))
        for first,last in [('high','mid'),('mid','low')]:
            a=(case/('selected.'+first+'.T')).read_text();b=(case/('selected.'+last+'.T')).read_text()
            (case/(first+'-'+last+'.diff')).write_text(''.join(difflib.unified_diff(a.splitlines(True),b.splitlines(True),fromfile=first,tofile=last)))
        ptx=out/(context+'-production-reemit.ptx')
        run(context+'-ptx-reemit',[tools/'ir_tools/tensor_selected_bridge','bridge-ptx',case/'selected.low.B',ptx])
        assert ptx.read_bytes()==(case/'selected.ptx').read_bytes()==(previous/'bridge'/context/'selected.ptx').read_bytes()
        cu=case/'kernels.cu'
        run(context+'-cuda',[tools/'whirl2c/whirl2c','-cuda','-TARG:abi=n64','-O0','-CLIST:dotc_file='+str(cu),
            '-CLIST:doth_file='+str(case/'kernels.h'),case/'selected.low.B'])
        assert 'Selected High MLA' in cu.read_text() and 'No global score/probability vectors' in cu.read_text()
        for mode,helper in [('missing',out/'does-not-exist'),('failed',Path('/bin/false')),('incomplete',Path('/bin/true'))]:
            refused,refused_trace,refused_dir,result=backend(context+'-'+mode,context,device=True,helper=helper)
            assert 'AI_HIGH_DEVICE 1 REFUSED' in result.stderr and refused_trace.read_bytes()==base_trace.read_bytes()
            assert not list(refused_dir.glob('pu-*/selected.complete.json'))
            report['fallback'][context+'-'+mode]=dict(host_full_T_equal=True,no_completed_device_product=True)
        # A directory collision may not replace any pre-existing file.
        collision=out/(context+'-collision-device');collision.mkdir();sentinel=collision/'retain';sentinel.write_text('preserve existing artifact\n')
        _,collision_trace,_,result=backend(context+'-collision',context,device=True)
        assert 'reason=output_not_fresh' in result.stderr and sentinel.read_text()=='preserve existing artifact\n'
        assert collision_trace.read_bytes()==base_trace.read_bytes();report['fallback'][context+'-collision']=dict(existing_output_preserved=True,host_full_T_equal=True)
        for label,options in [('estimate',dict(apply='off')),('O2',dict(level=2)),('search-off',dict(search=False,apply='off'))]:
            _,_,skipped,result=backend(context+'-'+label,context,device=True,**options)
            assert 'AI_HIGH_DEVICE 1 SKIP' in result.stderr and not skipped.exists()
            report['fallback'][context+'-'+label]=dict(no_device_construction=True)
        report['production'][context]=dict(kernel_count=complete['kernel_count'],source_actual=True,native_boundaries_verified=3,
            ptx_equal_to_M5=True,comments_retained=True,search_native_isolation=True,one_selected_application=True,host_full_T_equal=True)

    # Rebinding after selection must refuse before any valid High/Mid/Low or
    # PTX success artifact. Direct tool checks exercise the same driver consumer.
    original=json.loads((previous/'fixtures/controls/short.v2.json').read_text())
    for field in ('source_key','facts_key','function','unit','numeric','host_target','gpu','model','search_controls','buffers'):
        changed=copy.deepcopy(original)
        if field=='gpu':changed[field]['sms']+=1
        elif field=='model':changed[field]['id']+='changed'
        elif field=='search_controls':changed[field]['minimum_gain_cycles']+=1
        elif field=='buffers':changed[field]=changed[field][:-1]
        else:changed[field]+='changed'
        binding=out/('stale-'+field+'.json');binding.write_text(json.dumps(changed)+'\n');prefix=out/('stale-'+field)
        result=run('stale-'+field,[tools/'ir_tools/tensor_selected_bridge','bridge-production',out/'short-production-device/pu-0/source.B',binding,prefix],expected=None)
        assert result.returncode and not prefix.with_suffix('.high.B').exists() and not prefix.with_suffix('.complete.json').exists()
        report['manifest_refusals'][field]=True

    # The test-only producer creates independent neighbors; the production
    # command is absent from the release binary and never grants IPA scope.
    multi=out/'multiple.input.B'
    run('multiple-produce',[tools/'ir_tools/tensor_selected_bridge_test','bridge-test-multiple',previous/'fixtures/observer/short.recipe.json',multi])
    _,multi_base,_,_=backend('multiple-baseline','short',image=multi)
    _,multi_trace,multi_dir,p=backend('multiple-production','short',device=True,image=multi)
    assert multi_trace.read_bytes()==multi_base.read_bytes()
    assert p.stderr.count('AI_HIGH_DEVICE 1 PUBLISHED')==1 and 'SKIP reason=no_selected_high' in p.stderr
    assert len(list(multi_dir.glob('pu-*/selected.complete.json')))==1
    report['multiple_PU']=dict(selected_products=1,ordinary_neighbor_preserved=True,host_full_T_equal=True)
    report.update(status='PASS',host_reference_executions=sum(len(x['reference_checks']) for x in report['dependence'].values()),
        fallback_cases=len(report['fallback']),manifest_refusal_cases=len(report['manifest_refusals']))
    (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(dict(status='PASS',production_contexts=2,scalar_host_references=report['host_reference_executions'],
        fallback_cases=report['fallback_cases'],manifest_refusals=report['manifest_refusal_cases'],multiple_PU='PASS',gpu_execution=False),indent=2))


def main():
    """Explicit paths make isolated native replay independent of a build tree."""
    parser=argparse.ArgumentParser(description=__doc__)
    for name in ('tools','previous','baseline','include','output'):parser.add_argument('--'+name,type=Path,required=True)
    review(parser.parse_args())


if __name__=='__main__':main()
