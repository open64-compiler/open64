"""Selected Q6c native phases, real CUDA/PTX compile and host collectives.

The host lane model is a correctness instrument, never a GPU performance result
or serving/device lease. Existing Q3 callback bindings are retained unchanged.
"""
import ctypes as C
import difflib
import json
from pathlib import Path
from mla_q6b_native_review import Q6B_REVIEW


def wrapper(root, manifests, reverse=False):
    """Drive exactly the three emitted kernels; no duplicate MLA algorithm."""
    types = dict(float32='float', int8='signed char', uint8='unsigned char',
                 int64='long long', bool='unsigned char')
    first = manifests[0]
    buffers = first['buffers']
    params = ','.join('void *a'+str(i) for i in range(len(buffers)+1))
    args = ','.join('('+types[first['recipe']['values'][s]['dtype']]+' *)a'+str(i)
                    for i, s in enumerate(buffers))+', (int *)a'+str(len(buffers))
    includes = '\n'.join('#include "'+str(root/(m['stage']+'.cu'))+'"' for m in manifests)
    calls = '\n'.join('open64_host_launch('+str(m['grid_x'])+','+str(m['block_x'])+', [&]{'+m['function']+'('+args+');}, '+str(reverse).lower()+');'
                      for m in manifests)
    return includes+'''
static std::mutex q6c_review_lock;
extern "C" void q6c_review_entry('''+params+''') {
 std::lock_guard<std::mutex> lock(q6c_review_lock);
 q6c_need(q6c_barriers.empty());
 q6c_bulk_bytes=q6c_copies=q6c_waits=q6c_syncs=0;
 '''+calls+'''
 q6c_need(q6c_barriers.empty());
}
extern "C" const char *q6c_review_events() {
 static char text[256];snprintf(text,sizeof(text),"{\\"bulk_bytes\\":%llu,\\"copies\\":%llu,\\"waits\\":%llu,\\"cta_barriers\\":%llu,\\"quiescent\\":%s}",q6c_bulk_bytes,q6c_copies,q6c_waits,q6c_syncs,q6c_barriers.empty()?"true":"false");return text;
}
'''


def build_device(source, target, tools, repo, cuda, command):
    """Only the already selected Q6b tree is lowered; no alternate candidates."""
    target.mkdir()
    stages = []
    for stage in ('prepare', 'attention', 'finalize'):
        prefix = target/stage
        command(stage+'-device', [tools/'ir_tools/tensor_selected_bridge', 'q6-device', source, prefix, stage, '--opt-level=3'], target)
        manifest = json.loads(prefix.with_suffix('.json').read_text())
        assert manifest['stage_operations_lowered'] and not manifest['device_bindings_implemented']
        assert manifest['search_invocations'] == manifest['alternate_candidates_materialized'] == 0
        stages.append(manifest)
        for level in ('before', 'high', 'mid', 'low'):
            image = prefix.with_suffix('.'+level+'.B')
            command(stage+'-'+level+'-dump', [tools/'ir_tools/ir_b2a', '-st', '-src', image, image.with_suffix('.T')], target)
            if level != 'before':
                command(stage+'-'+level+'-audit', [tools/'ir_tools/tensor_selected_bridge', 'q6-device-audit', image, level, target/(stage+'.'+level+'.keys.json')], target)
        for left,right in (('before','high'),('high','mid'),('mid','low')):
            a=target/(stage+'.'+left+'.T');b=target/(stage+'.'+right+'.T')
            (target/(stage+'.'+left+'-'+right+'.raw.diff')).write_text(''.join(difflib.unified_diff(a.read_text().splitlines(True),b.read_text().splitlines(True))))
        command(stage+'-emit', [tools/'whirl2c/whirl2c', '-cuda', '-TARG:abi=n64', '-O0', '-ft,'+str(target/(stage+'.w2c.phase.t')),
                '-CLIST:dotc_file='+str(prefix.with_suffix('.cu')), '-CLIST:doth_file='+str(prefix.with_suffix('.h')), prefix.with_suffix('.low.B')], target)
        text=prefix.with_suffix('.cu').read_text()
        assert 'Q6c native GPU realization: '+stage in text and 'Q6b selected native MLA staging:' in text
        assert '__open64_mla_stage_' not in text
        command(stage+'-cuda', [cuda/'bin/nvcc','-arch=sm_90','--fmad=false','--ftz=false','--prec-div=true','--prec-sqrt=true',
                '-I'+str(repo/'osprey/include'),'-c',prefix.with_suffix('.cu'),'-o',target/(stage+'.cuda.o')], target)
        command(stage+'-ptx', [cuda/'bin/ptxas','-arch=sm_90','-c',prefix.with_suffix('.ptx'),'-o',target/(stage+'.ptx.o')], target)
    assert len({m['source_key'] for m in stages}) == len({m['q3_binding_key'] for m in stages}) == len({m['mapping_key'] for m in stages}) == 1
    math=target/'math.cu'
    math.write_text('#include <cuda_runtime.h>\nextern "C" __device__ float __open64_exp_f32(float x){return expf(x);}\nextern "C" __device__ float __open64_log_f32(float x){return logf(x);}\n')
    command('device-math',[cuda/'bin/nvcc','-arch=sm_90','--fmad=false','--ftz=false','-dc',math,'-o',target/'math.o'],target)
    command('native-device-link',[cuda/'bin/nvlink','-arch=sm_90','-o',target/'native.cubin',target/'prepare.ptx.o',target/'attention.ptx.o',target/'finalize.ptx.o',target/'math.o'],target)
    libraries=[]
    for reverse in (False,True):
        name='reverse' if reverse else 'forward'
        host=target/(name+'.cxx');host.write_text(wrapper(target, stages, reverse))
        library=target/(name+'.so')
        command(name+'-host',['g++','-std=c++11','-O2','-shared','-fPIC','-pthread','-ffp-contract=off','-fno-fast-math','-fno-strict-aliasing',
            '-I'+str(repo/'osprey/be/lno/tests/q6c_cuda_stub'),host,'-o',library],target)
        libraries.append(library)
    return stages,libraries


class Q6C_REVIEW(Q6B_REVIEW):
    """Preserve the fresh Q3 source/snapshot callback, changing only its code."""
    def __init__(self, api, args, out):
        super().__init__(api,args,out)
        self.device_cases={}

    def Before(self,c,check,name=None):
        super().Before(c,check,name)
        name=name or c.case
        if not self.stage_cases[name]['transformed']:
            self.device_cases[name]=dict(source_fallback=True)
            self.Device_report();return
        import torch
        from run_quantized_high_review import execute
        selected=self.out/(name+'.q5')
        expected=execute(selected/'native.so',json.loads((selected/'selected.manifest.json').read_text()),c.inputs)
        results=[]
        for transport in self.stage_cases[name]['transports']:
            source=Path(transport['native_image'])
            target=source.parent/'q6c'
            stages,libraries=build_device(source,target,self.tools,self.args.source,self.args.cuda,self.Command)
            events=[]
            for library in libraries:
                manifest=dict(stages[0],function='q6c_review_entry')
                actual=execute(library,manifest,c.inputs)
                for a,b in zip(actual,expected):torch.testing.assert_close(a,b,rtol=0,atol=0,equal_nan=True)
                handle=C.CDLL(str(library));handle.q6c_review_events.restype=C.c_char_p
                observed=json.loads(handle.q6c_review_events());assert observed['quiescent']
                groups=stages[1]['grid_x'];tiles=(c.schema['geometry']['L']+transport['selected_width']-1)//transport['selected_width']
                assert observed['waits']==(groups*3*tiles if transport['delivery']==2 else 0)
                for bad in self.Bad_inputs(c):
                    execute(library,manifest,bad,1)
                    state=json.loads(handle.q6c_review_events());assert state['quiescent'] and state['bulk_bytes']==state['waits']==0
                # Eight-byte shifted buffers retain scalar ABI alignment while
                # exercising actual device-address residue fallback. No host
                # alignment is promoted into a compiled device lease.
                shifted=execute(library,manifest,c.inputs,prefix_bytes=40)
                for a,b in zip(shifted,expected):torch.testing.assert_close(a,b,rtol=0,atol=0,equal_nan=True)
                assert json.loads(handle.q6c_review_events())['quiescent']
                events.append(observed)
            (target/'execution.json').write_text(json.dumps(events,indent=2)+'\n')
            # Native seal must reject altered waits, collective, bulk and Q3/source
            # bindings before any audit result or CUDA product can be accepted.
            for fault in (('wait','sync','bulk','binding') if transport['delivery']==2 else ('sync','binding')):
                import os
                saved=os.environ.get('OPEN64_Q6C_TEST_FAULT')
                os.environ['OPEN64_Q6C_TEST_FAULT']=fault
                try:
                    p=self.Command('q6c-stale-'+fault,[self.tools/'ir_tools/tensor_selected_bridge_test','q6-device-audit',target/'attention.low.B','low',target/(fault+'.invalid.json'),'--test-write-mutated'],target,False)
                finally:
                    if saved is None:os.environ.pop('OPEN64_Q6C_TEST_FAULT',None)
                    else:os.environ['OPEN64_Q6C_TEST_FAULT']=saved
                assert 'q6c_stale_native_level_or_binding' in p.stderr and not (target/(fault+'.invalid.json')).exists()
                self.Command('q6c-emitter-reject-'+fault,[self.tools/'whirl2c/whirl2c','-cuda','-TARG:abi=n64','-O0',
                    '-ft,'+str(target/(fault+'.invalid.phase.t')),'-CLIST:dotc_file='+str(target/(fault+'.rejected.cu')),
                    '-CLIST:doth_file='+str(target/(fault+'.rejected.h')),target/(fault+'.invalid.json.B')],target,False)
            if name=='dense_s8_decode':
                self.Command('q6c-O0',[self.tools/'ir_tools/tensor_selected_bridge','q6-device',source,target/'O0','attention','--opt-level=0'],target,False)
                assert not (target/'O0.before.B').exists()
                import os
                os.environ['OPEN64_Q6C_TEST_FAULT']='source-binding'
                try:
                    self.Command('q6c-source-refusal',[self.tools/'ir_tools/tensor_selected_bridge_test','q6-device',source,target/'stale','attention','--opt-level=3'],target,False)
                finally:os.environ.pop('OPEN64_Q6C_TEST_FAULT',None)
                assert not (target/'stale.before.B').exists()
                os.environ['OPEN64_Q6C_TEST_REJECT_AFTER_MEMORY']='1'
                try:
                    self.Command('q6c-failed-application',[self.tools/'ir_tools/tensor_selected_bridge_test','q6-device',source,target/'failed','attention','--opt-level=3'],target,False)
                finally:os.environ.pop('OPEN64_Q6C_TEST_REJECT_AFTER_MEMORY',None)
                assert (target/'failed.before.B').exists() and (target/'failed.mid.B').exists() and not (target/'failed.low.B').exists() and not (target/'failed.json').exists()
            check(name+' Q6c CUDA/native PTX compile, link, collective reference and guards',True)
            results.append(dict(delivery=transport['delivery'],events=events,grid_x=stages[1]['grid_x'],block_x=stages[1]['block_x'],exact_source_outputs=12))
        library=C.CDLL(str(libraries[0]));raw=library.q6c_review_entry
        raw.argtypes,raw.restype=[C.c_void_p]*len(c.views),None
        c.function,c.q6c_library=raw,library
        self.device_cases[name]=dict(transports=results,q3_host_callback_join=True,device_authority=False)
        self.Device_report()

    def Device_report(self):
        (self.out/'q6c.report.json').write_text(json.dumps(dict(status='PASS',cases=self.device_cases,native_gpu_mapping=True,cuda_compiled=True,native_ptx_assembled_and_device_linked=True,
            reference_scope='host collective/delayed-copy model through Q3, not GPU execution',two_phase_search=True,device_bindings=False,gpu_execution=False,aot_selection='deferred'),indent=2)+'\n')
