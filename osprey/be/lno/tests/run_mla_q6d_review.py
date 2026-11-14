"""Q6d driver-model qualification of fixed retained Q6c code.

Environment parameters select source/tools/previous/output directories. No
candidate search, compiler transformation or GPU timing is performed.
"""
from pathlib import Path
import ctypes as C,hashlib,json,os,subprocess,sys
import torch
root=Path(os.environ.get('OPEN64_Q6D_PREVIOUS','/previous'));repo=Path(os.environ.get('OPEN64_Q6D_SOURCE','/src'));out=Path(os.environ.get('OPEN64_Q6D_OUTPUT','/out/device-review-final'));out.mkdir(exist_ok=False)
tools=Path(os.environ.get('OPEN64_Q6D_TOOLS','/build/osprey/targdir'));commands=[];checks=[];cases=[]
unchanged={}
assert hashlib.sha256((root/'SHA256SUMS').read_bytes()).hexdigest()==os.environ.get('OPEN64_Q6D_PREVIOUS_SHA256','41298aa3a567af0e96653ecacc7af86390a2e5504c9051e34d2a1bf320dc0e21')
for row in (root/'SHA256SUMS').read_text().splitlines():
 digest,name=row.split('  ',1)
 assert hashlib.sha256((root/name).read_bytes()).hexdigest()==digest,name
 if name.endswith(('.B','.T','.cu','.ptx')):unchanged[name]=digest
sys.path.insert(0,str(repo/'osprey/be/lno/tests'));sys.path.insert(0,str(repo/'osprey/torch2whirl/python/tests'))
from run_quantized_high_review import execute,DTYPES
from models.mla_quantized_paged import make_fixture
from mla_q5_long_fixture import make_fixture as long_fixture
from models.mla_quantized_reference import reference
from run_mla_serving_review import Handle,View,U
api=C.CDLL(str(tools/'ir_tools/libmla_serving_cuda_review.so'))
HASH=C.CFUNCTYPE(C.c_bool,C.c_void_p,C.c_size_t,C.POINTER(C.c_ubyte))
@HASH
def sha(data,n,target):C.memmove(target,hashlib.sha256(C.string_at(data,n)).digest(),32);return True
KERNEL=C.CFUNCTYPE(C.c_bool,C.c_uint,C.POINTER(C.c_void_p))
specs={'create':([C.c_char_p,C.c_char_p,C.c_void_p,C.c_size_t,HASH],C.c_void_p),'valid':([C.c_void_p],C.c_int),'destroy':([C.c_void_p],C.c_int),'error':([C.c_void_p],C.c_char_p),
 'allocate':([C.c_void_p,U,C.POINTER(Handle)],C.c_int),'write':([C.c_void_p,Handle,U,C.c_void_p,U],C.c_int),'read':([C.c_void_p,Handle,U,C.c_void_p,U],C.c_int),'retire':([C.c_void_p,Handle],C.c_int),
 'import':([C.c_void_p,C.POINTER(View),C.c_uint,View,C.POINTER(U)],C.c_int),'begin':([C.c_void_p,C.POINTER(View),C.c_uint,View,U,C.POINTER(U)],C.c_int),
 'submit':([C.c_void_p,U],C.c_int),'complete':([C.c_void_p,U,C.c_int,C.POINTER(U)],C.c_int),'cancel':([C.c_void_p,U,C.c_int],C.c_int),'drain':([C.c_void_p],C.c_int),'live':([C.c_void_p,U],C.c_int),
 'reader':([C.c_void_p,U,C.POINTER(U),C.POINTER(U),C.POINTER(C.c_uint)],C.c_int),'reader_done':([C.c_void_p,U,C.c_int],C.c_int),'bundle_retire':([C.c_void_p,U],C.c_int),'bundle':([C.c_void_p,U,C.POINTER(U),C.POINTER(C.c_uint)],C.c_int)}
f={}
for name,(args,result) in specs.items():x=getattr(api,'open64_q6d_'+name);x.argtypes=args;x.restype=result;f[name]=x
gpu_device=os.environ.get('OPEN64_Q6D_GPU_DEVICE')
if gpu_device:
 assert gpu_device in ('h20_96gb','h20_141gb')
 create_gpu=api.open64_h1d_q6d_create;create_gpu.argtypes=specs['create'][0]+[C.c_char_p];create_gpu.restype=C.c_void_p
 f['create']=lambda *args:create_gpu(*args,gpu_device.encode())
 change_gpu=api.open64_h1d_q6d_change;change_gpu.argtypes=[C.c_void_p,C.c_char_p];change_gpu.restype=None
api.q6d_mock_configure.argtypes=[C.c_uint,C.c_uint,KERNEL];api.q6d_mock_configure.restype=C.c_int
api.q6d_mock_fault.argtypes=[C.c_char_p];api.q6d_mock_advance.argtypes=[];api.q6d_mock_stat.argtypes=[C.c_uint];api.q6d_mock_stat.restype=U
api.q6d_mock_base.argtypes=[C.c_uint];api.q6d_mock_base.restype=U
api.q6d_mock_recycle.argtypes=[U];api.q6d_mock_poke.argtypes=[U,C.c_void_p,C.c_size_t];api.q6d_mock_consume.argtypes=[U];api.q6d_mock_canaries.restype=C.c_int
torch.set_num_threads(1)
def check(label,value):assert value,label;checks.append(label)
def fault(value=''):api.q6d_mock_fault(value.encode())
def run(label,args,cwd=out):
 p=subprocess.run(list(map(str,args)),cwd=cwd,capture_output=True,text=True,timeout=300)
 for stream in ('stdout','stderr'):(cwd/(label+'.'+stream)).write_text(getattr(p,stream))
 commands.append(dict(label=label,argv=list(map(str,args)),returncode=p.returncode));(out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n');assert not p.returncode,(label,p.stderr[-4000:])
class Session:
 def __init__(self,descriptor,image,payloads,metadata,views,codewire=None):
  self.object=f['create'](codewire or descriptor,descriptor,image,len(image),sha);self.handles=[];self.views=[];self.bundles=[];self.paged=views['paged'];self.parent=0
  check('valid CUDA owner: '+self.error(),f['valid'](self.object))
  for payload in payloads:self.views.append(self.allocate(payload))
  self.meta=self.allocate(metadata)
  if self.paged:
   bundle=U();check('actual-device Q3 import',f['import'](self.object,self.array(),len(self.views),self.meta,C.byref(bundle)));self.parent=bundle.value;self.bundles.append(bundle.value)
 def error(self):return f['error'](self.object).decode()
 def array(self,v=None):return (View*len(self.views))(*(self.views if v is None else v))
 def allocate(self,payload):
  h=Handle();check('owned device allocate',f['allocate'](self.object,len(payload),C.byref(h)));self.handles.append(h)
  check('producer ready event upload',f['write'](self.object,h,0,payload,len(payload)));return View(h,0,len(payload))
 def begin(self,v=None):
  ticket=U();check('fresh device Q3 Begin',f['begin'](self.object,self.array(v),len(self.views),self.meta,self.parent,C.byref(ticket)));return ticket.value
 def complete(self,ticket,wait=True,expect=True):
  bundle=U();ok=bool(f['complete'](self.object,ticket,wait,C.byref(bundle)));check('completion publication outcome '+str(expect),ok==expect)
  if bundle.value:self.bundles.append(bundle.value)
  return bundle.value
 def read(self,view):
  data=C.create_string_buffer(view.bytes);check('bounded device download',f['read'](self.object,view.handle,view.offset,data,view.bytes));return data.raw
 def close(self):
  for b in self.bundles:check('whole-bundle retirement',f['bundle_retire'](self.object,b))
  for h in self.handles:check('unregister before device free',f['retire'](self.object,h))
  check('safe module/event teardown',f['destroy'](self.object));self.object=None
  check('no CUDA mock resource leaks',all(api.q6d_mock_stat(i)==0 for i in range(3)))
for index,item in enumerate(json.loads((root/'cases.json').read_text())):
 if gpu_device and (item['case']!='dense_s8_decode' or item['delivery']!=2):continue
 old=root/item['relative'];target=out/(item['case']+'-'+str(item['delivery']));target.mkdir()
 stages=[json.loads((old/(s+'.json')).read_text()) for s in ('prepare','attention','finalize')]
 recipe=stages[0]['recipe'];buffers=stages[0]['buffers'];schema_name='dense_s8_decode' if item['case']=='zero-active' else item['case']
 schema=json.loads((root/item['family']/(schema_name+'.schema.json')).read_text())
 m,inputs,meta=(long_fixture if 'long' in item['family'] else make_fixture)(schema_name);inputs=[x.clone() for x in inputs]
 if item['case']=='zero-active':inputs[6].zero_()
 by_input={i:inputs[v['ordinal']] for i,v in enumerate(recipe['values']) if v['role']==1};payloads=[]
 for storage in buffers:
  v=recipe['values'][storage];size=torch.empty((),dtype=DTYPES[v['dtype']]).element_size()
  for dim in v['shape']:size*=dim
  payloads.append(bytes(by_input[storage].contiguous().view(torch.uint8).flatten().tolist()) if storage in by_input else b'\xcd'*size)
 payloads.append((-1).to_bytes(4,'little',signed=True));g=schema['geometry'];rows=[]
 for b in range(g['B']):
  q,l,a=g['Q'],g['L'],g['A'];phase={'prefill':1,'decode':2,'speculative-verify':3}[meta['phase']];update=2 if a else 0 if schema['paged'] else 1
  rows.append([phase,q,l-a,l,b*q,1,0,update,a]+list(range(l-q,l))+list(range(l))+list(range(l-a,l)))
 metadata=bytes(torch.tensor(rows,dtype=torch.int64).view(torch.uint8).flatten().tolist())
 # Include actual generated CUDA and call each emitted stage separately.
 types=dict(float32='float',int8='signed char',uint8='unsigned char',int64='long long',bool='unsigned char')
 args=','.join('('+types[recipe['values'][s]['dtype']]+' *)args['+str(i)+']' for i,s in enumerate(buffers))+', (int *)args['+str(len(buffers))+']'
 text='\n'.join('#include "'+str(old/(s['stage']+'.cu'))+'"' for s in stages)+'\nextern "C" bool q6d_reference_launch(unsigned stage,void **args) {\n'
 for i,s in enumerate(stages):text+='if(stage=='+str(i)+') open64_host_launch('+str(s['grid_x'])+','+str(s['block_x'])+',[&]{'+s['function']+'('+args+');});\n'
 text+='return true;}\n';(target/'kernel-model.cxx').write_text(text)
 run('kernel-model',['g++','-std=c++11','-O2','-shared','-fPIC','-pthread','-ffp-contract=off','-fno-fast-math','-fno-strict-aliasing','-I'+str(repo/'osprey/be/lno/tests/q6c_cuda_stub'),target/'kernel-model.cxx','-o',target/'kernel-model.so'],target)
 library=C.CDLL(str(target/'kernel-model.so'));kernel=KERNEL(('q6d_reference_launch',library));check('model reset',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel))
 image=(old/'native.cubin').read_bytes();selected=json.loads(json.loads((old.parent/'manifest.json').read_text())['selected_plan'])
 code=dict(program=schema["program"],recipe=recipe,source_key=selected['source_key'],mapping_key=stages[0]['mapping_key'],cubin_bytes=len(image),sha256=list(hashlib.sha256(image).digest()),stages=stages)
 descriptor=json.dumps(code).encode();(target/'code.json').write_bytes(descriptor)
 c=Session(descriptor,image,payloads,metadata,schema);ticket=c.begin();before=api.q6d_mock_stat(3)
 check('exact three-stage submit',f['submit'](c.object,ticket) and api.q6d_mock_stat(3)==before+3)
 check('duplicate submit refuses',not f['submit'](c.object,ticket))
 c.complete(ticket,False,False);check('pending event keeps ticket',f['live'](c.object,ticket))
 check('prepared metadata cannot mutate',not f['write'](c.object,c.meta.handle,0,metadata,len(metadata)))
 check('pending buffers cannot retire',not f['retire'](c.object,c.views[-1].handle));check('pending owner cannot destroy',not f['destroy'](c.object))
 check('pending writes refuse',not f['write'](c.object,c.views[0].handle,0,payloads[0],len(payloads[0])))
 read=C.create_string_buffer(c.views[-1].bytes);check('pending output download refuses',not f['read'](c.object,c.views[-1].handle,0,read,c.views[-1].bytes))
 api.q6d_mock_advance();bundle=c.complete(ticket,False);check('completed ticket expires',not f['live'](c.object,ticket))
 # Observe all source outputs and immutable inputs through actual adapter I/O.
 expected=execute(old.parent.parent/'native.so',json.loads((old.parent.parent/'selected.manifest.json').read_text()),inputs)
 with torch.no_grad():torch_target=m(*inputs)
 scalar=reference([x.tolist() for x in inputs],meta);actual=[]
 for i,s in enumerate(buffers):
  v=recipe['values'][s]
  if v['role']==2:actual.append(torch.frombuffer(bytearray(c.read(c.views[i])),dtype=DTYPES[v['dtype']]).clone().reshape(v['shape']))
  elif v['role']==1:check('device input remains immutable',c.read(c.views[i])==payloads[i])
 for i,(a,b,t,o) in enumerate(zip(actual,expected,torch_target,scalar)):
  torch.testing.assert_close(a,b,rtol=0,atol=0,equal_nan=True);torch.testing.assert_close(a,t,rtol=2e-5 if i<2 else 0,atol=2e-6 if i<2 else 0,equal_nan=True)
  oracle=torch.tensor(o,dtype=torch.float64 if i<2 else a.dtype);torch.testing.assert_close(a.double() if i<2 else a,oracle,rtol=2e-5 if i<2 else 0,atol=2e-6 if i<2 else 0,equal_nan=True)
 check('six device-route source/reference outputs',len(actual)==6);check('device canaries',api.q6d_mock_canaries())
 reader=U();addresses=(U*16)();count=C.c_uint();check('consumer stream gets whole tuple',f['reader'](c.object,bundle,C.byref(reader),addresses,C.byref(count)) and count.value>=5)
 for i in range(count.value):api.q6d_mock_consume(addresses[i])
 check('consumer pending retains lease',not f['reader_done'](c.object,reader.value,0));check('bundle pinned until consumer completes',not f['bundle_retire'](c.object,bundle))
 api.q6d_mock_advance();check('consumer completion releases once',f['reader_done'](c.object,reader.value,0));check('duplicate reader completion refuses',not f['reader_done'](c.object,reader.value,0))
 c.close()
 if gpu_device:
  for phase in ('admission','submit','completion'):
   for mismatch in ('device','budget','expired','calibration'):
    check('H1d model reset',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));c=Session(descriptor,image,payloads,metadata,schema)
    if phase=='admission':
     change_gpu(c.object,mismatch.encode());t=U();ok=bool(f['begin'](c.object,c.array(),len(c.views),c.meta,0,C.byref(t)))
     # A fresh budget generation is legal at Begin; calibration affects evidence only.
     check('H1d admission outcome',ok==(mismatch in ('budget','calibration')))
     if ok:check('H1d prepared cleanup',f['cancel'](c.object,t.value,1))
    elif phase=='submit':
     t=c.begin();change_gpu(c.object,mismatch.encode());before=api.q6d_mock_stat(3);ok=bool(f['submit'](c.object,t))
     check('H1d submit outcome',ok==(mismatch=='calibration'))
     if ok:api.q6d_mock_advance();c.complete(t,False)
     else:check('H1d no mismatched launch',api.q6d_mock_stat(3)==before);check('H1d refused cleanup',f['cancel'](c.object,t,1))
    else:
     t=c.begin();check('H1d selected submit',f['submit'](c.object,t));change_gpu(c.object,mismatch.encode())
     api.q6d_mock_advance();c.complete(t,False,mismatch=='calibration');check('H1d completed mismatch expires',not f['live'](c.object,t))
    c.close()
 # Lifecycle/failure qualification on a fixed small source, separate from search.
 if item['case']=='dense_s8_decode' and item['delivery']==2:
  for mode in ('launch0','launch1','launch2','event_record'):
   check('reset failure model',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));c=Session(descriptor,image,payloads,metadata,schema);t=c.begin();fault(mode)
   check(mode+' refusal',not f['submit'](c.object,t));fault('event_sync|drain');c.complete(t,True,False)
   check(mode+' unknown drain pins',f['live'](c.object,t));check(mode+' unknown drain free refuses',not f['retire'](c.object,c.views[0].handle))
   fault();check(mode+' cancellation drains safely',f['cancel'](c.object,t,1));check(mode+' expires without publication',not f['live'](c.object,t));c.close()
  check('reset I/O model',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));c=Session(descriptor,image,payloads,metadata,schema);fault('io_sync|drain');t=U()
  check('uncertain control read refuses Begin',not f['begin'](c.object,c.array(),len(c.views),c.meta,0,C.byref(t)))
  check('I/O staging survives failed synchronization',api.q6d_mock_stat(2)>0);check('uncertain I/O pins reclamation',not f['retire'](c.object,c.views[0].handle));fault();check('I/O drain retry',f['drain'](c.object));c.close()
  for mode in ('context','context_id','stream_id','stream_context','capture'):
   check('reset context model',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));c=Session(descriptor,image,payloads,metadata,schema);fault(mode);t=U()
   check(mode+' binding refuses',not f['begin'](c.object,c.array(),len(c.views),c.meta,0,C.byref(t)));fault();c.close()
  for mode in ('resources','abi','load'):
   check('reset open failure model',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));fault(mode);p=f['create'](descriptor,descriptor,image,len(image),sha)
   check(mode+' loader refuses',not f['valid'](p));fault();check(mode+' acquired handles retained for cleanup',f['destroy'](p))
  bad=dict(code);bad['mapping_key']+='stale';check('reset authorization model',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel))
  p=f['create'](json.dumps(bad).encode(),descriptor,image,len(image),sha);check('unauthorized code never loads',not f['valid'](p) and api.q6d_mock_stat(4)==0);check('refused code cleanup',f['destroy'](p))
 if item['case']=='paged_s8_decode':
  check('reset parent CAS model',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));c=Session(descriptor,image,payloads,metadata,schema)
  first=c.begin();other=[]
  for ordinal,view in enumerate(c.views):
   is_input=ordinal<len(buffers) and recipe['values'][buffers[ordinal]]['role']==1
   other.append(view if is_input else c.allocate(payloads[ordinal]))
  second=c.begin(other);check('first independent submit',f['submit'](c.object,first));check('second independent submit',f['submit'](c.object,second))
  api.q6d_mock_advance();winner=c.complete(first,False);loser=c.complete(second,False,False)
  check('parent CAS publishes one complete version',winner and not loser and not f['live'](c.object,second));c.close()
 if item['case']=='dense_s8_decode' and item['delivery']==2:
  check('reset status model',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));c=Session(descriptor,image,payloads,metadata,schema);t=c.begin();check('status-case submit',f['submit'](c.object,t));api.q6d_mock_advance()
  status=(C.c_int32*1)(1);api.q6d_mock_poke(api.q6d_mock_base(len(c.views)-1),status,4);c.complete(t,False,False);check('source status prevents publication',not f['live'](c.object,t));c.close()
  check('reset native source guard',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));bad_payloads=list(payloads);bad=inputs[2].clone();bad.reshape(-1)[0]=float('nan');bad_payloads[buffers.index(2)]=bytes(bad.contiguous().view(torch.uint8).flatten().tolist())
  c=Session(descriptor,image,bad_payloads,metadata,schema);t=c.begin();check('guarded source submit',f['submit'](c.object,t));api.q6d_mock_advance();c.complete(t,False,False);check('actual emitted source guard refuses publication',not f['live'](c.object,t));c.close()
  check('reset scale publication guard',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));c=Session(descriptor,image,payloads,metadata,schema);t=c.begin();check('scale-case submit',f['submit'](c.object,t));api.q6d_mock_advance()
  scale=(C.c_float*1)(float('nan'));api.q6d_mock_poke(api.q6d_mock_base(buffers.index(schema['returned'][1])),scale,4);c.complete(t,False,False);check('invalid returned scale rejects whole tuple',not f['live'](c.object,t));c.close()
  check('reset consumer recovery',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));c=Session(descriptor,image,payloads,metadata,schema);t=c.begin();check('consumer-case submit',f['submit'](c.object,t));api.q6d_mock_advance();b=c.complete(t,False)
  reader=U();addresses=(U*16)();count=C.c_uint();check('consumer acquires lease',f['reader'](c.object,b,C.byref(reader),addresses,C.byref(count)));api.q6d_mock_consume(addresses[0]);fault('event_record|drain')
  check('consumer failed record retains lease',not f['reader_done'](c.object,reader.value,1));check('consumer failed drain pins publication',not f['bundle_retire'](c.object,b));fault();check('consumer retry drains once',f['reader_done'](c.object,reader.value,1));c.close()
  check('reset allocation ABA',api.q6d_mock_configure(len(payloads),stages[1]['shared_bytes'],kernel));c=Session(descriptor,image,payloads,metadata,schema);base=api.q6d_mock_base(0);api.q6d_mock_recycle(base);t=U()
  check('recycled CUDA buffer ID refuses Begin',not f['begin'](c.object,c.array(),len(c.views),c.meta,0,C.byref(t)));check('recycled address cannot free foreign allocation',not f['retire'](c.object,c.views[0].handle));check('recycled address cannot upload into foreign allocation',not f['write'](c.object,c.views[0].handle,0,payloads[0],len(payloads[0])));api.q6d_mock_recycle(base)
  fault('free');check('failed free retains retry handle',not f['retire'](c.object,c.views[0].handle));fault();check('free retry unregisters only once',f['retire'](c.object,c.views[0].handle));c.handles=c.handles[1:]
  for h in c.handles:check('ABA fixture cleanup',f['retire'](c.object,h))
  c.handles=[];fault('unload');check('unload failure retains cleanup object',not f['destroy'](c.object));fault();check('unload retry succeeds',f['destroy'](c.object));c.object=None
 cases.append(dict(case=item['case'],delivery=item['delivery'],status='PASS',source_outputs=6,ordered_kernels=3))
 (out/'progress.json').write_text(json.dumps(dict(cases=cases,checks=len(checks)),indent=2)+'\n');print('PASS Q6d',item['case'],item['delivery'],flush=True)
report=dict(status='PASS',gpu_device=gpu_device,checks=checks,staging_products=len(cases),source_outputs=6*len(cases),torch_and_scalar_comparisons=12*len(cases),
 authenticated_Q6c_files=True,unchanged_ir_and_explanations=True,exact_Q3_device_publication=True,opaque_device_addresses=True,explicit_context_stream_event_bindings=True,unknown_drain_keeps_leases=True,
 gpu_execution=False,driver_model_only=True,search_invocations=0,aot_selection='deferred')
(out/'unchanged-Q6c-artifacts.json').write_text(json.dumps(unchanged,indent=2)+'\n')
(out/'report.json').write_text(json.dumps(report,indent=2)+'\n');print('PASS Q6d checks',len(checks))
