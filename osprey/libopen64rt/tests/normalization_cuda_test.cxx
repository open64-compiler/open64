/* Copyright (C) 2026 Open64 Project */

/* Exercise the real LN4 adapter with CUDA API mocks and audited native guards.
 * No GPU work or replacement adapter implementation; runtime leases stay real.
 * Design: doc/AI-COMPILER-LAYERNORM-COMPILED-ENTRY-v0.1.md. */
#include "open64_normalization_cuda.h"
#include <cuda.h>
#include <vector>
#include <string.h>
#include <stdlib.h>
#include "entries.h"

static FILE *trace;
static unsigned checks;
static OPEN64_NORMALIZATION_COMPILED_ENTRY manifest;
static std::vector<unsigned char> image;
static std::vector<OPEN64_TARGET_BUFFER_VIEW> roots;
static OPEN64_NORMALIZATION_CUDA *adapter;
static OPEN64_NORMALIZATION_RUNTIME *runtime;
static uint64_t ticket;
static unsigned loads,unloads,launches,drains,hook_calls;
static int fault,fail_event;
static void (*launch_observer)(void **);
static void (*drain_observer)();

/* Every assertion is retained with native source line, including refusals. */
static void check(bool value,const char *name)
{
  fprintf(trace,"AI_NORM_CUDA_TEST 1 CHECK %s %u %llu\n",name,value?1:0,(unsigned long long)manifest.guard.source_line);
  if (!value) {fprintf(stderr,"failed %s fault=%d\n",name,fault);exit(1);}++checks;
}

/* Stable fake driver handles are never dereferenced by test or production code. */
static CUcontext context() {return reinterpret_cast<CUcontext>(0x100);}
/* Explicit nondefault stream; real adapter still calls cuStreamGetCtx. */
static CUstream stream() {return reinterpret_cast<CUstream>(0x200);}

/* Mock context observations let the actual adapter reject foreign sessions. */
extern "C" CUresult CUDAAPI cuCtxGetCurrent(CUcontext *out)
{*out=fault==20 ? NULL:context();return CUDA_SUCCESS;}
/* The caller's expected device is zero in every retained case. */
extern "C" CUresult CUDAAPI cuCtxGetDevice(CUdevice *out)
{*out=fault==21 ? 1:0;return CUDA_SUCCESS;}
/* Stream ownership is checked independently from current context. */
extern "C" CUresult CUDAAPI cuStreamGetCtx(CUstream input,CUcontext *out)
{check(input==stream(),"exact_stream_query");*out=fault==19 ? NULL:context();return CUDA_SUCCESS;}
/* An old driver cannot supply this bounded adapter's minimum API contract. */
extern "C" CUresult CUDAAPI cuDriverGetVersion(int *out)
{*out=fault==18 ? 12000:12080;return CUDA_SUCCESS;}
/* The exact owned image reaches the actual module-load call after digest checking. */
extern "C" CUresult CUDAAPI cuModuleLoadData(CUmodule *out,const void *bytes)
{
  ++loads;check(bytes!=image.data() && !memcmp(bytes,image.data(),image.size()),"exact_owned_cubin_loaded");
  if (fault==16) return CUDA_ERROR_INVALID_IMAGE;
  *out=reinterpret_cast<CUmodule>(0x300);return CUDA_SUCCESS;
}
/* Rollback/unload failure is observable and must never lose module ownership. */
extern "C" CUresult CUDAAPI cuModuleUnload(CUmodule module)
{++unloads;check(module==reinterpret_cast<CUmodule>(0x300),"exact_module_unloaded");return fault==31 ? CUDA_ERROR_UNKNOWN:CUDA_SUCCESS;}
/* Foreign symbols are never searched as a fallback for the selected entry. */
extern "C" CUresult CUDAAPI cuModuleGetFunction(CUfunction *out,CUmodule module,const char *name)
{
  check(module==reinterpret_cast<CUmodule>(0x300) && !strcmp(name,manifest.kernel),"exact_symbol_lookup");
  if (fault==17) return CUDA_ERROR_NOT_FOUND;
  *out=reinterpret_cast<CUfunction>(0x400);return CUDA_SUCCESS;
}
/* Live device observations deliberately disagree with each resource independently. */
extern "C" CUresult CUDAAPI cuDeviceGetAttribute(int *out,CUdevice_attribute attribute,CUdevice device)
{
  check(device==0,"exact_device_query");
  switch (attribute) {
  case CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR:*out=manifest.guard.architecture<=2 ? 9:10;if(fault==8)++*out;break;
  case CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR:*out=fault==9 ? 1:0;break;
  case CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X:*out=fault==10 ? 0:INT32_MAX;break;
  case CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X:*out=fault==11 ? 1:1024;break;
  case CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK:*out=fault==12 ? -1:49152;break;
  default:return CUDA_ERROR_INVALID_VALUE;
  }
  return fault==30 ? CUDA_ERROR_UNKNOWN:CUDA_SUCCESS;
}
/* Function queries test compiled facts, not plan estimates masquerading as queries. */
extern "C" CUresult CUDAAPI cuFuncGetAttribute(int *out,CUfunction_attribute attribute,CUfunction function)
{
  check(function==reinterpret_cast<CUfunction>(0x400),"exact_function_query");
  switch (attribute) {
  case CU_FUNC_ATTRIBUTE_NUM_REGS:*out=static_cast<int>(manifest.compiled_registers)+(fault==1 || fault==31);break;
  case CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES:*out=static_cast<int>(manifest.static_shared)+(fault==2);break;
  case CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK:*out=fault==3 ? 1:1024;break;
  case CU_FUNC_ATTRIBUTE_BINARY_VERSION:*out=(manifest.guard.architecture<=2 ? 90:100)+(fault==4);break;
  case CU_FUNC_ATTRIBUTE_PTX_VERSION:*out=fault==5 ? 80:87;break;
  case CU_FUNC_ATTRIBUTE_CLUSTER_SIZE_MUST_BE_SET:*out=fault==6 ? 1:0;break;
  default:return CUDA_ERROR_INVALID_VALUE;
  }
  return CUDA_SUCCESS;
}
/* Model only resource admission; never infer measured scheduling performance. */
extern "C" CUresult CUDAAPI cuOccupancyMaxActiveBlocksPerMultiprocessor(int *out,CUfunction function,int threads,size_t shared)
{
  check(function==reinterpret_cast<CUfunction>(0x400) && threads==static_cast<int>(manifest.guard.threads) && !shared,
      "selected_occupancy_geometry");*out=static_cast<int>(manifest.resident_floor)-(fault==7);return CUDA_SUCCESS;
}
/* Verify four n64 pointer slots and an absent fifth parameter. */
extern "C" CUresult CUDAAPI cuFuncGetParamInfo(CUfunction,size_t index,size_t *offset,size_t *size)
{
  if (index==4 && fault!=15) return CUDA_ERROR_INVALID_VALUE;
  *offset=index*8+(fault==14);*size=fault==13 ? 4:8;return CUDA_SUCCESS;
}
/* Validate pointer metadata independently of the registry's mock root resolver. */
extern "C" CUresult CUDAAPI cuPointerGetAttribute(void *out,CUpointer_attribute attribute,CUdeviceptr)
{
  switch (attribute) {
  case CU_POINTER_ATTRIBUTE_CONTEXT:*static_cast<CUcontext *>(out)=fault==32 ? NULL:context();break;
  case CU_POINTER_ATTRIBUTE_MEMORY_TYPE:*static_cast<CUmemorytype *>(out)=fault==33 ? CU_MEMORYTYPE_HOST:CU_MEMORYTYPE_DEVICE;break;
  case CU_POINTER_ATTRIBUTE_DEVICE_ORDINAL:*static_cast<int *>(out)=fault==34 ? 1:0;break;
  default:return CUDA_ERROR_INVALID_VALUE;
  }return CUDA_SUCCESS;
}
/* Address-range queries cover actual subranges, not caller-declared extents. */
extern "C" CUresult CUDAAPI cuMemGetAddressRange(CUdeviceptr *base,size_t *bytes,CUdeviceptr address)
{
  for (size_t i=0;i<roots.size();++i) {
    CUdeviceptr start=reinterpret_cast<uintptr_t>(roots[i].address);
    if (address>=start && address-start<roots[i].bytes) {*base=start;*bytes=fault==35 ? 0:roots[i].bytes;return CUDA_SUCCESS;}
  }return CUDA_ERROR_INVALID_VALUE;
}
/* Capture the actual launch ABI, including per-role offsets and zero dynamic SMEM. */
extern "C" CUresult CUDAAPI cuLaunchKernel(CUfunction function,unsigned gx,unsigned gy,unsigned gz,
    unsigned bx,unsigned by,unsigned bz,unsigned shared,CUstream input,void **args,void **extra)
{
  ++launches;check(function==reinterpret_cast<CUfunction>(0x400) && input==stream() && !extra && !shared &&
      gx==manifest.guard.rows && gy==1 && gz==1 && bx==manifest.guard.threads && by==1 && bz==1,"exact_launch_geometry");
  for (unsigned i=0;i<4;++i) {
    uintptr_t expected=0;
    for (unsigned j=0;j<=i;++j) if (manifest.guard.ranges[i].role==manifest.guard.ranges[j].role) {
      expected=reinterpret_cast<uintptr_t>(roots[j].address)+manifest.guard.ranges[i].offset;break;
    }
    check(*static_cast<CUdeviceptr *>(args[i])==expected,"exact_subrange_pointer_argument");
  }
  if(fail_event!=1 && launch_observer)launch_observer(args);
  return fail_event==1 ? CUDA_ERROR_UNKNOWN:CUDA_SUCCESS;
}
/* Ambiguous launches must join this exact stream before releasing any lease. */
extern "C" CUresult CUDAAPI cuStreamSynchronize(CUstream input)
{
  ++drains;check(input==stream(),"exact_stream_drain");
  if(fail_event!=11 && drain_observer)drain_observer();
  return fail_event==11 ? CUDA_ERROR_UNKNOWN:CUDA_SUCCESS;
}

/* SHA hook stand-in is an exact-byte oracle for the retained cubin. Production
 * supplies a trusted SHA-256 implementation, not this test-only comparator. */
static int digest(void *,const void *bytes,size_t size,unsigned char out[32])
{
  check(bytes!=image.data(),"digest_uses_owned_image_copy");
  if (fault==22 || size!=image.size() || memcmp(bytes,image.data(),size)) return 1;
  memcpy(out,manifest.cubin_sha256,32);return 0;
}
/* Input hook observes module pins and supports safe runtime reentry. */
static int ready(void *,const OPEN64_NORMALIZATION_INVOCATION *v)
{
  ++hook_calls;check(v->ticket==ticket,"hook_ticket_binding");
  check(open64_normalization_cuda_destroy(adapter)==OPEN64_NORM_BUSY,"callback_module_pin");
  check(open64_normalization_advance(runtime,ticket,0)==OPEN64_NORM_BUSY,"callback_runtime_busy");
  return fail_event==0;
}
/* Consumer completion is separate from kernel drain and may fail/retry. */
static int publish(void *,const OPEN64_NORMALIZATION_INVOCATION *) {++hook_calls;return fail_event==12;}
/* Registry accepts only exact externally owned whole allocations in this mock. */
static int validate(void *,const OPEN64_TARGET_BUFFER_VIEW *view,size_t bytes,size_t align)
{
  for (size_t i=0;i<roots.size();++i) if (roots[i].address==view->address && roots[i].bytes==view->bytes &&
      bytes==view->bytes && reinterpret_cast<uintptr_t>(view->address)%align==0) return 0;
  return 1;
}
/* Resolve repeated read roles to the same actual backing key. */
static int resolve(void *,const OPEN64_TARGET_BUFFER_VIEW *view,uint64_t *key)
{
  for (size_t i=0;i<roots.size();++i) if (roots[i].address==view->address && roots[i].bytes==view->bytes) {*key=i+1;return 0;}
  return 1;
}
/* Use the public real adapter entry point; no test copy of admission logic. */
static int open(const OPEN64_NORMALIZATION_COMPILED_ENTRY &m,OPEN64_NORMALIZATION_CUDA **out)
{
  OPEN64_NORMALIZATION_CUDA_HOOKS hooks={NULL,digest,ready,publish};
  return open64_normalization_cuda_open(&m,image.data(),image.size(),context(),stream(),0,m.guard.context,1,&hooks,trace,out);
}
/* Existing base protocol owns cancellation, while wrapper close releases module pins. */
static void cancel()
{
  check(open64_normalization_abort(runtime,ticket)==0,"abort_safe");
  check(open64_normalization_cuda_close(adapter,runtime,ticket)==0,"wrapper_close");
}

/* Consume generated audited manifests/cubins; no hand-constructed guard facts. */
int main(int argc,char **argv)
{
  if (argc!=4) return 2;
  unsigned index=static_cast<unsigned>(atoi(argv[1]));if(index>=sizeof(entries)/sizeof(entries[0]))return 2;
  manifest=entries[index];FILE *file=fopen(argv[2],"rb");if(!file)return 2;
  image.resize(static_cast<size_t>(manifest.cubin_bytes));bool read=fread(image.data(),1,image.size(),file)==image.size() && fgetc(file)==EOF;
  fclose(file);if(!read)return 2;trace=fopen(argv[3],"wx");if(!trace)return 2;
  for (fault=1;fault<=30;++fault) {
    fprintf(trace,"AI_NORM_CUDA_TEST 1 FAULT %d %llu\n",fault,(unsigned long long)manifest.guard.source_line);
    OPEN64_NORMALIZATION_COMPILED_ENTRY bad=manifest;OPEN64_NORMALIZATION_CUDA *out=reinterpret_cast<OPEN64_NORMALIZATION_CUDA *>(0x77);
    if(fault==23)bad.cubin_sha256[0]^=1;
    if(fault==24)--bad.cubin_bytes;
    if(fault==25)bad.guard.source_version=1;
    if(fault==26)bad.kernel[0]='x';
    if(fault==27)bad.registers_upper=bad.compiled_registers-1;
    if(fault==28)bad.static_shared=49153;
    if(fault==29)bad.resident_floor=0;
    unsigned before=loads;
    check(open(bad,&out)!=0 && out==reinterpret_cast<OPEN64_NORMALIZATION_CUDA *>(0x77),"open_refusal_unchanged_output");
    if(fault>=18 && fault<=29)check(loads==before,"preflight_failure_never_loads");
  }
  fault=0;
  for(unsigned field=0;field<6;++field) {
    OPEN64_NORMALIZATION_COMPILED_ENTRY bad=manifest;OPEN64_NORMALIZATION_CUDA *out=NULL;
    if(field==0)bad.guard.ptx=UINT64_MAX;
    if(field==1)bad.guard.toolkit=UINT64_MAX;
    if(field==2)bad.guard.ctas=UINT64_MAX;
    if(field==3)bad.guard.threads=0;
    if(field==4)bad.version=2;
    if(field==5)--bad.size;
    unsigned before=loads;
    check(open(bad,&out)==OPEN64_NORM_INVALID && !out && before==loads,"descriptor_extent_or_version_refused");
  }
  fault=31;fprintf(trace,"AI_NORM_CUDA_TEST 1 FAULT 31 %llu\n",(unsigned long long)manifest.guard.source_line);
  OPEN64_NORMALIZATION_CUDA *cleanup=NULL;
  check(open(manifest,&cleanup)!=0 && cleanup,"failed_rollback_retains_cleanup_handle");
  fault=0;check(open64_normalization_cuda_destroy(cleanup)==0,"cleanup_retry_unload");
  check(open(manifest,&adapter)==0 && adapter,"open_exact_compiled_entry");
  OPEN64_TARGET_BUFFER_SERVICE service={};service.abi_version=1;service.target_id=1;service.device_id=0;service.validate_import=validate;
  runtime=open64_normalization_runtime_create(&service,resolve,static_cast<uint32_t>(manifest.guard.architecture),manifest.guard.context,1,trace);
  check(runtime!=NULL,"create_registry");uint64_t handles[4]={};
  for(unsigned i=0;i<4;++i) {
    unsigned same=i;for(unsigned j=0;j<i;++j)if(manifest.guard.ranges[i].role==manifest.guard.ranges[j].role){same=j;break;}
    if(same!=i){roots.push_back(roots[same]);handles[i]=handles[same];continue;}
    void *memory=NULL;check(posix_memalign(&memory,static_cast<size_t>(manifest.guard.ranges[i].alignment),
        static_cast<size_t>(manifest.guard.ranges[i].capacity))==0,"allocate_owned_root");
    OPEN64_TARGET_BUFFER_VIEW view={memory,static_cast<size_t>(manifest.guard.ranges[i].capacity),2,0};roots.push_back(view);
    check(open64_normalization_register(runtime,&view,&handles[i])==0,"register_root");
  }
  OPEN64_NORMALIZATION_RUNTIME *production=open64_normalization_runtime_create(&service,resolve,
      static_cast<uint32_t>(manifest.guard.architecture),manifest.guard.context,0,NULL);
  uint64_t unchanged=77;check(production && open64_normalization_cuda_admit(adapter,production,handles,&unchanged)!=0 && unchanged==77,"host_adapter_not_production");
  check(open64_normalization_runtime_destroy(production)==0,"destroy_unused_production_registry");
  for(unsigned scenario=0;scenario<9;++scenario) {
    check(open64_normalization_cuda_admit(adapter,runtime,handles,&ticket)==0,"admit_exact_entry");
    check(open64_normalization_cuda_destroy(adapter)==OPEN64_NORM_BUSY,"live_ticket_pins_module");
    check(open64_normalization_cuda_close(adapter,runtime,ticket)==OPEN64_NORM_BUSY,"early_close_keeps_pin");
    if(scenario==0) {
      fail_event=0;check(open64_normalization_advance(runtime,ticket,0)==OPEN64_NORM_BACKEND,"failed_input_wait");fail_event=-1;cancel();continue;
    }
    fail_event=-1;check(open64_normalization_advance(runtime,ticket,0)==0,"inputs_ready");
    if(scenario>=4)fault=28+static_cast<int>(scenario); // pointer context/type/device/extent, then current context
    if(scenario==8)fault=20;
    if(scenario==1)fail_event=1;
    unsigned before=launches;int launched=open64_normalization_advance(runtime,ticket,1);
    if(scenario==1 || scenario>=4) {
      check(launched==OPEN64_NORM_BACKEND,"failed_launch_or_pointer");
      check(launches==before+(scenario==1),"pointer_failure_no_launch");
      check(open64_normalization_abort(runtime,ticket)==OPEN64_NORM_BUSY,"ambiguous_launch_pins_leases");
      fault=0;fail_event=11;check(open64_normalization_advance(runtime,ticket,11)==OPEN64_NORM_BACKEND,"drain_failure_keeps_pin");
      check(open64_normalization_cuda_destroy(adapter)==OPEN64_NORM_BUSY,"failed_drain_pins_module");
      fail_event=-1;check(open64_normalization_advance(runtime,ticket,11)==0,"drain_retry");cancel();continue;
    }
    check(launched==0,"launch_exact_entry");check(open64_normalization_advance(runtime,ticket,11)==0,"kernel_drained");
    if(scenario==2){fail_event=12;check(open64_normalization_advance(runtime,ticket,12)==OPEN64_NORM_BACKEND,"consumer_failure");
      check(open64_normalization_abort(runtime,ticket)==OPEN64_NORM_BUSY,"consumer_failure_pins_output");fail_event=-1;}
    check(open64_normalization_advance(runtime,ticket,12)==0,"consumer_joined");
    check(open64_normalization_advance(runtime,ticket,13)==0 && open64_normalization_advance(runtime,ticket,14)==0,"leases_released");
    check(open64_normalization_cuda_close(adapter,runtime,ticket)==0,"completed_wrapper_close");
  }
  fault=31;check(open64_normalization_cuda_destroy(adapter)==OPEN64_NORM_BACKEND,"unload_failure_retains_object");
  fault=0;check(open64_normalization_cuda_destroy(adapter)==0,"destroy_after_all_tickets");
  OPEN64_NORMALIZATION_CUDA_HOOKS hooks={NULL,digest,ready,publish};
  check(open64_normalization_cuda_open(&manifest,image.data(),image.size(),context(),stream(),0,
      manifest.guard.context,1,&hooks,NULL,&adapter)==0,"trace_off_open_identical_contract");
  check(open64_normalization_cuda_admit(adapter,runtime,handles,&ticket)==0,"trace_off_admit");
  const unsigned events[]={0,1,11,12,13,14};
  for(unsigned i=0;i<6;++i)check(open64_normalization_advance(runtime,ticket,events[i])==0,"trace_off_event_parity");
  check(open64_normalization_cuda_close(adapter,runtime,ticket)==0 && open64_normalization_cuda_destroy(adapter)==0,
      "trace_off_release_and_unload");
  for(unsigned i=0;i<4;++i){bool first=true;for(unsigned j=0;j<i;++j)if(handles[j]==handles[i])first=false;
    if(first){check(open64_normalization_retire(runtime,handles[i])==0,"retire_unleased_root");free(roots[i].address);}}
  check(open64_normalization_runtime_destroy(runtime)==0,"destroy_registry");
  fprintf(trace,"AI_NORM_CUDA_TEST 1 SUMMARY %u %u %u %u %u %u host_mock_only %llu\n",checks,loads,unloads,launches,drains,hook_calls,
      (unsigned long long)manifest.guard.source_line);fclose(trace);return 0;
}
