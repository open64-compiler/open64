/* Copyright (C) 2026 Open64 Project */

/* LN4 optional CUDA Driver adapter: authenticated cubin, observed resources,
 * exact launch and module pins. No compiler linkage or new kernel producer.
 * Design: doc/AI-COMPILER-LAYERNORM-COMPILED-ENTRY-v0.1.md. */
#include "open64_normalization_cuda.h"
#include <cuda.h>
#include <atomic>
#include <mutex>
#include <new>
#include <string.h>
#include <limits.h>

static std::atomic<uint64_t> next_adapter_id(1);

struct OPEN64_NORMALIZATION_CUDA {
  std::mutex mutex;
  OPEN64_NORMALIZATION_COMPILED_ENTRY manifest;
  OPEN64_NORMALIZATION_CUDA_HOOKS hooks;
  OPEN64_NORMALIZATION_ENTRY entry;
  CUcontext context;
  CUstream stream;
  CUmodule module;
  CUfunction function;
  uint32_t device;
  bool ready;
  FILE *trace;
  std::atomic<uint64_t> sequence;
  uint64_t id;
  struct {OPEN64_NORMALIZATION_RUNTIME *runtime; uint64_t ticket;} pins[64];
  /* Initialize only owned plain state; mutex/atomic retain their constructors. */
  OPEN64_NORMALIZATION_CUDA():context(NULL),stream(NULL),module(NULL),function(NULL),
      device(0),ready(false),trace(NULL),sequence(0),id(next_adapter_id.fetch_add(1))
  {memset(&manifest,0,sizeof(manifest));memset(&hooks,0,sizeof(hooks));
   memset(&entry,0,sizeof(entry));memset(pins,0,sizeof(pins));}
};

namespace {
/* Trace actual API observations with source-line-last, not GPU certification. */
int report(OPEN64_NORMALIZATION_CUDA *p,const char *action,int status,uint64_t a=0,uint64_t b=0)
{
  if (p->trace) fprintf(p->trace,"AI_NORM_CUDA 1 EVENT %llu %llu %s %d %llu %llu %llu\n",
      (unsigned long long)p->id,(unsigned long long)++p->sequence,action,status,(unsigned long long)a,
      (unsigned long long)b,(unsigned long long)p->manifest.guard.source_line);
  return status;
}

/* Never switch a service thread's context or silently use its default stream. */
bool current(OPEN64_NORMALIZATION_CUDA *p)
{
  CUcontext context=NULL,stream_context=NULL;CUdevice device=-1;
  return cuCtxGetCurrent(&context)==CUDA_SUCCESS && context==p->context &&
      cuCtxGetDevice(&device)==CUDA_SUCCESS && device>=0 && static_cast<uint32_t>(device)==p->device &&
      cuStreamGetCtx(p->stream,&stream_context)==CUDA_SUCCESS && stream_context==p->context;
}

/* Refuse before device work if a live subrange differs from actual CUDA
 * ownership/type/extent. Registry remains responsible for alias/lease policy. */
bool pointer(OPEN64_NORMALIZATION_CUDA *p,const OPEN64_TARGET_BUFFER_VIEW &v)
{
  CUdeviceptr address=static_cast<CUdeviceptr>(reinterpret_cast<uintptr_t>(v.address)),base=0;
  CUcontext context=NULL;CUmemorytype memory=CU_MEMORYTYPE_HOST;int device=-1;size_t bytes=0;
  return v.device_id==p->device && v.tier==OPEN64_TARGET_BUFFER_DEVICE && v.bytes &&
      cuPointerGetAttribute(&context,CU_POINTER_ATTRIBUTE_CONTEXT,address)==CUDA_SUCCESS && context==p->context &&
      cuPointerGetAttribute(&memory,CU_POINTER_ATTRIBUTE_MEMORY_TYPE,address)==CUDA_SUCCESS && memory==CU_MEMORYTYPE_DEVICE &&
      cuPointerGetAttribute(&device,CU_POINTER_ATTRIBUTE_DEVICE_ORDINAL,address)==CUDA_SUCCESS &&
      device>=0 && static_cast<uint32_t>(device)==p->device &&
      cuMemGetAddressRange(&base,&bytes,address)==CUDA_SUCCESS && address>=base &&
      address-base<=bytes && v.bytes<=bytes-(address-base);
}

/* Runtime calls unlocked while its ticket is busy. Module pins remain until
 * wrapper close; deployment hooks may reenter the base runtime safely. */
int perform(void *context,uint32_t event,const OPEN64_NORMALIZATION_INVOCATION *v)
{
  OPEN64_NORMALIZATION_CUDA *p=static_cast<OPEN64_NORMALIZATION_CUDA *>(context);
  if (!p->ready || !v || memcmp(&v->guard,&p->manifest.guard,sizeof(v->guard)) || !current(p))
    return report(p,"context_or_binding",OPEN64_NORM_INVALID);
  int status=OPEN64_NORM_INVALID;
  if (event==OPEN64_NORM_READY) status=p->hooks.ready(p->hooks.context,v);
  else if (event==OPEN64_NORM_PUBLISH) status=p->hooks.publish(p->hooks.context,v);
  else if (event==OPEN64_NORM_DRAIN) status=cuStreamSynchronize(p->stream)==CUDA_SUCCESS ? 0:OPEN64_NORM_BACKEND;
  else if (event==OPEN64_NORM_LAUNCH) {
    CUdeviceptr addresses[4];void *args[4];
    for (unsigned i=0;i<4;++i) {
      if (!pointer(p,v->views[i])) return report(p,"pointer",OPEN64_NORM_INVALID,i,v->ticket);
      addresses[i]=static_cast<CUdeviceptr>(reinterpret_cast<uintptr_t>(v->views[i].address));args[i]=&addresses[i];
    }
    status=cuLaunchKernel(p->function,static_cast<unsigned>(v->guard.ctas),1,1,
        static_cast<unsigned>(v->guard.threads),1,1,0,p->stream,args,NULL)==CUDA_SUCCESS ? 0:OPEN64_NORM_BACKEND;
  }
  return report(p,"perform",status,event,v->ticket);
}

/* Descriptor checks do not duplicate the runtime's full guard verifier.
 * They prevent unsafe indexing/loading; exact guard legality is rechecked at admit. */
bool descriptor(const OPEN64_NORMALIZATION_COMPILED_ENTRY &m,size_t bytes,uint64_t context)
{
  const OPEN64_NORMALIZATION_GUARD &g=m.guard;char expected[80];unsigned digest=0;
  for (unsigned i=0;i<32;++i) digest|=m.cubin_sha256[i];
  if (m.version!=1 || m.size!=sizeof(m) || !bytes || bytes>64U*1024U*1024U || bytes!=m.cubin_bytes ||
      !digest || !m.plan_seal || !m.tree_seal || !m.registers_upper || m.registers_upper>255 ||
      !m.compiled_registers || m.compiled_registers>m.registers_upper || !m.resident_floor ||
      m.resident_floor>32 || g.abi_version!=1 || g.size!=sizeof(g) || g.source_version!=2 ||
      g.source_flags!=29687 || g.architecture<1 || g.architecture>4 || g.kind<1 || g.kind>3 ||
      !g.pu || g.pu>UINT32_MAX || !g.node || g.node>UINT32_MAX || !g.rows || g.rows!=g.ctas ||
      g.ctas>UINT32_MAX || !g.threads || g.threads>1024 || g.threads%32 ||
      g.context!=context || !context || g.route!=1 || g.environment!=1 || g.fp_options ||
      g.toolkit<12000 || g.toolkit>UINT32_MAX || g.ptx<(g.architecture<=2 ? 800U:807U) ||
      g.ptx>999 || g.ptx%100>9 ||
      m.static_shared!=g.reserved_shared || m.static_shared>49152 ||
      !memchr(m.kernel,0,sizeof(m.kernel))) return false;
  snprintf(expected,sizeof(expected),"open64_ln_ref_%llu_%llu_c%llu",(unsigned long long)g.pu,
      (unsigned long long)g.node,(unsigned long long)(g.kind-1));
  return !strcmp(expected,m.kernel);
}

/* Query exact loaded-entry resource/ABI facts; a smaller occupancy needs recost,
 * never an unreported fallback. Driver enforces accelerated-ISA compatibility. */
bool attribute(OPEN64_NORMALIZATION_CUDA *p,CUfunction_attribute key,uint64_t low,uint64_t high,int *out)
{
  *out=0;CUresult status=cuFuncGetAttribute(out,key,p->function);
  bool valid=status==CUDA_SUCCESS && *out>=0 && static_cast<uint64_t>(*out)>=low && static_cast<uint64_t>(*out)<=high;
  report(p,"attribute",valid ? 0:OPEN64_NORM_BACKEND,static_cast<uint64_t>(key),static_cast<uint64_t>(*out));
  if(status!=CUDA_SUCCESS)report(p,"cuda_error",static_cast<int>(status),static_cast<uint64_t>(key));
  return valid;
}

/* Keep launch/resource bounds separate from the guard's structural verifier.
 * Every failed resource stage produces an observation before rollback. */
bool resources(OPEN64_NORMALIZATION_CUDA *p)
{
  const OPEN64_NORMALIZATION_COMPILED_ENTRY &m=p->manifest;const OPEN64_NORMALIZATION_GUARD &g=m.guard;
  CUdevice device=static_cast<CUdevice>(p->device);int major=0,minor=0,grid=0,block=0,shared_limit=0;
  if (cuDeviceGetAttribute(&major,CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR,device)!=CUDA_SUCCESS ||
      cuDeviceGetAttribute(&minor,CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR,device)!=CUDA_SUCCESS ||
      major!=(g.architecture<=2 ? 9:10) || minor!=0 ||
      cuDeviceGetAttribute(&grid,CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X,device)!=CUDA_SUCCESS || grid<=0 || g.ctas>static_cast<uint64_t>(grid) ||
      cuDeviceGetAttribute(&block,CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X,device)!=CUDA_SUCCESS || block<=0 || g.threads>static_cast<uint64_t>(block) ||
      cuDeviceGetAttribute(&shared_limit,CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK,device)!=CUDA_SUCCESS || shared_limit<0 ||
      m.static_shared>static_cast<uint64_t>(shared_limit)) {
    report(p,"device_limits",OPEN64_NORM_BACKEND,static_cast<uint64_t>(major),static_cast<uint64_t>(minor));return false;
  }
  int registers=0,shared=0,threads=0,binary=0,ptx=0,cluster=0,resident=0;
  uint64_t binary_version=static_cast<uint64_t>(major*10+minor);
  if (!attribute(p,CU_FUNC_ATTRIBUTE_NUM_REGS,m.compiled_registers,m.compiled_registers,&registers) ||
      !attribute(p,CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES,m.static_shared,m.static_shared,&shared) ||
      !attribute(p,CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK,g.threads,INT_MAX,&threads) ||
      !attribute(p,CU_FUNC_ATTRIBUTE_BINARY_VERSION,binary_version,binary_version,&binary) ||
      !attribute(p,CU_FUNC_ATTRIBUTE_PTX_VERSION,(g.ptx/100)*10+g.ptx%100,INT_MAX,&ptx) ||
      !attribute(p,CU_FUNC_ATTRIBUTE_CLUSTER_SIZE_MUST_BE_SET,0,0,&cluster)) return false;
  if (cuOccupancyMaxActiveBlocksPerMultiprocessor(&resident,p->function,static_cast<int>(g.threads),0)!=CUDA_SUCCESS ||
      resident<=0 || static_cast<uint64_t>(resident)<m.resident_floor) {
    report(p,"resident",OPEN64_NORM_BACKEND,static_cast<uint64_t>(resident),m.resident_floor);return false;
  }
  size_t offset=0,size=0;
  for (unsigned i=0;i<4;++i) if (cuFuncGetParamInfo(p->function,i,&offset,&size)!=CUDA_SUCCESS || offset!=8*i || size!=8) {
    report(p,"parameter",OPEN64_NORM_BACKEND,i,size);return false;
  }
  if (cuFuncGetParamInfo(p->function,4,&offset,&size)!=CUDA_ERROR_INVALID_VALUE) {
    report(p,"parameter_count",OPEN64_NORM_BACKEND,4);return false;
  }
  p->entry.max_ctas=static_cast<uint64_t>(grid);p->entry.max_threads=static_cast<uint64_t>(threads);
  p->entry.max_shared=m.static_shared;
  report(p,"resources",0,static_cast<uint64_t>(registers),static_cast<uint64_t>(shared));
  report(p,"resident",0,static_cast<uint64_t>(resident),m.resident_floor);return true;
}

/* Rollback owns the module even when CUDA unload fails. Return a cleanup-only
 * handle rather than hiding an acquired resource or allowing partial admission. */
int rollback(OPEN64_NORMALIZATION_CUDA *p,OPEN64_NORMALIZATION_CUDA **output)
{
  report(p,"refuse",OPEN64_NORM_BACKEND);
  if (p->module && cuModuleUnload(p->module)!=CUDA_SUCCESS) {*output=p;return OPEN64_NORM_BACKEND;}
  delete p;return OPEN64_NORM_BACKEND;
}
}

/* Preflight/copy/hash precede CUDA loading. No source transformation or implicit
 * context creation occurs; publication is last except explicit cleanup handles. */
int open64_normalization_cuda_open(const OPEN64_NORMALIZATION_COMPILED_ENTRY *manifest,
    const void *image,size_t bytes,void *context,void *stream,uint32_t device,uint64_t context_id,
    uint32_t flags,const OPEN64_NORMALIZATION_CUDA_HOOKS *hooks,FILE *trace,OPEN64_NORMALIZATION_CUDA **output)
{
  if (!manifest || !image || !output || !context || reinterpret_cast<uintptr_t>(stream)<=2 ||
      device>INT_MAX || flags>1 || !hooks || !hooks->sha256 || !hooks->ready || !hooks->publish ||
      !descriptor(*manifest,bytes,context_id)) return OPEN64_NORM_INVALID;
  OPEN64_NORMALIZATION_CUDA *p=new(std::nothrow) OPEN64_NORMALIZATION_CUDA;
  unsigned char *copy=new(std::nothrow) unsigned char[bytes];
  if (!p || !copy) {delete p;delete[] copy;return OPEN64_NORM_CAPACITY;}
  p->manifest=*manifest;p->hooks=*hooks;p->context=static_cast<CUcontext>(context);
  p->stream=static_cast<CUstream>(stream);p->device=device;p->trace=trace;
  memcpy(copy,image,bytes);unsigned char digest[32]={};
  bool hashed=p->hooks.sha256(p->hooks.context,copy,bytes,digest)==0 && !memcmp(digest,p->manifest.cubin_sha256,32);
  // Cubin-only: reject PTX/fatbinary input rather than permitting hidden JIT code.
  hashed=hashed && bytes>=20 && !memcmp(copy,"\177ELF\2\1",6) && copy[18]==190 && copy[19]==0;
  int version=0;
  if (!hashed || !current(p) || cuDriverGetVersion(&version)!=CUDA_SUCCESS || version<12080 ||
      static_cast<uint64_t>(version)<p->manifest.guard.toolkit) {
    report(p,"preflight",OPEN64_NORM_INVALID);delete[] copy;delete p;return OPEN64_NORM_INVALID;
  }
  CUresult loaded=cuModuleLoadData(&p->module,copy);delete[] copy;report(p,"module_load",static_cast<int>(loaded));
  if (loaded!=CUDA_SUCCESS) return rollback(p,output);
  CUresult resolved=cuModuleGetFunction(&p->function,p->module,p->manifest.kernel);
  report(p,"resolve",static_cast<int>(resolved));
  if (resolved!=CUDA_SUCCESS || !resources(p)) return rollback(p,output);
  p->entry.manifest=p->manifest.guard;p->entry.flags=flags;p->entry.context=p;p->entry.perform=perform;
  p->ready=true;*output=p;return report(p,"open",0,p->manifest.plan_seal,p->manifest.tree_seal);
}

/* Never unload while registry tickets can call this adapter; retry CUDA failures
 * without dropping ownership. Caller externally excludes concurrent destruction. */
int open64_normalization_cuda_destroy(OPEN64_NORMALIZATION_CUDA *p)
{
  if (!p) return OPEN64_NORM_INVALID;
  {std::lock_guard<std::mutex> lock(p->mutex);
    for (unsigned i=0;i<64;++i) if (p->pins[i].ticket) return report(p,"pinned",OPEN64_NORM_BUSY);
    if (!current(p) || (p->module && cuModuleUnload(p->module)!=CUDA_SUCCESS)) return report(p,"unload",OPEN64_NORM_BACKEND);
    report(p,"unload",0);}
  delete p;return OPEN64_NORM_OK;
}

/* Lock only association bookkeeping; base admission cannot execute callbacks.
 * Reserve capacity first so an admitted ticket can never lose its module pin. */
int open64_normalization_cuda_admit(OPEN64_NORMALIZATION_CUDA *p,OPEN64_NORMALIZATION_RUNTIME *runtime,
    const uint64_t roots[4],uint64_t *output)
{
  if (!p || !runtime || !roots || !output) return OPEN64_NORM_INVALID;
  std::lock_guard<std::mutex> lock(p->mutex);
  if (!p->ready || !current(p)) return report(p,"admit",OPEN64_NORM_INVALID);
  unsigned slot=0;while (slot<64 && p->pins[slot].ticket) ++slot;
  if (slot==64) return report(p,"admit",OPEN64_NORM_CAPACITY);
  uint64_t ticket=0;int status=open64_normalization_admit(runtime,&p->manifest.guard,&p->entry,roots,&ticket);
  if (!status) {p->pins[slot].runtime=runtime;p->pins[slot].ticket=ticket;*output=ticket;}
  return report(p,"admit",status,ticket);
}

/* Failed or busy close preserves the pin; mismatched adapter/runtime/ticket
 * triples never reach the underlying registry's close operation. */
int open64_normalization_cuda_close(OPEN64_NORMALIZATION_CUDA *p,OPEN64_NORMALIZATION_RUNTIME *runtime,uint64_t ticket)
{
  if (!p || !runtime || !ticket) return OPEN64_NORM_INVALID;
  std::lock_guard<std::mutex> lock(p->mutex);
  for (unsigned i=0;i<64;++i) if (p->pins[i].ticket==ticket && p->pins[i].runtime==runtime) {
    int status=open64_normalization_close(runtime,ticket);
    if (!status) {p->pins[i].ticket=0;p->pins[i].runtime=NULL;}
    return report(p,"close",status,ticket);
  }
  return report(p,"close",OPEN64_NORM_INVALID,ticket);
}
