/* Copyright (C) 2026 Open64 Project */
/* Q6d test-only delayed CUDA Driver model, compiled against the real SDK.
 * Synthetic device addresses cannot be host-dereferenced. No GPU/timing claim.
 * Contract: doc/AI-COMPILER-MLA-Q6D-DEVICE-BINDINGS-v1.0.md. */
#include <cuda.h>
#include <map>
#include <vector>
#include <functional>
#include <cstring>
#include <cstdlib>
#include <string>
#include <algorithm>
namespace {
struct BUFFER {CUdeviceptr base;size_t bytes;unsigned long long id;std::vector<unsigned char> data;};
struct STREAM {unsigned long long id;std::vector<std::function<void()> > commands;size_t done;};
struct EVENT {CUstream stream;size_t target;bool recorded;};
std::map<CUdeviceptr,BUFFER> buffers;std::map<void *,size_t> hosts;std::map<CUstream,STREAM> streams;std::map<CUevent,EVENT> events;
std::string fault;unsigned long long next_id=100,next_address=0x100000000ULL,context_id=10;CUcontext current=(CUcontext)0x101;
unsigned arguments=0,shared_bytes=0,launches=0,loads=0,unloads=0,reads=0,writes=0;bool (*kernel)(unsigned,void **)=NULL;
/* Fault injection is explicit test state; the production adapter sees SDK errors. */
bool Fail(const char *name) {return ("|"+fault+"|").find(std::string("|")+name+"|")!=std::string::npos;}
/* Opaque numeric allocations resolve only inside this device model. */
BUFFER *Find(CUdeviceptr p,size_t n) {auto i=buffers.upper_bound(p);if(i==buffers.begin()) return NULL;--i;return p>=i->second.base && p-i->second.base<=i->second.bytes && n<=i->second.bytes-(p-i->second.base)?&i->second:NULL;}
/* Preserve canaries while exposing only the allocation's payload to CPU kernels. */
void *Address(CUdeviceptr p,size_t n) {BUFFER *b=Find(p,n);if(!b) std::abort();return b->data.data()+32+(p-b->base);}
/* Execute commands only on explicit progress/synchronization. */
void Advance(CUstream s,size_t target) {STREAM &stream=streams.at(s);while(stream.done<target) {auto command=stream.commands[stream.done++];command();}}
}
extern "C" {
/* A test session starts only after every prior resource has been released. */
int q6d_mock_configure(unsigned count,unsigned shared,bool (*entry)(unsigned,void **)) {
 if(!buffers.empty() || !events.empty() || !hosts.empty()) return 0;streams.clear();streams[(CUstream)0x201]=STREAM{20,{},0};streams[(CUstream)0x202]=STREAM{21,{},0};streams[(CUstream)0x203]=STREAM{22,{},0};
 arguments=count;shared_bytes=shared;kernel=entry;fault.clear();current=(CUcontext)0x101;context_id=10;launches=loads=unloads=reads=writes=0;return 1;
}
/* Tests can emulate wrong contexts and exact failure boundaries without IR changes. */
void q6d_mock_fault(const char *name) {fault=name?name:"";}
/* Consumer/execution progress is independent from host publication. */
void q6d_mock_advance() {for(auto &s:streams) Advance(s.first,s.second.commands.size());}
/* Recycled allocations must never pass identity validation or be freed as old. */
void q6d_mock_recycle(CUdeviceptr p) {BUFFER *b=Find(p,1);if(b) b->id^=0x100000000ULL;}
/* Test-only allocation order lookup never supplies runtime lease authority. */
CUdeviceptr q6d_mock_base(unsigned index) {for(auto &a:buffers) {if(!index--)return a.first;}return 0;}
/* Raw device mutation models a cooperative-owner violation for negative tests. */
void q6d_mock_poke(CUdeviceptr p,const void *data,size_t n) {memcpy(Address(p,n),data,n);}
/* Consumer work remains queued so reader release must observe its own event. */
void q6d_mock_consume(CUdeviceptr address) {streams.at((CUstream)0x203).commands.push_back([=]{volatile unsigned char value=*(unsigned char *)Address(address,1);(void)value;});}
/* Observations are diagnostics only, never an event readiness proof. */
unsigned long long q6d_mock_stat(unsigned kind) {if(kind==0)return buffers.size();if(kind==1)return events.size();if(kind==2)return hosts.size();if(kind==3)return launches;if(kind==4)return loads;if(kind==5)return unloads;if(kind==6)return reads;if(kind==7)return writes;return 0;}
/* Canary checks cover every allocation, including unpublished dirty outputs. */
int q6d_mock_canaries() {for(auto &a:buffers) {for(unsigned i=0;i<32;++i) if(a.second.data[i]!=0xa5 || a.second.data[a.second.bytes+32+i]!=0x5a)return 0;}return 1;}
/* Observe current context without implicitly switching threads. */
CUresult CUDAAPI cuCtxGetCurrent(CUcontext *out) {*out=Fail("context")?(CUcontext)0x999:current;return CUDA_SUCCESS;}
/* Report context generation so stale opaque handles fail binding. */
CUresult CUDAAPI cuCtxGetId(CUcontext,unsigned long long *out) {*out=context_id+(Fail("context_id")?1:0);return CUDA_SUCCESS;}
/* Observe the model device for real runtime ownership checks. */
CUresult CUDAAPI cuCtxGetDevice(CUdevice *out) {*out=0;return CUDA_SUCCESS;}
/* Expose the pinned SDK contract; this does not certify hardware. */
CUresult CUDAAPI cuDriverGetVersion(int *out) {*out=12080;return CUDA_SUCCESS;}
/* Refuse foreign streams and report their context association. */
CUresult CUDAAPI cuStreamGetCtx(CUstream s,CUcontext *out) {if(!streams.count(s))return CUDA_ERROR_INVALID_HANDLE;*out=Fail("stream_context")?(CUcontext)0x999:current;return CUDA_SUCCESS;}
/* Observe stream generation independently from its opaque pointer. */
CUresult CUDAAPI cuStreamGetId(CUstream s,unsigned long long *out) {if(!streams.count(s))return CUDA_ERROR_INVALID_HANDLE;*out=streams[s].id+(Fail("stream_id")?1:0);return CUDA_SUCCESS;}
/* Expose graph capture so the adapter can refuse it before work. */
CUresult CUDAAPI cuStreamIsCapturing(CUstream,CUstreamCaptureStatus *out) {*out=Fail("capture")?CU_STREAM_CAPTURE_STATUS_ACTIVE:CU_STREAM_CAPTURE_STATUS_NONE;return CUDA_SUCCESS;}
/* Supply explicit device limits to the real resource verifier. */
CUresult CUDAAPI cuDeviceGetAttribute(int *out,CUdevice_attribute attr,CUdevice) {*out=attr==CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR?9:attr==CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR?0:attr==CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X?INT32_MAX:attr==CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X?1024:49152;return CUDA_SUCCESS;}
/* Issue non-host device addresses with a fresh CUDA allocation ID. */
CUresult CUDAAPI cuMemAlloc(CUdeviceptr *out,size_t n) {if(Fail("allocate"))return CUDA_ERROR_OUT_OF_MEMORY;CUdeviceptr p=next_address;next_address+=((n+255)/256)*256+4096;BUFFER b{p,n,++next_id,std::vector<unsigned char>(n+64,0xcd)};std::fill(b.data.begin(),b.data.begin()+32,0xa5);std::fill(b.data.end()-32,b.data.end(),0x5a);buffers.emplace(p,std::move(b));*out=p;return CUDA_SUCCESS;}
/* Reclaim only registered model allocations; fault retains ownership. */
CUresult CUDAAPI cuMemFree(CUdeviceptr p) {if(Fail("free"))return CUDA_ERROR_UNKNOWN;if(!buffers.erase(p))return CUDA_ERROR_INVALID_VALUE;return CUDA_SUCCESS;}
/* Track pinned staging lifetime independently of caller buffers. */
CUresult CUDAAPI cuMemAllocHost(void **out,size_t n) {if(Fail("host_allocate"))return CUDA_ERROR_OUT_OF_MEMORY;*out=malloc(n);hosts[*out]=n;return *out?CUDA_SUCCESS:CUDA_ERROR_OUT_OF_MEMORY;}
/* Free only owned staging; failed cleanup preserves its record. */
CUresult CUDAAPI cuMemFreeHost(void *p) {if(Fail("host_free"))return CUDA_ERROR_UNKNOWN;if(!hosts.erase(p))return CUDA_ERROR_INVALID_VALUE;free(p);return CUDA_SUCCESS;}
/* Resolve actual modeled allocation identity and memory domain. */
CUresult CUDAAPI cuPointerGetAttribute(void *out,CUpointer_attribute attr,CUdeviceptr p) {BUFFER *b=Find(p,1);if(!b)return CUDA_ERROR_INVALID_VALUE;switch(attr) {case CU_POINTER_ATTRIBUTE_CONTEXT:*(CUcontext *)out=current;break;case CU_POINTER_ATTRIBUTE_MEMORY_TYPE:*(CUmemorytype *)out=CU_MEMORYTYPE_DEVICE;break;case CU_POINTER_ATTRIBUTE_DEVICE_ORDINAL:*(int *)out=0;break;case CU_POINTER_ATTRIBUTE_BUFFER_ID:*(unsigned long long *)out=b->id;break;default:return CUDA_ERROR_INVALID_VALUE;}return CUDA_SUCCESS;}
/* Bound subranges without allowing registry host dereferences. */
CUresult CUDAAPI cuMemGetAddressRange(CUdeviceptr *base,size_t *bytes,CUdeviceptr p) {BUFFER *b=Find(p,1);if(!b)return CUDA_ERROR_INVALID_VALUE;*base=b->base;*bytes=b->bytes;return CUDA_SUCCESS;}
/* Queue reads from owned pinned host staging; do not complete early. */
CUresult CUDAAPI cuMemcpyHtoDAsync(CUdeviceptr dst,const void *src,size_t n,CUstream s) {if(Fail("copy"))return CUDA_ERROR_UNKNOWN;if(!Find(dst,n) || !hosts.count((void *)src))return CUDA_ERROR_INVALID_VALUE;streams.at(s).commands.push_back([=]{memcpy(Address(dst,n),src,n);});++writes;return CUDA_SUCCESS;}
/* Queue writes to owned pinned staging; caller stacks remain untouched. */
CUresult CUDAAPI cuMemcpyDtoHAsync(void *dst,CUdeviceptr src,size_t n,CUstream s) {if(Fail("copy"))return CUDA_ERROR_UNKNOWN;if(!Find(src,n) || !hosts.count(dst))return CUDA_ERROR_INVALID_VALUE;streams.at(s).commands.push_back([=]{memcpy(dst,Address(src,n),n);});++reads;return CUDA_SUCCESS;}
/* Create a unique owned event; failures create no inferred readiness. */
CUresult CUDAAPI cuEventCreate(CUevent *out,unsigned) {if(Fail("event_create"))return CUDA_ERROR_OUT_OF_MEMORY;*out=(CUevent)(uintptr_t)(++next_id*16+0x10000);events[*out]=EVENT{NULL,0,false};return CUDA_SUCCESS;}
/* Capture one immutable stream position; rerecording is a test failure. */
CUresult CUDAAPI cuEventRecord(CUevent e,CUstream s) {if(Fail("event_record"))return CUDA_ERROR_UNKNOWN;if(!events.count(e)||!streams.count(s))return CUDA_ERROR_INVALID_HANDLE;if(events[e].recorded)return CUDA_ERROR_INVALID_VALUE;events[e]=EVENT{s,streams[s].commands.size(),true};return CUDA_SUCCESS;}
/* Observe readiness without advancing queued work. */
CUresult CUDAAPI cuEventQuery(CUevent e) {if(Fail("event_query"))return CUDA_ERROR_UNKNOWN;if(!events.count(e)||!events[e].recorded)return CUDA_ERROR_NOT_READY;EVENT v=events[e];return streams[v.stream].done>=v.target?CUDA_SUCCESS:CUDA_ERROR_NOT_READY;}
/* Complete only through the recorded stream position, or fail undrained. */
CUresult CUDAAPI cuEventSynchronize(CUevent e) {if(Fail("io_sync")||Fail("event_sync"))return CUDA_ERROR_UNKNOWN;if(!events.count(e)||!events[e].recorded)return CUDA_ERROR_INVALID_HANDLE;EVENT v=events[e];Advance(v.stream,v.target);return CUDA_SUCCESS;}
/* Copy the recorded generation before event handle destruction. */
CUresult CUDAAPI cuStreamWaitEvent(CUstream s,CUevent e,unsigned) {if(Fail("wait_event"))return CUDA_ERROR_UNKNOWN;if(!events.count(e)||!events[e].recorded)return CUDA_ERROR_INVALID_HANDLE;EVENT v=events[e];streams.at(s).commands.push_back([=]{Advance(v.stream,v.target);});return CUDA_SUCCESS;}
/* Release the event handle, retaining failures for explicit retry. */
CUresult CUDAAPI cuEventDestroy(CUevent e) {if(Fail("event_destroy"))return CUDA_ERROR_UNKNOWN;return events.erase(e)?CUDA_SUCCESS:CUDA_ERROR_INVALID_HANDLE;}
/* Advance queued work only on successful explicit drain. */
CUresult CUDAAPI cuStreamSynchronize(CUstream s) {if(Fail("drain"))return CUDA_ERROR_UNKNOWN;if(!streams.count(s))return CUDA_ERROR_INVALID_HANDLE;Advance(s,streams[s].commands.size());return CUDA_SUCCESS;}
/* Record an authenticated load boundary without executing GPU code. */
CUresult CUDAAPI cuModuleLoadData(CUmodule *out,const void *) {if(Fail("load"))return CUDA_ERROR_UNKNOWN;*out=(CUmodule)0x5000;++loads;return CUDA_SUCCESS;}
/* Retain module ownership when injected unload fails. */
CUresult CUDAAPI cuModuleUnload(CUmodule) {if(Fail("unload"))return CUDA_ERROR_UNKNOWN;++unloads;return CUDA_SUCCESS;}
/* Associate each fixed emitted name with its real stage model. */
CUresult CUDAAPI cuModuleGetFunction(CUfunction *out,CUmodule,const char *name) {unsigned stage=strstr(name,"prepare")?0:strstr(name,"attention")?1:2;*out=(CUfunction)(uintptr_t)(0x6000+stage);return CUDA_SUCCESS;}
/* Report loaded-entry resource observations and explicit refusals. */
CUresult CUDAAPI cuFuncGetAttribute(int *out,CUfunction_attribute attr,CUfunction f) {unsigned stage=(uintptr_t)f-0x6000;*out=attr==CU_FUNC_ATTRIBUTE_NUM_REGS?32:attr==CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES?(stage==1?shared_bytes:0):attr==CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK?(Fail("resources")?1:1024):attr==CU_FUNC_ATTRIBUTE_BINARY_VERSION?90:attr==CU_FUNC_ATTRIBUTE_PTX_VERSION?90:0;return CUDA_SUCCESS;}
/* Report a bounded resident capacity, never measured throughput. */
CUresult CUDAAPI cuOccupancyMaxActiveBlocksPerMultiprocessor(int *out,CUfunction,int,size_t) {*out=1;return CUDA_SUCCESS;}
/* Check the complete pointer ABI, including extra-formal refusal. */
CUresult CUDAAPI cuFuncGetParamInfo(CUfunction,size_t i,size_t *offset,size_t *bytes) {if(i>=arguments)return CUDA_ERROR_INVALID_VALUE;*offset=i*8;*bytes=Fail("abi")?4:8;return CUDA_SUCCESS;}
/* Copy parameter values now and defer the actual emitted stage until progress. */
CUresult CUDAAPI cuLaunchKernel(CUfunction f,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,CUstream s,void **args,void **) {
 unsigned stage=(uintptr_t)f-0x6000;if(Fail(("launch"+std::to_string(stage)).c_str()))return CUDA_ERROR_UNKNOWN;
 std::vector<CUdeviceptr> copy(arguments);for(unsigned i=0;i<arguments;++i)copy[i]=*(CUdeviceptr *)args[i];
 streams.at(s).commands.push_back([=]{std::vector<void *> addresses;for(CUdeviceptr p:copy)addresses.push_back(Address(p,1));if(!kernel || !kernel(stage,addresses.data()))std::abort();});++launches;return CUDA_SUCCESS;
}
}
