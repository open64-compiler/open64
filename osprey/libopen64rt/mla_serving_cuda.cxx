/* Copyright (C) 2026 Open64 Project */
/* Q6d optional CUDA Driver binding owner. Reuses Q3 leases/publication, never
 * changes WHIRL or invokes search/JIT. Unknown drain retains resources.
 * Contract: doc/AI-COMPILER-MLA-Q6D-DEVICE-BINDINGS-v1.0.md. */
#include "../be/vho/tensor_serving_cuda.h"
#include "../be/vho/tensor_quantized_plan.h"
#include "../be/vho/tensor_quantized_movement.h"
#include <cuda.h>
#include <mutex>
#include <list>
#include <map>
#include <cstring>
#include <climits>
#include <memory>

namespace {
/* Refusals carry no implied device completion or publication. */
void Require(bool value,const char *why) {if(!value) throw std::runtime_error(why);}
/* Raw addresses are opaque until CUDA has validated their allocation. */
CUdeviceptr Device(const void *address) {return (CUdeviceptr)(uintptr_t)address;}
/* Preserve owner/generation as well as the numeric allocation ID. */
bool Equal(const VHO_SERVING_HANDLE &a,const VHO_SERVING_HANDLE &b) {return a.owner==b.owner && a.id==b.id && a.generation==b.generation;}
}
struct VHO_MLA_CUDA::IMPL {
    struct ALLOCATION {VHO_SERVING_HANDLE handle;CUdeviceptr base;UINT64 bytes,identity;bool registered,identity_known;};
    struct IO {void *host;CUevent event;CUstream stream;unsigned long long stream_id;bool drained;};
    struct JOB {UINT64 ticket,bundle;std::string snapshot;CUevent event;bool submitted,armed,recorded,failed,done;};
    struct READER {UINT64 token,bundle;CUstream stream;unsigned long long stream_id;CUevent event;bool sealed,recorded;};
    std::recursive_mutex _mutex;
    VHO_MLA_CUDA_CODE _code;
    VHO_MLA_CUDA_HOOKS _hooks;
    CUcontext _context;
    CUstream _producer,_execution;
    unsigned long long _context_id,_producer_id,_execution_id;
    CUdevice _device;
    CUmodule _module;
    CUfunction _functions[3];
    VHO_TENSOR_SERVING *_q3;
    bool _ready;
    std::list<ALLOCATION> _allocations;
    std::list<IO> _io;
    std::list<JOB> _jobs;
    std::list<READER> _readers;
    /* Plain handles start null; incomplete opens remain cleanup-only objects. */
    IMPL():_context(NULL),_producer(NULL),_execution(NULL),_context_id(0),_producer_id(0),_execution_id(0),_device(-1),_module(NULL),_q3(NULL),_ready(false) {memset(_functions,0,sizeof(_functions));}
    /* Identity/capture checking never changes the current CUDA context. */
    bool Stream(CUstream stream,unsigned long long id) {
        CUcontext context=NULL;unsigned long long actual=0;CUstreamCaptureStatus capture=CU_STREAM_CAPTURE_STATUS_ACTIVE;
        return (uintptr_t)stream>2 && cuStreamGetCtx(stream,&context)==CUDA_SUCCESS && context==_context &&
            cuStreamGetId(stream,&actual)==CUDA_SUCCESS && actual==id &&
            cuStreamIsCapturing(stream,&capture)==CUDA_SUCCESS && capture==CU_STREAM_CAPTURE_STATUS_NONE;
    }
    /* Context and stream generations prevent recycled opaque-handle ABA. */
    bool Current() {
        CUcontext context=NULL;CUdevice device=-1;unsigned long long id=0;
        return cuCtxGetCurrent(&context)==CUDA_SUCCESS && context==_context &&
            cuCtxGetId(context,&id)==CUDA_SUCCESS && id==_context_id && cuCtxGetDevice(&device)==CUDA_SUCCESS && device==_device &&
            Stream(_producer,_producer_id) && Stream(_execution,_execution_id);
    }
    /* Actual CUDA extent/type/device/context and buffer ID precede every access. */
    bool Pointer(void *address,UINT64 bytes,UINT64 &identity,std::string &why) {
        CUcontext context=NULL;CUmemorytype memory=CU_MEMORYTYPE_HOST;int device=-1;CUdeviceptr base=0;size_t size=0;unsigned long long id=0;
        bool valid=Current() && address && bytes && cuPointerGetAttribute(&context,CU_POINTER_ATTRIBUTE_CONTEXT,Device(address))==CUDA_SUCCESS && context==_context &&
            cuPointerGetAttribute(&memory,CU_POINTER_ATTRIBUTE_MEMORY_TYPE,Device(address))==CUDA_SUCCESS && memory==CU_MEMORYTYPE_DEVICE &&
            cuPointerGetAttribute(&device,CU_POINTER_ATTRIBUTE_DEVICE_ORDINAL,Device(address))==CUDA_SUCCESS && device==_device &&
            cuPointerGetAttribute(&id,CU_POINTER_ATTRIBUTE_BUFFER_ID,Device(address))==CUDA_SUCCESS &&
            cuMemGetAddressRange(&base,&size,Device(address))==CUDA_SUCCESS && Device(address)>=base && Device(address)-base<=size && bytes<=size-(Device(address)-base);
        if(!valid) {why="q6d_device_pointer_or_context";return false;}identity=id;return true;
    }
    /* Service callback validates address-space identity, never host readability. */
    static bool Validate(void *p,void *address,UINT64 bytes,UINT64 &identity,std::string &why) {return ((IMPL *)p)->Pointer(address,bytes,identity,why);}
    /* CUDA-owned staging outlives errors; Read never queues into a caller stack. */
    static bool Read(void *p,const void *address,void *out,UINT64 bytes,std::string &why) {return ((IMPL *)p)->Transfer(false,(void *)address,out,bytes,why);}
    /* Q3 calls this only under an exclusive unpinned allocation mutation. */
    static bool Write(void *p,void *address,const void *in,UINT64 bytes,std::string &why) {return ((IMPL *)p)->Transfer(true,address,(void *)in,bytes,why);}
    /* Resource-only cleanup errors do not imply pending DMA; undrained I/O does. */
    static bool Idle(void *p) {for(const IO &io:((IMPL *)p)->_io) if(!io.drained) return false;return true;}
    /* Destroy only proven-drained I/O resources; failures preserve handles. */
    bool Cleanup_IO(std::string &why) {
        for(auto i=_io.begin();i!=_io.end();) {
            if(!i->drained) {why="q6d_io_drain_required";return false;}
            if(i->event && cuEventDestroy(i->event)!=CUDA_SUCCESS) {why="q6d_event_destroy";return false;}i->event=NULL;
            if(i->host && cuMemFreeHost(i->host)!=CUDA_SUCCESS) {why="q6d_host_free";return false;}i->host=NULL;i=_io.erase(i);
        }return true;
    }
    /* Owned ready events bind producer writes to the execution stream. A failed
     * queue/record/wait retains staging and requires a successful stream drain. */
    bool Transfer(bool upload,void *address,void *host,UINT64 bytes,std::string &why) {
        UINT64 identity=0;if(!Idle(this) || !Pointer(address,bytes,identity,why)) return false;
        _io.push_back(IO{NULL,NULL,upload?_producer:_execution,upload?_producer_id:_execution_id,true});IO &io=_io.back();
        if(cuMemAllocHost(&io.host,bytes)!=CUDA_SUCCESS || cuEventCreate(&io.event,CU_EVENT_DISABLE_TIMING)!=CUDA_SUCCESS) {why="q6d_io_allocate";return false;}
        if(upload) memcpy(io.host,host,bytes);io.drained=false;
        CUresult copied=upload?cuMemcpyHtoDAsync(Device(address),io.host,bytes,io.stream):cuMemcpyDtoHAsync(io.host,Device(address),bytes,io.stream);
        if(copied!=CUDA_SUCCESS || cuEventRecord(io.event,io.stream)!=CUDA_SUCCESS ||
           (upload && cuStreamWaitEvent(_execution,io.event,0)!=CUDA_SUCCESS) || cuEventSynchronize(io.event)!=CUDA_SUCCESS) {why="q6d_io_pending_or_failed";return false;}
        io.drained=true;if(!upload) memcpy(host,io.host,bytes);return Cleanup_IO(why);
    }
    /* Completed retired jobs no longer pin modules/events. Failed destruction
     * retains the record for Drain/Destroy; ticket identities are never reused. */
    bool Cleanup_jobs(std::string &why) {
        for(auto i=_jobs.begin();i!=_jobs.end();) {
            if(!i->done || i->bundle) {++i;continue;}
            if(i->event && cuEventDestroy(i->event)!=CUDA_SUCCESS) {why="q6d_completion_event_cleanup";return false;}
            i=_jobs.erase(i);
        }return true;
    }
    /* Lookup never confuses foreign/recycled Q3 identities with owned memory. */
    ALLOCATION &Allocation(const VHO_SERVING_HANDLE &handle) {
        for(ALLOCATION &a:_allocations) if(Equal(a.handle,handle) && handle.id) return a;throw std::runtime_error("q6d_foreign_allocation");
    }
    /* Jobs remain owned through event cleanup and publication retirement. */
    JOB &Job(UINT64 ticket) {for(JOB &j:_jobs) if(j.ticket==ticket && ticket) return j;throw std::runtime_error("q6d_stale_job");}
    /* The reported PTX virtual target is not an ISA .version check; the owner
     * authenticates Q6c ISA/toolchain provenance and binary sm_90 is checked.
     * Bind exact loaded resource/ABI facts; the owner-authenticated code record
     * supplies launch configuration, not a runtime heuristic or selector. */
    bool Resources(std::string &why) {
        int major=0,minor=0,grid=0,block=0,shared=0;
        if(cuDeviceGetAttribute(&major,CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR,_device)!=CUDA_SUCCESS || major!=9 ||
           cuDeviceGetAttribute(&minor,CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR,_device)!=CUDA_SUCCESS || minor!=0 ||
           cuDeviceGetAttribute(&grid,CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X,_device)!=CUDA_SUCCESS || grid<=0 ||
           cuDeviceGetAttribute(&block,CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X,_device)!=CUDA_SUCCESS || block<=0 ||
           cuDeviceGetAttribute(&shared,CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK,_device)!=CUDA_SUCCESS || shared<0) {why="q6d_device_resources";return false;}
        VHO_TENSOR_SERVING_SCHEMA schema;if(!VHO_tensor_serving_schema(_code.program,schema,why)) return false;
        unsigned count=schema.roots.size()+1;
        for(unsigned stage=0;stage<3;++stage) {
            int regs=0,smem=0,threads=0,binary=0,ptx=0,cluster=0,resident=0;
            if(cuModuleGetFunction(&_functions[stage],_module,_code.names[stage].c_str())!=CUDA_SUCCESS ||
               cuFuncGetAttribute(&regs,CU_FUNC_ATTRIBUTE_NUM_REGS,_functions[stage])!=CUDA_SUCCESS || regs<=0 || regs>255 ||
               cuFuncGetAttribute(&smem,CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES,_functions[stage])!=CUDA_SUCCESS || smem<0 || (UINT64)smem!=_code.shared[stage] || smem>shared ||
               cuFuncGetAttribute(&threads,CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK,_functions[stage])!=CUDA_SUCCESS || threads<0 || (UINT64)threads<_code.block[stage] || _code.block[stage]>(UINT64)block ||
               cuFuncGetAttribute(&binary,CU_FUNC_ATTRIBUTE_BINARY_VERSION,_functions[stage])!=CUDA_SUCCESS || binary!=90 ||
               cuFuncGetAttribute(&ptx,CU_FUNC_ATTRIBUTE_PTX_VERSION,_functions[stage])!=CUDA_SUCCESS || ptx<0 ||
               cuFuncGetAttribute(&cluster,CU_FUNC_ATTRIBUTE_CLUSTER_SIZE_MUST_BE_SET,_functions[stage])!=CUDA_SUCCESS || cluster ||
               cuOccupancyMaxActiveBlocksPerMultiprocessor(&resident,_functions[stage],_code.block[stage],0)!=CUDA_SUCCESS || resident<=0 || _code.grid[stage]>(UINT64)grid) {why="q6d_loaded_kernel_resources";return false;}
            for(unsigned i=0;i<count;++i) {size_t offset=0,bytes=0;
                if(cuFuncGetParamInfo(_functions[stage],i,&offset,&bytes)!=CUDA_SUCCESS || offset!=8*i || bytes!=8) {why="q6d_kernel_parameter";return false;}}
            size_t offset=0,bytes=0;if(cuFuncGetParamInfo(_functions[stage],count,&offset,&bytes)!=CUDA_ERROR_INVALID_VALUE) {why="q6d_kernel_arity";return false;}
        }return true;
    }
};
/* Only an owned implementation is created; no CUDA effect occurs here. */
VHO_MLA_CUDA::VHO_MLA_CUDA():_impl(new IMPL) {}
/* Destroy is reachable only after explicit successful resource cleanup. */
VHO_MLA_CUDA::~VHO_MLA_CUDA() {delete _impl->_q3;delete _impl;}
/* Authenticate/copy cubin before loading, establish real context/stream IDs and
 * build a Q3 device registry. A failed open preserves acquired cleanup handles. */
bool VHO_MLA_CUDA::Open(const VHO_MLA_CUDA_CODE &code,const void *image,size_t bytes,void *context,void *producer,void *execution,const VHO_MLA_CUDA_HOOKS &hooks,VHO_MLA_CUDA *&out,std::string &why) {
    return Open(code,image,bytes,context,producer,execution,hooks,DSL_GPU_SERVING_BINDING(),out,why);
}
/* Explicit H1d owner binds requirements to the authenticated fixed cubin.
 * Observation gates load and Q3 admission/publication; legacy ABI stays intact. */
bool VHO_MLA_CUDA::Open(const VHO_MLA_CUDA_CODE &code,const void *image,size_t bytes,void *context,void *producer,void *execution,const VHO_MLA_CUDA_HOOKS &hooks,const DSL_GPU_SERVING_BINDING &gpu,VHO_MLA_CUDA *&out,std::string &why) {
    out=NULL;VHO_MLA_CUDA *object=NULL;
    try {
        Require(context && (uintptr_t)producer>2 && (uintptr_t)execution>2 && hooks.authorize && hooks.sha256,"q6d_explicit_owner_context_streams");
        Require(bytes && bytes<=64U*1024U*1024U && bytes==code.cubin_bytes && image,"q6d_cubin_extent");
        VHO_Q5_LAYOUT layout;Require(VHO_q5_layout(code.program,layout,why) && code.source_key==VHO_tensor_quantized_high_key(code.program),"q6d_source_contract");
        Require(!code.mapping_key.empty(),"q6d_mapping_identity");const char *names[]={"prepare","attention","finalize"};
        UINT64 group=code.block[1]/32;Require((group==1 || group==2 || group==4) && code.block[1]==group*32,"q6d_head_mapping");
        UINT64 grid=layout.schema.geometry.batch*layout.schema.geometry.queries*((layout.schema.geometry.heads+group-1)/group);
        std::string suffix="_q6c_prepare";Require(code.names[0].size()>suffix.size() && code.names[0].substr(code.names[0].size()-suffix.size())==suffix,"q6d_entry_names");
        std::string prefix=code.names[0].substr(0,code.names[0].size()-suffix.size());
        Require(prefix.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")==std::string::npos,"q6d_entry_identifier");
        for(unsigned i=0;i<3;++i) Require(!code.stage_keys[i].empty() && code.names[i]==prefix+"_q6c_"+names[i] &&
            code.grid[i]==(i==1?grid:1) && code.block[i]==(i==1?group*32:1) && code.shared[i]<=(i==1?49152U:0U),"q6d_stage_contract");
        std::vector<unsigned char> copy((const unsigned char *)image,(const unsigned char *)image+bytes);unsigned char digest[32]={};
        Require(bytes>=20 && !memcmp(&copy[0],"\177ELF\2\1",6) && copy[18]==190 && copy[19]==0 && hooks.sha256(hooks.context,&copy[0],bytes,digest) && !memcmp(digest,code.sha256,32) && hooks.authorize(hooks.context,code),"q6d_code_authorization");
        object=new VHO_MLA_CUDA;IMPL &p=*object->_impl;p._code=code;p._hooks=hooks;p._context=(CUcontext)context;p._producer=(CUstream)producer;p._execution=(CUstream)execution;
        int version=0;Require(cuDriverGetVersion(&version)==CUDA_SUCCESS && version>=12080 && cuCtxGetId(p._context,&p._context_id)==CUDA_SUCCESS &&
            cuStreamGetId(p._producer,&p._producer_id)==CUDA_SUCCESS && cuStreamGetId(p._execution,&p._execution_id)==CUDA_SUCCESS && cuCtxGetDevice(&p._device)==CUDA_SUCCESS && p.Current(),"q6d_context_stream_generation");
        Require(cuModuleLoadData(&p._module,&copy[0])==CUDA_SUCCESS && p.Resources(why),why.empty()?"q6d_module_load":why.c_str());
        if(!gpu.requirements.selection_key.empty()) {
            DSL_GPU_RUNTIME_OBSERVATION actual;bool reusable=false;
            bool compatible=gpu.observe && gpu.observe(gpu.context,actual,why) && gpu.requirements.Check(actual,0,reusable,why);
            Require(compatible,why.empty()?"gpu_runtime_observer":why.c_str());
        }
        VHO_SERVING_MEMORY memory={1,sizeof(VHO_SERVING_MEMORY),&p,(UINT64)(uintptr_t)p._context,IMPL::Validate,IMPL::Read,IMPL::Write,IMPL::Idle};
        std::vector<UINT64> legal(1,layout.schema.page_tokens);p._q3=new VHO_TENSOR_SERVING(code.program,legal,&memory,gpu,why);Require(p._q3->Valid(),why.c_str());
        p._ready=true;out=object;return true;
    } catch(const std::exception &e) {why=e.what();
        if(object) {if(object->_impl->_module) {out=object;return false;}delete object;}return false;}
}
/* Token-only validity never substitutes for a current live device binding. */
bool VHO_MLA_CUDA::Valid() const {return _impl->_ready;}
/* Allocate into a pre-owned record so even failed registration/free is retained. */
bool VHO_MLA_CUDA::Allocate(UINT64 bytes,VHO_SERVING_HANDLE &out,std::string &why) {
    out=VHO_SERVING_HANDLE();std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);IMPL &p=*_impl;
    try {Require(p._ready && p.Current() && IMPL::Idle(&p) && bytes && bytes<=1073741824,"q6d_allocate_state");
        p._allocations.push_back(IMPL::ALLOCATION{VHO_SERVING_HANDLE(),0,bytes,0,false,false});IMPL::ALLOCATION &a=p._allocations.back();
        Require(cuMemAlloc(&a.base,bytes)==CUDA_SUCCESS,"q6d_device_allocate");Require(p.Pointer((void *)(uintptr_t)a.base,bytes,a.identity,why),why.c_str());a.identity_known=true;
        Require(p._q3->Register((void *)(uintptr_t)a.base,bytes,"cuda-device-v1",a.handle,why),why.c_str());a.registered=true;out=a.handle;return true;
    }catch(const std::exception &e) {why=e.what();return false;}
}
/* Q3 checks exclusive write permission before producer DMA is queued. */
bool VHO_MLA_CUDA::Write(const VHO_SERVING_HANDLE &h,UINT64 offset,const void *data,UINT64 bytes,std::string &why) {
    std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);return _impl->_ready && _impl->Current() && IMPL::Idle(_impl) && _impl->_q3->Write(h,offset,data,bytes,why);
}
/* Read-only download shares Q3's active-writer exclusion. */
bool VHO_MLA_CUDA::Read(const VHO_SERVING_HANDLE &h,UINT64 offset,void *data,UINT64 bytes,std::string &why) {
    std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);return _impl->_ready && _impl->Current() && IMPL::Idle(_impl) && _impl->_q3->Read(h,offset,data,bytes,why);
}
/* Unregister first, free second; a failed CUDA free leaves an owned retry record. */
bool VHO_MLA_CUDA::Retire(const VHO_SERVING_HANDLE &h,std::string &why) {
    std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);IMPL &p=*_impl;
    try {Require(p.Current() && IMPL::Idle(&p),"q6d_retire_drain_required");IMPL::ALLOCATION &a=p.Allocation(h);UINT64 id=0;
        Require(p.Pointer((void *)(uintptr_t)a.base,a.bytes,id,why) && id==a.identity,"q6d_recycled_allocation");
        if(a.registered) {Require(p._q3->Retire(h,why),why.c_str());a.registered=false;}
        Require(cuMemFree(a.base)==CUDA_SUCCESS,"q6d_device_free");a.base=0;
        for(auto i=p._allocations.begin();i!=p._allocations.end();++i) if(&*i==&a) {p._allocations.erase(i);break;}return true;
    }catch(const std::exception &e) {why=e.what();return false;}
}
/* Device-ready cache/scale/map observation uses Q3's original atomic import. */
bool VHO_MLA_CUDA::Import(const std::vector<VHO_SERVING_VIEW> &v,const VHO_SERVING_VIEW &m,UINT64 &bundle,std::string &why) {
    bundle=0;std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);return _impl->_ready && _impl->Current() && IMPL::Idle(_impl) && _impl->_q3->Import(v,m,bundle,why);
}
/* Fixed-code instantiation captures fresh device Q3 facts. Historical host
 * selection keys are never reused as live leases or a new optimality claim. */
bool VHO_MLA_CUDA::Begin(const std::vector<VHO_SERVING_VIEW> &v,const VHO_SERVING_VIEW &m,UINT64 parent,VHO_SERVING_FACTS &facts,std::string &why) {
    std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);IMPL &p=*_impl;facts=VHO_SERVING_FACTS();
    try {Require(p._ready && p.Current() && IMPL::Idle(&p),"q6d_begin_state");
        p._jobs.push_back(IMPL::JOB{0,0,"",NULL,false,false,false,false,false});IMPL::JOB &j=p._jobs.back();
        if(!p._q3->Begin(v,m,parent,facts,why)) {p._jobs.pop_back();return false;}j.ticket=facts.ticket;
        VHO_Q4_SNAPSHOT snapshot;
        if(!p._q3->Freeze_costing(j.ticket,snapshot,why)) {if(IMPL::Idle(&p)) {p._q3->Cancel(j.ticket,why);p._jobs.pop_back();}return false;}
        j.snapshot=snapshot.key;return true;
    }catch(const std::exception &e) {why=e.what();return false;}
}
/* Reserve events/arguments before launch; any uncertain partial submission is
 * retained as failed work and cannot be implicitly retried or published. */
bool VHO_MLA_CUDA::Submit(UINT64 ticket,std::string &why) {
    std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);IMPL &p=*_impl;
    try {Require(p._ready && p.Current() && IMPL::Idle(&p),"q6d_submit_state");IMPL::JOB &j=p.Job(ticket);
        Require(!j.submitted && !j.done && !j.failed,"q6d_duplicate_submit");Require(p._q3->Revalidate_costing(ticket,j.snapshot,why),why.c_str());
        if(cuEventCreate(&j.event,CU_EVENT_DISABLE_TIMING)!=CUDA_SUCCESS) {j.failed=true;why="q6d_completion_event";return false;}
        std::vector<void *> addresses;bool armed=false;
        if(!p._q3->Start_external(ticket,addresses,armed,why)) {j.armed=armed;j.failed=true;j.submitted=armed || !IMPL::Idle(&p);j.done=!p._q3->Is_live(ticket);return false;}
        j.armed=true;j.submitted=true;std::vector<CUdeviceptr> device(addresses.size());std::vector<void *> arguments(addresses.size());
        for(unsigned i=0;i<addresses.size();++i) {device[i]=Device(addresses[i]);arguments[i]=&device[i];}
        for(unsigned i=0;i<3;++i) if(cuLaunchKernel(p._functions[i],p._code.grid[i],1,1,p._code.block[i],1,1,0,p._execution,&arguments[0],NULL)!=CUDA_SUCCESS) {j.failed=true;break;}
        j.recorded=cuEventRecord(j.event,p._execution)==CUDA_SUCCESS;if(!j.recorded) j.failed=true;
        if(j.failed) {why="q6d_partial_launch_requires_drain";return false;}return true;
    }catch(const std::exception &e) {why=e.what();return false;}
}
/* Retry I/O drains on the exact originating stream generation; failed drains
 * retain pinned host staging and device registrations. No job is published. */
bool VHO_MLA_CUDA::Drain(std::string &why) {
    std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);IMPL &p=*_impl;
    if(!p.Current()) {why="q6d_drain_context";return false;}
    for(IMPL::IO &io:p._io) if(!io.drained) {
        if(!p.Stream(io.stream,io.stream_id) || cuStreamSynchronize(io.stream)!=CUDA_SUCCESS || cuStreamSynchronize(p._execution)!=CUDA_SUCCESS) {why="q6d_uncertain_io_drain";return false;}io.drained=true;
    }
    return p.Cleanup_IO(why) && p.Cleanup_jobs(why);
}
/* Event success or successful stream drain is required before Q3 can unpin.
 * Status/control/scale/parent failure never publishes any component. */
bool VHO_MLA_CUDA::Complete(UINT64 ticket,bool wait,UINT64 &bundle,std::string &why) {
    bundle=0;std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);IMPL &p=*_impl;
    try {Require(p.Current(),"q6d_complete_context");IMPL::JOB &j=p.Job(ticket);Require(j.submitted && !j.done,"q6d_not_submitted_or_done");
        CUresult status=j.recorded?(wait?cuEventSynchronize(j.event):cuEventQuery(j.event)):CUDA_ERROR_NOT_READY;
        if(status==CUDA_ERROR_NOT_READY && !wait) {why="q6d_pending";return false;}
        if(status!=CUDA_SUCCESS) {j.failed=true;if(cuStreamSynchronize(p._execution)!=CUDA_SUCCESS) {why="q6d_uncertain_kernel_drain";return false;}}
        Require(Drain(why),why.c_str());
        bool ok=j.armed?p._q3->Finish_external(ticket,!j.failed,bundle,why):p._q3->Cancel(ticket,why);
        if(p._q3->Is_live(ticket)) {j.failed=true;return false;}
        j.done=true;j.bundle=ok && j.armed?bundle:0;if(!j.bundle) j.failed=true;return j.bundle!=0;
    }catch(const std::exception &e) {why=e.what();return false;}
}
/* Prepared cancellation is immediate; in-flight cancellation denies publication
 * and retains all device work until Complete proves its drain. */
bool VHO_MLA_CUDA::Cancel(UINT64 ticket,bool wait,std::string &why) {
    std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);IMPL &p=*_impl;
    try {Require(p.Current(),"q6d_cancel_context_or_io");if(!IMPL::Idle(&p)) {Require(wait && Drain(why),"q6d_cancel_io_drain_required");}IMPL::JOB &j=p.Job(ticket);Require(!j.done,"q6d_stale_job");j.failed=true;
        if(!j.submitted) {Require(p._q3->Cancel(ticket,why),why.c_str());j.done=true;return p.Cleanup_jobs(why);}
        UINT64 bundle=0;Complete(ticket,wait,bundle,why);return !p._q3->Is_live(ticket);
    }catch(const std::exception &e) {why=e.what();return false;}
}
/* Consumer acquisition pins the complete Q3 tuple and records its own stream
 * generation; already-completed producer events still impose explicit ordering. */
bool VHO_MLA_CUDA::Reader(UINT64 bundle,void *stream,UINT64 &token,std::vector<UINT64> &addresses,std::string &why) {
    token=0;addresses.clear();std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);IMPL &p=*_impl;
    try {unsigned long long id=0;Require(p._ready && p.Current() && (uintptr_t)stream>2 && cuStreamGetId((CUstream)stream,&id)==CUDA_SUCCESS && p.Stream((CUstream)stream,id),"q6d_consumer_stream");
        CUevent ready=NULL;for(IMPL::JOB &j:p._jobs) if(j.bundle==bundle && j.done && !j.failed) ready=j.event;
        // Imported bundles are already synchronously ready; produced ones carry
        // an immutable event. Q3 remains the actual bundle authority.
        p._readers.push_back(IMPL::READER{0,bundle,(CUstream)stream,id,NULL,false,false});IMPL::READER &r=p._readers.back();
        Require(p._q3->Acquire_reader(bundle,r.token,why),why.c_str());token=r.token;
        std::vector<VHO_SERVING_VIEW> views;Require(p._q3->Bundle_views(bundle,views,why),why.c_str());
        for(const VHO_SERVING_VIEW &v:views) {IMPL::ALLOCATION &a=p.Allocation(v.handle);UINT64 current=0;
            Require(p.Pointer((void *)(uintptr_t)a.base,a.bytes,current,why) && current==a.identity && v.offset<=a.bytes && v.bytes<=a.bytes-v.offset,"q6d_consumer_allocation");addresses.push_back(a.base+v.offset);}
        if(ready) Require(cuStreamWaitEvent(r.stream,ready,0)==CUDA_SUCCESS,"q6d_consumer_wait_requires_drain");return true;
    }catch(const std::exception &e) {if(!p._readers.empty() && !p._readers.back().token) p._readers.pop_back();addresses.clear();why=e.what();return false;}
}
/* First completion call seals the consumer sequence; retries never re-record
 * the owned event or release a possibly busy reader on an error. */
bool VHO_MLA_CUDA::Reader_done(UINT64 token,bool wait,std::string &why) {
    std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);IMPL &p=*_impl;
    for(auto i=p._readers.begin();i!=p._readers.end();++i) if(i->token==token && token) {
        if(!p.Current() || !p.Stream(i->stream,i->stream_id)) {why="q6d_consumer_context";return false;}
        if(!i->sealed) {i->sealed=true;if(cuEventCreate(&i->event,CU_EVENT_DISABLE_TIMING)==CUDA_SUCCESS) i->recorded=cuEventRecord(i->event,i->stream)==CUDA_SUCCESS;}
        CUresult status=i->recorded?(wait?cuEventSynchronize(i->event):cuEventQuery(i->event)):CUDA_ERROR_NOT_READY;
        if(status==CUDA_ERROR_NOT_READY && !wait) {why="q6d_consumer_pending";return false;}
        if(status!=CUDA_SUCCESS && cuStreamSynchronize(i->stream)!=CUDA_SUCCESS) {why="q6d_consumer_drain";return false;}
        if(i->event && cuEventDestroy(i->event)!=CUDA_SUCCESS) {why="q6d_consumer_event_cleanup";return false;}i->event=NULL;
        if(!p._q3->Release_reader(token,why)) return false;p._readers.erase(i);return true;
    }why="q6d_stale_reader";return false;
}
/* Q3 refuses outstanding readers/parent users; event cleanup is retained if it
 * fails after retirement and retried during Destroy. */
bool VHO_MLA_CUDA::Retire_bundle(UINT64 bundle,std::string &why) {
    std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);if(!_impl->_ready || !_impl->Current() || !_impl->_q3->Retire_bundle(bundle,why)) return false;
    for(IMPL::JOB &j:_impl->_jobs) if(j.bundle==bundle) j.bundle=0;
    std::string cleanup;_impl->Cleanup_jobs(cleanup);return true;
}
/* Generation reports cannot authorize an allocation or consumer. */
bool VHO_MLA_CUDA::Inspect_bundle(UINT64 bundle,VHO_SERVING_BUNDLE_FACTS &facts,std::string &why) {std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);return _impl->_q3 && _impl->_q3->Inspect_bundle(bundle,facts,why);}
/* Token-only freshness includes prepared and running unknown-drain calls. */
bool VHO_MLA_CUDA::Is_live(UINT64 ticket) const {std::lock_guard<std::recursive_mutex> lock(_impl->_mutex);return _impl->_q3 && _impl->_q3->Is_live(ticket);}
/* Teardown never frees borrowed context/streams or active work. Unknown cleanup
 * preserves the entire object; successful retry releases each handle once. */
bool VHO_MLA_CUDA::Destroy(VHO_MLA_CUDA *&object,std::string &why) {
    if(!object) return true;IMPL &p=*object->_impl;
    {std::lock_guard<std::recursive_mutex> lock(p._mutex);
        if(!p.Current() || (p._q3 && !p._q3->Quiescent()) || !p._readers.empty() || !IMPL::Idle(&p)) {why="q6d_live_or_uncertain_resources";return false;}
        for(const IMPL::ALLOCATION &a:p._allocations) if(a.registered) {why="q6d_registered_allocations";return false;}
        for(const IMPL::JOB &j:p._jobs) if(!j.done || j.bundle) {why="q6d_live_job_or_bundle";return false;}
        p._ready=false;
        if(!p.Cleanup_IO(why)) return false;
        for(auto i=p._allocations.begin();i!=p._allocations.end();) {
            if(i->registered) {why="q6d_registered_allocations";return false;}
            UINT64 id=0;if(i->base && i->identity_known && (!p.Pointer((void *)(uintptr_t)i->base,i->bytes,id,why) || id!=i->identity)) {why="q6d_recycled_orphan";return false;}
            if(i->base && cuMemFree(i->base)!=CUDA_SUCCESS) {why="q6d_orphan_free";return false;}i=p._allocations.erase(i);
        }
        for(IMPL::JOB &j:p._jobs) {if(!j.done || j.bundle) {why="q6d_live_job_or_bundle";return false;}if(j.event && cuEventDestroy(j.event)!=CUDA_SUCCESS) {why="q6d_completion_event_cleanup";return false;}j.event=NULL;}
        if(p._module && cuModuleUnload(p._module)!=CUDA_SUCCESS) {why="q6d_module_unload";return false;}p._module=NULL;
    }delete object;object=NULL;return true;
}
