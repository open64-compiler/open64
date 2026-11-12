/* Copyright (C) 2026 Open64 Project */
/* Q6d model / Q7 real-Driver private qualification ABI. Typed code is decoded from owner-authenticated
 * Q6c fixtures; this does not define a serving-engine packaging/selection ABI.
 * Contracts: doc/AI-COMPILER-MLA-Q6D-DEVICE-BINDINGS-v1.0.md and
 * doc/AI-COMPILER-MLA-Q7-QUALIFICATION-v1.0.md. */
#include "../be/vho/tensor_serving_cuda.h"
#include "../be/vho/tensor_q1_program.h"
#include "../rapidjson/document.h"
#include "../rapidjson/writer.h"
#include "../rapidjson/stringbuffer.h"
#include <cstring>
struct Q6D_REVIEW_SESSION {VHO_MLA_CUDA *runtime;VHO_MLA_CUDA_CODE approved;std::string error;bool (*hash)(const void *,size_t,unsigned char *);DSL_GPU_RUNTIME_OBSERVATION gpu;};
/* Test owner validates fixed program/source and all manifest/digest fields. */
static bool Q6D_authorize(void *context,const VHO_MLA_CUDA_CODE &code) {
    Q6D_REVIEW_SESSION &s=*(Q6D_REVIEW_SESSION *)context;const VHO_MLA_CUDA_CODE &a=s.approved;
    if(code.source_key!=a.source_key || code.mapping_key!=a.mapping_key || code.cubin_bytes!=a.cubin_bytes || memcmp(code.sha256,a.sha256,32))return false;
    for(unsigned i=0;i<3;++i) if(code.stage_keys[i]!=a.stage_keys[i] || code.names[i]!=a.names[i] || code.grid[i]!=a.grid[i] || code.block[i]!=a.block[i] || code.shared[i]!=a.shared[i])return false;return true;
}
/* SHA service is test infrastructure; the runtime itself has no crypto library dependency. */
static bool Q6D_hash(void *p,const void *data,size_t bytes,unsigned char out[32]) {return ((Q6D_REVIEW_SESSION *)p)->hash(data,bytes,out);}
/* Strict fixture decoder retains full native keys; no native code is constructed. */
static VHO_MLA_CUDA_CODE Q6D_decode(const char *wire) {
    rapidjson::Document j;j.Parse(wire);std::string why;if(j.HasParseError() || !j.IsObject())throw std::runtime_error("q6d_test_json");
    VHO_MLA_CUDA_CODE code;if(!VHO_q1_program_decode(j["program"].GetString(),code.program,why))throw std::runtime_error(why);
    code.source_key=j["source_key"].GetString();code.mapping_key=j["mapping_key"].GetString();code.cubin_bytes=j["cubin_bytes"].GetUint64();
    for(unsigned i=0;i<32;++i)code.sha256[i]=j["sha256"][i].GetUint();
    for(unsigned i=0;i<3;++i) {code.stage_keys[i]=j["stages"][i]["native_key"].GetString();code.names[i]=j["stages"][i]["function"].GetString();code.grid[i]=j["stages"][i]["grid_x"].GetUint64();code.block[i]=j["stages"][i]["block_x"].GetUint64();code.shared[i]=j["stages"][i]["shared_bytes"].GetUint64();}return code;
}
/* Explicit synthetic H1d observer; never hardware attestation/calibration. */
static bool H1D_cuda_observe(void *context,DSL_GPU_RUNTIME_OBSERVATION &out,std::string &) {
    out=((Q6D_REVIEW_SESSION *)context)->gpu;return true;
}
extern "C" {
/* Qualification owner supplies actual current context and nondefault streams.
 * Authentication is external to this private fixture decoder. Failed Open
 * keeps acquired resources reachable; this is not a serving-engine ABI. */
void *open64_q6d_create_on(const char *wire,const char *approved,const void *image,size_t bytes,bool (*hash)(const void *,size_t,unsigned char *),void *context,void *producer,void *execution) {
    Q6D_REVIEW_SESSION *s=new Q6D_REVIEW_SESSION;s->runtime=NULL;s->hash=hash;
    try {s->approved=Q6D_decode(approved);VHO_MLA_CUDA_CODE code=Q6D_decode(wire);VHO_MLA_CUDA_HOOKS hooks={s,Q6D_authorize,Q6D_hash};
        VHO_MLA_CUDA::Open(code,image,bytes,context,producer,execution,hooks,s->runtime,s->error);
    }catch(const std::exception &e){s->error=e.what();}return s;
}
/* Opt-in fixture checks the new Open overload with existing CODE/HOOKS ABIs.
 * The test owner binds requirements to the authenticated fixed code record. */
void *open64_h1d_q6d_create(const char *wire,const char *approved,const void *image,size_t bytes,bool (*hash)(const void *,size_t,unsigned char *),const char *device) {
    Q6D_REVIEW_SESSION *s=new Q6D_REVIEW_SESSION;s->runtime=NULL;s->hash=hash;
    try {
        s->approved=Q6D_decode(approved);VHO_MLA_CUDA_CODE code=Q6D_decode(wire);
        VHO_MLA_CUDA_HOOKS hooks={s,Q6D_authorize,Q6D_hash};DSL_GPU_SERVING_BINDING binding;
        DSL_GPU_HOST_REQUIREMENTS &r=binding.requirements;r.device=device;r.architecture="sm_90";r.partition="fixture-partition";
        r.toolchain="fixture-toolchain";r.selection_key="fixture-selection";r.resource_key="fixture-resource";
        r.execution_key="fixture-execution";r.budget_key="fixture-budget";r.imported=true;
        s->gpu.device=r.device;s->gpu.architecture=r.architecture;s->gpu.partition=r.partition;s->gpu.toolchain=r.toolchain;
        s->gpu.resource_key=r.resource_key;s->gpu.execution_key=r.execution_key;s->gpu.budget_key=r.budget_key;
        s->gpu.known=s->gpu.reservation_live=true;s->gpu.context_generation=1;s->gpu.budget_generation=2;
        s->gpu.reservation_id=3;s->gpu.reserved_bytes=UINT64(1)<<30;binding.context=s;binding.observe=H1D_cuda_observe;
        VHO_MLA_CUDA::Open(code,image,bytes,(void *)0x101,(void *)0x201,(void *)0x202,hooks,binding,s->runtime,s->error);
    } catch(const std::exception &e) {s->error=e.what();}return s;
}
/* Change observations only in test infrastructure, after actual model events. */
void open64_h1d_q6d_change(void *object,const char *fault) {
    Q6D_REVIEW_SESSION *s=(Q6D_REVIEW_SESSION *)object;std::string f=fault;
    if(f=="device") s->gpu.device="other-device";
    if(f=="budget") ++s->gpu.budget_generation;
    if(f=="expired") s->gpu.reservation_live=false;
    if(f=="calibration") s->gpu.execution_key="other-calibration";
}
/* Preserve the existing Q6d model entry and its synthetic handle identities. */
void *open64_q6d_create(const char *wire,const char *approved,const void *image,size_t bytes,bool (*hash)(const void *,size_t,unsigned char *)) {
    return open64_q6d_create_on(wire,approved,image,bytes,hash,(void *)0x101,(void *)0x201,(void *)0x202);
}
/* Diagnostic-only refusal text never grants a device lease. */
const char *open64_q6d_error(void *p) {return ((Q6D_REVIEW_SESSION *)p)->error.c_str();}
/* Validity does not assert actual NVIDIA execution qualification. */
int open64_q6d_valid(void *p) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime && s->runtime->Valid();}
/* Failed teardown preserves the embedding session and every owned handle. */
int open64_q6d_destroy(void *p) {auto s=(Q6D_REVIEW_SESSION *)p;if(!VHO_MLA_CUDA::Destroy(s->runtime,s->error))return 0;delete s;return 1;}
/* Register only adapter-owned actual device allocations. */
int open64_q6d_allocate(void *p,UINT64 bytes,VHO_SERVING_HANDLE *out) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Allocate(bytes,*out,s->error);}
/* Cooperative upload passes Q3 writer exclusion before any DMA. */
int open64_q6d_write(void *p,VHO_SERVING_HANDLE h,UINT64 offset,const void *data,UINT64 bytes) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Write(h,offset,data,bytes,s->error);}
/* Output download refuses pending writers and detects recycled allocations. */
int open64_q6d_read(void *p,VHO_SERVING_HANDLE h,UINT64 offset,void *data,UINT64 bytes) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Read(h,offset,data,bytes,s->error);}
/* Retire precedes physical free; retry does not unregister twice. */
int open64_q6d_retire(void *p,VHO_SERVING_HANDLE h) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Retire(h,s->error);}
/* Import observes actual device metadata/page maps/scales through Q3. */
int open64_q6d_import(void *p,const VHO_SERVING_VIEW *v,unsigned n,VHO_SERVING_VIEW m,UINT64 *bundle) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Import(std::vector<VHO_SERVING_VIEW>(v,v+n),m,*bundle,s->error);}
/* Fresh device snapshots pin all source inputs and exclusive result buffers. */
int open64_q6d_begin(void *p,const VHO_SERVING_VIEW *v,unsigned n,VHO_SERVING_VIEW m,UINT64 parent,UINT64 *ticket) {auto s=(Q6D_REVIEW_SESSION *)p;VHO_SERVING_FACTS facts;bool ok=s->runtime->Begin(std::vector<VHO_SERVING_VIEW>(v,v+n),m,parent,facts,s->error);*ticket=facts.ticket;return ok;}
/* Submission cannot publish and cannot select another algorithm. */
int open64_q6d_submit(void *p,UINT64 ticket) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Submit(ticket,s->error);}
/* Completion gates the complete Q3 publication tuple. */
int open64_q6d_complete(void *p,UINT64 ticket,int wait,UINT64 *bundle) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Complete(ticket,wait,*bundle,s->error);}
/* Cancellation denies publication and drains before releasing leases. */
int open64_q6d_cancel(void *p,UINT64 ticket,int wait) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Cancel(ticket,wait,s->error);}
/* Explicit retry retains all unknown I/O lifetimes until successful synchronization. */
int open64_q6d_drain(void *p) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Drain(s->error);}
/* Pending token freshness does not expose an unpinned pointer. */
int open64_q6d_live(void *p,UINT64 ticket) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Is_live(ticket);}
/* Qualification caller supplies an actual consumer stream. The fixed MLA
 * contract bounds the whole tuple to seven views; caller reserves 16 slots. */
int open64_q6d_reader_on(void *p,UINT64 bundle,UINT64 *reader,UINT64 *addresses,unsigned *count,void *stream) {auto s=(Q6D_REVIEW_SESSION *)p;std::vector<UINT64> out;bool ok=s->runtime->Reader(bundle,stream,*reader,out,s->error);if(ok){*count=out.size();for(unsigned i=0;i<out.size();++i)addresses[i]=out[i];}return ok;}
/* Preserve the model consumer identity without changing its publication path. */
int open64_q6d_reader(void *p,UINT64 bundle,UINT64 *reader,UINT64 *addresses,unsigned *count) {return open64_q6d_reader_on(p,bundle,reader,addresses,count,(void *)0x203);}
/* Consumer completion is recorded once and retryable on pending/error. */
int open64_q6d_reader_done(void *p,UINT64 reader,int wait) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Reader_done(reader,wait,s->error);}
/* No cache/scale component can retire while a consumer or parent user is live. */
int open64_q6d_bundle_retire(void *p,UINT64 bundle) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Retire_bundle(bundle,s->error);}
/* Inspect a complete immutable generation tuple, without acquiring a lease. */
int open64_q6d_bundle(void *p,UINT64 bundle,UINT64 *versions,unsigned *count) {auto s=(Q6D_REVIEW_SESSION *)p;VHO_SERVING_BUNDLE_FACTS out;if(!s->runtime->Inspect_bundle(bundle,out,s->error))return 0;*count=out.content_versions.size();for(unsigned i=0;i<*count;++i)versions[i]=out.content_versions[i];return 1;}
}
