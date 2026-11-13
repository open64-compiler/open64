/* Copyright (C) 2026 Open64 Project */
/* Q6d test-only embedding ABI. Typed code is decoded from owner-authenticated
 * Q6c fixtures; this does not define a serving-engine packaging/selection ABI.
 * Contract: doc/AI-COMPILER-MLA-Q6D-DEVICE-BINDINGS-v1.0.md. */
#include "../be/vho/tensor_serving_cuda.h"
#include "../be/vho/tensor_q1_program.h"
#include "../rapidjson/document.h"
#include "../rapidjson/writer.h"
#include "../rapidjson/stringbuffer.h"
#include <cstring>
struct Q6D_REVIEW_SESSION {VHO_MLA_CUDA *runtime;VHO_MLA_CUDA_CODE approved;std::string error;bool (*hash)(const void *,size_t,unsigned char *);};
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
extern "C" {
/* Test Open keeps a cleanup-only object reachable after any acquired-resource failure. */
void *open64_q6d_create(const char *wire,const char *approved,const void *image,size_t bytes,bool (*hash)(const void *,size_t,unsigned char *)) {
    Q6D_REVIEW_SESSION *s=new Q6D_REVIEW_SESSION;s->runtime=NULL;s->hash=hash;
    try {s->approved=Q6D_decode(approved);VHO_MLA_CUDA_CODE code=Q6D_decode(wire);VHO_MLA_CUDA_HOOKS hooks={s,Q6D_authorize,Q6D_hash};
        VHO_MLA_CUDA::Open(code,image,bytes,(void *)0x101,(void *)0x201,(void *)0x202,hooks,s->runtime,s->error);
    }catch(const std::exception &e){s->error=e.what();}return s;
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
/* Consumer stream obtains a single atomic cache/scale/map reader lease. */
int open64_q6d_reader(void *p,UINT64 bundle,UINT64 *reader,UINT64 *addresses,unsigned *count) {auto s=(Q6D_REVIEW_SESSION *)p;std::vector<UINT64> out;bool ok=s->runtime->Reader(bundle,(void *)0x203,*reader,out,s->error);if(ok){*count=out.size();for(unsigned i=0;i<out.size();++i)addresses[i]=out[i];}return ok;}
/* Consumer completion is recorded once and retryable on pending/error. */
int open64_q6d_reader_done(void *p,UINT64 reader,int wait) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Reader_done(reader,wait,s->error);}
/* No cache/scale component can retire while a consumer or parent user is live. */
int open64_q6d_bundle_retire(void *p,UINT64 bundle) {auto s=(Q6D_REVIEW_SESSION *)p;return s->runtime->Retire_bundle(bundle,s->error);}
/* Inspect a complete immutable generation tuple, without acquiring a lease. */
int open64_q6d_bundle(void *p,UINT64 bundle,UINT64 *versions,unsigned *count) {auto s=(Q6D_REVIEW_SESSION *)p;VHO_SERVING_BUNDLE_FACTS out;if(!s->runtime->Inspect_bundle(bundle,out,s->error))return 0;*count=out.content_versions.size();for(unsigned i=0;i<*count;++i)versions[i]=out.content_versions[i];return 1;}
}
