/* Copyright (C) 2026 Open64 Project */
/* C ABI for exercising the native host-serving adapter from embedding tests.
 * Does not construct WHIRL or certify a vendor engine/GPU integration.
 * Canonical source metadata is decoded by the existing native Q1 service.
 * Design: doc/AI-COMPILER-MLA-Q3-SERVING-v1.0.md. */
#include "../be/vho/tensor_serving.h"
#include "../be/vho/tensor_q1_program.h"
#include "../be/vho/tensor_attention_search.h"
#include "../rapidjson/document.h"
#include <set>
#include "../rapidjson/writer.h"
#include "../rapidjson/stringbuffer.h"
#include <string>
#include <sstream>
#include <vector>
#include <stdexcept>
namespace {
thread_local std::string error,report_text;
struct SESSION {
    VHO_TENSOR_SERVING *adapter;
    VHO_TENSOR_PROGRAM program;
    DSL_GPU_RUNTIME_OBSERVATION gpu;
    /* A session owns only adapter bookkeeping; buffers remain caller owned. */
    SESSION():adapter(NULL) {}
    /* The public destructor gate has already checked outstanding work/readers. */
    ~SESSION() {delete adapter;}
};
/* Forward exact scalar UTF8 fields to a bounded JSON report. */
void string(rapidjson::Writer<rapidjson::StringBuffer> &w,const std::string &s) {w.String(s.c_str(),s.size());}
/* Reports copy registry-issued identities and current pinned observation only. */
std::string report(const VHO_SERVING_FACTS &f,const DSL_GPU_SERVING_RESULT &gpu=DSL_GPU_SERVING_RESULT()) {
    rapidjson::StringBuffer b;rapidjson::Writer<rapidjson::StringBuffer> w(b);w.StartObject();
    w.Key("schema");w.String("open64.mla-serving-facts.v1");w.Key("bound");w.Bool(f.bound);w.Key("ticket");w.Uint64(f.ticket);w.Key("parent_bundle");w.Uint64(f.parent_bundle);
    if(!gpu.selection_key.empty()) {
        w.Key("gpu_selection_key");string(w,gpu.selection_key);w.Key("gpu_lease_key");string(w,gpu.lease_key);
        w.Key("gpu_performance_reusable");w.Bool(gpu.performance_reusable);
    }
    w.Key("phase");string(w,f.workload.phase);w.Key("workload");string(w,f.workload.workload);w.Key("ragged");w.Bool(f.workload.ragged);
    w.Key("allocations");w.StartArray();for(unsigned i=0;i<f.allocation_ids.size();++i) {w.StartArray();w.Uint64(f.allocation_ids[i]);w.Uint64(f.allocation_generations[i]);w.Uint64(f.content_versions[i]);w.EndArray();}w.EndArray();
    w.Key("pages");w.StartArray();for(unsigned i=0;i<f.pages.size();++i) {
        const VHO_SERVING_PAGE &p=f.pages[i];w.StartObject();w.Key("request");w.Uint64(p.request);w.Key("logical_page");w.Uint64(p.logical_page);w.Key("physical_page");w.Uint64(p.physical_page);w.Key("capacity_tokens");w.Uint64(p.capacity_tokens);w.Key("valid_tokens");w.Uint64(p.valid_tokens);
        w.Key("pools");w.StartArray();for(unsigned k=0;k<4;++k) {w.StartArray();w.Uint64(p.byte_offsets[k]);w.Uint64(p.row_bytes[k]);w.Uint64(p.base_alignment[k]);w.Uint64(p.page_alignment[k]);w.EndArray();}w.EndArray();w.EndObject();
    }w.EndArray();w.Key("search_invoked");w.Bool(false);w.Key("gpu_binding");w.Bool(false);w.Key("tma_eligible");w.Bool(false);w.EndObject();return std::string(b.GetString(),b.GetSize());
}
}
/* Synthetic observer exercises contract plumbing only, never a GPU claim. */
static bool H1D_observe(void *context,DSL_GPU_RUNTIME_OBSERVATION &out,std::string &) {
    out=((SESSION *)context)->gpu;return true;
}
extern "C" {
/* Create from genuine producer metadata; invalid source or P policy refuses. */
void *open64_q3_create(const char *wire,UINT64 page_tokens) {
    SESSION *s=new SESSION;VHO_TENSOR_PROGRAM p;error.clear();
    if(!wire || !VHO_q1_program_decode(wire,p,error)) return s;
    std::vector<UINT64> legal(1,page_tokens);s->program=p;s->adapter=new VHO_TENSOR_SERVING(p,legal,error);return s;
}
/* Opt-in fixture owner: explicit requirements, known synthetic inventory and
 * a held reservation. Uses the real Q3 admission/publication implementation. */
void *open64_h1d_q3_create(const char *wire,UINT64 page_tokens,const char *device) {
    SESSION *s=(SESSION *)open64_q3_create(wire,page_tokens);if(!s->adapter || !s->adapter->Valid()) return s;
    DSL_GPU_SERVING_BINDING binding;DSL_GPU_HOST_REQUIREMENTS &r=binding.requirements;
    r.device=device;r.architecture="sm_90";r.partition="fixture-partition";r.toolchain="fixture-toolchain";
    r.selection_key="fixture-selected-profile";r.resource_key="fixture-resource";r.execution_key="fixture-execution";r.budget_key="fixture-budget";r.imported=true;
    s->gpu.device=r.device;s->gpu.architecture=r.architecture;s->gpu.partition=r.partition;s->gpu.toolchain=r.toolchain;
    s->gpu.resource_key=r.resource_key;s->gpu.execution_key=r.execution_key;s->gpu.budget_key=r.budget_key;
    s->gpu.known=s->gpu.reservation_live=true;s->gpu.context_generation=1;s->gpu.budget_generation=2;s->gpu.reservation_id=3;s->gpu.reserved_bytes=UINT64(1)<<30;
    binding.context=s;binding.observe=H1D_observe;delete s->adapter;
    s->adapter=new VHO_TENSOR_SERVING(s->program,std::vector<UINT64>(1,page_tokens),NULL,binding,error);return s;
}
/* Change one external observation; production has no fault-injection API. */
void open64_h1d_q3_change(void *object,const char *fault) {
    SESSION *s=(SESSION *)object;std::string f=fault;
    if(f=="unknown") s->gpu.known=false;
    if(f=="device") s->gpu.device="other-device";
    if(f=="isa") s->gpu.architecture="sm_90a";
    if(f=="partition") s->gpu.partition="other-partition";
    if(f=="resource") s->gpu.resource_key="other-resource";
    if(f=="budget") s->gpu.budget_key="other-budget";
    if(f=="toolchain") s->gpu.toolchain="other-toolchain";
    if(f=="calibration") s->gpu.execution_key="other-execution";
    if(f=="generation") ++s->gpu.budget_generation;
    if(f=="context") ++s->gpu.context_generation;
    if(f=="reservation") ++s->gpu.reservation_id;
    if(f=="expired") s->gpu.reservation_live=false;
    if(f=="capacity") s->gpu.reserved_bytes=1;
}
/* Refuse teardown while any prepared/running invocation or reader is alive. */
int open64_q3_destroy(void *object) {
    SESSION *s=(SESSION *)object;if(!s) return 1;
    if(s->adapter && !s->adapter->Quiescent()) {error="q3_session_in_use";return 0;}delete s;return 1;
}
/* The owner can inspect precise refusal text without treating it as authority. */
const char *open64_q3_error(void *object) {(void)object;return error.c_str();}
/* Check construction success without touching any registered address. */
int open64_q3_valid(void *object) {SESSION *s=(SESSION *)object;return s->adapter && s->adapter->Valid();}
/* Native borrowed host allocation registration; no Python runtime dependency. */
int open64_q3_register(void *object,void *base,UINT64 bytes,const char *kind,VHO_SERVING_HANDLE *out) {SESSION *s=(SESSION *)object;return s->adapter && s->adapter->Register(base,bytes,kind?kind:"unknown",*out,error);}
/* Cooperative mutations are rejected while the allocation is pinned/published. */
int open64_q3_write(void *object,VHO_SERVING_HANDLE handle,UINT64 offset,const void *data,UINT64 bytes) {SESSION *s=(SESSION *)object;return s->adapter && s->adapter->Write(handle,offset,data,bytes,error);}
/* Retire before the owning caller frees/reuses host storage. */
int open64_q3_retire(void *object,VHO_SERVING_HANDLE handle) {SESSION *s=(SESSION *)object;return s->adapter && s->adapter->Retire(handle,error);}
/* Native atomic import is the initial owner-ready publication boundary. */
int open64_q3_import(void *object,const VHO_SERVING_VIEW *views,unsigned n,VHO_SERVING_VIEW metadata,UINT64 *bundle) {SESSION *s=(SESSION *)object;return s->adapter && s->adapter->Import(std::vector<VHO_SERVING_VIEW>(views,views+n),metadata,*bundle,error);}
/* Bind the actual pointers and read their current runtime controls under leases. */
int open64_q3_begin(void *object,const VHO_SERVING_VIEW *views,unsigned n,VHO_SERVING_VIEW metadata,UINT64 parent,UINT64 *ticket) {
    SESSION *s=(SESSION *)object;VHO_SERVING_FACTS facts;
    bool ok=s->adapter && s->adapter->Begin(std::vector<VHO_SERVING_VIEW>(views,views+n),metadata,parent,facts,error);
    *ticket=ok?facts.ticket:0;DSL_GPU_SERVING_RESULT gpu;
    if(ok) s->adapter->Inspect_gpu(facts.ticket,false,gpu,error);report_text=report(facts,gpu);return ok;
}
/* The qualified caller launches exactly one synchronous entry, not a candidate. */
int open64_q3_run(void *object,UINT64 ticket,bool (*entry)(void *,void **,UINT32),void *context,UINT64 *bundle) {SESSION *s=(SESSION *)object;std::string why;bool ok=s->adapter && s->adapter->Run(ticket,entry,context,*bundle,why);error=why;return ok;}
/* Prepared cancellation never publishes dirty result buffers. */
int open64_q3_cancel(void *object,UINT64 ticket) {SESSION *s=(SESSION *)object;return s->adapter && s->adapter->Cancel(ticket,error);}
/* Expired/foreign tokens cannot dereference former allocations. */
int open64_q3_live(void *object,UINT64 ticket) {SESSION *s=(SESSION *)object;return s->adapter && s->adapter->Is_live(ticket);}
/* A reader pins the entire published cache/scale/mapping version. */
int open64_q3_reader(void *object,UINT64 bundle,UINT64 *reader) {SESSION *s=(SESSION *)object;return s->adapter && s->adapter->Acquire_reader(bundle,*reader,error);}
/* Copy all cache/scale/mapping generations from one completed publication. */
const char *open64_q3_bundle_report(void *object,UINT64 bundle) {
    SESSION *s=(SESSION *)object;VHO_SERVING_BUNDLE_FACTS f;if(!s->adapter || !s->adapter->Inspect_bundle(bundle,f,error)) return NULL;
    rapidjson::StringBuffer b;rapidjson::Writer<rapidjson::StringBuffer> w(b);w.StartObject();w.Key("bundle");w.Uint64(f.bundle);w.Key("components");w.StartArray();
    for(unsigned i=0;i<f.allocation_ids.size();++i) {w.StartArray();w.Uint64(f.allocation_ids[i]);w.Uint64(f.allocation_generations[i]);w.Uint64(f.content_versions[i]);w.EndArray();}w.EndArray();
    w.Key("valid_lengths");w.StartArray();for(unsigned i=0;i<f.valid_lengths.size();++i) w.Uint64(f.valid_lengths[i]);w.EndArray();
    DSL_GPU_SERVING_RESULT gpu;s->adapter->Inspect_gpu(bundle,true,gpu,error);
    if(!gpu.selection_key.empty()) {
        w.Key("gpu_selection_key");string(w,gpu.selection_key);w.Key("gpu_lease_key");string(w,gpu.lease_key);
        w.Key("gpu_performance_reusable");w.Bool(gpu.performance_reusable);
    }
    w.Key("completion");w.String("synchronous-host-status-zero");w.EndObject();report_text.assign(b.GetString(),b.GetSize());return report_text.c_str();
}
/* Completion releases all four buffers and their mapping together. */
int open64_q3_reader_done(void *object,UINT64 reader) {SESSION *s=(SESSION *)object;return s->adapter && s->adapter->Release_reader(reader,error);}
/* Bundle retirement is explicit and refuses outstanding consumers. */
int open64_q3_bundle_retire(void *object,UINT64 bundle) {SESSION *s=(SESSION *)object;return s->adapter && s->adapter->Retire_bundle(bundle,error);}
/* Owned report bytes are stable until the next Begin on this embedding object. */
const char *open64_q3_report(void *object) {(void)object;return report_text.c_str();}
}

#ifdef OPEN64_SERVING_TEST
/* Local exhaustion proof is available only in the focused embedding product. */
extern bool VHO_tensor_serving_counter_test();
extern "C" int open64_q3_counter_test() {return VHO_tensor_serving_counter_test();}
#endif

#include "tensor_quantized_movement_review.inc"

#include "tensor_q5_review.inc"
