/* Copyright (C) 2026 Open64 Project */

/* Real execution-consumer composition with shared registry and existing CUDA
 * API mocks. Sentinel writes verify movement, NOT LayerNorm arithmetic or GPU.
 * Design: doc/AI-COMPILER-LAYERNORM-EXECUTION-CONSUMER-v0.1.md. */
#define main LN4_adapter_fixture_main
#include "normalization_cuda_test.cxx"
#undef main
#include "open64_normalization_execution.h"
#include <condition_variable>
#include <thread>

struct EXECUTION_MEMORY {
  OPEN64_TARGET_BUFFER_VIEW view;
  uint64_t key,handle;
  bool live,borrowed;
};
struct EXECUTION_TEST_STATE {
  std::vector<EXECUTION_MEMORY> memory;
  OPEN64_NORMALIZATION_ROOT_REQUEST requests[4];
  OPEN64_NORMALIZATION_EXECUTION *consumer;
  unsigned allocs,frees,release_calls,h2d,d2h,ready_calls,publish_calls,quiesces;
  int fail_alloc,fail_h2d,fail_d2h,fail_release;
  bool empty_alloc,invalid_alloc,fail_resolve,fail_ready,fail_publish,fail_quiesce,pending,input_ready,drained;
  bool pause,entered,proceed;
  std::mutex mutex;std::condition_variable cv;
  /* Preserve synchronization objects and explicitly initialize plain state. */
  EXECUTION_TEST_STATE():consumer(NULL),allocs(0),frees(0),release_calls(0),h2d(0),d2h(0),ready_calls(0),
      publish_calls(0),quiesces(0),fail_alloc(0),fail_h2d(0),fail_d2h(0),fail_release(0),empty_alloc(false),invalid_alloc(false),
      fail_resolve(false),fail_ready(false),fail_publish(false),fail_quiesce(false),pending(false),
      input_ready(false),drained(false),pause(false),entered(false),proceed(false)
  {memset(requests,0,sizeof(requests));}
};
static EXECUTION_TEST_STATE state;
static unsigned execution_checks,scenarios;

/* A separate test tag keeps new composition evidence distinct from API mocks. */
static void execution_check(bool value,const char *name)
{
  fprintf(trace,"AI_NORM_EXEC_TEST 1 CHECK %s %u %llu\n",name,value?1:0,(unsigned long long)manifest.guard.source_line);
  if(!value){fprintf(stderr,"execution failed: %s\n",name);exit(1);}++execution_checks;
}

/* Exact view equality without depending on private runtime helpers. */
static bool equal_view(const OPEN64_TARGET_BUFFER_VIEW &a,const OPEN64_TARGET_BUFFER_VIEW &b)
{return a.address==b.address && a.bytes==b.bytes && a.tier==b.tier && a.device_id==b.device_id;}

/* Provider import validation is read-only and checks real owned allocation bounds. */
static int execution_validate(void *,const OPEN64_TARGET_BUFFER_VIEW *v,size_t bytes,size_t alignment)
{
  for(size_t i=0;i<state.memory.size();++i)if(state.memory[i].live && equal_view(*v,state.memory[i].view) &&
      v->bytes==bytes && !(reinterpret_cast<uintptr_t>(v->address)%alignment))return 0;
  return 1;
}

/* Stable backing keys remain unique even when malloc later reuses an address. */
static int execution_resolve(void *,const OPEN64_TARGET_BUFFER_VIEW *v,uint64_t *key)
{
  if(state.fail_resolve)return 1;
  for(size_t i=0;i<state.memory.size();++i)if(state.memory[i].live && equal_view(*v,state.memory[i].view))
    {*key=state.memory[i].key;return 0;}
  return 1;
}

/* Reserve whole roots without reading host inputs; failures may still own memory. */
static int execution_allocate(void *,size_t bytes,size_t alignment,OPEN64_TARGET_BUFFER_VIEW *out)
{
  ++state.allocs;void *address=NULL;
  if(state.empty_alloc)return 1;
  if(posix_memalign(&address,alignment,bytes))return 1;
  memset(address,0xa5,bytes);EXECUTION_MEMORY m={{address,bytes,2,0},state.memory.size()+1,0,true,false};
  state.memory.push_back(m);*out=m.view;
  if(state.invalid_alloc)out->tier=OPEN64_TARGET_BUFFER_HOST;
  if(state.fail_alloc==static_cast<int>(state.allocs)){state.pending=true;return 1;}
  return 0;
}

/* Copies touch only guard-sized valid spans; failures deliberately copy half,
 * modeling ambiguous work that must quiesce before retry/free. */
static int execution_transfer(void *,const OPEN64_TARGET_BUFFER_VIEW *destination,const OPEN64_TARGET_BUFFER_VIEW *source,size_t bytes)
{
  execution_check(bytes==source->bytes && bytes==destination->bytes,"copy_exact_valid_extent");
  execution_check(open64_normalization_advance(runtime,ticket,1)==OPEN64_NORM_BUSY,"copies_inside_leased_busy_event");
  bool input=source->tier==OPEN64_TARGET_BUFFER_HOST;
  execution_check(input ? state.input_ready:state.drained,"copy_after_required_join");
  unsigned count=input ? ++state.h2d:++state.d2h;
  bool fail=input ? state.fail_h2d==static_cast<int>(count):state.fail_d2h==static_cast<int>(count);
  memcpy(destination->address,source->address,fail ? bytes/2:bytes);
  if(fail){state.pending=true;return 1;}return 0;
}

/* No free before quiescence/retirement; a retryable release failure keeps ownership. */
static int execution_release(void *,const OPEN64_TARGET_BUFFER_VIEW *v)
{
  ++state.release_calls;
  execution_check(!state.pending,"release_after_quiescence");
  EXECUTION_MEMORY *m=NULL;
  for(size_t i=0;i<state.memory.size();++i)if(state.memory[i].live && state.memory[i].view.address==v->address)m=&state.memory[i];
  execution_check(m && !m->borrowed,"consumer_never_frees_borrowed_root");
  if(state.fail_release==static_cast<int>(state.release_calls)){state.pending=true;return 1;}
  uint64_t probe=0;
  execution_check(open64_normalization_register(runtime,&m->view,&probe)==0,"owned_root_retired_before_free");
  execution_check(open64_normalization_retire(runtime,probe)==0,"retire_release_probe");
  free(m->view.address);m->live=false;++state.frees;return 0;
}

/* Configure shared driver address observations from the exact admitted views. */
static int execution_ready(void *,const OPEN64_NORMALIZATION_INVOCATION *v)
{
  ++state.ready_calls;ticket=v->ticket;
  execution_check(state.h2d==0,"producer_join_before_host_reads");
  execution_check(open64_normalization_execution_destroy(state.consumer)==OPEN64_NORM_BUSY,"reentrant_destroy_busy");
  execution_check(open64_normalization_execution_resume(state.consumer)==OPEN64_NORM_BUSY,"reentrant_resume_busy");
  roots.clear();
  for(unsigned i=0;i<4;++i){OPEN64_TARGET_BUFFER_VIEW root=v->views[i];
    root.address=reinterpret_cast<void *>(reinterpret_cast<uintptr_t>(root.address)-manifest.guard.ranges[i].offset);
    root.bytes=static_cast<size_t>(manifest.guard.ranges[i].capacity);roots.push_back(root);}
  if(state.pause){std::unique_lock<std::mutex> lock(state.mutex);state.entered=true;state.cv.notify_all();
    state.cv.wait(lock,[]{return state.proceed;});}
  if(state.fail_ready){state.pending=true;return 1;}
  state.input_ready=true;return 0;
}

/* Host Y is copied before consumption; retries must not repeat a successful copy. */
static int execution_publish(void *,const OPEN64_NORMALIZATION_INVOCATION *)
{
  ++state.publish_calls;execution_check(state.drained,"consumer_after_device_drain");
  if(state.requests[3].view.tier==OPEN64_TARGET_BUFFER_HOST)execution_check(state.d2h>=1,"host_output_before_consumer");
  if(state.fail_publish){state.pending=true;return 1;}return 0;
}

/* Explicit join for every ambiguous provider/hook failure; failing join pins state. */
static int execution_quiesce(void *)
{
  ++state.quiesces;
  execution_check(open64_normalization_execution_cancel(state.consumer)==OPEN64_NORM_BUSY,"quiesce_reentry_busy");
  if(state.fail_quiesce)return 1;
  state.pending=false;return 0;
}

/* Simulated kernel writes check transfers and canaries, never normalization math. */
static void execution_launch(void **args)
{
  execution_check(state.input_ready,"launch_after_input_ready");
  for(unsigned i=0;i<3;++i) {
    const unsigned char *source=static_cast<const unsigned char *>(state.requests[i].view.address)+manifest.guard.ranges[i].offset;
    const void *actual=reinterpret_cast<void *>(static_cast<uintptr_t>(*static_cast<CUdeviceptr *>(args[i])));
    execution_check(!memcmp(source,actual,static_cast<size_t>(manifest.guard.ranges[i].bytes)),"kernel_receives_exact_input_bytes");
  }
  void *output=reinterpret_cast<void *>(static_cast<uintptr_t>(*static_cast<CUdeviceptr *>(args[3])));
  memset(output,0x42,static_cast<size_t>(manifest.guard.ranges[3].bytes));
}

/* Observe only the mock stream join; no internal CTA events are fabricated. */
static void execution_drain() {state.drained=true;}

/* Fresh per-invocation counters, while allocation identities/registry stay shared. */
static void reset_job()
{
  state.allocs=state.frees=state.release_calls=state.h2d=state.d2h=state.ready_calls=state.publish_calls=state.quiesces=0;
  state.fail_alloc=state.fail_h2d=state.fail_d2h=state.fail_release=0;
  state.empty_alloc=state.invalid_alloc=state.fail_resolve=state.fail_ready=state.fail_publish=state.fail_quiesce=state.pending=false;
  state.input_ready=state.drained=state.pause=state.entered=state.proceed=false;fault=0;fail_event=-1;
  ++scenarios;fprintf(trace,"AI_NORM_EXEC_TEST 1 SCENARIO %u %llu\n",scenarios,(unsigned long long)manifest.guard.source_line);
}

/* Construct whole host/device roots according to canonical guard roles. */
static void requests(unsigned device_mask)
{
  memset(state.requests,0,sizeof(state.requests));
  for(unsigned i=0;i<4;++i) {
    unsigned previous=i;
    for(unsigned j=0;j<i;++j)if(manifest.guard.ranges[i].role==manifest.guard.ranges[j].role){previous=j;break;}
    if(previous!=i){state.requests[i]=state.requests[previous];continue;}
    const OPEN64_NORMALIZATION_RANGE &g=manifest.guard.ranges[i];void *address=NULL;
    execution_check(posix_memalign(&address,static_cast<size_t>(g.alignment),static_cast<size_t>(g.capacity))==0,"allocate_caller_root");
    memset(address,i==3 ? 0xcc:0x17+i,static_cast<size_t>(g.capacity));
    OPEN64_NORMALIZATION_ROOT_REQUEST &request=state.requests[i];
    request.view={address,static_cast<size_t>(g.capacity),(device_mask&(1U<<i)) ? 2U:1U,0};
    if(request.view.tier==OPEN64_TARGET_BUFFER_DEVICE) {
      EXECUTION_MEMORY m={request.view,state.memory.size()+1,0,true,true};state.memory.push_back(m);
      execution_check(open64_normalization_register(runtime,&request.view,&request.allocation)==0,"register_borrowed_caller_root");
      state.memory.back().handle=request.allocation;
    }
  }
}

/* Caller cleanup never depends on consumer freeing borrowed allocations. */
static void free_requests()
{
  for(unsigned i=0;i<4;++i) {
    bool first=true;for(unsigned j=0;j<i;++j)if(state.requests[i].view.address==state.requests[j].view.address)first=false;
    if(!first)continue;
    if(state.requests[i].allocation) {
      OPEN64_TARGET_BUFFER_VIEW snapshot={};
      execution_check(open64_normalization_root_view(runtime,state.requests[i].allocation,&snapshot)==0 &&
          equal_view(snapshot,state.requests[i].view),"borrowed_root_survives_dispatch");
      execution_check(open64_normalization_retire(runtime,state.requests[i].allocation)==0,"caller_retires_borrowed_root");
      for(size_t j=0;j<state.memory.size();++j)if(state.memory[j].live && state.memory[j].view.address==snapshot.address)state.memory[j].live=false;
    }
    free(state.requests[i].view.address);
  }
}

/* Only Y's valid range changes; all root padding retains caller canaries. */
static void output(bool changed)
{
  const OPEN64_NORMALIZATION_RANGE &g=manifest.guard.ranges[3];const unsigned char *bytes=static_cast<unsigned char *>(state.requests[3].view.address);
  bool valid=true;
  for(size_t i=0;i<state.requests[3].view.bytes;++i)if(bytes[i]!=(changed && i>=g.offset && i-g.offset<g.bytes ? 0x42:0xcc))valid=false;
  execution_check(valid,"output_and_padding_bytes");
}

/* Consumer opens always use the same shared registry and reviewed service hooks. */
static OPEN64_NORMALIZATION_EXECUTION_CONFIG config(bool quiet=false)
{
  OPEN64_NORMALIZATION_EXECUTION_CONFIG c={};c.runtime=runtime;c.buffers={1,1,0,NULL,execution_allocate,
      execution_transfer,execution_validate,execution_release};c.hooks={NULL,digest,execution_ready,execution_publish};
  c.quiesce=execution_quiesce;c.cuda_context=context();c.cuda_stream=stream();c.device=0;c.flags=1;
  c.context_id=manifest.guard.context;c.trace=quiet ? NULL:trace;return c;
}

/* Reuse the actual compiled-entry fixture; no invented producer or GPU claim. */
int main(int argc,char **argv)
{
  if(argc!=4)return 2;
  unsigned index=static_cast<unsigned>(atoi(argv[1]));
  if(index>=sizeof(entries)/sizeof(entries[0]))return 2;
  manifest=entries[index];
  FILE *file=fopen(argv[2],"rb");if(!file)return 2;image.resize(static_cast<size_t>(manifest.cubin_bytes));
  bool loaded=fread(image.data(),1,image.size(),file)==image.size() && fgetc(file)==EOF;fclose(file);if(!loaded)return 2;
  trace=fopen(argv[3],"wx");if(!trace)return 2;fail_event=-1;
  OPEN64_TARGET_BUFFER_SERVICE service={1,1,0,NULL,execution_allocate,execution_transfer,execution_validate,execution_release};
  runtime=open64_normalization_runtime_create(&service,execution_resolve,static_cast<uint32_t>(manifest.guard.architecture),manifest.guard.context,1,trace);
  execution_check(runtime!=NULL,"create_shared_registry");OPEN64_NORMALIZATION_EXECUTION_CONFIG c=config();
  execution_check(open64_normalization_execution_open(&manifest,image.data(),image.size(),&c,&state.consumer)==0,"open_execution_consumer");
  launch_observer=execution_launch;drain_observer=execution_drain;
  for(unsigned mode=0;mode<3;++mode) {
    reset_job();requests(mode==0 ? 0:mode==1 ? 6:15);
    execution_check(open64_normalization_execution_run(state.consumer,state.requests)==0,"host_mixed_device_dispatch");
    execution_check(state.h2d==(mode==2 ? 0:mode==1 ? 1:manifest.guard.ranges[1].role==manifest.guard.ranges[2].role ? 2:3),"deduplicated_input_copy_count");
    execution_check(state.d2h==(mode==2 ? 0:1),"output_copy_count");output(true);free_requests();
  }
  reset_job();requests(15);
  OPEN64_TARGET_BUFFER_VIEW unchanged={reinterpret_cast<void *>(0x77),77,77,77},saved=unchanged;
  execution_check(open64_normalization_root_view(runtime,UINT64_MAX,&unchanged)==1 && equal_view(unchanged,saved),"invalid_snapshot_preserves_output");
  OPEN64_NORMALIZATION_ROOT_REQUEST bad[4];memcpy(bad,state.requests,sizeof(bad));bad[0].allocation=UINT64_MAX;
  execution_check(open64_normalization_execution_begin(state.consumer,bad)==1,"stale_borrowed_handle_refused");
  OPEN64_NORMALIZATION_EXECUTION *second=NULL;
  execution_check(open64_normalization_execution_open(&manifest,image.data(),image.size(),&c,&second)==0,"open_second_shared_consumer");
  execution_check(open64_normalization_execution_begin(state.consumer,state.requests)==0,"first_shared_lease");
  execution_check(open64_normalization_execution_begin(second,state.requests)==2,"cross_consumer_output_conflict");
  OPEN64_NORMALIZATION_EXECUTION *first=state.consumer;state.consumer=second;
  execution_check(open64_normalization_execution_cancel(second)==0,"conflicting_consumer_cleanup");state.consumer=first;
  execution_check(open64_normalization_retire(runtime,state.requests[0].allocation)==4,"other_consumer_cannot_drop_lease");
  execution_check(open64_normalization_execution_resume(state.consumer)==0,"first_shared_consumer_finishes");
  execution_check(open64_normalization_execution_destroy(second)==0,"destroy_second_consumer");free_requests();
  reset_job();requests(15);OPEN64_NORMALIZATION_ROOT_REQUEST first_output=state.requests[3];
  void *extra=NULL;const OPEN64_NORMALIZATION_RANGE &output_range=manifest.guard.ranges[3];
  execution_check(posix_memalign(&extra,static_cast<size_t>(output_range.alignment),static_cast<size_t>(output_range.capacity))==0,"allocate_independent_output");
  memset(extra,0xcc,static_cast<size_t>(output_range.capacity));
  OPEN64_NORMALIZATION_ROOT_REQUEST second_output={{extra,static_cast<size_t>(output_range.capacity),2,0},0};
  EXECUTION_MEMORY extra_memory={second_output.view,state.memory.size()+1,0,true,true};state.memory.push_back(extra_memory);
  execution_check(open64_normalization_register(runtime,&second_output.view,&second_output.allocation)==0,"register_independent_output");
  execution_check(open64_normalization_execution_open(&manifest,image.data(),image.size(),&c,&second)==0,"open_independent_output_consumer");
  execution_check(open64_normalization_execution_begin(state.consumer,state.requests)==0,"acquire_first_shared_reads");
  state.requests[3]=second_output;
  execution_check(open64_normalization_execution_begin(second,state.requests)==0,"independent_outputs_share_read_leases");
  first=state.consumer;state.consumer=second;
  execution_check(open64_normalization_execution_resume(second)==0,"second_output_completes_first");output(true);
  execution_check(open64_normalization_retire(runtime,state.requests[0].allocation)==4,"first_reader_still_pins_shared_input");
  state.consumer=first;state.requests[3]=first_output;state.input_ready=state.drained=false;
  execution_check(open64_normalization_execution_resume(first)==0,"first_output_completes_second");output(true);
  execution_check(open64_normalization_execution_destroy(second)==0,"destroy_independent_output_consumer");
  execution_check(open64_normalization_retire(runtime,second_output.allocation)==0,"retire_independent_output");
  for(size_t i=0;i<state.memory.size();++i)if(state.memory[i].live && state.memory[i].view.address==extra)state.memory[i].live=false;
  free(extra);free_requests();
  reset_job();requests(0);memcpy(bad,state.requests,sizeof(bad));--bad[0].view.bytes;
  execution_check(open64_normalization_execution_begin(state.consumer,bad)==1 && !state.allocs,"bad_extent_preflight_no_allocation");
  memcpy(bad,state.requests,sizeof(bad));bad[3].view.address=bad[0].view.address;
  execution_check(open64_normalization_execution_begin(state.consumer,bad)==1 && !state.allocs,"overlapping_host_roles_refused");
  execution_check(open64_normalization_execution_cancel(state.consumer)==3,"preflight_failure_stays_idle");
  execution_check(open64_normalization_execution_run(state.consumer,state.requests)==0,"valid_dispatch_after_preflight_refusal");output(true);free_requests();
  reset_job();requests(0);unsigned before_cancel_launches=launches;
  execution_check(open64_normalization_execution_begin(state.consumer,state.requests)==0,"admit_before_cancel");
  state.fail_quiesce=true;
  execution_check(open64_normalization_execution_cancel(state.consumer)==5 && !state.frees && !state.h2d,"prelaunch_cancel_join_failure_keeps_loans");
  execution_check(open64_normalization_execution_resume(state.consumer)==6,"canceling_invocation_cannot_resume_work");
  state.fail_quiesce=false;execution_check(open64_normalization_execution_cancel(state.consumer)==0,"cancel_prelaunch_after_join");
  execution_check(launches==before_cancel_launches && !state.h2d,"prelaunch_cancel_has_no_kernel_or_copy");output(false);free_requests();
  reset_job();requests(0);state.empty_alloc=true;
  execution_check(open64_normalization_execution_begin(state.consumer,state.requests)==5 && !state.frees,"empty_allocation_failure_retains_only_loans");
  execution_check(open64_normalization_execution_cancel(state.consumer)==0 && !state.frees,"empty_allocation_failure_cleanup");output(false);free_requests();
  for(unsigned failure=0;failure<11;++failure) {
    reset_job();requests(0);unsigned prior_launches=launches;
    if(failure==0)state.fail_alloc=1;
    if(failure==1)state.invalid_alloc=true;
    if(failure==2)state.fail_resolve=true;
    if(failure<=2) {
      execution_check(open64_normalization_execution_begin(state.consumer,state.requests)!=0,"partial_binding_failure");
      execution_check(!state.frees && !state.h2d && open64_normalization_execution_destroy(state.consumer)==4,"failed_binding_keeps_loans");
      state.fail_quiesce=true;execution_check(open64_normalization_execution_cancel(state.consumer)==5 && !state.frees,"quiesce_failure_pins_partial_binding");
      state.fail_quiesce=state.invalid_alloc=state.fail_resolve=false;
      execution_check(open64_normalization_execution_cancel(state.consumer)==0,"cancel_partial_binding");output(false);free_requests();continue;
    }
    if(failure==3)state.fail_ready=true;
    if(failure==4)state.fail_h2d=2;
    if(failure==5)fail_event=1;
    if(failure==6)fail_event=11;
    if(failure==7)state.fail_d2h=1;
    if(failure==8 || failure==10)state.fail_publish=true;
    if(failure==9)state.fail_release=1;
    execution_check(open64_normalization_execution_run(state.consumer,state.requests)==5,"execution_failure_retains_state");
    execution_check(open64_normalization_execution_destroy(state.consumer)==4,"failed_work_cannot_destroy");
    if(failure==5) {
      execution_check(open64_normalization_execution_resume(state.consumer)==6,"failed_launch_not_reissued");
      fail_event=11;execution_check(open64_normalization_execution_cancel(state.consumer)==5 && !state.frees,"failed_drain_pins_allocations");
      fail_event=-1;execution_check(open64_normalization_execution_cancel(state.consumer)==0,"cancel_after_ambiguous_launch_drain");output(false);
    } else {
      unsigned before_copies=state.d2h;state.fail_quiesce=true;
      execution_check((failure==10 ? open64_normalization_execution_cancel(state.consumer):open64_normalization_execution_resume(state.consumer))==5,
          "retry_requires_quiescence");
      state.fail_quiesce=state.fail_ready=state.fail_publish=false;state.fail_h2d=state.fail_d2h=state.fail_release=0;fail_event=-1;
      execution_check((failure==10 ? open64_normalization_execution_cancel(state.consumer):open64_normalization_execution_resume(state.consumer))==0,"retry_or_cancel_completes");
      if(failure==8 || failure==10)execution_check(state.d2h==before_copies,"consumer_retry_does_not_recopy_output");
      if(failure==4)execution_check(state.h2d==(manifest.guard.ranges[1].role==manifest.guard.ranges[2].role ? 3U:4U),"input_retry_skips_completed_chunks");
      if(failure==7)execution_check(state.d2h==2,"copyback_failure_retries_only_failed_copy");
      output(true);
    }
    execution_check(launches==prior_launches+1,"kernel_never_relaunched_on_retry");free_requests();
  }
  reset_job();requests(15);state.pause=true;
  int worker_status=-1;std::thread worker([&]{worker_status=open64_normalization_execution_run(state.consumer,state.requests);});
  {std::unique_lock<std::mutex> lock(state.mutex);state.cv.wait(lock,[]{return state.entered;});}
  execution_check(open64_normalization_execution_resume(state.consumer)==4 && open64_normalization_execution_cancel(state.consumer)==4 &&
      open64_normalization_execution_destroy(state.consumer)==4,"concurrent_consumer_busy");
  {std::lock_guard<std::mutex> lock(state.mutex);state.proceed=true;}state.cv.notify_all();worker.join();
  execution_check(worker_status==0,"concurrent_dispatch_completes");output(true);free_requests();
  reset_job();requests(15);OPEN64_NORMALIZATION_EXECUTION *normal=state.consumer,*quiet=NULL;
  OPEN64_NORMALIZATION_EXECUTION_CONFIG q=config(true);
  execution_check(open64_normalization_execution_open(&manifest,image.data(),image.size(),&q,&quiet)==0,"open_trace_off_consumer");
  state.consumer=quiet;execution_check(open64_normalization_execution_run(quiet,state.requests)==0,"trace_off_dispatch_parity");
  execution_check(open64_normalization_execution_destroy(quiet)==0,"trace_off_destroy");state.consumer=normal;output(true);free_requests();
  reset_job();OPEN64_NORMALIZATION_EXECUTION *cleanup=NULL;fault=31;
  execution_check(open64_normalization_execution_open(&manifest,image.data(),image.size(),&c,&cleanup)==5 && cleanup,
      "failed_module_rollback_keeps_execution_handle");
  execution_check(open64_normalization_execution_begin(cleanup,NULL)==1,"cleanup_only_handle_cannot_dispatch");
  fault=0;execution_check(open64_normalization_execution_destroy(cleanup)==0,"cleanup_only_handle_unload_retry");
  execution_check(open64_normalization_execution_destroy(state.consumer)==0,"destroy_idle_consumer");
  for(size_t i=0;i<state.memory.size();++i)execution_check(!state.memory[i].live,"all_owned_and_borrowed_roots_accounted");
  execution_check(open64_normalization_runtime_destroy(runtime)==0,"destroy_shared_registry_last");
  fprintf(trace,"AI_NORM_EXEC_TEST 1 SUMMARY %u %u %u host_mock_sentinel_only %llu\n",execution_checks,scenarios,checks,
      (unsigned long long)manifest.guard.source_line);fclose(trace);return 0;
}
