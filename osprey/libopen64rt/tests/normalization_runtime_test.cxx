/* Copyright (C) 2026 Open64 Project */

/* LN2-d host-mock enforcement using actual native-exported guard bytes.
 * Design: doc/AI-COMPILER-LAYERNORM-RUNTIME-GUARDS-v0.1.md.
 * This test never implements or executes a LayerNorm arithmetic kernel. */
#include "open64_normalization_runtime.h"
#include <condition_variable>
#include <mutex>
#include <thread>
#include <vector>
#include <string.h>
#include <stdlib.h>

static FILE *trace;
static unsigned checks;
static uint64_t line;

/* Fail immediately; every successful assertion remains independently named. */
static void check(bool ok,const char *name)
{
  fprintf(trace,"AI_NORM_TEST 1 CHECK %s %u %llu\n",name,ok?1:0,(unsigned long long)line);
  if (!ok) {fprintf(stderr,"failed: %s\n",name);exit(1);}++checks;
}

struct OPEN64_NORMALIZATION_MOCK {
  std::vector<OPEN64_TARGET_BUFFER_VIEW> roots;
  unsigned calls[15];int fail;
  uint64_t forced_key;
  OPEN64_NORMALIZATION_RUNTIME *runtime;
  bool reenter,pause,entered,proceed;
  std::mutex mutex;std::condition_variable cv;
  /* Plain state is initialized explicitly; mutex/condition variable stay live. */
  OPEN64_NORMALIZATION_MOCK():fail(-1),forced_key(0),runtime(NULL),reenter(false),pause(false),entered(false),proceed(false)
  {memset(calls,0,sizeof(calls));}
};

/* Trusted mock accepts only owned WHOLE roots, not arbitrary input metadata. */
static int validate(void *context,const OPEN64_TARGET_BUFFER_VIEW *v,size_t bytes,size_t align)
{
  OPEN64_NORMALIZATION_MOCK *m=static_cast<OPEN64_NORMALIZATION_MOCK *>(context);
  for (size_t i=0;i<m->roots.size();++i) if (m->roots[i].address==v->address &&
      m->roots[i].bytes==v->bytes && bytes==v->bytes && reinterpret_cast<uintptr_t>(v->address)%align==0) return 0;
  return 1;
}

/* Root keys remain stable for each mock allocation's complete lifetime. */
static int resolve(void *context,const OPEN64_TARGET_BUFFER_VIEW *v,uint64_t *key)
{
  OPEN64_NORMALIZATION_MOCK *m=static_cast<OPEN64_NORMALIZATION_MOCK *>(context);
  for (size_t i=0;i<m->roots.size();++i) if (m->roots[i].address==v->address && m->roots[i].bytes==v->bytes)
    {*key=m->forced_key ? m->forced_key:i+1;return 0;}
  return 1;
}

/* Model synchronous host event callbacks only. No numeric work, fake GPU
 * instruction evidence, or internal-CTA event observation is manufactured. */
static int perform(void *context,uint32_t event,const OPEN64_NORMALIZATION_INVOCATION *v)
{
  OPEN64_NORMALIZATION_MOCK *m=static_cast<OPEN64_NORMALIZATION_MOCK *>(context);++m->calls[event];
  check(v->ticket && v->guard.source_version==2,"callback_exact_source");
  for (unsigned i=0;i<4;++i) check(v->views[i].bytes==v->guard.ranges[i].bytes,"callback_exact_range");
  if (m->reenter) {
    check(open64_normalization_advance(m->runtime,v->ticket,event)==OPEN64_NORM_BUSY,"callback_reentrant_busy");
    check(open64_normalization_abort(m->runtime,v->ticket)==OPEN64_NORM_BUSY,"callback_abort_busy");
  }
  if (event==1 && m->pause) {
    std::unique_lock<std::mutex> lock(m->mutex);m->entered=true;m->cv.notify_all();
    m->cv.wait(lock,[m]{return m->proceed;});
  }
  return m->fail==static_cast<int>(event) ? 1:0;
}

/* Drive only the observable host protocol, then explicitly close its ticket. */
static void finish(OPEN64_NORMALIZATION_RUNTIME *r,uint64_t id)
{
  const unsigned events[]={0,1,11,12,13,14};
  for (unsigned i=0;i<6;++i) check(open64_normalization_advance(r,id,events[i])==0,"ordered_host_event");
  check(open64_normalization_close(r,id)==0,"close_released");
}

/* Read the precise host-native guard image exported by production WOPT. */
int main(int argc,char **argv)
{
  if (argc!=3) return 2;
  OPEN64_NORMALIZATION_GUARD g;
  FILE *f=fopen(argv[1],"rb");if (!f) return 2;
  bool loaded=fread(&g,sizeof(g),1,f)==1 && fgetc(f)==EOF;fclose(f);if (!loaded) return 2;
  trace=fopen(argv[2],"wx");if (!trace) return 2;line=g.source_line;
  OPEN64_NORMALIZATION_MOCK m;uint64_t roots[4]={};
  OPEN64_TARGET_BUFFER_SERVICE service={};service.abi_version=1;service.target_id=1;service.device_id=7;
  service.context=&m;service.validate_import=validate;
  m.runtime=open64_normalization_runtime_create(&service,resolve,static_cast<uint32_t>(g.architecture),g.context,1,trace);
  OPEN64_NORMALIZATION_RUNTIME *r=m.runtime;check(r!=NULL,"create_host_mock");
  for (unsigned i=0;i<4;++i) {
    bool shared=false;
    for (unsigned j=0;j<i;++j) if (g.ranges[j].role==g.ranges[i].role) {roots[i]=roots[j];shared=true;break;}
    if (shared) continue;
    void *memory=NULL;check(posix_memalign(&memory,static_cast<size_t>(g.ranges[i].alignment),
          static_cast<size_t>(g.ranges[i].capacity))==0,"allocate_mock_root");
    OPEN64_TARGET_BUFFER_VIEW view={memory,static_cast<size_t>(g.ranges[i].capacity),2,7};m.roots.push_back(view);
    check(open64_normalization_register(r,&view,&roots[i])==0,"register_canonical");
    uint64_t unchanged=123;check(open64_normalization_register(r,&view,&unchanged)==2 && unchanged==123,"duplicate_root_refused");
  }
  OPEN64_NORMALIZATION_ENTRY e={};e.manifest=g;e.max_ctas=g.ctas;e.max_threads=g.threads;
  e.max_shared=g.reserved_shared;e.flags=1;e.context=&m;e.perform=perform;
  uint64_t id=123;
  OPEN64_NORMALIZATION_RUNTIME *foreign=open64_normalization_runtime_create(&service,resolve,
      static_cast<uint32_t>(g.architecture),g.context+1,1,NULL);
  check(foreign && open64_normalization_admit(foreign,&g,&e,roots,&id)==1 && id==123 &&
      open64_normalization_runtime_destroy(foreign)==0,"foreign_context_refused");
  OPEN64_NORMALIZATION_RUNTIME *production=open64_normalization_runtime_create(&service,resolve,
      static_cast<uint32_t>(g.architecture),g.context,0,NULL);
  check(production && open64_normalization_admit(production,&g,&e,roots,&id)==1 && id==123 &&
      open64_normalization_runtime_destroy(production)==0,"host_mock_not_production");
  /* Mutate every field without changing the trusted entry manifest. */
  for (unsigned word=0;word<sizeof(g)/8;++word) {
    OPEN64_NORMALIZATION_GUARD bad=g;uint64_t value;
    memcpy(&value,reinterpret_cast<char *>(&bad)+word*8,8);value^=1;
    memcpy(reinterpret_cast<char *>(&bad)+word*8,&value,8);
    check(open64_normalization_admit(r,&bad,&e,roots,&id)==1 && id==123,"exact_manifest_mismatch");
  }
  for (unsigned fault=0;fault<18;++fault) {
    OPEN64_NORMALIZATION_GUARD bad=g;OPEN64_NORMALIZATION_ENTRY entry=e;uint64_t ids[4];memcpy(ids,roots,sizeof(ids));
    switch (fault) {
    case 0:bad.source_version=1;break;
    case 1:bad.fp_options=1;break;
    case 2:bad.context++;break;
    case 3:bad.before[14]=1;break;
    case 4:bad.before[0]=0;break;
    case 5:bad.ranges[0].offset=UINT64_MAX;break;
    case 6:bad.ranges[3].lease=1;break;
    case 7:bad.ranges[3].capacity++;break;
    case 8:bad.ranges[0].alignment=3;break;
    case 9:bad.rows=UINT64_MAX;break;
    case 10:bad.width=0;break;
    case 11:bad.tail=0;break;
    case 12:entry.max_ctas=g.ctas-1;break;
    case 13:entry.max_threads=g.threads-1;break;
    case 14:entry.flags=0;break;
    case 15:entry.perform=NULL;break;
    case 16:ids[3]=roots[0];break;
    case 17:bad.obligations=0;break;
    }
    entry.manifest=bad;
    check(open64_normalization_admit(r,&bad,&entry,ids,&id)==1 && id==123,"runtime_structural_refusal");
  }
  if (g.reserved_shared) {
    OPEN64_NORMALIZATION_ENTRY low=e;low.max_shared--;
    check(open64_normalization_admit(r,&g,&low,roots,&id)==1 && id==123,"actual_shared_limit_refusal");
  }
  check(m.calls[1]==0,"refusal_never_launches");
  check(open64_normalization_admit(r,&g,&e,roots,&id)==0,"admit_exact_export");
  /* Same read-only inputs can serve independent outputs concurrently. */
  void *extra=NULL;check(posix_memalign(&extra,16,static_cast<size_t>(g.ranges[3].capacity))==0,"allocate_second_output");
  OPEN64_TARGET_BUFFER_VIEW extra_view={extra,static_cast<size_t>(g.ranges[3].capacity),2,7};m.roots.push_back(extra_view);
  uint64_t extra_root=123;
  m.forced_key=1;
  check(open64_normalization_register(r,&extra_view,&extra_root)==2 && extra_root==123,"virtual_alias_root_refused");
  m.forced_key=0;check(open64_normalization_register(r,&extra_view,&extra_root)==0,"independent_output_root");
  OPEN64_NORMALIZATION_GUARD sibling=g;sibling.ranges[3].role+=1000;
  OPEN64_NORMALIZATION_ENTRY sibling_entry=e;sibling_entry.manifest=sibling;
  uint64_t siblings[4]={roots[0],roots[1],roots[2],extra_root},sibling_ticket=123;
  check(open64_normalization_admit(r,&sibling,&sibling_entry,siblings,&sibling_ticket)==0,"immutable_inputs_shared_across_tickets");
  check(open64_normalization_abort(r,sibling_ticket)==0 && open64_normalization_close(r,sibling_ticket)==0 &&
      open64_normalization_retire(r,extra_root)==0,"shared_input_ticket_cleanup");
  uint64_t other=123;
  check(open64_normalization_admit(r,&g,&e,roots,&other)==2 && other==123,"exclusive_output_conflict");
  check(open64_normalization_retire(r,roots[0])==4 && open64_normalization_retire(r,roots[3])==4,"live_leases_pin_roots");
  check(open64_normalization_runtime_destroy(r)==4,"live_context_not_destroyed");
  check(open64_normalization_advance(r,id,1)==3 && m.calls[1]==0,"launch_gated_on_ready_completion");
  for (unsigned i=2;i<11;++i) check(open64_normalization_advance(r,id,i)==1,"internal_events_not_host_observations");
  check(open64_normalization_advance(r,id,12)==3 && open64_normalization_advance(r,id,13)==3,"no_early_publish_or_release");
  m.reenter=true;finish(r,id);m.reenter=false;
  check(open64_normalization_advance(r,id,0)==1,"closed_ticket_stale");

  /* Retry a failed input wait without launching, then cancel before launch. */
  check(open64_normalization_admit(r,&g,&e,roots,&id)==0,"readmit_after_release");
  m.fail=0;check(open64_normalization_advance(r,id,0)==5,"failed_ready");
  check(open64_normalization_advance(r,id,1)==3,"failed_ready_no_launch");m.fail=-1;
  check(open64_normalization_abort(r,id)==0 && open64_normalization_close(r,id)==0,"abort_before_launch");

  /* Ambiguous launch pins storage until successful drain; no publication. */
  check(open64_normalization_admit(r,&g,&e,roots,&id)==0,"admit_failure_path");
  check(open64_normalization_advance(r,id,0)==0,"ready_failure_path");m.fail=1;
  check(open64_normalization_advance(r,id,1)==5,"ambiguous_failed_launch");unsigned launches=m.calls[1];
  check(open64_normalization_advance(r,id,1)==6 && m.calls[1]==launches,"failed_launch_not_reissued");
  check(open64_normalization_abort(r,id)==4,"failed_launch_pins_until_drain");m.fail=11;
  check(open64_normalization_advance(r,id,11)==5 && open64_normalization_abort(r,id)==4,"failed_drain_keeps_leases");
  m.fail=-1;check(open64_normalization_advance(r,id,11)==0,"retry_drain");
  check(open64_normalization_advance(r,id,12)==6,"failed_launch_never_published");
  check(open64_normalization_abort(r,id)==0 && open64_normalization_close(r,id)==0,"abort_after_drain");

  /* Consumer failure also pins output until its join succeeds. */
  check(open64_normalization_admit(r,&g,&e,roots,&id)==0,"admit_consumer_failure");
  check(open64_normalization_advance(r,id,0)==0 && open64_normalization_advance(r,id,1)==0 &&
      open64_normalization_advance(r,id,11)==0,"prepare_consumer_failure");m.fail=12;
  check(open64_normalization_advance(r,id,12)==5 && open64_normalization_abort(r,id)==4,"consumer_failure_pins");
  m.fail=-1;finish(r,id);

  /* Actual concurrent transition, not just reentrant callback calls. */
  check(open64_normalization_admit(r,&g,&e,roots,&id)==0,"admit_concurrent");
  check(open64_normalization_advance(r,id,0)==0,"ready_concurrent");m.pause=true;
  int launch_status=-1;
  std::thread worker([&]{launch_status=open64_normalization_advance(r,id,1);});
  {std::unique_lock<std::mutex> lock(m.mutex);m.cv.wait(lock,[&]{return m.entered;});}
  check(open64_normalization_advance(r,id,11)==4 && open64_normalization_close(r,id)==4,"concurrent_busy");
  {std::lock_guard<std::mutex> lock(m.mutex);m.proceed=true;}m.cv.notify_all();worker.join();m.pause=false;
  check(launch_status==0,"concurrent_launch_completes");finish(r,id);

  /* Validate real half-open overlap checks, not just role/root mismatch.
   * Adjacent X/Y views may share backing but never overlapping bytes. */
  if (g.width<=4096) {
    void *arena=NULL;size_t capacity=static_cast<size_t>(2*g.ranges[0].bytes);
    check(posix_memalign(&arena,16,capacity)==0,"allocate_adjacent_arena");
    OPEN64_TARGET_BUFFER_VIEW view={arena,capacity,2,7};m.roots.push_back(view);uint64_t arena_root=0;
    check(open64_normalization_register(r,&view,&arena_root)==0,"register_adjacent_arena");
    OPEN64_NORMALIZATION_GUARD adjacent=g;
    adjacent.ranges[0].role=400000;adjacent.ranges[0].capacity=capacity;
    adjacent.ranges[3]=adjacent.ranges[0];adjacent.ranges[3].lease=2;
    OPEN64_NORMALIZATION_ENTRY adapter=e;adapter.manifest=adjacent;
    uint64_t aliases[4]={arena_root,roots[1],roots[2],arena_root};other=123;
    check(open64_normalization_admit(r,&adjacent,&adapter,aliases,&other)==1 && other==123,"physical_output_overlap_refused");
    adjacent.ranges[3].offset=g.ranges[0].bytes;adapter.manifest=adjacent;
    check(open64_normalization_admit(r,&adjacent,&adapter,aliases,&other)==0,"adjacent_output_range_admitted");
    check(open64_normalization_retire(r,arena_root)==4,"adjacent_arena_pinned");finish(r,other);
    check(open64_normalization_retire(r,arena_root)==0,"adjacent_arena_released");
  }

  for (unsigned i=0;i<4;++i) {
    bool duplicate=false;for (unsigned j=0;j<i;++j) if (roots[j]==roots[i]) duplicate=true;
    if (!duplicate) check(open64_normalization_retire(r,roots[i])==0,"retire_after_terminal");
  }
  check(open64_normalization_admit(r,&g,&e,roots,&id)==1,"retired_roots_stale");
  check(open64_normalization_runtime_destroy(r)==0,"destroy_clean");
  for (size_t i=0;i<m.roots.size();++i) free(m.roots[i].address);
  fprintf(trace,"AI_NORM_TEST 1 SUMMARY %u host_mock_only %llu\n",checks,(unsigned long long)line);
  return fclose(trace) ? 1:0;
}
