/* Copyright (C) 2026 Open64 Project */

/* Runtime-owned LN4 host/service composition. Shared registry leases precede
 * data movement; failed work keeps owned buffers and borrowed loans retryable.
 * Design: doc/AI-COMPILER-LAYERNORM-EXECUTION-CONSUMER-v0.1.md. */
#include "open64_normalization_execution.h"
#include <atomic>
#include <new>
#include <string.h>

static std::atomic<uint64_t> next_execution_id(1);

struct OPEN64_NORMALIZATION_EXECUTION {
  std::atomic<bool> _operating;
  OPEN64_NORMALIZATION_COMPILED_ENTRY _manifest;
  OPEN64_NORMALIZATION_EXECUTION_CONFIG _config;
  OPEN64_NORMALIZATION_CUDA *_adapter;
  OPEN64_NORMALIZATION_ROOT_REQUEST _requests[4];
  OPEN64_TARGET_BUFFER_BINDING _bindings[4];
  uint64_t _roots[4],_ticket,_id,_sequence;
  unsigned _leader[4],_done,_copied_inputs;
  bool _registered[4],_ready,_active,_canceling,_launch_failed,_launch_attempted,
       _publish_attempted,_needs_quiesce,_inputs_ready,_output_copied;
  /* The operation token admits exactly one caller, including callback reentry. */
  OPEN64_NORMALIZATION_EXECUTION():_operating(false),_adapter(NULL),_ticket(0),_id(next_execution_id.fetch_add(1)),
      _sequence(0),_done(0),_copied_inputs(0),_ready(false),_active(false),_canceling(false),
      _launch_failed(false),_launch_attempted(false),_publish_attempted(false),
      _needs_quiesce(false),_inputs_ready(false),_output_copied(false)
  {memset(&_manifest,0,sizeof(_manifest));memset(&_config,0,sizeof(_config));
   memset(_requests,0,sizeof(_requests));memset(_bindings,0,sizeof(_bindings));
   memset(_roots,0,sizeof(_roots));memset(_leader,0,sizeof(_leader));memset(_registered,0,sizeof(_registered));}
};

namespace {
struct OPEN64_NORMALIZATION_EXECUTION_CALL {
  OPEN64_NORMALIZATION_EXECUTION *_owner;
  bool _entered;
  /* An atomic token avoids same-thread try_lock on a nonrecursive mutex. */
  explicit OPEN64_NORMALIZATION_EXECUTION_CALL(OPEN64_NORMALIZATION_EXECUTION *owner):_owner(owner),_entered(false)
  {bool expected=false;_entered=owner->_operating.compare_exchange_strong(expected,true,std::memory_order_acquire);}
  /* Release only if this call admitted; refused reentry never changes ownership. */
  ~OPEN64_NORMALIZATION_EXECUTION_CALL(){if(_entered)_owner->_operating.store(false,std::memory_order_release);}
  /* Query local admission without reading invocation state on a failed attempt. */
  bool Entered() const{return _entered;}
  /* Destroy is externally lifetime-synchronized; detach before deleting owner. */
  void Detach(){_entered=false;_owner->_operating.store(false,std::memory_order_release);}
};

/* One compact observation per stage; no raw addresses or GPU certification. */
int report(OPEN64_NORMALIZATION_EXECUTION *p,const char *action,int status,uint64_t a=0,uint64_t b=0)
{
  if(p->_config.trace)fprintf(p->_config.trace,"AI_NORM_EXEC 1 EVENT %llu %llu %s %d %llu %llu %llu\n",
      (unsigned long long)p->_id,(unsigned long long)++p->_sequence,action,status,
      (unsigned long long)a,(unsigned long long)b,(unsigned long long)p->_manifest.guard.source_line);
  return status;
}

/* Whole allocation identity is stricter than numeric tensor extents. */
bool same(const OPEN64_TARGET_BUFFER_VIEW &a,const OPEN64_TARGET_BUFFER_VIEW &b)
{return a.address==b.address && a.bytes==b.bytes && a.tier==b.tier && a.device_id==b.device_id;}

/* Checked span construction prevents pointer-wrap before alias/slice checks. */
bool span(const OPEN64_TARGET_BUFFER_VIEW &v,uint64_t bytes,uint64_t alignment)
{
  return v.address && bytes && bytes<=SIZE_MAX && v.bytes==bytes && alignment && alignment<=SIZE_MAX &&
      !(alignment&(alignment-1)) && !(reinterpret_cast<uintptr_t>(v.address)%alignment) &&
      bytes<=UINTPTR_MAX-reinterpret_cast<uintptr_t>(v.address);
}

/* Guard offsets, never inferred tensor layouts, determine all transfer views. */
OPEN64_TARGET_BUFFER_VIEW slice(OPEN64_NORMALIZATION_EXECUTION *p,unsigned slot,bool host)
{
  OPEN64_TARGET_BUFFER_VIEW view=host ? p->_requests[slot].view:p->_bindings[p->_leader[slot]].view;
  const OPEN64_NORMALIZATION_RANGE &range=p->_manifest.guard.ranges[slot];
  view.address=reinterpret_cast<void *>(reinterpret_cast<uintptr_t>(view.address)+static_cast<uintptr_t>(range.offset));
  view.bytes=static_cast<size_t>(range.bytes);return view;
}

/* Bind every admitted view before waiting or reading caller data. The shared
 * runtime already owns leases; its exact snapshots must match bound buffers. */
bool invocation(OPEN64_NORMALIZATION_EXECUTION *p,const OPEN64_NORMALIZATION_INVOCATION *v)
{
  if(!p->_active || !v || v->ticket!=p->_ticket || memcmp(&v->guard,&p->_manifest.guard,sizeof(v->guard)))return false;
  for(unsigned i=0;i<4;++i)if(!same(v->views[i],slice(p,i,false)))return false;
  return true;
}

/* Forward crypto through the copied deployment hook without installing a policy. */
int digest(void *context,const void *bytes,size_t size,unsigned char out[32])
{
  OPEN64_NORMALIZATION_EXECUTION *p=static_cast<OPEN64_NORMALIZATION_EXECUTION *>(context);
  return p->_config.hooks.sha256(p->_config.hooks.context,bytes,size,out);
}

/* READY holds real shared-registry leases before producer waits and H2D copies.
 * Successful chunks are not recopied after a later failed transfer. */
int ready(void *context,const OPEN64_NORMALIZATION_INVOCATION *v)
{
  OPEN64_NORMALIZATION_EXECUTION *p=static_cast<OPEN64_NORMALIZATION_EXECUTION *>(context);
  if(!invocation(p,v))return report(p,"binding",OPEN64_NORM_INVALID);
  if(!p->_inputs_ready) {
    int status=p->_config.hooks.ready(p->_config.hooks.context,v);
    report(p,"input_join",status ? OPEN64_NORM_BACKEND:0);
    if(status)return OPEN64_NORM_BACKEND;
    p->_inputs_ready=true;
  }
  for(unsigned i=0;i<3;++i) {
    if(p->_requests[i].view.tier!=OPEN64_TARGET_BUFFER_HOST || (p->_copied_inputs&(1U<<i)))continue;
    bool duplicate=false;
    for(unsigned j=0;j<i;++j)if(p->_leader[i]==p->_leader[j] &&
        p->_manifest.guard.ranges[i].offset==p->_manifest.guard.ranges[j].offset &&
        p->_manifest.guard.ranges[i].bytes==p->_manifest.guard.ranges[j].bytes)duplicate=true;
    if(duplicate){p->_copied_inputs|=1U<<i;continue;}
    OPEN64_TARGET_BUFFER_VIEW source=slice(p,i,true),destination=slice(p,i,false);
    int status=p->_config.buffers.transfer(p->_config.buffers.context,&destination,&source,source.bytes);
    report(p,"h2d",status ? OPEN64_NORM_BACKEND:0,i,source.bytes);
    if(status)return OPEN64_NORM_BACKEND;
    p->_copied_inputs|=1U<<i;
  }
  return 0;
}

/* PUBLISH occurs only after device drain. A completed copy is not repeated while
 * joining a failed/unfinished consumer; host visibility cannot be rolled back. */
int publish(void *context,const OPEN64_NORMALIZATION_INVOCATION *v)
{
  OPEN64_NORMALIZATION_EXECUTION *p=static_cast<OPEN64_NORMALIZATION_EXECUTION *>(context);
  if(!invocation(p,v))return report(p,"binding",OPEN64_NORM_INVALID);
  if(!p->_output_copied && p->_requests[3].view.tier==OPEN64_TARGET_BUFFER_HOST) {
    OPEN64_TARGET_BUFFER_VIEW source=slice(p,3,false),destination=slice(p,3,true);
    int status=p->_config.buffers.transfer(p->_config.buffers.context,&destination,&source,source.bytes);
    report(p,"d2h",status ? OPEN64_NORM_BACKEND:0,3,source.bytes);
    if(status)return OPEN64_NORM_BACKEND;
    p->_output_copied=true;
  }
  int status=p->_config.hooks.publish(p->_config.hooks.context,v);
  return report(p,"consumer_join",status ? OPEN64_NORM_BACKEND:0);
}

/* Validate without allocations/movement; snapshots grant no lease. Deliberately
 * reject overlapping distinct host roots until canonicalized by the caller. */
bool preflight(OPEN64_NORMALIZATION_EXECUTION *p,const OPEN64_NORMALIZATION_ROOT_REQUEST roots[4])
{
  if(!roots)return false;
  for(unsigned i=0;i<4;++i) {
    const OPEN64_NORMALIZATION_RANGE &g=p->_manifest.guard.ranges[i];const OPEN64_TARGET_BUFFER_VIEW &v=roots[i].view;
    if(!g.role || !span(v,g.capacity,g.alignment) || g.offset>g.capacity || !g.bytes || g.bytes>g.capacity-g.offset ||
        (v.tier!=OPEN64_TARGET_BUFFER_HOST && v.tier!=OPEN64_TARGET_BUFFER_DEVICE))return false;
    if(v.tier==OPEN64_TARGET_BUFFER_HOST){if(roots[i].allocation)return false;}
    else {
      OPEN64_TARGET_BUFFER_VIEW registered={};
      if(v.device_id!=p->_config.device || !roots[i].allocation ||
          open64_normalization_root_view(p->_config.runtime,roots[i].allocation,&registered) || !same(v,registered))return false;
    }
    for(unsigned j=0;j<i;++j) {
      if(g.role==p->_manifest.guard.ranges[j].role) {
        if(!same(v,roots[j].view) || roots[i].allocation!=roots[j].allocation)return false;
      } else if(v.tier==OPEN64_TARGET_BUFFER_HOST && roots[j].view.tier==OPEN64_TARGET_BUFFER_HOST) {
        uintptr_t a=reinterpret_cast<uintptr_t>(v.address),b=reinterpret_cast<uintptr_t>(roots[j].view.address);
        if(a<b+roots[j].view.bytes && b<a+v.bytes)return false;
      }
    }
  }
  return true;
}

/* Retain caller loans before allocating, and quarantine every partial provider
 * result. Old eager bind helpers cannot preserve this failure ownership model. */
int begin(OPEN64_NORMALIZATION_EXECUTION *p,const OPEN64_NORMALIZATION_ROOT_REQUEST requests[4])
{
  if(!p->_ready || p->_active)return report(p,"begin",p->_active ? OPEN64_NORM_BUSY:OPEN64_NORM_INVALID);
  if(!preflight(p,requests))return report(p,"preflight",OPEN64_NORM_INVALID);
  memcpy(p->_requests,requests,sizeof(p->_requests));p->_active=true;
  for(unsigned i=0;i<4;++i) {
    p->_leader[i]=i;
    for(unsigned j=0;j<i;++j)if(p->_manifest.guard.ranges[i].role==p->_manifest.guard.ranges[j].role){p->_leader[i]=p->_leader[j];break;}
    if(p->_leader[i]!=i){p->_roots[i]=p->_roots[p->_leader[i]];continue;}
    const OPEN64_NORMALIZATION_RANGE &g=p->_manifest.guard.ranges[i];
    if(requests[i].view.tier==OPEN64_TARGET_BUFFER_DEVICE) {
      p->_bindings[i].view=requests[i].view;p->_roots[i]=requests[i].allocation;
      if(p->_config.buffers.validate_import(p->_config.buffers.context,&p->_bindings[i].view,
          static_cast<size_t>(g.capacity),static_cast<size_t>(g.alignment)))return report(p,"import",OPEN64_NORM_INVALID,i);
    } else {
      int status=p->_config.buffers.allocate(p->_config.buffers.context,static_cast<size_t>(g.capacity),
          static_cast<size_t>(g.alignment),&p->_bindings[i].view);
      p->_bindings[i].runtime_owned=p->_bindings[i].view.address ? 1:0;
      report(p,"allocate",status ? OPEN64_NORM_BACKEND:0,i,g.capacity);
      if(status || !span(p->_bindings[i].view,g.capacity,g.alignment) ||
          p->_bindings[i].view.tier!=OPEN64_TARGET_BUFFER_DEVICE || p->_bindings[i].view.device_id!=p->_config.device ||
          p->_config.buffers.validate_import(p->_config.buffers.context,&p->_bindings[i].view,
              static_cast<size_t>(g.capacity),static_cast<size_t>(g.alignment))) {
        p->_needs_quiesce=true;return report(p,"allocation_quarantined",OPEN64_NORM_BACKEND,i,p->_bindings[i].runtime_owned);
      }
      int registered=open64_normalization_register(p->_config.runtime,&p->_bindings[i].view,&p->_roots[i]);
      if(registered)return report(p,"register",registered,i);
      p->_registered[i]=true;
    }
    report(p,p->_bindings[i].runtime_owned ? "owned_root":"borrowed_root",0,i,p->_roots[i]);
  }
  int status=open64_normalization_cuda_admit(p->_adapter,p->_config.runtime,p->_roots,&p->_ticket);
  return report(p,"admit",status,p->_ticket);
}

/* A failed callback may still reference buffers; successful quiescence is a
 * prerequisite for retries/cancellation, not fabricated runtime event completion. */
int quiesce(OPEN64_NORMALIZATION_EXECUTION *p)
{
  int status=p->_config.quiesce(p->_config.hooks.context);
  p->_needs_quiesce=status!=0;return report(p,"quiesce",status ? OPEN64_NORM_BACKEND:0);
}

/* Only success advances local progress. A failed launch is irreversible and
 * may only drain/cancel; base runtime retains its own authoritative event state. */
int advance(OPEN64_NORMALIZATION_EXECUTION *p,unsigned event)
{
  if(p->_needs_quiesce){int status=quiesce(p);if(status)return status;}
  if(event==OPEN64_NORM_LAUNCH)p->_launch_attempted=true;
  if(event==OPEN64_NORM_PUBLISH)p->_publish_attempted=true;
  int status=open64_normalization_advance(p->_config.runtime,p->_ticket,event);
  if(!status)p->_done|=1U<<event;
  else {p->_needs_quiesce=true;if(event==OPEN64_NORM_LAUNCH)p->_launch_failed=true;}
  return report(p,"advance",status,event,p->_ticket);
}

/* Close precedes retirement; retirement precedes free. Failed steps preserve
 * their owned handle/binding and are never repeated after successful completion. */
int cleanup(OPEN64_NORMALIZATION_EXECUTION *p)
{
  if(p->_ticket) {
    int status=open64_normalization_cuda_close(p->_adapter,p->_config.runtime,p->_ticket);
    report(p,"close",status,p->_ticket);if(status)return status;p->_ticket=0;
  }
  for(unsigned i=0;i<4;++i) {
    if(p->_leader[i]!=i)continue;
    if(p->_registered[i]) {
      int status=open64_normalization_retire(p->_config.runtime,p->_roots[i]);
      report(p,"retire",status,i,p->_roots[i]);if(status)return status;p->_registered[i]=false;
    }
    if(p->_bindings[i].view.address) {
      int status=open64_target_buffer_release(&p->_config.buffers,&p->_bindings[i]);
      report(p,"release",status ? OPEN64_NORM_BACKEND:0,i);
      if(status){p->_needs_quiesce=true;return OPEN64_NORM_BACKEND;}
    }
  }
  memset(p->_requests,0,sizeof(p->_requests));memset(p->_bindings,0,sizeof(p->_bindings));
  memset(p->_roots,0,sizeof(p->_roots));memset(p->_leader,0,sizeof(p->_leader));
  p->_active=p->_canceling=p->_launch_failed=p->_launch_attempted=p->_publish_attempted=false;
  p->_needs_quiesce=p->_inputs_ready=p->_output_copied=false;p->_done=p->_copied_inputs=0;
  return report(p,"idle",0);
}

/* Continue exactly the first incomplete host event, never choosing another plan. */
int resume(OPEN64_NORMALIZATION_EXECUTION *p)
{
  if(!p->_active || !p->_ticket)return report(p,"resume",OPEN64_NORM_ORDER);
  if(p->_canceling || p->_launch_failed)return report(p,"resume",OPEN64_NORM_FAILED);
  const unsigned events[]={0,1,11,12,13,14};
  for(unsigned i=0;i<6;++i)if(!(p->_done&(1U<<events[i]))){int status=advance(p,events[i]);if(status)return status;}
  return cleanup(p);
}
}

/* Copy configuration and open the existing adapter with wrapper callbacks.
 * Preserve cleanup ownership if module rollback cannot complete. */
int open64_normalization_execution_open(const OPEN64_NORMALIZATION_COMPILED_ENTRY *manifest,
    const void *cubin,size_t bytes,const OPEN64_NORMALIZATION_EXECUTION_CONFIG *config,
    OPEN64_NORMALIZATION_EXECUTION **output)
{
  if(!manifest || !config || !output || !config->runtime || !config->quiesce ||
      !config->hooks.sha256 || !config->hooks.ready || !config->hooks.publish ||
      config->buffers.abi_version!=1 || config->buffers.target_id!=OPEN64_TARGET_NVIDIA ||
      config->buffers.device_id!=config->device || !config->buffers.allocate || !config->buffers.transfer ||
      !config->buffers.validate_import || !config->buffers.release)return OPEN64_NORM_INVALID;
  OPEN64_NORMALIZATION_EXECUTION *p=new(std::nothrow) OPEN64_NORMALIZATION_EXECUTION;
  if(!p)return OPEN64_NORM_CAPACITY;
  p->_manifest=*manifest;p->_config=*config;
  OPEN64_NORMALIZATION_CUDA_HOOKS hooks={p,digest,ready,publish};
  int status=open64_normalization_cuda_open(&p->_manifest,cubin,bytes,config->cuda_context,config->cuda_stream,
      config->device,config->context_id,config->flags,&hooks,config->trace,&p->_adapter);
  if(status && !p->_adapter){delete p;return status;}
  p->_ready=status==0;*output=p;return report(p,"open",status);
}

/* Nonblocking serialization makes callback reentry a refusal, not a deadlock. */
int open64_normalization_execution_begin(OPEN64_NORMALIZATION_EXECUTION *p,const OPEN64_NORMALIZATION_ROOT_REQUEST roots[4])
{
  if(!p)return OPEN64_NORM_INVALID;
  OPEN64_NORMALIZATION_EXECUTION_CALL call(p);
  if(!call.Entered())return OPEN64_NORM_BUSY;
  return begin(p,roots);
}

/* Resume preserves active state through both event failures and cleanup retries. */
int open64_normalization_execution_resume(OPEN64_NORMALIZATION_EXECUTION *p)
{
  if(!p)return OPEN64_NORM_INVALID;
  OPEN64_NORMALIZATION_EXECUTION_CALL call(p);
  if(!call.Entered())return OPEN64_NORM_BUSY;
  if(p->_active && !p->_ticket && !p->_canceling && (p->_done&(1U<<14))) {
    if(p->_needs_quiesce){int status=quiesce(p);if(status)return status;}
    return cleanup(p);
  }
  return resume(p);
}

/* Hold one consumer operation across begin+resume; other consumers still share
 * the real registry and may run concurrently on nonconflicting leased ranges. */
int open64_normalization_execution_run(OPEN64_NORMALIZATION_EXECUTION *p,const OPEN64_NORMALIZATION_ROOT_REQUEST roots[4])
{
  if(!p)return OPEN64_NORM_INVALID;
  OPEN64_NORMALIZATION_EXECUTION_CALL call(p);
  if(!call.Entered())return OPEN64_NORM_BUSY;
  int status=begin(p,roots);return status ? status:resume(p);
}

/* Cancel joins every started phase, including ambiguous copy operations, before
 * dropping leases or storage. Once cancellation starts, resume cannot restart it. */
int open64_normalization_execution_cancel(OPEN64_NORMALIZATION_EXECUTION *p)
{
  if(!p)return OPEN64_NORM_INVALID;
  OPEN64_NORMALIZATION_EXECUTION_CALL call(p);
  if(!call.Entered())return OPEN64_NORM_BUSY;
  if(!p->_active)return report(p,"cancel",OPEN64_NORM_ORDER);
  p->_canceling=true;
  if(p->_ticket) {
    if(p->_launch_attempted && !(p->_done&(1U<<11))){int status=advance(p,11);if(status)return status;}
    if(p->_publish_attempted && !(p->_done&(1U<<12))){int status=advance(p,12);if(status)return status;}
  }
  int status=quiesce(p);if(status)return status;
  if(p->_ticket) {
    status=open64_normalization_abort(p->_config.runtime,p->_ticket);report(p,"abort",status,p->_ticket);
    if(status)return status;
  }
  return cleanup(p);
}

/* Caller externally synchronizes destruction. Shared registry/service ownership
 * is never taken; active or cleanup-only resources survive failed unload. */
int open64_normalization_execution_destroy(OPEN64_NORMALIZATION_EXECUTION *p)
{
  if(!p)return OPEN64_NORM_INVALID;
  OPEN64_NORMALIZATION_EXECUTION_CALL call(p);
  if(!call.Entered())return OPEN64_NORM_BUSY;
  if(p->_active)return report(p,"destroy",OPEN64_NORM_BUSY);
  int status=open64_normalization_cuda_destroy(p->_adapter);report(p,"destroy",status);
  if(status)return status;
  call.Detach();delete p;return 0;
}
