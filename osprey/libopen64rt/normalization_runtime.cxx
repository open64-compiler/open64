/* Copyright (C) 2026 Open64 Project */

/* LN2-d canonical leases and host-event enforcement. No reduction dependency.
 * Design: doc/AI-COMPILER-LAYERNORM-RUNTIME-GUARDS-v0.1.md.
 * Separate runtime library: no compiler linkage, CUDA calls, or kernel body. */
#include "open64_normalization_runtime.h"
#include <atomic>
#include <mutex>
#include <new>
#include <string.h>
#include <limits.h>

namespace {
const unsigned limit=64;
struct OPEN64_NORMALIZATION_ALLOCATION { uint64_t id, backing; OPEN64_TARGET_BUFFER_VIEW view; };
struct OPEN64_NORMALIZATION_TICKET {
  uint64_t id, roots[4];
  OPEN64_NORMALIZATION_ENTRY entry;
  OPEN64_NORMALIZATION_INVOCATION invocation;
  uint32_t done, leases;
  bool busy, attempted, launch_failed, consumer_started, aborted;
};
std::atomic<uint64_t> next_id(1);

/* Never reuse IDs, even across contexts; exhaustion is a refusal. */
uint64_t mint()
{
  uint64_t id=next_id.load();
  while (id!=UINT64_MAX) if (next_id.compare_exchange_weak(id,id+1)) return id;
  return 0;
}

/* Registration guarantees nonwrapping intervals before comparison. */
bool overlap(const OPEN64_TARGET_BUFFER_VIEW &a,const OPEN64_TARGET_BUFFER_VIEW &b)
{
  uintptr_t x=reinterpret_cast<uintptr_t>(a.address),y=reinterpret_cast<uintptr_t>(b.address);
  return x<y+b.bytes && y<x+a.bytes;
}

/* Check the complete conservative DAG, including redundant/extra edges.
 * Host completion collapses internal nodes only at successful kernel drain;
 * it does not claim that those individual GPU events were observed. */
bool graph(const OPEN64_NORMALIZATION_GUARD &g)
{
  uint32_t reach[15];
  for (unsigned i=0;i<15;++i) {
    if (g.before[i]&~uint64_t(32767)) return false;
    reach[i]=static_cast<uint32_t>(g.before[i]);
  }
  for (unsigned k=0;k<15;++k) for (unsigned i=0;i<15;++i)
    if (reach[i]&(1U<<k)) reach[i]|=reach[k];
  for (unsigned i=0;i<15;++i)
    if ((reach[i]&(1U<<i)) || (i<14 && !(reach[i]&(1U<<(i+1))))) return false;
  return true;
}

/* Validate structure and checked arithmetic again at the trust boundary.
 * Exact manifest agreement supplies entry-specific arithmetic/implementation
 * admission, not a caller Boolean or invented instruction certification. */
bool valid(const OPEN64_NORMALIZATION_GUARD &g,const OPEN64_NORMALIZATION_ENTRY &e)
{
  static_assert(sizeof(OPEN64_NORMALIZATION_GUARD)==83*sizeof(uint64_t),"guard padding/ABI changed");
  if (g.abi_version!=1 || g.size!=sizeof(g) || g.source_version!=2 || !g.pu || !g.node || !g.result ||
      !g.result_type || g.source_flags!=29687 || g.pu>UINT32_MAX || g.node>UINT32_MAX ||
      g.result>UINT32_MAX || g.result_type>UINT32_MAX || g.source_line>UINT32_MAX ||
      !g.rows || !g.width || g.width>(1U<<24) || g.rows>uint64_t(INT64_MAX) ||
      g.rows>UINT64_MAX/4/g.width || g.epsilon_bits>UINT32_MAX ||
      (g.epsilon_bits&0x80000000U) || !(g.epsilon_bits&0x7fffffffU) || g.epsilon_bits>=0x7f800000U ||
      g.architecture<1 || g.architecture>4 || g.route!=1 || g.environment!=1 || g.fp_options ||
      g.kind<1 || g.kind>3 || g.layout!=1 || !g.contract || !g.context || g.ownership!=1 ||
      g.tail!=1 || g.ctas!=g.rows || !g.warp || !g.threads || g.threads%g.warp ||
      g.threads>e.max_threads || g.ctas>e.max_ctas || g.reserved_shared>e.max_shared ||
      g.obligations!=63 || !g.shared_quantum || g.shared_quantum>UINT32_MAX ||
      !g.banks || g.banks>UINT32_MAX || g.warp>UINT32_MAX || g.bank_word!=4 ||
      !e.perform || e.flags>1 || memcmp(&g,&e.manifest,sizeof(g)) || !graph(g)) return false;
  for (unsigned i=0;i<3;++i) if (!g.operands[i] || !g.operand_types[i] ||
      g.operands[i]>UINT32_MAX || g.operand_types[i]>UINT32_MAX) return false;
  if (g.toolkit<(g.architecture<=2 ? 12000U:12080U) || g.ptx<(g.architecture<=2 ? 800U:807U)) return false;
  uint64_t bytes[4]={g.rows*g.width*4,g.width*4,g.width*4,g.rows*g.width*4};
  for (unsigned i=0;i<4;++i) {
    const OPEN64_NORMALIZATION_RANGE &a=g.ranges[i];
    if (!a.role || a.bytes!=bytes[i] || a.lease!=(i==3 ? 2U:1U) || a.alignment<4 ||
        a.alignment>UINT32_MAX || (a.alignment&(a.alignment-1)) || a.offset%4 ||
        a.offset>a.capacity || a.bytes>a.capacity-a.offset) return false;
    for (unsigned j=0;j<i;++j) if (a.role==g.ranges[j].role &&
        (a.capacity!=g.ranges[j].capacity || a.alignment!=g.ranges[j].alignment)) return false;
  }
  if (g.operands[1]==g.operands[2] && (g.ranges[1].role!=g.ranges[2].role ||
      g.ranges[1].offset!=g.ranges[2].offset)) return false;
  if (g.kind==1) return !g.stride && !g.shared && !g.reserved_shared && !g.barriers && !g.participants && !g.bank_conflict;
  uint64_t barriers=688+(g.kind==3 ? 4:0);
  return g.stride>=g.width*4 && g.stride%16==0 && g.stride<=UINT64_MAX-8 &&
      g.shared==g.stride+8 && g.shared<=UINT64_MAX-(g.shared_quantum-1) &&
      g.reserved_shared==((g.shared+g.shared_quantum-1)/g.shared_quantum)*g.shared_quantum &&
      g.reserved_shared<=g.shared_limit && g.participants==g.threads && g.barriers==barriers &&
      g.bank_conflict==((g.width<g.warp ? g.width:g.warp)+g.banks-1)/g.banks;
}
}

struct OPEN64_NORMALIZATION_RUNTIME {
  std::mutex mutex;
  OPEN64_TARGET_BUFFER_SERVICE service;
  OPEN64_NORMALIZATION_ROOT_RESOLVER resolve;
  uint64_t context,seq;
  uint32_t architecture,flags;
  FILE *trace;
  OPEN64_NORMALIZATION_ALLOCATION allocations[limit]; OPEN64_NORMALIZATION_TICKET tickets[limit];
  /* Clear plain state only; never memset the mutex. */
  OPEN64_NORMALIZATION_RUNTIME():resolve(NULL),context(0),seq(0),architecture(0),flags(0),trace(NULL)
  { memset(&service,0,sizeof(service));memset(allocations,0,sizeof(allocations));memset(tickets,0,sizeof(tickets)); }
};

namespace {
/* Serialize concise observations; internal CTA nodes are never marked done. */
int report(OPEN64_NORMALIZATION_RUNTIME *r,uint64_t id,const char *action,int status,const OPEN64_NORMALIZATION_TICKET *t=NULL)
{
  if (r->trace) fprintf(r->trace,"AI_NORM_RUN 1 EVENT %llu %llu %s %d %u %u %u %llu\n",
      (unsigned long long)++r->seq,(unsigned long long)id,action,status,t?t->done:0,t?t->leases:0,
      r->flags,(unsigned long long)(t?t->invocation.guard.source_line:0));
  return status;
}

/* Registry lookups run under the owning context lock only. */
OPEN64_NORMALIZATION_ALLOCATION *allocation(OPEN64_NORMALIZATION_RUNTIME *r,uint64_t id)
{
  for (unsigned i=0;i<limit;++i) if (id && r->allocations[i].id==id) return &r->allocations[i];
  return NULL;
}

/* Never accept a retired, closed or foreign-context ticket. */
OPEN64_NORMALIZATION_TICKET *ticket(OPEN64_NORMALIZATION_RUNTIME *r,uint64_t id)
{
  for (unsigned i=0;i<limit;++i) if (id && r->tickets[i].id==id) return &r->tickets[i];
  return NULL;
}
}

/* Reuse generic synchronous buffer validation, not a domain runtime module. */
OPEN64_NORMALIZATION_RUNTIME *open64_normalization_runtime_create(
    const OPEN64_TARGET_BUFFER_SERVICE *s,OPEN64_NORMALIZATION_ROOT_RESOLVER resolve,
    uint32_t arch,uint64_t context,uint32_t flags,FILE *trace)
{
  if (!s || s->abi_version!=1 || s->target_id!=OPEN64_TARGET_NVIDIA || !s->validate_import ||
      !resolve || arch<1 || arch>4 || !context || flags>1) return NULL;
  OPEN64_NORMALIZATION_RUNTIME *r=new(std::nothrow) OPEN64_NORMALIZATION_RUNTIME;
  if (!r) return NULL;
  r->service=*s;r->resolve=resolve;r->architecture=arch;r->context=context;r->flags=flags;r->trace=trace;
  if (trace) fprintf(trace,"AI_NORM_RUN 1 HEADER seq ticket action status host_done leases host_test source_line\n");
  return r;
}

/* Destruction requires externally synchronized lifetime and no live objects. */
int open64_normalization_runtime_destroy(OPEN64_NORMALIZATION_RUNTIME *r)
{
  if (!r) return OPEN64_NORM_INVALID;
  { std::lock_guard<std::mutex> lock(r->mutex);
    for (unsigned i=0;i<limit;++i) if (r->allocations[i].id || r->tickets[i].id) return OPEN64_NORM_BUSY; }
  delete r;return OPEN64_NORM_OK;
}

/* Resolve WHOLE backing extent, disallow duplicate roots/virtual aliases and
 * overlapping address roots; externally serialize registration/free/rebind. */
int open64_normalization_register(OPEN64_NORMALIZATION_RUNTIME *r,const OPEN64_TARGET_BUFFER_VIEW *view,uint64_t *out)
{
  if (!r || !view || !out) return OPEN64_NORM_INVALID;
  OPEN64_TARGET_BUFFER_VIEW v=*view;uint64_t backing=0;
  bool bad=!v.address || !v.bytes || v.tier!=OPEN64_TARGET_BUFFER_DEVICE || v.device_id!=r->service.device_id ||
      v.bytes>UINTPTR_MAX-reinterpret_cast<uintptr_t>(v.address) ||
      r->service.validate_import(r->service.context,&v,v.bytes,4) || r->resolve(r->service.context,&v,&backing) || !backing;
  std::lock_guard<std::mutex> lock(r->mutex);
  if (bad) return report(r,0,"register",OPEN64_NORM_INVALID);
  OPEN64_NORMALIZATION_ALLOCATION *slot=NULL;
  for (unsigned i=0;i<limit;++i) {
    if (!r->allocations[i].id) slot=&r->allocations[i];
    else if (r->allocations[i].backing==backing || overlap(v,r->allocations[i].view))
      return report(r,0,"register",OPEN64_NORM_CONFLICT);
  }
  if (!slot) return report(r,0,"register",OPEN64_NORM_CAPACITY);
  uint64_t id=mint();if (!id) return report(r,0,"register",OPEN64_NORM_CAPACITY);
  slot->id=id;slot->backing=backing;slot->view=v;*out=id;
  return report(r,0,"register",OPEN64_NORM_OK);
}

/* No cooperative free/rebind may outlive any leased view of this root. */
int open64_normalization_retire(OPEN64_NORMALIZATION_RUNTIME *r,uint64_t id)
{
  if (!r) return OPEN64_NORM_INVALID;
  std::lock_guard<std::mutex> lock(r->mutex);OPEN64_NORMALIZATION_ALLOCATION *a=allocation(r,id);
  if (!a) return report(r,0,"retire",OPEN64_NORM_INVALID);
  for (unsigned i=0;i<limit;++i) for (unsigned j=0;j<4;++j)
    if (r->tickets[i].id && (r->tickets[i].leases&(1U<<j)) && r->tickets[i].roots[j]==id)
      return report(r,r->tickets[i].id,"retire",OPEN64_NORM_BUSY,&r->tickets[i]);
  a->id=0;return report(r,0,"retire",OPEN64_NORM_OK);
}

/* Validate immutable snapshots before atomically acquiring every range lease.
 * Canonical-role equality must match root equality in BOTH directions. */
int open64_normalization_admit(OPEN64_NORMALIZATION_RUNTIME *r,const OPEN64_NORMALIZATION_GUARD *guard,
    const OPEN64_NORMALIZATION_ENTRY *entry,const uint64_t roots[4],uint64_t *out)
{
  if (!r || !guard || !entry || !roots || !out) return OPEN64_NORM_INVALID;
  OPEN64_NORMALIZATION_TICKET t={};t.invocation.guard=*guard;t.entry=*entry;memcpy(t.roots,roots,sizeof(t.roots));
  const OPEN64_NORMALIZATION_GUARD &g=t.invocation.guard;
  bool good=valid(g,t.entry) && g.context==r->context && g.architecture==r->architecture && t.entry.flags==r->flags;
  std::lock_guard<std::mutex> lock(r->mutex);
  if (!good) return report(r,0,"admit",OPEN64_NORM_INVALID,&t);
  for (unsigned i=0;i<4;++i) {
    OPEN64_NORMALIZATION_ALLOCATION *a=allocation(r,t.roots[i]);const OPEN64_NORMALIZATION_RANGE &v=g.ranges[i];
    if (!a || a->view.bytes!=v.capacity || reinterpret_cast<uintptr_t>(a->view.address)%v.alignment)
      return report(r,0,"admit",OPEN64_NORM_INVALID,&t);
    t.invocation.views[i]=a->view;
    t.invocation.views[i].address=reinterpret_cast<void *>(reinterpret_cast<uintptr_t>(a->view.address)+v.offset);
    t.invocation.views[i].bytes=static_cast<size_t>(v.bytes);
    for (unsigned j=0;j<i;++j) if ((v.role==g.ranges[j].role)!=(t.roots[i]==t.roots[j]))
      return report(r,0,"admit",OPEN64_NORM_INVALID,&t);
    if (i==3) for (unsigned j=0;j<3;++j) if (overlap(t.invocation.views[i],t.invocation.views[j]))
      return report(r,0,"admit",OPEN64_NORM_INVALID,&t);
  }
  OPEN64_NORMALIZATION_TICKET *slot=NULL;
  for (unsigned n=0;n<limit;++n) {
    const OPEN64_NORMALIZATION_TICKET &other=r->tickets[n];if (!other.id) {slot=&r->tickets[n];continue;}
    for (unsigned i=0;i<4;++i) for (unsigned j=0;j<4;++j)
      if ((other.leases&(1U<<j)) && (i==3 || j==3) && overlap(t.invocation.views[i],other.invocation.views[j]))
        return report(r,0,"admit",OPEN64_NORM_CONFLICT,&t);
  }
  if (!slot || !(t.id=mint())) return report(r,0,"admit",OPEN64_NORM_CAPACITY,&t);
  t.leases=15;t.invocation.ticket=t.id;*slot=t;*out=t.id;
  if (r->trace) {
    fprintf(r->trace,"AI_NORM_RUN 1 BIND %llu 83",(unsigned long long)t.id);
    for (unsigned i=0;i<sizeof(g)/sizeof(uint64_t);++i) {
      uint64_t word;memcpy(&word,reinterpret_cast<const char *>(&g)+i*sizeof(word),sizeof(word));
      fprintf(r->trace," %llu",(unsigned long long)word);
    }
    fprintf(r->trace," %llu\n",(unsigned long long)g.source_line);
    fprintf(r->trace,"AI_NORM_RUN 1 LIMIT %llu %llu %llu %llu %u %llu\n",(unsigned long long)t.id,
        (unsigned long long)t.entry.max_ctas,(unsigned long long)t.entry.max_threads,
        (unsigned long long)t.entry.max_shared,t.entry.flags,(unsigned long long)g.source_line);
    for (unsigned i=0;i<4;++i) {
      const OPEN64_NORMALIZATION_RANGE &v=g.ranges[i];
      fprintf(r->trace,"AI_NORM_RUN 1 RANGE %llu %u %llu %llu %llu %llu %llu %llu %llu %llu %llu\n",
          (unsigned long long)t.id,i,(unsigned long long)v.role,(unsigned long long)t.roots[i],
          (unsigned long long)allocation(r,t.roots[i])->backing,(unsigned long long)v.capacity,
          (unsigned long long)v.offset,(unsigned long long)v.alignment,(unsigned long long)v.bytes,
          (unsigned long long)v.lease,(unsigned long long)g.source_line);
    }
  }
  return report(r,t.id,"admit",OPEN64_NORM_OK,slot);
}

/* Gate phase BEGIN before calling work; success records completion afterward.
 * Internal nodes cannot be fabricated through host advance calls. Failed launch
 * may have queued work: allow only drain+abort, never publish or early release. */
int open64_normalization_advance(OPEN64_NORMALIZATION_RUNTIME *r,uint64_t id,uint32_t event)
{
  if (!r || (event>1 && (event<11 || event>14))) return OPEN64_NORM_INVALID;
  static const char *names[15]={"ready","launch",NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
      "drain","publish","input_release","output_release"};
  std::unique_lock<std::mutex> lock(r->mutex);OPEN64_NORMALIZATION_TICKET *t=ticket(r,id);
  if (!t) return report(r,id,names[event],OPEN64_NORM_INVALID);
  if (t->busy) return report(r,id,names[event],OPEN64_NORM_BUSY,t);
  if (t->aborted || (t->launch_failed && event!=11)) return report(r,id,names[event],OPEN64_NORM_FAILED,t);
  if (t->done&(1U<<event)) return report(r,id,names[event],OPEN64_NORM_OK,t);
  uint32_t predecessor=event==0 ? 0:event==1 ? 1:event==11 ? 2:1U<<(event-1);
  if (event==11 && t->launch_failed) predecessor=0; /* drain ambiguity only */
  if ((t->done&predecessor)!=predecessor) return report(r,id,names[event],OPEN64_NORM_ORDER,t);
  if (event>=13) {
    t->done|=1U<<event;t->leases&=event==13 ? 8:7;
    return report(r,id,names[event],OPEN64_NORM_OK,t);
  }
  t->busy=true;if (event==1) t->attempted=true;if (event==12) t->consumer_started=true;
  OPEN64_NORMALIZATION_ENTRY adapter=t->entry;OPEN64_NORMALIZATION_INVOCATION invocation=t->invocation;
  const char *begin=event==0 ? "begin_ready":event==1 ? "begin_launch":event==11 ? "begin_drain":"begin_publish";
  report(r,id,begin,OPEN64_NORM_OK,t);
  lock.unlock();int status=adapter.perform(adapter.context,event,&invocation);lock.lock();
  t->busy=false;if (event==1 && status) t->launch_failed=true;
  if (!status) t->done|=1U<<event;
  return report(r,id,names[event],status ? OPEN64_NORM_BACKEND:OPEN64_NORM_OK,t);
}

/* Cancellation pins storage through ambiguous launch and consumer failures. */
int open64_normalization_abort(OPEN64_NORMALIZATION_RUNTIME *r,uint64_t id)
{
  if (!r) return OPEN64_NORM_INVALID;
  std::lock_guard<std::mutex> lock(r->mutex);OPEN64_NORMALIZATION_TICKET *t=ticket(r,id);
  if (!t) return report(r,id,"abort",OPEN64_NORM_INVALID);
  if (t->busy || (t->attempted && !(t->done&(1U<<11))) || (t->consumer_started && !(t->done&(1U<<12))))
    return report(r,id,"abort",OPEN64_NORM_BUSY,t);
  t->aborted=true;t->leases=0;return report(r,id,"abort",OPEN64_NORM_OK,t);
}

/* Only released/aborted tickets may disappear; stale IDs stay invalid. */
int open64_normalization_close(OPEN64_NORMALIZATION_RUNTIME *r,uint64_t id)
{
  if (!r) return OPEN64_NORM_INVALID;
  std::lock_guard<std::mutex> lock(r->mutex);OPEN64_NORMALIZATION_TICKET *t=ticket(r,id);
  if (!t) return report(r,id,"close",OPEN64_NORM_INVALID);
  if (t->busy || t->leases) return report(r,id,"close",OPEN64_NORM_BUSY,t);
  report(r,id,"close",OPEN64_NORM_OK,t);t->id=0;return OPEN64_NORM_OK;
}
