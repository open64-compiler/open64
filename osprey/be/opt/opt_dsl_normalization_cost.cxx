/* Copyright (C) 2026 Open64 Project */

/* LN3 active-PU legality, literal analytical schedule and gated selection.
 * Reuses target records and VHO-owned transient plan/evolution services; no reduction dependency, mapped mutation or kernel.
 * Design: doc/AI-COMPILER-LAYERNORM-PROFITABILITY-v0.1.md. */
#include <algorithm>
#include <queue>
#include <vector>
#include <string.h>
#include "opt_dsl_normalization_cost.h"
#include "pu_info.h"
#include "config_dsl.h"
#include "../vho/dsl_gemm_occupancy.h"

namespace {
typedef unsigned long long LN3_U64;
const UINT64 ln3_max=~UINT64(0);
enum LN3_JOB_KIND { LN3_CONTROL, LN3_GLOBAL, LN3_SHARED, LN3_SCALAR, LN3_BARRIER, LN3_JOIN };
struct LN3_JOB {
  UINT32 kind, sm, field, active;
  UINT64 bytes;
  std::vector<UINT32> predecessors;
};
struct LN3_EVENT { UINT64 time, delta; UINT32 parent, term; };
struct LN3_READY {
  UINT64 time; UINT32 job;
  /* Stable earliest-start ordering never relies on insertion order. */
  bool operator<(const LN3_READY &r) const
  {
    if (time!=r.time) return time>r.time;
    return job>r.job;
  }
};

/* Checked arithmetic is mandatory even for explicitly synthetic profiles. */
static BOOL LN3_Add(UINT64 a,UINT64 b,UINT64 *out)
{ if (a>ln3_max-b) return FALSE;*out=a+b;return TRUE; }

/* Avoid multiplication wrap in bytes, reservations, ledgers and time. */
static BOOL LN3_Mul(UINT64 a,UINT64 b,UINT64 *out)
{ if (a && b>ln3_max/a) return FALSE;*out=a*b;return TRUE; }

/* Ceiling division without the usual numerator+denominator overflow. */
static UINT64 LN3_Ceil(UINT64 n,UINT64 d) { return n/d+(n%d!=0); }

/* Bind every field and provenance to the exact architecture/revision. Unknown
 * zero payloads are incomplete, not zero-cost. Known zero durations are legal. */
static UINT32 LN3_Profile(const DSL_SCALAR_EXECUTION_PROFILE &p,const DSL_HARDWARE_DESCRIPTION_RECORD &hw)
{
  if (p.version!=1 || p.arch_id!=hw.arch_id || p.description_revision!=hw.revision || !p.model_id)
    return WOPT_DSL_NORM_COST_PROFILE_INVALID;
  BOOL unknown=FALSE;
  for (UINT32 f=0;f<DSL_SCALAR_FIELD_COUNT;++f) {
    if (p.provenance[f]>3 || p.values[f][0]>p.values[f][1] || p.values[f][1]>p.values[f][2])
      return WOPT_DSL_NORM_COST_PROFILE_INVALID;
    if (!p.provenance[f]) {
      if (p.values[f][2]) return WOPT_DSL_NORM_COST_PROFILE_INVALID;
      unknown=TRUE;
    } else if ((f<=DSL_SCALAR_COMPUTE_RATE || f==DSL_SCALAR_SECTOR_BYTES) && !p.values[f][0])
      return WOPT_DSL_NORM_COST_PROFILE_INVALID;
  }
  if (p.provenance[DSL_SCALAR_SECTOR_BYTES]) {
    UINT64 s=p.values[DSL_SCALAR_SECTOR_BYTES][0];
    if (s<4 || s>256 || (s&(s-1)) || s!=p.values[DSL_SCALAR_SECTOR_BYTES][2])
      return WOPT_DSL_NORM_COST_PROFILE_INVALID;
  }
  return unknown ? WOPT_DSL_NORM_COST_UNKNOWN:WOPT_DSL_NORM_COST_COMPLETE;
}

/* Evaluate declared upper RF/shared reservations. Imported one-CTA gates and
 * rounded per-SM bounds replace synthetic capacities; unknown required facts
 * stay distinguishable from a known zero residency. This remains a scenario
 * occupancy estimate, not generated-register/spill or native allocation proof. */
static UINT32 LN3_Residents(const DSL_HARDWARE_DESCRIPTION_RECORD &hw,
    const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &r,const WOPT_DSL_NORMALIZATION_PLAN_BINDING &b,UINT32 registers,
    const DSL_GPU_PLAN_SNAPSHOT &gpu,BOOL *known)
{
  *known = registers != 0;
  if (gpu.Bound()) {
    uint64_t blocks, threads, cta_threads, lanes, rf, quantum, shared = 0, shared_quantum = 0, cta_shared = 0;
    if (!registers || !gpu.Resource_limit(DSL_GPU_MAX_BLOCKS_PER_SM, &blocks) ||
        !gpu.Resource_limit(DSL_GPU_MAX_THREADS_PER_SM, &threads) ||
        !gpu.Resource_limit(DSL_GPU_MAX_THREADS_PER_CTA, &cta_threads) ||
        !gpu.Resource_limit(DSL_GPU_WARP_LANES, &lanes) ||
        !gpu.Resource_limit(DSL_GPU_REGISTERS_PER_SM, &rf) ||
        !gpu.Resource_limit(DSL_GPU_REGISTER_QUANTUM, &quantum) || !quantum ||
        (b.reserved_shared && (!gpu.Resource_limit(DSL_GPU_SHARED_BYTES_PER_SM, &shared) ||
         !gpu.Resource_limit(DSL_GPU_SHARED_BYTES_PER_CTA_DEFAULT, &cta_shared) ||
         !gpu.Resource_limit(DSL_GPU_SHARED_QUANTUM, &shared_quantum) || !shared_quantum))) {
      *known = FALSE;
      return 0;
    }
    if (!gpu.Fits_CTA(r.threads_per_cta, registers, b.reserved_shared) ||
        registers > hw.resources.max_registers_per_thread) return 0;
    uint64_t rounded;
    if (!gpu.Register_reservation(r.threads_per_cta,registers,&rounded) || !rounded) return 0;
    UINT64 resident = std::min<UINT64>(blocks, threads / r.threads_per_cta);
    resident = std::min<UINT64>(resident, rf / rounded);
    if (b.reserved_shared) {
      UINT64 reserved;
      if (!LN3_Add(b.reserved_shared, b.reserved_shared % shared_quantum ?
          shared_quantum - b.reserved_shared % shared_quantum : 0, &reserved) || !reserved) return 0;
      resident = std::min<UINT64>(resident, shared / reserved);
    }
    return resident > ~UINT32(0) ? ~UINT32(0) : UINT32(resident);
  }
  DSL_GEMM_OCCUPANCY_RESULT allocation;
  if (!registers || !DSL_gemm_occupancy(hw.resources,b.warp_lanes,hw.performance.sms,r.threads_per_cta,
      uint64_t(registers)*4,b.reserved_shared,r.launched_ctas,&allocation)) return 0;
  return allocation.resident>~UINT32(0)?0:UINT32(allocation.resident);
}

struct LN3_GRAPH {
  const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &r;
  const WOPT_DSL_NORMALIZATION_PLAN_BINDING &b;
  WOPT_DSL_NORMALIZATION_COST &cost;
  UINT64 budget, sector;
  UINT32 sm;
  BOOL good;
  std::vector<LN3_JOB> jobs;
  /* The graph is a transient phase-owned realization recipe, not executable IR. */
  LN3_GRAPH(const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &request,
      const WOPT_DSL_NORMALIZATION_PLAN_BINDING &binding,WOPT_DSL_NORMALIZATION_COST &output,
      UINT64 max_jobs,UINT64 sector_bytes):r(request),b(binding),cost(output),budget(max_jobs),sector(sector_bytes),sm(0),good(TRUE) {}

  /* Append only within budget; all edges refer backward to already built jobs. */
  UINT32 Append(UINT32 kind,UINT32 field,UINT32 active,UINT64 bytes,const std::vector<UINT32> &preds)
  {
    if (!good || jobs.size()>=budget || jobs.size()>=~UINT32(0)) {good=FALSE;return 0;}
    LN3_JOB j;j.kind=kind;j.sm=sm;j.field=field;j.active=active;j.bytes=bytes;j.predecessors=preds;
    jobs.push_back(j);return UINT32(jobs.size()-1);
  }

  /* Unary dependency helper retains the original ordered arithmetic chain. */
  UINT32 One(UINT32 kind,UINT32 field,UINT32 active,UINT64 bytes,UINT32 pred)
  {
    const std::vector<UINT32> predecessors(1,pred);
    return Append(kind,field,active,bytes,predecessors);
  }

  /* Charge control/index/predicate work explicitly, including masked tails. */
  UINT32 Control(UINT32 pred)
  { ++cost.control_instructions;return One(LN3_CONTROL,DSL_SCALAR_CONTROL_CYCLES,1,0,pred); }

  /* Worst sector projection over bases consistent with the declared alignment.
   * This is issued-sector traffic under a stated no-cache-hit model, not a
   * claim of measured HBM bytes. Lane-local carrier scratch is sector-aligned. */
  UINT64 Sectors(UINT64 offset,UINT64 bytes,UINT64 alignment)
  {
    UINT64 worst=0,step=std::min(sector,alignment);
    for (UINT64 base=0;base<sector;base+=step) {
      UINT64 first=(base+offset%sector)/sector;
      UINT64 last=(base+offset%sector+bytes-1)/sector;
      worst=std::max(worst,(last-first+1)*sector);
    }
    return worst;
  }

  /* All-miss carrier and external GMEM use the SAME device bandwidth engine;
   * shared accesses use the common bank-bound conflict factor. */
  UINT32 Memory(UINT32 pred,BOOL shared,BOOL carrier,UINT64 offset,UINT32 active,UINT32 operand=0)
  {
    if (!good) return 0;
    UINT64 logical=UINT64(active)*4,bytes=shared ? logical:Sectors(offset,logical,carrier?sector:r.ranges[operand].base_alignment);
    if (shared) good=LN3_Add(cost.shared_bytes,bytes,&cost.shared_bytes);
    else if (carrier) good=LN3_Add(cost.carrier_sector_bytes,bytes,&cost.carrier_sector_bytes);
    else good=LN3_Add(cost.global_logical_bytes,logical,&cost.global_logical_bytes) &&
        LN3_Add(cost.global_sector_bytes,bytes,&cost.global_sector_bytes);
    pred=Control(pred);
    return One(shared?LN3_SHARED:LN3_GLOBAL,shared?DSL_SCALAR_SHARED_LATENCY:
        carrier?DSL_SCALAR_CARRIER_LATENCY:DSL_SCALAR_GLOBAL_LATENCY,active,bytes,pred);
  }

  /* Pointer-scalar packaging: binary has 2 input stores+2 ASM loads+result
   * store+unpack load; unary has 1+1+1+1. No carrier traffic disappears for free. */
  UINT32 Scalar(UINT32 pred,UINT32 field,UINT32 active)
  {
    BOOL unary=field==DSL_SCALAR_SQRT_LATENCY;
    for (UINT32 i=0;i<(unary?2U:4U);++i) pred=Memory(pred,FALSE,TRUE,0,active);
    cost.scalar_lanes+=active;++cost.scalar_instructions;
    pred=One(LN3_SCALAR,field,active,0,pred);
    for (UINT32 i=0;i<2;++i) pred=Memory(pred,FALSE,TRUE,0,active);
    return pred;
  }

  /* One ordered owner computes both folds; no tree/reassociation substitution. */
  UINT32 Statistics(UINT32 seed,UINT64 row,BOOL variance)
  {
    const UINT64 n=b.target.source.width;
    for (UINT64 k=0;k<n && good;++k) {
      seed=Control(seed);
      BOOL shared=variance ? r.kind!=1:r.kind==3;
      seed=Memory(seed,shared,FALSE,shared?k*4:r.ranges[0].offset_bytes+(row*n+k)*4,1);
      if (variance) {
        if (r.kind==1) seed=Scalar(seed,DSL_SCALAR_ADD_LATENCY,1); /* d=sub32(X,mu) */
        seed=Scalar(seed,DSL_SCALAR_MUL_LATENCY,1);
      }
      seed=Scalar(seed,DSL_SCALAR_ADD_LATENCY,1);
    }
    seed=Scalar(seed,DSL_SCALAR_DIV_LATENCY,1);
    if (variance) {
      seed=Scalar(seed,DSL_SCALAR_ADD_LATENCY,1);seed=Scalar(seed,DSL_SCALAR_SQRT_LATENCY,1);
      seed=Scalar(seed,DSL_SCALAR_DIV_LATENCY,1);
    }
    return seed;
  }

  /* Cooperative warp actors cover every valid element once. Inactive groups
   * execute predicate control but no invalid accesses/arithmetic; all join. */
  UINT32 Cooperative(UINT32 seed,UINT64 row,UINT32 mode)
  {
    UINT64 n=b.target.source.width;UINT32 warps=r.threads_per_cta/b.warp_lanes;
    std::vector<UINT32> tails;
    for (UINT32 w=0;w<warps && good;++w) {
      UINT32 next=seed;UINT64 first=UINT64(w)*b.warp_lanes;
      if (mode && first<n) next=Memory(next,TRUE,FALSE,r.row_stride_bytes+(mode==2?4:0),1);
      for (UINT64 base=0;base<n && good;base+=r.threads_per_cta) {
        next=Control(next);UINT64 k=base+first;
        if (k>=n) continue;
        UINT32 active=UINT32(std::min(UINT64(b.warp_lanes),n-k));
        BOOL shared=mode==2 || (mode==1 && r.kind==3);
        next=Memory(next,shared,FALSE,shared?k*4:r.ranges[0].offset_bytes+(row*n+k)*4,active);
        if (mode==1) next=Scalar(next,DSL_SCALAR_ADD_LATENCY,active); /* centered subtraction */
        if (mode==2) {
          next=Scalar(next,DSL_SCALAR_MUL_LATENCY,active);
          next=Memory(next,FALSE,FALSE,r.ranges[1].offset_bytes+k*4,active,1);
          next=Scalar(next,DSL_SCALAR_MUL_LATENCY,active);
          next=Memory(next,FALSE,FALSE,r.ranges[2].offset_bytes+k*4,active,2);
          next=Scalar(next,DSL_SCALAR_ADD_LATENCY,active);
          next=Memory(next,FALSE,FALSE,r.ranges[3].offset_bytes+(row*n+k)*4,active,3);
        } else next=Memory(next,TRUE,FALSE,k*4,active);
      }
      tails.push_back(next);
    }
    ++cost.barriers;return Append(LN3_BARRIER,DSL_SCALAR_BARRIER_CYCLES,0,0,tails);
  }

  /* Fixed row-to-SM/slot assignment accounts for underfill, shared/RF limits
   * and later waves. Slot reuse depends on the prior CTA's complete readers. */
  BOOL Build(UINT32 sms)
  {
    LN3_JOB root;root.kind=LN3_JOIN;root.sm=0;root.field=0;root.active=0;root.bytes=0;jobs.push_back(root);
    UINT64 rows=b.target.source.rows,n=b.target.source.width;
    if (!budget || rows>budget || n>budget/rows) {good=FALSE;return FALSE;}
    UINT64 slots=std::min(rows,UINT64(sms)*cost.resident_ctas);
    std::vector<UINT32> previous(size_t(slots),0),outputs;
    for (UINT64 row=0;row<rows && good;++row) {
      sm=UINT32((row%slots)%sms);UINT32 seed=previous[size_t(row%slots)];
      if (r.kind==3) seed=Cooperative(seed,row,0);
      seed=Statistics(seed,row,FALSE);
      if (r.kind!=1) {
        seed=Memory(seed,TRUE,FALSE,r.row_stride_bytes,1);
        ++cost.barriers;seed=One(LN3_BARRIER,DSL_SCALAR_BARRIER_CYCLES,0,0,seed);
        seed=Cooperative(seed,row,1);
      }
      seed=Statistics(seed,row,TRUE);
      if (r.kind==1) {
        for (UINT64 k=0;k<n && good;++k) {
          seed=Control(seed);seed=Memory(seed,FALSE,FALSE,r.ranges[0].offset_bytes+(row*n+k)*4,1);
          seed=Scalar(seed,DSL_SCALAR_ADD_LATENCY,1);seed=Scalar(seed,DSL_SCALAR_MUL_LATENCY,1);
          seed=Memory(seed,FALSE,FALSE,r.ranges[1].offset_bytes+k*4,1,1);
          seed=Scalar(seed,DSL_SCALAR_MUL_LATENCY,1);seed=Memory(seed,FALSE,FALSE,r.ranges[2].offset_bytes+k*4,1,2);
          seed=Scalar(seed,DSL_SCALAR_ADD_LATENCY,1);seed=Memory(seed,FALSE,FALSE,r.ranges[3].offset_bytes+(row*n+k)*4,1,3);
        }
      } else {
        seed=Memory(seed,TRUE,FALSE,r.row_stride_bytes+4,1);
        ++cost.barriers;seed=One(LN3_BARRIER,DSL_SCALAR_BARRIER_CYCLES,0,0,seed);
        seed=Cooperative(seed,row,2);
      }
      previous[size_t(row%slots)]=seed;outputs.push_back(seed);
    }
    Append(LN3_JOIN,0,0,0,outputs);cost.jobs=jobs.size();return good;
  }
};

/* Resolve one job's engine service and completion duration at one envelope
 * corner. Rates reverse; scalar completion includes, not adds, issue cycles. */
static BOOL LN3_Duration(const LN3_JOB &j,const DSL_SCALAR_EXECUTION_PROFILE &p,UINT32 corner,
    UINT32 conflict,UINT64 *service,UINT64 *duration)
{
  if (j.kind==LN3_JOIN) {*service=*duration=0;return TRUE;}
  UINT32 rate=j.kind==LN3_GLOBAL?DSL_SCALAR_GLOBAL_RATE:j.kind==LN3_SHARED?DSL_SCALAR_SHARED_RATE:DSL_SCALAR_COMPUTE_RATE;
  UINT64 latency=p.values[j.field][corner];
  if (j.kind==LN3_GLOBAL || j.kind==LN3_SHARED) {
    *service=LN3_Ceil(j.bytes,p.values[rate][2-corner]);
    if (j.kind==LN3_SHARED && !LN3_Mul(*service,conflict,service)) return FALSE;
    return LN3_Add(*service,latency,duration);
  }
  if (j.kind==LN3_SCALAR) {
    *service=LN3_Ceil(j.active,p.values[rate][2-corner]);
    if (j.field==DSL_SCALAR_DIV_LATENCY || j.field==DSL_SCALAR_SQRT_LATENCY)
      *service=std::max<UINT64>(*service,p.values[j.field==DSL_SCALAR_DIV_LATENCY?DSL_SCALAR_DIV_ISSUE_CYCLES:DSL_SCALAR_SQRT_ISSUE_CYCLES][corner]);
    *duration=std::max(*service,latency);
  }
  else *service=*duration=latency;
  return TRUE;
}

/* Emit one critical-path node with exclusive attribution. Service completion
 * and operand readiness are separate, so bandwidth can overlap startup waits. */
static UINT32 LN3_Time(std::vector<LN3_EVENT> &events,UINT32 parent,UINT64 delta,UINT32 term,BOOL *good)
{
  LN3_EVENT e;e.parent=parent;e.delta=delta;e.term=term;
  if (!LN3_Add(events[parent].time,delta,&e.time)) {*good=FALSE;return 0;}
  events.push_back(e);return UINT32(events.size()-1);
}

/* Exact earliest-start list scheduler for this bounded declared graph. Each
 * memory job reserves byte service only, not its full delivery latency.
 * Lower/upper bounds are independent optimistic dependency/serial schedules. */
static BOOL LN3_Evaluate(const LN3_GRAPH &g,const DSL_SCALAR_EXECUTION_PROFILE &p,UINT32 sms,
    WOPT_DSL_NORMALIZATION_COST *out)
{
  const std::vector<LN3_JOB> &jobs=g.jobs;size_t count=jobs.size();BOOL good=TRUE;
  std::vector<UINT64> lower(count,0),service(count,0),duration(count,0);
  std::vector<UINT64> lower_engines(size_t(sms)*3+1,0);
  UINT64 lower_setup=0,upper_setup=0,setup=0;
  const UINT32 overhead[]={DSL_SCALAR_GUARD_CYCLES,DSL_SCALAR_LAUNCH_CYCLES,DSL_SCALAR_WORKSPACE_CYCLES};
  for (UINT32 i=0;i<3;++i) if (!LN3_Add(lower_setup,p.values[overhead[i]][0],&lower_setup) ||
      !LN3_Add(upper_setup,p.values[overhead[i]][2],&upper_setup) || !LN3_Add(setup,p.values[overhead[i]][1],&setup)) return FALSE;
  lower[0]=lower_setup;UINT64 upper=upper_setup;
  for (size_t i=1;i<count;++i) {
    UINT64 fast_service,fast_duration,slow_service,slow_duration;
    if (!LN3_Duration(jobs[i],p,0,g.b.bank_conflict_max,&fast_service,&fast_duration) ||
        !LN3_Duration(jobs[i],p,1,g.b.bank_conflict_max,&service[i],&duration[i]) ||
        !LN3_Duration(jobs[i],p,2,g.b.bank_conflict_max,&slow_service,&slow_duration)) return FALSE;
    UINT64 ready=0;
    for (size_t k=0;k<jobs[i].predecessors.size();++k) ready=std::max(ready,lower[jobs[i].predecessors[k]]);
    if (!LN3_Add(ready,fast_duration,&lower[i]) || !LN3_Add(upper,slow_duration,&upper)) return FALSE;
    if (jobs[i].kind!=LN3_JOIN) {
      UINT32 engine=jobs[i].kind==LN3_GLOBAL?0:1+jobs[i].sm*3+(jobs[i].kind==LN3_SHARED?1:jobs[i].kind==LN3_BARRIER?2:0);
      if (!LN3_Add(lower_engines[engine],fast_service,&lower_engines[engine])) return FALSE;
    }
  }
  UINT64 engine_bound;
  if (!LN3_Add(lower_setup,*std::max_element(lower_engines.begin(),lower_engines.end()),&engine_bound)) return FALSE;
  if (!LN3_Add(std::max(lower.back(),engine_bound),p.values[DSL_SCALAR_PUBLICATION_CYCLES][0],&out->lower) ||
      !LN3_Add(upper,p.values[DSL_SCALAR_PUBLICATION_CYCLES][2],&out->upper)) return FALSE;
  std::vector<std::vector<UINT32> > successors(count);
  std::vector<UINT32> pending(count,0),ready_parent(count,0),finish(count,0),engines(size_t(sms)*3+1,0);
  std::vector<LN3_EVENT> events(1);events[0].time=0;events[0].delta=0;events[0].parent=0;events[0].term=0;
  UINT32 root=LN3_Time(events,0,p.values[DSL_SCALAR_GUARD_CYCLES][1],DSL_OPT_COST_RUNTIME_SELECTION,&good);
  root=LN3_Time(events,root,p.values[DSL_SCALAR_LAUNCH_CYCLES][1],DSL_OPT_COST_LAUNCH,&good);
  root=LN3_Time(events,root,p.values[DSL_SCALAR_WORKSPACE_CYCLES][1],DSL_OPT_COST_LAUNCH,&good);finish[0]=root;
  for (size_t i=1;i<count;++i) {
    pending[i]=jobs[i].predecessors.size();
    for (size_t k=0;k<jobs[i].predecessors.size();++k) successors[jobs[i].predecessors[k]].push_back(UINT32(i));
  }
  std::priority_queue<LN3_READY> queue;
  for (size_t k=0;k<successors[0].size();++k) {
    UINT32 id=successors[0][k];ready_parent[id]=root;
    if (!--pending[id]) {LN3_READY q;q.time=setup;q.job=id;queue.push(q);}
  }
  size_t processed=1;
  while (!queue.empty() && good) {
    LN3_READY q=queue.top();queue.pop();UINT32 id=q.job;const LN3_JOB &job=jobs[id];
    UINT32 engine=job.kind==LN3_GLOBAL ? 0:1+job.sm*3+(job.kind==LN3_SHARED?1:job.kind==LN3_BARRIER?2:0);
    UINT32 parent=ready_parent[id];
    if (job.kind!=LN3_JOIN && events[engines[engine]].time>events[parent].time) parent=engines[engine];
    UINT64 start=events[parent].time;
    if (start>q.time) {q.time=start;queue.push(q);continue;}
    UINT32 term=(job.kind==LN3_GLOBAL || job.kind==LN3_SHARED)?DSL_OPT_COST_MEMORY_UNHIDDEN:
        job.kind==LN3_SCALAR?DSL_OPT_COST_COMPUTE:DSL_OPT_COST_SYNC;
    UINT32 service_end=LN3_Time(events,parent,service[id],term,&good);
    finish[id]=LN3_Time(events,service_end,duration[id]-service[id],term,&good);
    if (job.kind!=LN3_JOIN) engines[engine]=service_end;
    ++processed;
    for (size_t k=0;k<successors[id].size();++k) {
      UINT32 next=successors[id][k];
      if (events[finish[id]].time>events[ready_parent[next]].time) ready_parent[next]=finish[id];
      if (!--pending[next]) {LN3_READY n;n.job=next;n.time=events[ready_parent[next]].time;queue.push(n);}
    }
  }
  if (!good || processed!=count) return FALSE;
  UINT32 last=LN3_Time(events,finish.back(),p.values[DSL_SCALAR_PUBLICATION_CYCLES][1],DSL_OPT_COST_SYNC,&good);
  out->total=events[last].time;
  while (last) {
    const LN3_EVENT &e=events[last];
    if (!LN3_Add(out->terms[e.term],e.delta,&out->terms[e.term])) return FALSE;
    last=e.parent;
  }
  return good && out->lower<=out->total && out->total<=out->upper;
}

/* Cross-plan equality covers invocation/target/layout/owners and all backing
 * bindings; only kind-specific SMEM/barrier planning may differ. */
static BOOL LN3_Comparable(const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &a,
    const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &b)
{
  if (!a.target.architecture || !b.target.architecture || strcmp(a.target.architecture,b.target.architecture) ||
      a.target.route!=b.target.route || a.target.toolkit!=b.target.toolkit || a.target.ptx!=b.target.ptx ||
      a.target.environment!=b.target.environment || a.target.fp_options!=b.target.fp_options ||
      a.layout!=b.layout || a.runtime_contract_id!=b.runtime_contract_id || a.context_id!=b.context_id ||
      a.ownership!=b.ownership || a.threads_per_cta!=b.threads_per_cta || a.tail_policy!=b.tail_policy || a.launched_ctas!=b.launched_ctas) return FALSE;
  for (UINT32 i=0;i<4;++i) {
    const WOPT_DSL_NORMALIZATION_RANGE &x=a.ranges[i],&y=b.ranges[i];
    if (x.allocation_id!=y.allocation_id || x.allocation_bytes!=y.allocation_bytes || x.offset_bytes!=y.offset_bytes ||
        x.base_alignment!=y.base_alignment || x.lease!=y.lease) return FALSE;
  }
  return TRUE;
}

/* Store verified analytical plans through AIO-1/AIO-2 instead of a second
 * candidate registry. Complete legal C0 is the mandatory fallback anchor. */
static BOOL LN3_Record(const DSL_HARDWARE_DESCRIPTION_RECORD &hw,WOPT_DSL_NORMALIZATION_COST_RESULT *out,FILE *diagnostic)
{
  DSL_TENSOR_EVOLUTION_GRAPH *graph=DSL_tensor_evolution_create(Current_PU_Info,diagnostic);
  if (!graph) return FALSE;
  DSL_TENSOR_EVOLUTION_NODE_ID source=0,result=0;
  BOOL good=DSL_tensor_evolution_add_semantic_root(graph,out->source.result_value_id,out->source.result_ty,&result,diagnostic);
  source=result; /* Alternative realizations of Y, not an identity edge from X to Y. */
  DSL_OPT_PLAN_BUDGET budget={3,3};DSL_OPT_PLAN_CONTEXT *context=good?DSL_opt_plan_create(Current_PU_Info,graph,&budget,diagnostic):NULL;
  if (!context) good=FALSE;
  DSL_OPT_PLAN_ID baseline=0,selected=0;
  for (UINT32 i=0;i<3 && good;++i) {
    WOPT_DSL_NORMALIZATION_COST &c=out->costs[i];if (c.status!=WOPT_DSL_NORM_COST_COMPLETE) continue;
    DSL_OPT_CANDIDATE_INPUT candidate;memset(&candidate,0,sizeof(candidate));
    candidate.kind=i?DSL_OPT_CANDIDATE_RESIDENCY:DSL_OPT_CANDIDATE_BASELINE;
    candidate.semantic_node_id=out->source.node_id;candidate.source_evolution_node_id=source;candidate.result_evolution_node_id=result;
    candidate.legality=DSL_OPT_LEGALITY_PROVEN;candidate.ordering_key=i+1;
    candidate.flags=i?DSL_OPT_CANDIDATE_FLAG_PROVISIONAL:DSL_OPT_CANDIDATE_FLAG_BASELINE;
    candidate.parent_candidate_id=i?out->costs[0].candidate_id:0;
    DSL_OPT_COST_INPUT cost;memset(&cost,0,sizeof(cost));cost.target_profile_id=hw.target_profile_id;cost.ordering_key=i+1;
    for (UINT32 t=0;t<DSL_OPT_COST_TERM_COUNT;++t) {
      cost.terms[t].amount=c.terms[t];cost.terms[t].unit=DSL_OPT_COST_UNIT_RELATIVE;
      cost.terms[t].confidence=DSL_OPT_COST_CONFIDENCE_LOW;cost.terms[t].evidence=DSL_OPT_COST_EVIDENCE_TARGET_MODEL;
    }
    DSL_OPT_COST_ID cost_id=0;
    good=DSL_opt_plan_add_candidate(context,&candidate,&c.candidate_id,diagnostic) && DSL_opt_plan_add_cost(context,&cost,&cost_id,diagnostic);
    DSL_OPT_PLAN_INPUT plan;memset(&plan,0,sizeof(plan));plan.candidate_ids=&c.candidate_id;plan.candidate_count=1;plan.cost_id=cost_id;
    plan.fallback_plan_id=i?baseline:0;plan.legality=DSL_OPT_LEGALITY_PROVEN;plan.ordering_key=i+1;
    plan.flags=DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY | (i?0:DSL_OPT_PLAN_FLAG_BASELINE);
    if (good) good=DSL_opt_plan_add_plan(context,&plan,&c.plan_id,diagnostic);
    if (!i) baseline=c.plan_id;if (out->selected_kind==i+1) selected=c.plan_id;
    ++out->common_selection.legal_plan_count;++out->common_selection.complete_cost_count;
  }
  if (good) {
    out->common_selection.selected_plan_id=selected;out->common_selection.target_profile_id=hw.target_profile_id;
    good=DSL_opt_plan_record_selection(context,&out->common_selection,diagnostic) && DSL_opt_plan_verify(context,diagnostic);
  }
  DSL_opt_plan_destroy(context);DSL_tensor_evolution_destroy(graph);return good;
}

/* Compact columns expose every cost component, scope and non-selection reason.
 * Model declarations and estimated decisions never become WHIRL action rows. */
static void LN3_Trace(const WOPT_DSL_NORMALIZATION_COST_REQUEST &r,
    const WOPT_DSL_NORMALIZATION_COST_RESULT &b,FILE *trace)
{
  UINT32 line=b.source.source_line;
  DSL_HARDWARE_DESCRIPTION_RECORD hw;
  if (DSL_hardware_description_find_arch(r.plans[0].target.architecture,&hw)) {
    const DSL_HARDWARE_RESOURCE_RECORD &h=hw.resources;
    fprintf(trace,"AI_NORM_RESOURCE 1 BIND %u %u %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %u\n",
        hw.arch_id,hw.revision,(LN3_U64)hw.performance.sms,(LN3_U64)h.max_blocks,(LN3_U64)h.max_warps,
        (LN3_U64)h.max_threads,(LN3_U64)h.max_threads_per_cta,(LN3_U64)h.registers_per_sm,(LN3_U64)h.max_registers_per_thread,
        (LN3_U64)h.register_quantum,(LN3_U64)h.register_warp_quantum,(LN3_U64)h.shared_capacity,
        (LN3_U64)h.shared_budget,(LN3_U64)h.shared_quantum,(LN3_U64)h.shared_reserved,line);
    fprintf(trace,"AI_NORM_RESOURCE 1 ORIGIN %s %s %s %u\n",hw.table_name,hw.unit,hw.provenance,line);
  }
  fprintf(trace,"AI_NORM_MODEL 1 HEADER version arch revision model regs0 regs1 regs2 max_jobs margin source_line\n");
  fprintf(trace,"AI_NORM_MODEL 1 BIND %u %u %u %u %u %u %u %llu %llu %u\n",r.profile.version,r.profile.arch_id,r.profile.description_revision,
      r.profile.model_id,r.registers_per_thread[0],r.registers_per_thread[1],r.registers_per_thread[2],(LN3_U64)r.max_jobs,(LN3_U64)r.margin,line);
  for (UINT32 i=0;i<DSL_SCALAR_FIELD_COUNT;++i) fprintf(trace,"AI_NORM_MODEL 1 FIELD %u %u %llu %llu %llu %u\n",i,r.profile.provenance[i],
      (LN3_U64)r.profile.values[i][0],(LN3_U64)r.profile.values[i][1],(LN3_U64)r.profile.values[i][2],line);
  fprintf(trace,"AI_NORM_COST 1 HEADER pu node result kind plan_status cost_status resident jobs global_logical global_sectors carrier_sectors shared scalar_lanes scalar_insts control barriers lower total upper compute memory_unhidden communication_unhidden sync launch guard candidate_id plan_id source_line\n");
  for (UINT32 i=0;i<3;++i) {
    const WOPT_DSL_NORMALIZATION_COST &c=b.costs[i];
    fprintf(trace,"AI_NORM_COST 1 COST %u %u %u %u %u %u %u %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu",
        (UINT32)b.source.owner_pu,b.source.node_id,b.source.result_value_id,i+1,c.plan_status,c.status,c.resident_ctas,
        (LN3_U64)c.jobs,(LN3_U64)c.global_logical_bytes,(LN3_U64)c.global_sector_bytes,(LN3_U64)c.carrier_sector_bytes,
        (LN3_U64)c.shared_bytes,(LN3_U64)c.scalar_lanes,(LN3_U64)c.scalar_instructions,(LN3_U64)c.control_instructions,
        (LN3_U64)c.barriers,(LN3_U64)c.lower,(LN3_U64)c.total,(LN3_U64)c.upper);
    for (UINT32 t=0;t<DSL_OPT_COST_TERM_COUNT;++t) fprintf(trace," %llu",(LN3_U64)c.terms[t]);
    fprintf(trace," %u %u %u\n",c.candidate_id,c.plan_id,line);
  }
  fprintf(trace,"AI_NORM_SELECT 1 HEADER pu node result nominal selected fallback reason common_plan analysis_only source_line\n");
  fprintf(trace,"AI_NORM_SELECT 1 SELECT %u %u %u %u %u %u %u %u 1 %u\n",(UINT32)b.source.owner_pu,b.source.node_id,b.source.result_value_id,
      b.nominal_kind,b.selected_kind,b.fallback_kind,b.reason,b.common_selection.selected_plan_id,line);
  if (b.selected_kind) {
    fprintf(trace,"AI_NORM_CHOICE_GUARD 1 EXPORT 83");
    for (UINT32 i=0;i<sizeof(b.selected_guard)/sizeof(UINT64);++i) {
      UINT64 word;memcpy(&word,(const char *)&b.selected_guard+i*sizeof(word),sizeof(word));
      fprintf(trace," %llu",(LN3_U64)word);
    }
    fprintf(trace," %u\n",line);
  }
}
}

/* Bind native plans and one immutable imported snapshot before exploration.
 * Physical limits/visible SMs constrain the declared recipe; caller-supplied
 * reference_cycles instruction costs remain a generic analytical scenario.
 * Report HBM ns separately, seal the context even for unchanged choices and
 * require it current before guard export. No source/table mutation occurs. */
BOOL WOPT_DSL_select_normalization_plan(WN *scope,WN *assignment,
    const WOPT_DSL_NORMALIZATION_COST_REQUEST *r,WOPT_DSL_NORMALIZATION_COST_RESULT *output,FILE *diagnostic,FILE *trace)
{
  if (!r || !output) return FALSE;
  WOPT_DSL_NORMALIZATION_PLAN_BINDING bindings[3];
  for (UINT32 i=0;i<3;++i)
    if (r->plans[i].kind!=i+1 || (i && !LN3_Comparable(r->plans[0],r->plans[i])) ||
        !WOPT_DSL_bind_normalization_plan(scope,assignment,&r->plans[i],&bindings[i],diagnostic,NULL)) return FALSE;
  WOPT_DSL_NORMALIZATION_COST_RESULT out;memset(&out,0,sizeof(out));out.source=bindings[0].target.source;
  out.reason=WOPT_DSL_NORM_NO_BASELINE;
  const DSL_GPU_PLAN_SNAPSHOT gpu = DSL_GPU_PLAN_SNAPSHOT::Capture(VHO_DSL_GPU_Context);
  out.gpu_stamp = gpu.Stamp(); out.gpu_live_bytes = bindings[0].live_bytes;
  for (UINT32 i = 0; i < 3; ++i)
    if (!DSL_gpu_plan_stamp_equal(bindings[i].gpu_stamp, out.gpu_stamp)) return FALSE;
  DSL_HARDWARE_DESCRIPTION_RECORD hw;memset(&hw,0,sizeof(hw));
  UINT32 profile_status=DSL_hardware_description_get_arch(bindings[0].target.architecture_id,&hw)?
      LN3_Profile(r->profile,hw):WOPT_DSL_NORM_COST_PROFILE_INVALID;
  uint64_t sms = hw.performance.sms;
  BOOL sms_known = !gpu.Bound() || gpu.Resource_limit(DSL_GPU_VISIBLE_SMS, &sms);
  BOOL incomplete=FALSE;
  for (UINT32 i=0;i<3;++i) {
    WOPT_DSL_NORMALIZATION_COST &c=out.costs[i];c.plan_status=bindings[i].status;
    c.status=c.plan_status?WOPT_DSL_NORM_COST_PLAN_REFUSED:profile_status;
    if (c.plan_status) continue;
    if (!i) out.fallback_kind=1;
    if (c.status==WOPT_DSL_NORM_COST_COMPLETE) {
      BOOL resources_known = FALSE;
      c.resident_ctas=LN3_Residents(hw,r->plans[i],bindings[i],r->registers_per_thread[i],gpu,&resources_known);
      if (!sms_known || !sms || !resources_known) c.status=WOPT_DSL_NORM_COST_UNKNOWN;
      else if (!c.resident_ctas) c.status=WOPT_DSL_NORM_COST_RESOURCE;
      else {
        LN3_GRAPH graph(r->plans[i],bindings[i],c,r->max_jobs,r->profile.values[DSL_SCALAR_SECTOR_BYTES][1]);
        if (r->max_jobs>1000000 || sms>~UINT32(0) || (gpu.Bound() && sms>r->max_jobs) || !graph.Build(UINT32(sms))) {
          UINT32 resident=c.resident_ctas;memset(&c,0,sizeof(c));c.resident_ctas=resident;c.status=WOPT_DSL_NORM_COST_BUDGET;
        } else if (!LN3_Evaluate(graph,r->profile,UINT32(sms),&c)) {
          c.status=WOPT_DSL_NORM_COST_OVERFLOW;c.lower=c.total=c.upper=0;memset(c.terms,0,sizeof(c.terms));
        }
      }
    }
    if (c.status!=WOPT_DSL_NORM_COST_COMPLETE && c.status!=WOPT_DSL_NORM_COST_RESOURCE) incomplete=TRUE;
  }
  if (out.costs[0].status==WOPT_DSL_NORM_COST_RESOURCE) out.fallback_kind=0;
  if (out.fallback_kind) {
    out.reason=WOPT_DSL_NORM_INCOMPLETE_COST;
    if (!incomplete) {
      UINT32 best=0;
      for (UINT32 i=1;i<3;++i) if (!out.costs[i].status && out.costs[i].total<out.costs[best].total) best=i;
      out.nominal_kind=best+1;out.selected_kind=1;
      out.reason=best?WOPT_DSL_NORM_UNCERTAIN_BENEFIT:WOPT_DSL_NORM_BASELINE_MINIMUM;
      for (UINT32 i=1;i<3;++i) {
        UINT64 threshold;
        if (!out.costs[i].status && LN3_Add(out.costs[i].upper,r->margin,&threshold) && threshold<out.costs[0].lower &&
            (out.selected_kind==1 || out.costs[i].total<out.costs[out.selected_kind-1].total)) {
          out.selected_kind=i+1;out.reason=WOPT_DSL_NORM_ROBUST_BENEFIT;
        }
      }
      if (!gpu.Is_current(VHO_DSL_GPU_Context) || !LN3_Record(hw,&out,diagnostic) ||
          !WOPT_DSL_export_normalization_guard(scope,assignment,
          &r->plans[out.selected_kind-1],&out.selected_guard,diagnostic,NULL)) return FALSE;
    }
  }
  if (!gpu.Is_current(VHO_DSL_GPU_Context)) return FALSE;
  *output=out;
  if (trace) {
    LN3_Trace(*r,out,trace);
    if (gpu.Bound()) {
      for (UINT32 i = 0; i < 3; ++i) {
        fprintf(trace,"DSL_GPU_PLAN_SCENARIO 1 family=layernorm kind=%u status=%u sms=%llu ranking=reference_cycles device_calibrated=0\n",
            i+1,out.costs[i].status,(unsigned long long)(sms_known?sms:0));
        gpu.Print(trace,"layernorm-scenario-traffic",out.costs[i].global_sector_bytes);
      }
    }
  }
  return TRUE;
}
