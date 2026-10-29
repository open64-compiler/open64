/* Copyright (C) 2026 Open64 Project */

/* LN2 owning-phase legality reuses source/target binding and common hardware
 * facts, not reduction planning or a second executable representation.
 * Design: doc/AI-COMPILER-LAYERNORM-STORAGE-EVENTS-v0.1.md. */
#include <string.h>
#include "opt_dsl_normalization_plan.h"
#include "dsl_memory_hierarchy.h"

/* Validate power-of-two guarantees, never assume an offset repairs a weak base. */
static BOOL LN2_Power_Of_Two(UINT32 value)
{
  return value && !(value & (value - 1));
}

/* Checked half-open byte ranges avoid wrapped offset+length arithmetic. */
static BOOL LN2_Range(const WOPT_DSL_NORMALIZATION_RANGE &r, UINT64 bytes, UINT32 lease)
{
  return r.allocation_id && r.allocation_bytes && bytes &&
      r.lease == lease && LN2_Power_Of_Two(r.base_alignment) && r.base_alignment >= 4 &&
      r.offset_bytes % 4 == 0 && r.offset_bytes <= r.allocation_bytes &&
      bytes <= r.allocation_bytes - r.offset_bytes;
}

/* Same canonical allocation must have one capacity/alignment; different IDs
 * mean disjoint objects only under the outstanding canonical-storage obligation. */
static BOOL LN2_Alias(const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &r,
                     const WOPT_DSL_NORMALIZATION_PLAN_BINDING &b)
{
  for (UINT32 i = 0; i < 4; ++i)
    for (UINT32 j = i + 1; j < 4; ++j) {
      const WOPT_DSL_NORMALIZATION_RANGE &a = r.ranges[i], &c = r.ranges[j];
      if (a.allocation_id != c.allocation_id) continue;
      if (a.allocation_bytes != c.allocation_bytes || a.base_alignment != c.base_alignment) return FALSE;
      if (j == 3 && a.offset_bytes < c.offset_bytes + b.bytes[j] &&
          c.offset_bytes < a.offset_bytes + b.bytes[i]) return FALSE;
    }
  const WOPT_DSL_NORMALIZATION_CHARACTERIZATION &s = b.target.source;
  return s.operand_value_ids[1] != s.operand_value_ids[2] ||
      (r.ranges[1].allocation_id == r.ranges[2].allocation_id &&
       r.ranges[1].offset_bytes == r.ranges[2].offset_bytes);
}

/* Require all conservative phase boundaries by reachability, not a particular
 * adjacency spelling. Check every node for cycles, self/out-of-domain edges. */
static BOOL LN2_Events(const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &r)
{
  const UINT32 count = WOPT_DSL_NORM_EVENT_COUNT, mask = (1U << count) - 1;
  UINT32 reach[WOPT_DSL_NORM_EVENT_COUNT];
  for (UINT32 i = 0; i < count; ++i) {
    if ((r.before[i] & ~mask) || (r.before[i] & (1U << i))) return FALSE;
    reach[i] = r.before[i];
  }
  for (UINT32 k = 0; k < count; ++k)
    for (UINT32 i = 0; i < count; ++i)
      if (reach[i] & (1U << k)) reach[i] |= reach[k];
  for (UINT32 i = 0; i < count; ++i)
    if ((reach[i] & (1U << i)) || (i + 1 < count && !(reach[i] & (1U << (i + 1))))) return FALSE;
  return TRUE;
}

/* Refuse by ordered gates. Fields describe only checked prefixes on refusal;
 * no output mask asserts that a provider actually enforced a runtime contract. */
static UINT32 LN2_Status(const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &r,
                        WOPT_DSL_NORMALIZATION_PLAN_BINDING *b)
{
  if (b->target.status != WOPT_DSL_NORM_TARGET_BOUND) return WOPT_DSL_NORM_PLAN_TARGET_REFUSED;
  b->stages = 1;
  if (r.kind < WOPT_DSL_NORM_C0 || r.kind > WOPT_DSL_NORM_C2) return WOPT_DSL_NORM_PLAN_KIND_UNSUPPORTED;
  if (!r.runtime_contract_id || !r.context_id) return WOPT_DSL_NORM_PLAN_RUNTIME_UNBOUND;
  if (r.layout != WOPT_DSL_NORM_CONTIGUOUS_ROW_MAJOR_F32) return WOPT_DSL_NORM_PLAN_LAYOUT_UNSUPPORTED;
  const WOPT_DSL_NORMALIZATION_CHARACTERIZATION &s = b->target.source;
  b->bytes[0] = b->bytes[3] = s.rows * s.width * 4; /* source already checked footprint */
  b->bytes[1] = b->bytes[2] = b->row_bytes = s.width * 4;
  for (UINT32 i = 0; i < 4; ++i)
    if (!LN2_Range(r.ranges[i], b->bytes[i], i == 3 ? WOPT_DSL_NORM_EXCLUSIVE_OUTPUT : WOPT_DSL_NORM_IMMUTABLE_INPUT))
      return WOPT_DSL_NORM_PLAN_RANGE_INVALID;
  if (!LN2_Alias(r, *b)) return WOPT_DSL_NORM_PLAN_ALIAS_INVALID;
  b->stages |= 2;
  DSL_HARDWARE_DESCRIPTION_RECORD hw;
  DSL_MEMORY_TIER_RECORD tier;
  DSL_SHARED_BANK_MODEL_RECORD banks;
  if (!DSL_hardware_description_get_arch(b->target.architecture_id, &hw) ||
      !DSL_memory_hierarchy_find_tier(hw.target_profile_id, DSL_MEMORY_TIER_SHARED, &tier) ||
      !DSL_memory_hierarchy_shared_bank_model(hw.target_profile_id, &banks)) return WOPT_DSL_NORM_PLAN_STORAGE_INVALID;
  if (r.ownership != WOPT_DSL_NORM_CTA_PER_ROW_ORDERED_OWNER_ZERO ||
      r.launched_ctas != s.rows || !r.threads_per_cta || !banks.warp_lanes ||
      r.threads_per_cta % banks.warp_lanes || r.threads_per_cta > hw.resources.max_threads_per_cta ||
      r.tail_policy != WOPT_DSL_NORM_PREDICATE_VALID_BEFORE_ACCESS_ARITHMETIC)
    return WOPT_DSL_NORM_PLAN_OWNER_INVALID;
  b->stages |= 4;
  b->shared_limit = tier.capacity_bytes < hw.resources.shared_budget ? tier.capacity_bytes : hw.resources.shared_budget;
  b->shared_quantum = tier.allocation_granularity;
  b->bank_count = banks.bank_count; b->bank_word_bytes = banks.word_bytes; b->warp_lanes = banks.warp_lanes;
  if (r.kind == WOPT_DSL_NORM_C0) {
    if (r.row_stride_bytes || r.shared_bytes || r.cta_barriers || r.barrier_participants)
      return WOPT_DSL_NORM_PLAN_STORAGE_INVALID;
  } else {
    if (!LN2_Power_Of_Two(tier.minimum_alignment) || !b->shared_quantum || !banks.bank_count || banks.word_bytes != 4 ||
        r.row_stride_bytes < b->row_bytes || r.row_stride_bytes % tier.minimum_alignment ||
        r.row_stride_bytes > ~(UINT64)0 - 8) return WOPT_DSL_NORM_PLAN_STORAGE_INVALID;
    b->required_shared = r.row_stride_bytes + 8; /* distinct mu and q slots */
    if (b->required_shared > ~(UINT64)0 - (b->shared_quantum - 1)) return WOPT_DSL_NORM_PLAN_STORAGE_INVALID;
    b->reserved_shared = ((b->required_shared + b->shared_quantum - 1) / b->shared_quantum) * b->shared_quantum;
    if (r.shared_bytes != b->required_shared || b->reserved_shared > b->shared_limit)
      return WOPT_DSL_NORM_PLAN_STORAGE_INVALID;
    b->required_barriers = (1U << WOPT_DSL_NORM_MU_VISIBLE) | (1U << WOPT_DSL_NORM_CENTERED_READY) |
        (1U << WOPT_DSL_NORM_Q_VISIBLE) | (1U << WOPT_DSL_NORM_READERS_DONE);
    if (r.kind == WOPT_DSL_NORM_C2) b->required_barriers |= 1U << WOPT_DSL_NORM_ROW_READY;
    UINT64 active = s.width < banks.warp_lanes ? s.width : banks.warp_lanes;
    b->bank_conflict_max = (UINT32)((active + banks.bank_count - 1) / banks.bank_count);
    if (r.cta_barriers != b->required_barriers || r.barrier_participants != r.threads_per_cta)
      return WOPT_DSL_NORM_PLAN_EVENTS_INVALID;
  }
  b->stages |= 8;
  if (!LN2_Events(r)) return WOPT_DSL_NORM_PLAN_EVENTS_INVALID;
  b->stages |= 16;
  b->runtime_obligations = 63;
  return WOPT_DSL_NORM_PLAN_BOUND;
}

/* Condensed native trace exposes all gate inputs and derived resource facts.
 * Native source line remains last on every row, including ranges and edges. */
static void LN2_Trace(const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &r,
                      const WOPT_DSL_NORMALIZATION_PLAN_BINDING &b, FILE *file)
{
  const char *reasons[] = { "conditional_storage_events", "target_refused", "unsupported_plan", "runtime_unbound",
      "invalid_range", "invalid_alias", "invalid_owner", "invalid_storage", "invalid_events", "unsupported_layout" };
  UINT32 line = b.target.source.source_line;
  fprintf(file, "AI_NORM_PLAN 1 HEADER pu node result kind layout status target_status stages obligations runtime context ownership threads ctas tail stride shared participants barriers row_bytes required reserved limit quantum banks bank_word warp conflict required_barriers reason source_line\n");
  fprintf(file, "AI_NORM_PLAN 1 BIND %u %u %u %u %u %u %u %u %u %llu %llu %u %u %llu %u %llu %llu %u %u %llu %llu %llu %llu %llu %u %u %u %u %u %s %u\n",
      (UINT32)b.target.source.owner_pu,b.target.source.node_id,b.target.source.result_value_id,r.kind,r.layout,b.status,b.target.status,
      b.stages,b.runtime_obligations,(unsigned long long)r.runtime_contract_id,(unsigned long long)r.context_id,r.ownership,
      r.threads_per_cta,(unsigned long long)r.launched_ctas,r.tail_policy,(unsigned long long)r.row_stride_bytes,
      (unsigned long long)r.shared_bytes,r.barrier_participants,r.cta_barriers,(unsigned long long)b.row_bytes,
      (unsigned long long)b.required_shared,(unsigned long long)b.reserved_shared,(unsigned long long)b.shared_limit,
      (unsigned long long)b.shared_quantum,b.bank_count,b.bank_word_bytes,b.warp_lanes,b.bank_conflict_max,b.required_barriers,reasons[b.status],line);
  for (UINT32 i = 0; i < 4; ++i) {
    const WOPT_DSL_NORMALIZATION_RANGE &a = r.ranges[i];
    fprintf(file,"AI_NORM_RANGE 1 RANGE %u %llu %llu %llu %u %u %llu %u\n",i,(unsigned long long)a.allocation_id,
        (unsigned long long)a.allocation_bytes,(unsigned long long)a.offset_bytes,a.base_alignment,a.lease,(unsigned long long)b.bytes[i],line);
  }
  for (UINT32 i = 0; i < WOPT_DSL_NORM_EVENT_COUNT; ++i)
    fprintf(file,"AI_NORM_EVENT 1 EDGE %u %u %u\n",i,r.before[i],line);
}

/* Target API rederives active-PU meaning before optional plan checks/trace.
 * Invalid arguments never publish a partial binding or diagnostic trace. */
BOOL WOPT_DSL_bind_normalization_plan(WN *scope, WN *assignment,
    const WOPT_DSL_NORMALIZATION_PLAN_REQUEST *request,
    WOPT_DSL_NORMALIZATION_PLAN_BINDING *output, FILE *diagnostic, FILE *trace)
{
  WOPT_DSL_NORMALIZATION_PLAN_BINDING b;
  memset(&b, 0, sizeof(b));
  if (!request || !output || !WOPT_DSL_bind_normalization_target(scope, assignment,
      &request->target, &b.target, diagnostic, NULL)) return FALSE;
  b.status = LN2_Status(*request, &b);
  *output = b;
  if (trace) LN2_Trace(*request, b, trace);
  return TRUE;
}

/* Export freshly checked facts, never trust a caller-supplied prior binding.
 * Homogeneous uint64 fields permit a compact fixed-column review trace. */
BOOL WOPT_DSL_export_normalization_guard(WN *scope, WN *assignment,
    const WOPT_DSL_NORMALIZATION_PLAN_REQUEST *r, OPEN64_NORMALIZATION_GUARD *out,
    FILE *diagnostic, FILE *trace)
{
  WOPT_DSL_NORMALIZATION_PLAN_BINDING b;
  if (!out || !WOPT_DSL_bind_normalization_plan(scope,assignment,r,&b,diagnostic,NULL)) return FALSE;
  if (b.status != WOPT_DSL_NORM_PLAN_BOUND) {
    if (trace) fprintf(trace,"AI_NORM_GUARD 1 REFUSE %u %u\n",b.status,b.target.source.source_line);
    return FALSE;
  }
  const WOPT_DSL_NORMALIZATION_CHARACTERIZATION &s = b.target.source;
  OPEN64_NORMALIZATION_GUARD g;
  memset(&g,0,sizeof(g));
  g.abi_version=1; g.size=sizeof(g); g.source_version=s.recipe; g.source_flags=s.flags;
  g.pu=s.owner_pu; g.node=s.node_id; g.result=s.result_value_id; g.result_type=s.result_ty;
  for (UINT32 i=0;i<3;++i) { g.operands[i]=s.operand_value_ids[i]; g.operand_types[i]=s.operand_types[i]; }
  g.rows=s.rows; g.width=s.width; g.epsilon_bits=s.epsilon_bits; g.source_line=s.source_line;
  g.architecture=b.target.architecture_id; g.route=r->target.route; g.toolkit=r->target.toolkit;
  g.ptx=r->target.ptx; g.environment=r->target.environment; g.fp_options=r->target.fp_options;
  g.kind=r->kind; g.layout=r->layout; g.contract=r->runtime_contract_id; g.context=r->context_id;
  g.ownership=r->ownership; g.threads=r->threads_per_cta; g.ctas=r->launched_ctas; g.tail=r->tail_policy;
  g.stride=r->row_stride_bytes; g.shared=r->shared_bytes; g.reserved_shared=b.reserved_shared;
  g.shared_limit=b.shared_limit; g.shared_quantum=b.shared_quantum; g.participants=r->barrier_participants;
  g.barriers=r->cta_barriers; g.banks=b.bank_count; g.bank_word=b.bank_word_bytes; g.warp=b.warp_lanes;
  g.bank_conflict=b.bank_conflict_max; g.obligations=b.runtime_obligations;
  for (UINT32 i=0;i<4;++i) {
    g.ranges[i].role=r->ranges[i].allocation_id; g.ranges[i].capacity=r->ranges[i].allocation_bytes;
    g.ranges[i].offset=r->ranges[i].offset_bytes; g.ranges[i].alignment=r->ranges[i].base_alignment;
    g.ranges[i].lease=r->ranges[i].lease; g.ranges[i].bytes=b.bytes[i];
  }
  for (UINT32 i=0;i<WOPT_DSL_NORM_EVENT_COUNT;++i) g.before[i]=r->before[i];
  *out=g;
  if (trace) {
    fprintf(trace,"AI_NORM_GUARD 1 EXPORT %u",(UINT32)(sizeof(g)/sizeof(UINT64)));
    /* memcpy avoids type-punning unrelated uint64 typedefs. Source line last. */
    for (UINT32 i=0;i<sizeof(g)/sizeof(UINT64);++i) {
      UINT64 word; memcpy(&word,(const char *)&g+i*sizeof(word),sizeof(word));
      fprintf(trace," %llu",(unsigned long long)word);
    }
    fprintf(trace," %u\n",s.source_line);
  }
  return TRUE;
}
