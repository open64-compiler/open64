/* Copyright (C) 2026 Open64 Project */

/* LN2 native planning fixtures reuse source/target producers; no reduction
 * code, runtime adapter or executable normalization is linked by this test.
 * Design: doc/AI-COMPILER-LAYERNORM-STORAGE-EVENTS-v0.1.md. */
#define DSL_NORMALIZATION_TARGET_NO_MAIN
#include "dsl_normalization_target_test.cxx"
#include "opt_dsl_normalization_plan.h"

/* Construct a complete provisional invocation and the conservative event DAG.
 * Storage IDs preserve shared-affine identity; no actual device pointers exist. */
static BOOL LN2_Request(WN *scope, WN *value, const char *arch, UINT32 kind,
                       WOPT_DSL_NORMALIZATION_PLAN_REQUEST *r)
{
  memset(r, 0, sizeof(*r));
  if (!WOPT_DSL_characterize_normalization(scope, value, &r->target.expected_source, stderr)) return FALSE;
  r->target.architecture = arch; r->target.route = 1; r->target.toolkit = 12080;
  r->target.ptx = 807; r->target.environment = 1; r->kind = kind; r->layout = 1;
  r->runtime_contract_id = 1; r->context_id = 1;
  const WOPT_DSL_NORMALIZATION_CHARACTERIZATION &s = r->target.expected_source;
  for (UINT32 i = 0; i < 4; ++i) {
    r->ranges[i].allocation_id = 10 + (i == 3 ? s.result_value_id : s.operand_value_ids[i]);
    r->ranges[i].allocation_bytes = (i == 0 || i == 3) ? s.rows * s.width * 4 : s.width * 4;
    r->ranges[i].base_alignment = 16;
    r->ranges[i].lease = i == 3 ? 2 : 1;
  }
  r->ownership = 1; r->threads_per_cta = 128; r->launched_ctas = s.rows; r->tail_policy = 1;
  if (kind != WOPT_DSL_NORM_C0) {
    DSL_HARDWARE_DESCRIPTION_RECORD hw; DSL_MEMORY_TIER_RECORD tier;
    if (!DSL_hardware_description_find_arch(arch, &hw) ||
        !DSL_memory_hierarchy_find_tier(hw.target_profile_id, DSL_MEMORY_TIER_SHARED, &tier)) return FALSE;
    r->row_stride_bytes = ((s.width * 4 + tier.minimum_alignment - 1) / tier.minimum_alignment) * tier.minimum_alignment;
    r->shared_bytes = r->row_stride_bytes + 8; r->barrier_participants = r->threads_per_cta;
    r->cta_barriers = (1U << WOPT_DSL_NORM_MU_VISIBLE) | (1U << WOPT_DSL_NORM_CENTERED_READY) |
        (1U << WOPT_DSL_NORM_Q_VISIBLE) | (1U << WOPT_DSL_NORM_READERS_DONE);
    if (kind == WOPT_DSL_NORM_C2) r->cta_barriers |= 1U << WOPT_DSL_NORM_ROW_READY;
  }
  for (UINT32 i = 0; i + 1 < WOPT_DSL_NORM_EVENT_COUNT; ++i) r->before[i] = 1U << (i + 1);
  return TRUE;
}

/* Query real source/target and plan APIs, preserving tables, outputs and trace
 * parity. The target trace precedes its one plan block for unambiguous joins. */
static BOOL LN2_Query(WN *scope, WN *value, const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &r,
                     UINT32 expected, FILE *trace)
{
  DSL_IR_IMAGE_HEADER before, after; DSL_IR_Image_Get_Header(&before);
  UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB), types = TY_Table_Size();
  WOPT_DSL_NORMALIZATION_TARGET_BINDING target;
  WOPT_DSL_NORMALIZATION_PLAN_BINDING b, quiet;
  if (!WOPT_DSL_bind_normalization_target(scope,value,&r.target,&target,stderr,trace) ||
      !WOPT_DSL_bind_normalization_plan(scope,value,&r,&b,stderr,trace) ||
      !WOPT_DSL_bind_normalization_plan(scope,value,&r,&quiet,NULL,NULL)) return FALSE;
  DSL_IR_Image_Get_Header(&after);
  return Check(b.status == expected && !memcmp(&b,&quiet,sizeof(b)) &&
      !memcmp(&before,&after,sizeof(before)) && symbols == ST_Table_Size(CURRENT_SYMTAB) && types == TY_Table_Size(),
      "plan_status_read_only_trace_parity");
}

/* Exercise every gate without changing source IR. Additional edges are legal
 * only if acyclic and still preserving all required completion boundaries. */
static BOOL LN2_Negatives(WN *scope, WN *value, const char *arch, FILE *trace)
{
  WOPT_DSL_NORMALIZATION_PLAN_REQUEST base;
  if (!LN2_Request(scope,value,arch,WOPT_DSL_NORM_C2,&base)) return FALSE;
  for (UINT32 fault = 0; fault < 31; ++fault) {
    WOPT_DSL_NORMALIZATION_PLAN_REQUEST r = base;
    UINT32 expected = WOPT_DSL_NORM_PLAN_EVENTS_INVALID;
    switch (fault) {
    case 0: r.kind = 4; expected = WOPT_DSL_NORM_PLAN_KIND_UNSUPPORTED; break;
    case 1: r.runtime_contract_id = 0; expected = WOPT_DSL_NORM_PLAN_RUNTIME_UNBOUND; break;
    case 2: r.context_id = 0; expected = WOPT_DSL_NORM_PLAN_RUNTIME_UNBOUND; break;
    case 3: r.ranges[0].allocation_id = 0; expected = WOPT_DSL_NORM_PLAN_RANGE_INVALID; break;
    case 4: r.ranges[1].base_alignment = 3; expected = WOPT_DSL_NORM_PLAN_RANGE_INVALID; break;
    case 5: r.ranges[2].base_alignment = 2; expected = WOPT_DSL_NORM_PLAN_RANGE_INVALID; break;
    case 6: r.ranges[0].offset_bytes = 2; expected = WOPT_DSL_NORM_PLAN_RANGE_INVALID; break;
    case 7: r.ranges[0].allocation_bytes--; expected = WOPT_DSL_NORM_PLAN_RANGE_INVALID; break;
    case 8: r.ranges[0].offset_bytes = ~(UINT64)0-3; expected = WOPT_DSL_NORM_PLAN_RANGE_INVALID; break;
    case 9: r.ranges[0].lease = 0; expected = WOPT_DSL_NORM_PLAN_RANGE_INVALID; break;
    case 10: r.ranges[3] = r.ranges[0]; r.ranges[3].lease = 2; expected = WOPT_DSL_NORM_PLAN_ALIAS_INVALID; break;
    case 11: r.ranges[1] = r.ranges[0]; r.ranges[1].allocation_bytes++; expected = WOPT_DSL_NORM_PLAN_ALIAS_INVALID; break;
    case 12: r.ownership = 0; expected = WOPT_DSL_NORM_PLAN_OWNER_INVALID; break;
    case 13: r.threads_per_cta = 0; expected = WOPT_DSL_NORM_PLAN_OWNER_INVALID; break;
    case 14: r.threads_per_cta = 33; expected = WOPT_DSL_NORM_PLAN_OWNER_INVALID; break;
    case 15: r.threads_per_cta = 2048; expected = WOPT_DSL_NORM_PLAN_OWNER_INVALID; break;
    case 16: r.launched_ctas++; expected = WOPT_DSL_NORM_PLAN_OWNER_INVALID; break;
    case 17: r.tail_policy = 0; expected = WOPT_DSL_NORM_PLAN_OWNER_INVALID; break;
    case 18: r.row_stride_bytes = 16; expected = WOPT_DSL_NORM_PLAN_STORAGE_INVALID; break;
    case 19: r.row_stride_bytes++; expected = WOPT_DSL_NORM_PLAN_STORAGE_INVALID; break;
    case 20: r.shared_bytes--; expected = WOPT_DSL_NORM_PLAN_STORAGE_INVALID; break;
    case 21: r.row_stride_bytes = ~(UINT64)0-15; expected = WOPT_DSL_NORM_PLAN_STORAGE_INVALID; break;
    case 22: r.barrier_participants--; break;
    case 23: r.cta_barriers ^= 1U << WOPT_DSL_NORM_ROW_READY; break;
    case 24: r.cta_barriers |= 1U << 31; break;
    case 25: r.before[0] |= 1U; break;
    case 26: r.before[14] = 1U; break;
    case 27: r.before[0] |= 1U << 31; break;
    case 28: r.target.expected_source.epsilon_bits ^= 1; expected = WOPT_DSL_NORM_PLAN_TARGET_REFUSED; break;
    case 29: r.target.fp_options = 1; expected = WOPT_DSL_NORM_PLAN_TARGET_REFUSED; break;
    case 30: r.layout = 0; expected = WOPT_DSL_NORM_PLAN_LAYOUT_UNSUPPORTED; break;
    }
    if (!LN2_Query(scope,value,r,expected,trace)) return FALSE;
  }
  for (UINT32 edge = 0; edge + 1 < WOPT_DSL_NORM_EVENT_COUNT; ++edge) {
    WOPT_DSL_NORMALIZATION_PLAN_REQUEST r = base; r.before[edge] = 0;
    if (!LN2_Query(scope,value,r,WOPT_DSL_NORM_PLAN_EVENTS_INVALID,trace)) return FALSE;
  }
  base.before[0] |= 1U << 3; /* redundant transitive edge remains legal */
  if (!LN2_Query(scope,value,base,WOPT_DSL_NORM_PLAN_BOUND,trace)) return FALSE;
  /* Inputs can overlap read-only, and disjoint subranges can share backing. */
  base.ranges[1] = base.ranges[0];
  base.ranges[2] = base.ranges[1];
  if (!LN2_Query(scope,value,base,WOPT_DSL_NORM_PLAN_BOUND,trace)) return FALSE;
  /* Adjacent output subrange shares canonical backing without overlapping. */
  UINT64 bytes = base.ranges[0].allocation_bytes;
  for (UINT32 i = 0; i < 3; ++i) base.ranges[i].allocation_bytes = 2 * bytes;
  base.ranges[3] = base.ranges[0]; base.ranges[3].offset_bytes = bytes; base.ranges[3].lease = 2;
  if (!LN2_Query(scope,value,base,WOPT_DSL_NORM_PLAN_BOUND,trace)) return FALSE;
  return TRUE;
}

/* Publish native plans and before/after source checkpoints, not executable IR.
 * Every variant is a fresh process; count-limit refuses retention capacity. */
int main(int argc, char **argv)
{
  if (argc != 5) return 2;
  char *source_args[] = {argv[0],argv[1],argv[4]};
  if (LN1b_Source_Producer(3,source_args)) return 1;
  WN *scope = PU_Info_tree_ptr(Current_PU_Info);
  OPT_Fast_Math = FALSE; Roundoff_Level = ROUNDOFF_NONE; IEEE_Arithmetic = IEEE_ACCURATE;
  FILE *trace = fopen(argv[2],"wx"); if (!trace) return 1;
  DSL_HARDWARE_DESCRIPTION_RECORD hw; DSL_MEMORY_TIER_RECORD tier; DSL_SHARED_BANK_MODEL_RECORD banks;
  if (!DSL_hardware_description_find_arch(argv[3],&hw) ||
      !DSL_memory_hierarchy_find_tier(hw.target_profile_id,DSL_MEMORY_TIER_SHARED,&tier) ||
      !DSL_memory_hierarchy_shared_bank_model(hw.target_profile_id,&banks)) return 1;
  fprintf(trace,"AI_NORM_HW 1 MODEL %s %u %u %u %llu %llu %llu %u %u %u %u %llu %u\n",argv[3],hw.arch_id,hw.target_profile_id,hw.revision,
      (unsigned long long)tier.capacity_bytes,(unsigned long long)hw.resources.shared_budget,
      (unsigned long long)tier.allocation_granularity,tier.minimum_alignment,banks.bank_count,banks.word_bytes,banks.warp_lanes,
      (unsigned long long)hw.resources.max_threads_per_cta,(UINT32)__LINE__);
  BOOL first = TRUE;
  for (WN *s = WN_first(WN_func_body(scope));s;s = WN_next(s)) {
    if (WN_operator(s) != OPR_STID || WN_operator(WN_kid0(s)) != OPR_DSL) continue;
    for (UINT32 kind = 1;kind <= 3;++kind) {
      WOPT_DSL_NORMALIZATION_PLAN_REQUEST r;
      if (!LN2_Request(scope,s,argv[3],kind,&r)) return 1;
      UINT32 expected = r.target.expected_source.recipe != 2 ? WOPT_DSL_NORM_PLAN_TARGET_REFUSED :
          (!strcmp(argv[4],"count-limit") && kind != 1 ? WOPT_DSL_NORM_PLAN_STORAGE_INVALID : WOPT_DSL_NORM_PLAN_BOUND);
      if (!LN2_Query(scope,s,r,expected,trace)) return 1;
      if (first && !strcmp(argv[4],"default")) {
        WOPT_DSL_NORMALIZATION_PLAN_BINDING b, saved; memset(&b,0x5a,sizeof(b));saved=b;
        long length=ftell(trace);
        if (!Check(!WOPT_DSL_bind_normalization_plan(WN_func_body(scope),s,&r,&b,NULL,trace) &&
            !WOPT_DSL_bind_normalization_plan(scope,WN_COPY_Tree(s),&r,&b,NULL,trace) &&
            !WOPT_DSL_bind_normalization_plan(scope,s,NULL,&b,NULL,trace) &&
            !WOPT_DSL_bind_normalization_plan(scope,s,&r,NULL,NULL,trace) &&
            !memcmp(&b,&saved,sizeof(b)) && ftell(trace)==length,"plan_invalid_api_no_output_or_trace")) return 1;
      }
    }
    if (first && !strcmp(argv[4],"default") && !LN2_Negatives(scope,s,argv[3],trace)) return 1;
    first = FALSE;
  }
  if (fclose(trace)) return 1;
  char path[4096]; if (snprintf(path,sizeof(path),"%s.preserved.B",argv[1]) >= sizeof(path)) return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST checkpoint = {}; checkpoint.path = path;
  return LN1c_Source_Checkpoint(&checkpoint) ? 0 : 1;
}
