/* Copyright (C) 2026 Open64 Project */

/* Structural numerical-contract verification and printing only. Target
 * legality, segment generation and executable mutation belong to VHO.
 * Design: doc/AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md. */
#include "dsl_accumulation.h"

/* Diagnose a malformed contract without changing its caller-owned records. */
static BOOL
DSL_Accumulation_Error(FILE *file, const char *reason)
{
    if (file != NULL)
        fprintf(file, "accumulation contract: %s\n", reason);
    return FALSE;
}

/* Accept only public formats; unknown does not mean native full precision. */
static BOOL
DSL_Accumulation_Format(UINT32 format)
{
    return format >= DSL_ACC_FORMAT_F32 && format <= DSL_ACC_FORMAT_F4_E2M1;
}

/* Verify evidence and complete contiguous segmentation before any consumer
 * uses this runtime-only contract; leave policy and target facts to VHO. */
BOOL
DSL_accumulation_verify(const DSL_NUMERICAL_CONTRACT_RECORD *contract,
    const DSL_ACCUMULATION_PLAN_RECORD *plan,
    const DSL_ACCUMULATION_SEGMENT_RECORD *segments, UINT32 count,
    FILE *diagnostic)
{
    if (contract == NULL || plan == NULL || segments == NULL || count == 0)
        return DSL_Accumulation_Error(diagnostic, "missing records");
    if (contract->id == 0 || plan->id == 0 || plan->owner_pu_st == 0 ||
        plan->contract_id != contract->id || contract->reserved || plan->reserved ||
        !DSL_Accumulation_Format(contract->source_format) ||
        !DSL_Accumulation_Format(contract->input_format) ||
        !DSL_Accumulation_Format(contract->product_format) ||
        contract->partial_format != DSL_ACC_FORMAT_F32 ||
        contract->outer_format != DSL_ACC_FORMAT_F32 ||
        !DSL_Accumulation_Format(contract->output_format))
        return DSL_Accumulation_Error(diagnostic, "invalid identity or format");
    if ((contract->permissions & ~3u) != 0 ||
        !(contract->permissions & DSL_ACC_PERMISSION_REASSOCIATE) ||
        (((contract->source_format != contract->input_format &&
           contract->input_format != DSL_ACC_FORMAT_F32) ||
          (contract->output_format != contract->source_format &&
           contract->output_format != DSL_ACC_FORMAT_F32)) &&
         !(contract->permissions & DSL_ACC_PERMISSION_NARROW)))
        return DSL_Accumulation_Error(diagnostic, "missing numerical permission");
    if (contract->maximum_span == 0 || contract->evidence_id == 0 ||
        contract->evidence_kind < DSL_ACC_EVIDENCE_ISA ||
        contract->evidence_kind > DSL_ACC_EVIDENCE_NATIVE ||
        plan->internal_evidence_kind > DSL_ACC_EVIDENCE_NATIVE ||
        ((plan->internal_evidence_kind == DSL_ACC_EVIDENCE_UNKNOWN) !=
         (plan->internal_evidence_id == 0)) ||
        (plan->internal_precision_bits != 0 &&
         plan->internal_evidence_kind == DSL_ACC_EVIDENCE_UNKNOWN))
        return DSL_Accumulation_Error(diagnostic, "invalid precision evidence");
    if (plan->target_profile_id == 0 || plan->instruction_id == 0 ||
        plan->instruction_k == 0 || plan->total_k == 0 || plan->staging_k == 0 ||
        plan->requested_span == 0 || plan->requested_span > contract->maximum_span ||
        plan->effective_max_span == 0 ||
        plan->effective_max_span > plan->requested_span || count != plan->segment_count)
        return DSL_Accumulation_Error(diagnostic, "invalid interval or count");
    UINT64 next = 0;
    UINT32 maximum = 0;
    for (UINT32 i = 0; i != count; ++i) {
        const DSL_ACCUMULATION_SEGMENT_RECORD &segment = segments[i];
        UINT64 end = next + segment.valid_k;
        if (segment.ordinal != i || segment.begin_k != next || segment.reserved ||
            segment.valid_k == 0 || segment.issued_k < segment.valid_k ||
            segment.issued_k > plan->requested_span ||
            segment.begin_k % plan->instruction_k != 0 ||
            segment.issued_k % plan->instruction_k != 0 || end > plan->total_k ||
            (i + 1 != count && segment.valid_k != segment.issued_k) ||
            (plan->scale_group_k != 0 &&
             segment.begin_k / plan->scale_group_k !=
                 (end - 1) / plan->scale_group_k))
            return DSL_Accumulation_Error(diagnostic, "invalid reduction segment");
        if (maximum < segment.issued_k)
            maximum = segment.issued_k;
        next = end;
    }
    if (next != plan->total_k || maximum != plan->effective_max_span)
        return DSL_Accumulation_Error(diagnostic, "incomplete coverage or span");
    return TRUE;
}

/* Emit positional fields with one header per stream and explicit planning
 * status; an action row is the materializer's separate responsibility. */
void
DSL_accumulation_print(FILE *file, const DSL_NUMERICAL_CONTRACT_RECORD *contract,
    const DSL_ACCUMULATION_PLAN_RECORD *plan,
    const DSL_ACCUMULATION_SEGMENT_RECORD *segments, UINT32 count)
{
    if (file == NULL || !DSL_accumulation_verify(contract, plan, segments, count, NULL))
        return;
    fprintf(file, "# AI_ACC 1 CONTRACT id source input product partial outer output permissions max_span evidence_kind evidence_id\n");
    fprintf(file, "AI_ACC 1 CONTRACT %u %u %u %u %u %u %u %u %u %u %u\n",
        contract->id, contract->source_format, contract->input_format,
        contract->product_format, contract->partial_format, contract->outer_format,
        contract->output_format, contract->permissions, contract->maximum_span,
        contract->evidence_kind, contract->evidence_id);
    fprintf(file, "# AI_ACC 1 PLAN id contract owner target instruction K_inst K_total BK requested_span effective_span K_scale segments internal_bits evidence_kind evidence_id status\n");
    fprintf(file, "AI_ACC 1 PLAN %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u planned\n",
        plan->id, plan->contract_id, plan->owner_pu_st, plan->target_profile_id,
        plan->instruction_id, plan->instruction_k, plan->total_k, plan->staging_k,
        plan->requested_span, plan->effective_max_span, plan->scale_group_k,
        count, plan->internal_precision_bits, plan->internal_evidence_kind,
        plan->internal_evidence_id);
    fprintf(file, "# AI_ACC 1 SEGMENT plan ordinal begin valid issued\n");
    for (UINT32 i = 0; i != count; ++i)
        fprintf(file, "AI_ACC 1 SEGMENT %u %u %u %u %u\n", plan->id,
            segments[i].ordinal, segments[i].begin_k, segments[i].valid_k,
            segments[i].issued_k);
}
