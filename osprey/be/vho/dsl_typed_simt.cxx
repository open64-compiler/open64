/* VHO MMA3 runtime source-record verification only; never mutate WHIRL, select
 * an implementation or broaden the old F32-buffer accumulation adapter.
 * Design: doc/AI-COMPILER-MMA3-TYPED-SIMT-BASELINE.md. */
#include "dsl_typed_simt.h"
#include <limits.h>

/* Reject unsupported accepted-source contracts before VHO touches any tree.
 * Full increasing-K accumulation needs no reassociation permission. Read both
 * borrowed records without mutation; return FALSE with an optional diagnostic
 * for invalid identities, formats, dimensions, thread counts or output extent. */
BOOL DSL_typed_simt_verify(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, FILE *diagnostic)
{
    BOOL valid = record && numerical && record->id && record->version == 1 &&
        record->owner_pu_st && record->generation && record->numerical_acceptance_id &&
        record->numerical_contract_id == numerical->id && numerical->id &&
        record->problem_m && record->problem_m < INT_MAX &&
        record->problem_n && record->problem_n < INT_MAX &&
        record->problem_k && record->problem_k < INT_MAX &&
        record->threads_per_cta && record->threads_per_cta <= 1024 &&
        record->disjoint_output == 1 && !record->reserved && !numerical->reserved &&
        (numerical->source_format == DSL_ACC_FORMAT_F16 || numerical->source_format == DSL_ACC_FORMAT_BF16) &&
        numerical->input_format == numerical->source_format &&
        numerical->product_format == DSL_ACC_FORMAT_F32 && numerical->partial_format == DSL_ACC_FORMAT_F32 &&
        numerical->outer_format == DSL_ACC_FORMAT_F32 && numerical->output_format == DSL_ACC_FORMAT_F32 &&
        !(numerical->permissions & ~DSL_ACC_PERMISSION_REASSOCIATE) &&
        numerical->maximum_span >= record->problem_k &&
        numerical->evidence_kind >= DSL_ACC_EVIDENCE_ISA && numerical->evidence_kind <= DSL_ACC_EVIDENCE_NATIVE &&
        numerical->evidence_id;
    if (valid) {
        UINT64 outputs = (UINT64)record->problem_m * record->problem_n;
        valid = outputs <= (((UINT64)1 << 63) - 1) / 4;
    }
    if (!valid && diagnostic) fprintf(diagnostic, "typed SIMT: invalid source numerical/shape contract\n");
    return valid;
}
