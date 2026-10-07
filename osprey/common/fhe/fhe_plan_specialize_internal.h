/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private in-memory FHE plan repartitioning used by the terminal PU
 * specialization transaction. Persistent producers use fhe_plan.h APIs.
 * See doc/FHE-SYNC6-PU-SPECIALIZATION-TRANSACTION.md and
 * doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#ifndef fhe_plan_specialize_internal_INCLUDED
#define fhe_plan_specialize_internal_INCLUDED

#include "fhe_plan.h"

/* Move one context-owned BN fold row to its specialized definition while
 * preserving the stable row ID and caller-owned payload provenance. */
extern BOOL DSL_FHE_Plan_Specialize_BN_Fold
                (DSL_FHE_BN_FOLD_PROVENANCE_ID id,
                 const DSL_FHE_BN_FOLD_PROVENANCE_RECORD *expected,
                 ST_IDX owner_pu_st, DSL_IR_NODE_ID conv_node_id,
                 DSL_IR_NODE_ID batch_norm_node_id,
                 DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id);

/* Narrow one source disposition to its retained context after all moved fold
 * rows have been retargeted. The row ID and all non-range fields are stable. */
extern BOOL DSL_FHE_Plan_Specialize_Disposition_BN_Range
                (DSL_FHE_CONVERSION_DISPOSITION_ID id,
                 DSL_FHE_BN_FOLD_PROVENANCE_ID expected_first,
                 UINT32 expected_count,
                 DSL_FHE_BN_FOLD_PROVENANCE_ID first,
                 UINT32 count);

/* Move one complete composite-ReLU context to a cloned definition. The
 * operation appends the clone disposition/association and preserves the IDs
 * of the range, six context states, and six materialization operations. */
extern BOOL DSL_FHE_Plan_Specialize_Composite_Context
                (const DSL_FHE_CONTEXT_RANGE_RECORD *expected_range,
                 ST_IDX owner_pu_st, DSL_IR_NODE_ID relu_node_id,
                 DSL_IR_VALUE_ID relu_value_id,
                 DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id,
                 DSL_FHE_CKKS_VALUE_STATE_ID result_ckks_value_state_id);

#endif /* fhe_plan_specialize_internal_INCLUDED */
