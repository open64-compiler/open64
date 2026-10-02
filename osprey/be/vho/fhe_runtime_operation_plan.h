/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: collect one complete FHE SYNC-5 operation-lowering transaction for
 * an active PU, then submit it through the generic DSL standard-WHIRL service.
 * The plan is process-local and never changes the mapped WHIRL contract.
 *
 * Design references:
 *   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-F
 *   doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 */

#ifndef fhe_runtime_operation_plan_INCLUDED
#define fhe_runtime_operation_plan_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "fhe_standard_whirl.h"

struct pu_info;

typedef struct {
    UINT32 computed_count;
    UINT32 promoted_source_count;
    UINT32 unpromoted_source_count;
    UINT32 live_unpromoted_source_count;
    UINT32 selector_count;
    UINT32 evaluation_count;
    UINT32 standard_call_count;
    UINT32 output_handle_count;
    UINT32 status_check_count;
} VHO_FHE_RUNTIME_OPERATION_PLAN_RESULT;

/* Release detached blocks and invalidate the borrowed request view. */
extern void VHO_FHE_Runtime_Operation_Plan_Reset (void);

/* Build every admitted request while the exact owner PU is active. */
extern BOOL VHO_FHE_Runtime_Operation_Plan_Prepare_PU
                                (struct pu_info *pu_info,
                                 VHO_FHE_STANDARD_FAILURE_BUILDER
                                     build_failure,
                                 void *failure_context,
                                 FILE *diagnostic,
                                 VHO_FHE_RUNTIME_OPERATION_PLAN_RESULT
                                     *result);

/* Borrow the immutable complete request array until Apply_PU or Reset. */
extern BOOL VHO_FHE_Runtime_Operation_Plan_Get
                                (const DSL_IR_NATIVE_VALUE_LOWER_REQUEST
                                     **requests,
                                 UINT32 *request_count);

/* Atomically replace every prepared definition in the same active PU. */
extern BOOL VHO_FHE_Runtime_Operation_Plan_Apply_PU
                                (struct pu_info *pu_info,
                                 FILE *diagnostic,
                                 DSL_IR_NATIVE_VALUE_LOWER_RESULT *results);

#endif /* fhe_runtime_operation_plan_INCLUDED */
