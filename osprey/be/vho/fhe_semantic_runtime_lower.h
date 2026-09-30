/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: expose the FHE-owned, active-PU interface that resolves certified
 * DSL values and program-input roles to exact runtime handles, then constructs
 * detached standard-WHIRL call sequences for the stable FHE C ABI. Persistent
 * WN/DSL-image mutation remains owned by reviewed common/com transactions.
 *
 * Compilation scope: VHO FHE semantic runtime lowering, one active PU.
 * Compatibility boundary: this interface does not change WHIRL opcodes, TY
 * encodings, mapped-image rows, or public runtime ABI declarations.
 *
 * Design references:
 *   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md
 *   doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md
 *   doc/FHE-SYNC5-RUNTIME-ENTRY-BINDING-GAP.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 *   doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md
 */

#ifndef fhe_semantic_runtime_lower_INCLUDED
#define fhe_semantic_runtime_lower_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "fhe_standard_whirl.h"

class WN;
struct pu_info;

typedef struct {
    ST_IDX owner_pu_st;
    ST_IDX handle_st;
    TY_IDX handle_ty;
    DSL_IR_VALUE_ID source_value_id;
    DSL_RUNTIME_INPUT_ID runtime_input_id;
    UINT32 binding_kind;
} VHO_FHE_RUNTIME_HANDLE_BINDING;

typedef struct {
    WN *block;
    ST_IDX output_st;
    UINT32 standard_call_count;
    UINT32 output_handle_count;
    UINT32 status_check_count;
} VHO_FHE_RUNTIME_CALL_SEQUENCE;

extern void VHO_FHE_Runtime_Call_Sequence_Init
                                (VHO_FHE_RUNTIME_CALL_SEQUENCE *sequence);
extern BOOL VHO_FHE_Runtime_Resolve_Value_Handle
                                (struct pu_info *pu_info,
                                 DSL_IR_VALUE_ID source_value_id,
                                 FILE *diagnostic,
                                 VHO_FHE_RUNTIME_HANDLE_BINDING *binding);
extern BOOL VHO_FHE_Runtime_Resolve_Role_Handle
                                (struct pu_info *pu_info,
                                 const char *semantic_role,
                                 FILE *diagnostic,
                                 VHO_FHE_RUNTIME_HANDLE_BINDING *binding);
extern BOOL VHO_FHE_Runtime_Build_Bootstrap_Sequence
                                (struct pu_info *pu_info,
                                 DSL_IR_VALUE_ID anchor_value_id,
                                 UINT32 static_ordinal,
                                 SRCPOS source_position,
                                 VHO_FHE_STANDARD_FAILURE_BUILDER
                                     build_failure,
                                 void *failure_context,
                                 FILE *diagnostic,
                                 VHO_FHE_RUNTIME_CALL_SEQUENCE *sequence);
extern BOOL VHO_FHE_Runtime_Build_Relu_Sequence
                                (struct pu_info *pu_info,
                                 DSL_IR_VALUE_ID anchor_value_id,
                                 UINT32 first_static_ordinal,
                                 SRCPOS source_position,
                                 VHO_FHE_STANDARD_FAILURE_BUILDER
                                     build_failure,
                                 void *failure_context,
                                 FILE *diagnostic,
                                 VHO_FHE_RUNTIME_CALL_SEQUENCE *sequence);
extern BOOL VHO_FHE_Runtime_Finalize_Projected_Output
                                (const VHO_FHE_RUNTIME_HANDLE_BINDING *output,
                                 SRCPOS source_position,
                                 VHO_FHE_RUNTIME_CALL_SEQUENCE *sequence,
                                 WN **result_definition);

#endif /* fhe_semantic_runtime_lower_INCLUDED */
