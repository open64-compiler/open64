/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: construct and retain the complete FHE-owned SYNC-5 program and
 * runtime interface request arrays before any PU is mutated. The module maps
 * persisted DSL/FHE semantic identities to the generic common/com interface
 * transaction; common/com remains the sole owner of validation and commit.
 *
 * Compilation scope: VHO FHE semantic runtime lowering, whole program.
 * Compatibility boundary: this planning view is process-local and adds no
 * WHIRL opcode, type kind, mapped-image row, or runtime ABI symbol.
 *
 * Design references:
 *   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-D
 *   doc/FHE-SYNC5-PROGRAM-INTERFACE-CONTRACT.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 */

#ifndef fhe_runtime_interface_plan_INCLUDED
#define fhe_runtime_interface_plan_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"

struct pu_info;

typedef struct {
    UINT32 retired_formal_count;
    UINT32 retired_call_argument_count;
    UINT32 runtime_input_count;
    UINT32 runtime_input_binding_count;
    UINT32 runtime_input_call_count;
    UINT32 value_projection_count;
    UINT32 call_projection_count;
    UINT64 fingerprint;
} VHO_FHE_RUNTIME_INTERFACE_PLAN_SUMMARY;

/*
 * Discard every process-local request and borrowed plan view. Call this at a
 * new program boundary and on every failed or completed checkpoint path.
 */
extern void VHO_FHE_Runtime_Interface_Plans_Reset (void);

/*
 * Build and common-prevalidate the complete immutable request family while
 * the unique root PU is active. The active root supplies owner-safe external
 * tensor/TCON and source-position evidence; no WN, ST, or image row is
 * committed by this operation.
 */
extern BOOL VHO_FHE_Runtime_Interface_Plans_Prepare
                                (struct pu_info *root_pu,
                                 FILE *diagnostic,
                                 VHO_FHE_RUNTIME_INTERFACE_PLAN_SUMMARY
                                     *summary);

/*
 * Return borrowed views of the prepared arrays. Their pointers remain valid
 * until Reset or the next Prepare and must never be retained across either.
 */
extern BOOL VHO_FHE_Runtime_Interface_Plans_Get
                                (DSL_PROGRAM_INTERFACE_PLAN *program_plan,
                                 DSL_RUNTIME_INTERFACE_PLAN *runtime_plan);

/*
 * Apply the retained plan to one active PU exactly once. A failed apply is
 * terminal for this in-memory image; the enclosing checkpoint must abort.
 */
extern BOOL VHO_FHE_Runtime_Interface_Plans_Apply_PU
                                (struct pu_info *pu, FILE *diagnostic,
                                 DSL_PROGRAM_INTERFACE_RESULT *result);

/* Verify all planned PUs and persisted interface rows after the last apply. */
extern BOOL VHO_FHE_Runtime_Interface_Plans_Verify_Complete
                                (FILE *diagnostic);

#endif /* fhe_runtime_interface_plan_INCLUDED */
