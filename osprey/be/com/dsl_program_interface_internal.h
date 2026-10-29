/*
 * Copyright (C) 2026 Open64 Project
 *
 * Process-local commit state for staged program-interface transactions.
 * The state keeps per-PU reader validation aligned with PU-scoped mutation;
 * it is not part of the mapped WHIRL image. See
 * doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md.
 */

#ifndef dsl_program_interface_internal_INCLUDED
#define dsl_program_interface_internal_INCLUDED

#include "dsl_ir_image.h"

/* Clear all process-local plan and PU commit evidence. */
extern void DSL_Program_Interface_Reset_Commit_State (void);

/* Mark a successfully loaded mapped interface as fully committed. */
extern void DSL_Program_Interface_Mark_Mapped_Committed (void);

/* Test whether reader-side physical validation applies to this PU now. */
extern BOOL DSL_Program_Interface_PU_Is_Committed (ST_IDX owner_pu_st);

#endif /* dsl_program_interface_internal_INCLUDED */
