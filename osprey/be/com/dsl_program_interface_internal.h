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

/* Clear the prepared key, committed-PU list and mapped-eligibility flag at a
 * caller-controlled program/image boundary. No persisted rows or WNs change;
 * this is invalidation, not restoration of earlier commit evidence. */
extern void DSL_Program_Interface_Reset_Commit_State (void);

/* Set mapped eligibility for every owner after successful image loading. This
 * setter performs no structural/physical verification and clears neither the
 * prepared key nor the per-PU list; caller owns loading and lifetime ordering. */
extern void DSL_Program_Interface_Mark_Mapped_Committed (void);

/* Report global mapped eligibility or exact membership in the committed-PU
 * list. No owner/ST validation, physical-correctness proof, mutation or PU switch;
 * callers use this only to decide whether to run the real physical verifier. */
extern BOOL DSL_Program_Interface_PU_Is_Committed (ST_IDX owner_pu_st);

#endif /* dsl_program_interface_internal_INCLUDED */
