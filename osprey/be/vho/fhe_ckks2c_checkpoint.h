/*
 * Copyright (C) 2026 Open64 Project
 *
 * All-PU CKKS2C collection session for the existing conversion checkpoint.
 * The session preserves active-PU ownership while collecting, then delegates
 * deterministic auxiliary staging to fhe_ckks2c_publish.  It owns no driver
 * option and no WHIRL mutation.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D3.
 */

#ifndef fhe_ckks2c_checkpoint_INCLUDED
#define fhe_ckks2c_checkpoint_INCLUDED

#include <stdint.h>
#include <stdio.h>

/* Collect the current active PU when a CKKS event image and conversion
 * checkpoint are present.  Repeated/mismatched PUs make the session terminal. */
bool VHO_FHE_CKKS2C_Checkpoint_Collect_Active_PU(
    uint32_t owner_pu_st, const char *binary_final_path, FILE *diagnostic);

/* Prove expected all-PU coverage and stage every CKKS2C auxiliary inside the
 * active shared checkpoint.  An inactive session is a valid no-op. */
bool VHO_FHE_CKKS2C_Checkpoint_Finalize(
    uint32_t expected_pu_count, FILE *diagnostic);

/* Clear all process-local collection and publication state after either
 * successful checkpoint completion or rollback. */
void VHO_FHE_CKKS2C_Checkpoint_Complete(bool committed);

#endif /* fhe_ckks2c_checkpoint_INCLUDED */
