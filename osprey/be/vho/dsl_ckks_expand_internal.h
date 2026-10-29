/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private journals and commit helpers for one owner-PU CKKS expansion of native
 * DSL values. The owning transaction establishes scope and image lifetime and
 * coordinates physical, string, symbol, event and core-table rollback. These
 * are not frontend or persistent-edit APIs and add no mapped WHIRL format.
 * Design: doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#ifndef dsl_ckks_expand_internal_INCLUDED
#define dsl_ckks_expand_internal_INCLUDED

#include "../../common/ir_image/dsl_image_checkpoint_internal.h"
#include "../../common/util/dsl_strtab_checkpoint.h"

/* Truncate only the event-table suffix to a genuine same-image watermark;
 * caller preserves prefixes and coordinates other rollback. No event
 * reconstruction or status is provided. */
extern void DSL_CKKS_Event_Image_Trim (UINT32 record_count);

/* Focused transaction fault injection; zero disables the test hook. */
extern void DSL_CKKS_Expand_Set_Test_Fault (UINT32 stage);

#endif /* dsl_ckks_expand_internal_INCLUDED */
