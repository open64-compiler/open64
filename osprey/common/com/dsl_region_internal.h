/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private REGION interface transactions used by generic compiler-owned IR
 * rewrites. These services are not frontend APIs. See
 * doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md.
 */

#ifndef dsl_region_internal_INCLUDED
#define dsl_region_internal_INCLUDED

#include "dsl_region.h"

/* Count all managed REGION interface rows that refer to one symbol. */
extern UINT32 DSL_Region_Symbol_Use_Count (PU_Info *pu, ST_IDX st);

/* Prove that removing the exact plain-input symbol set preserves the store. */
extern BOOL DSL_Region_Can_Prune_Input_Symbols
                                (PU_Info *pu, const ST_IDX *symbols,
                                 UINT32 symbol_count);

/* Commit a previously preflighted complete input-pruning transaction. */
extern BOOL DSL_Region_Prune_Input_Symbols
                                (PU_Info *pu, const ST_IDX *symbols,
                                 UINT32 symbol_count);

#endif /* dsl_region_internal_INCLUDED */
