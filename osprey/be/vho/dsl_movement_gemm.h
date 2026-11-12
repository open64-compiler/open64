/* Copyright (C) 2026 Open64 Project */
/* VHO H2 projection of a validated common F32 GEMM site and locality census.
 * Captures source geometry once before joint movement search; no selection.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#ifndef dsl_movement_gemm_INCLUDED
#define dsl_movement_gemm_INCLUDED
#include "dsl_tile_candidate.h"
#include "dsl_tensor_locality.h"
#include "../com/dsl_movement_facts.h"
class WN;
/* Caller supplies its admitted active-PU common site and source/baseline tile.
 * Native joins and dense F32 source are checked before owned publication.
 * Missing locality keeps alias/reuse unknown; failure clears facts only. */
extern BOOL VHO_DSL_capture_gemm_movement(WN *,const DSL_TILE_SITE_RECORD &,
    const DSL_TILE_PLAN_RECORD &,const DSL_TENSOR_LOCALITY_ANALYSIS *,
    DSL_MOVEMENT_FACTS &,std::string &);
#endif
