/* Copyright (C) 2026 Open64 Project */
/* VHO H2 projection of a validated common F32 GEMM site and locality census.
 * Captures source geometry once before joint movement search; no selection.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#ifndef dsl_movement_gemm_INCLUDED
#define dsl_movement_gemm_INCLUDED
#include "dsl_tile_candidate.h"
#include "dsl_tensor_locality.h"
#include "../com/dsl_movement_facts.h"
#include "dsl_fetch_pipeline_opt.h"
#include "dsl_movement_cost.h"
class WN;
/* Caller supplies its admitted active-PU common site and source/baseline tile.
 * Native joins and dense F32 source are checked before owned publication.
 * Missing locality keeps alias/reuse unknown; failure clears facts only. */
extern BOOL VHO_DSL_capture_gemm_movement(WN *,const DSL_TILE_SITE_RECORD &,
    const DSL_TILE_PLAN_RECORD &,const DSL_TENSOR_LOCALITY_ANALYSIS *,
    DSL_MOVEMENT_FACTS &,std::string &);
/* Project an existing admitted SIMT tile/fetch recipe into H3 physical work.
 * The owning selector supplies legality/capability; no native state is read. */
extern bool VHO_DSL_gemm_h3_plan(const DSL_TILE_PLAN_RECORD &,const DSL_FETCH_PLAN_RECORD &,
    const DSL_HARDWARE_DESCRIPTION_RECORD &,DSL_H3_PLAN &,std::string &);
#endif
