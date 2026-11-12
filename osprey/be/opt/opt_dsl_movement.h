/* Copyright (C) 2026 Open64 Project */
/* WOPT active-PU H2 projections over existing LN2 and ordered-reduction
 * bindings. No new matcher, algebra, numerical permission or executable plan.
 * The caller recaptures native source before applying any schedule.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#ifndef opt_dsl_movement_INCLUDED
#define opt_dsl_movement_INCLUDED
#include "opt_dsl_normalization_plan.h"
#include "opt_dsl_reduction_staged.h"
#include "../com/dsl_movement_facts.h"
/* Fresh native/source/target/storage/event admission, then owned source counts.
 * Failure clears facts and preserves the ordinary source/fallback. O3 only;
 * source and common tables are read, never rewritten. */
extern BOOL WOPT_DSL_capture_normalization_movement(WN *,WN *,
    const WOPT_DSL_NORMALIZATION_PLAN_REQUEST *,DSL_MOVEMENT_FACTS &,std::string &);
/* Capture the exact ascending F32 source fold and canonical range identities.
 * No tree reduction, staging, spill-free or hardware traffic claim is added. */
extern BOOL WOPT_DSL_capture_reduction_movement(WN *,WN *,
    const WOPT_DSL_REDUCTION_DIRECT_REQUEST *,DSL_MOVEMENT_FACTS &,std::string &);
/* Add the existing verified one-slot copy/read/release generation contract.
 * Tails describe only active rows/elements; no inactive access is synthesized. */
extern BOOL WOPT_DSL_capture_staged_movement(WN *,WN *,
    const WOPT_DSL_REDUCTION_STAGED_REQUEST *,DSL_MOVEMENT_FACTS &,std::string &);
#endif
