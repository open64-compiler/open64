/* Copyright (C) 2026 Open64 Project */
/* LNO-owned canonical High loop/memory admission under active-PU lifetime.
 * Design: doc/AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md.
 * Read-only structural/coverage checks; no snapshot, search or scheduling proof. */
#ifndef dsl_high_admission_INCLUDED
#define dsl_high_admission_INCLUDED
#include "defs.h"
class WN;
class DU_MANAGER;
struct ALIAS_MANAGER;
class ARRAY_DIRECTED_GRAPH16;
/* Consume current PREOPT/LNO facts synchronously. Reject unsupported scope,
 * unsigned/nonconstant loops, opaque memory or incomplete graph coverage.
 * No source/tables/maps are mutated; refusal retains ordinary LNO behavior.
 * A positive result grants form admission only, never reassociation/parallelism. */
extern BOOL LNO_DSL_admit_common_high(WN *, DU_MANAGER *, ALIAS_MANAGER *,
    ARRAY_DIRECTED_GRAPH16 *, FILE *);
#endif
