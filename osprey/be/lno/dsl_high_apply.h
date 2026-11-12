/* Copyright (C) 2026 Open64 Project */
/* LNO-owned selected-only transaction, separate from the detached observer.
 * Design: doc/AI-COMPILER-SEARCH-M4-ATOMIC-HIGH-v1.0.md. */
#ifndef dsl_high_apply_INCLUDED
#define dsl_high_apply_INCLUDED
#include "dsl_high_observer.h"
/* Revalidate the selected winner at the live active-PU boundary. Stage one
 * High algorithm, rebuild all analyses, then atomically replace root/managers.
 * Refusal/rollback returns false and retains the original tree and analyses;
 * epoch remains expired. The caller must bypass competing LNO on success. */
extern bool LNO_DSL_apply_selected_high(WN *&,DU_MANAGER *&,ALIAS_MANAGER *&,
    DSL_HIGH_EPOCH &,const DSL_HIGH_BINDINGS &,const DSL_HIGH_SNAPSHOT &,
    const DSL_HIGH_SEARCH_INPUT &,const DSL_HIGH_SEARCH_REPORT &,FILE *);
#endif
