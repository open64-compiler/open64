/* Copyright (C) 2026 Open64 Project */
/* LNO H2 projection of existing MR1 owned High access/alias/version proofs.
 * No native handles, new schedule, cache-hit credit or grouped realization.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#ifndef dsl_high_movement_INCLUDED
#define dsl_high_movement_INCLUDED
#include "dsl_high_reuse.h"
#include "../com/dsl_movement_facts.h"
/* Capture only after the snapshot/recipe and MR1 proof are current. Atomic
 * refusal clears output and retains the existing bounded G1 fallback. */
extern bool LNO_DSL_capture_movement(const DSL_HIGH_SNAPSHOT &,const DSL_HIGH_EPOCH &,
    const DSL_HIGH_BINDINGS &,const VHO_TENSOR_PROGRAM &,const DSL_HIGH_REUSE_FACTS &,
    DSL_MOVEMENT_FACTS &,std::string &);
#endif
