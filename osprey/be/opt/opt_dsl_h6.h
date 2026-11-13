/* Copyright (C) 2026 Open64 Project */
/* WOPT-owned H6 fixed memory-hierarchy plans. Source capture/preparation are
 * active-PU operations; search accepts only owned H2/H3 records. The driver
 * owns fresh device PU selection and runtime allocation/lease/launch authority.
 * No mapped/runtime ABI change. Design: doc/AI-COMPILER-H6-NORM-REDUCTION-v1.0.md. */
#ifndef opt_dsl_h6_INCLUDED
#define opt_dsl_h6_INCLUDED
#include "opt_dsl_movement.h"
#include "opt_dsl_normalization_lower.h"
/* Declared RF planning estimate; selected ptxas/resource qualification remains
 * independent. The compiled initial panel requires 26 registers, not 16. */
enum { WOPT_DSL_H6_REDUCTION_REGISTERS = 32 };
struct WOPT_DSL_H6_CANDIDATE {
    UINT32 kind,threads,chunk;
    DSL_H3_PLAN recipe;
    DSL_H3_ESTIMATE estimate;
    /* Detached specifications confer no authority until complete validation. */
    inline WOPT_DSL_H6_CANDIDATE() : kind(0),threads(0),chunk(0) {}
};
struct WOPT_DSL_H6_SELECTION {
    DSL_MOVEMENT_FACTS source;
    std::string controls,model;
    std::vector<WOPT_DSL_H6_CANDIDATE> candidates;
    UINT32 winner;
    bool complete;
    /* Missing budgets/evidence publish no selected implementation. */
    inline WOPT_DSL_H6_SELECTION() : winner(0),complete(false) {}
};
struct WOPT_DSL_H6_REDUCTION {
    OPEN64_REDUCTION_GUARD source_guard;
    DSL_GPU_PLAN_SNAPSHOT device;
    std::string model,binding;
    UINT64 rows,width,threads,chunk,kind,seal;
    /* No runtime ticket, registry pointer, native row or compiler analysis. */
    inline WOPT_DSL_H6_REDUCTION() : rows(0),width(0),threads(0),chunk(0),kind(0),seal(0) {memset(&source_guard,0,sizeof(source_guard));}
};
/* Native harvest is outside estimation. Require O3, imported H20, H2 proof and
 * the driver's finite H3 request owner. Refusal preserves source and fallback. */
extern BOOL WOPT_DSL_select_h6_normalization(WN *,WN *,const WOPT_DSL_NORMALIZATION_COST_REQUEST *,WOPT_DSL_H6_SELECTION &,FILE *);
/* Fresh source/control/profile/model equality and fixed-only re-estimation;
 * prepare one LN4 handoff without reranking or materializing any alternative. */
extern BOOL WOPT_DSL_prepare_h6_normalization(WN *,WN *,const WOPT_DSL_NORMALIZATION_COST_REQUEST *,const WOPT_DSL_H6_SELECTION &,WOPT_DSL_NORMALIZATION_LOWERING &);
/* Same native harvest/fixed-plan contract for ordered F32 Reduction; unknown
 * source versions/effects/strides/leases cannot grant cooperative execution. */
extern BOOL WOPT_DSL_select_h6_reduction(WN *,WN *,const WOPT_DSL_REDUCTION_DIRECT_REQUEST *,WOPT_DSL_H6_SELECTION &,FILE *);
/* Revalidate one winner and preserve the original allocation/lease/event
 * obligations. Cooperative launch is a NEW explicit H6 host obligation, not
 * falsely discharged by the old row-thread runtime guard. */
extern BOOL WOPT_DSL_prepare_h6_reduction(WN *,WN *,const WOPT_DSL_REDUCTION_DIRECT_REQUEST *,const WOPT_DSL_H6_SELECTION &,WOPT_DSL_H6_REDUCTION &);
/* Fill only the driver's active empty kernel PU. Strict F32 ordered add,
 * guarded cooperative staging, publication/release and COMMENT are native WN.
 * Stale/malformed inputs decline before allocation; allocation failure fatal. */
extern BOOL WOPT_DSL_materialize_h6_reduction(const WOPT_DSL_H6_REDUCTION &,SRCPOS,std::string &);
/* Read-only native body/declaration and launch identity, valid after mapped
 * reopening; no metadata-only certificate or native mutation. */
extern std::string WOPT_DSL_h6_reduction_native_key(WN *);
/* Full selected native identity equality, never a legality substitute. */
extern BOOL WOPT_DSL_verify_h6_reduction(WN *,const std::string &);
/* Bind complete selected fields, used by preflight and independent tests;
 * corruption detector only, never an adversarial authorization mechanism. */
extern UINT64 WOPT_DSL_h6_reduction_seal(const WOPT_DSL_H6_REDUCTION &);
#endif
