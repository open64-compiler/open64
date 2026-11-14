/* Copyright (C) 2026 Open64 Project */
/* Phase-owned Q1 invalidation/rebinding session around real PREOPT calls.
 * Design: doc/AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md. No native handle is kept
 * in owned facts and no candidate is explored or transformed by this session. */
#ifndef dsl_q1_preopt_INCLUDED
#define dsl_q1_preopt_INCLUDED
#include "dsl_high_quantized.h"
class DSL_Q1_PREOPT_SESSION {
    bool _active;
    unsigned _stage;
    VHO_TENSOR_PROGRAM _program;
    DSL_HIGH_EPOCH _epoch;
    DSL_HIGH_QUANTIZED_FACTS _facts;
public:
    /* Identify only the active whole-function persistent Q1 binding. */
    explicit DSL_Q1_PREOPT_SESSION(WN *);
    /* Owned facts expire before the enclosing phase/PU lifetime ends. */
    ~DSL_Q1_PREOPT_SESSION();
    /* False preserves ordinary PREOPT/LNO behavior and no Q1 authority. */
    bool Active() const {return _active;}
    /* Snapshot and invalidate before PREOPT changes any WN or analysis. */
    void Before(WN *);
    /* Restore explanations and recapture actual canonical sites, using live
     * DU only synchronously. Failure is an analysis refusal, not compilation. */
    void After(WN *,const DSL_HIGH_Q1_RESOLVER & = DSL_HIGH_Q1_RESOLVER());
    /* Call before subsequent optimizer mutation; old facts never stay live. */
    void Invalidate();
};
#endif
