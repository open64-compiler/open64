/* Copyright (C) 2026 Open64 Project */
/* VHO H4 detached GEMM consumer-work contract. Counts describe an explicit
 * padded CTA execution envelope, never H2 source bytes or actual GPU spills.
 * No native context, allocation, probing or transformation is accepted.
 * Design: doc/AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md. */
#ifndef dsl_gemm_hierarchy_INCLUDED
#define dsl_gemm_hierarchy_INCLUDED
#include "../com/dsl_movement_facts.h"

struct DSL_GEMM_HIERARCHY_WORK {
    uint64_t shared_reads,shared_writes,scalar_ops,b_loads,extra_registers;
    /* Empty work grants no cost or materialization permission. */
    inline DSL_GEMM_HIERARCHY_WORK() : shared_reads(0),shared_writes(0),scalar_ops(0),b_loads(0),extra_registers(0) {}
};
/* Count actual staged loop ownership over padded K. Retained B requires more
 * than one row/thread and at most32 scalar columns. Checked failure clears
 * output; counts do not prove source alias/lifetime or device feasibility. */
extern bool VHO_DSL_gemm_hierarchy_work(uint64_t bm,uint64_t bn,uint64_t bk,
    uint64_t tm,uint64_t tn,uint64_t k,bool retain_b,DSL_GEMM_HIERARCHY_WORK &,std::string &);
#endif
