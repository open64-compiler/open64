/* Copyright (C) 2026 Open64 Project */
/* VHO H4 pure staged-GEMM work projection, shared by search and fixed-winner
 * validation. No native types/symbols/trees or hardware observation is used.
 * Design: doc/AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md. */
#include "dsl_gemm_hierarchy.h"

/* Derive A hoisting, B reload/retention and cooperative-copy work from the
 * executable ownership map. Tail padding is paid in the model, never loaded
 * from inactive global addresses by this service. Refuse every overflow. */
bool VHO_DSL_gemm_hierarchy_work(uint64_t bm,uint64_t bn,uint64_t bk,
    uint64_t tm,uint64_t tn,uint64_t k,bool retain_b,DSL_GEMM_HIERARCHY_WORK &out,std::string &reason)
{
    out=DSL_GEMM_HIERARCHY_WORK();
    if(!bm || !bn || !bk || !tm || !tn || !k || bm%tm || bn%tn ||
        (retain_b && (tm<2 || tn>32))) {reason="h4_geometry";return false;}
    uint64_t padded,threads,a,b,copy,ops;
    DSL_GEMM_HIERARCHY_WORK work;
    if(!DSL_movement_mul(k/bk+(k%bk!=0),bk,padded) ||
        !DSL_movement_mul(bm/tm,bn/tn,threads) || !threads || threads>1024 ||
        !DSL_movement_mul(threads,tm,a) || !DSL_movement_mul(a,padded,a) ||
        !DSL_movement_mul(threads,tn,b) || !DSL_movement_mul(b,retain_b?1:tm,b) ||
        !DSL_movement_mul(b,padded,b) || !DSL_movement_add(a,b,work.shared_reads) ||
        !DSL_movement_mul(work.shared_reads,4,work.shared_reads) ||
        !DSL_movement_add(bm,bn,copy) || !DSL_movement_mul(copy,padded,copy) ||
        !DSL_movement_mul(copy,4,work.shared_writes) ||
        !DSL_movement_mul(bm,bn,ops) || !DSL_movement_mul(ops,padded,ops) ||
        !DSL_movement_mul(ops,2,work.scalar_ops)) {reason="h4_work_overflow";return false;}
    work.b_loads=b;work.extra_registers=retain_b?tn:0;
    out=work;reason="padded-execution-envelope";return true;
}
