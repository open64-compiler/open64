/* Active-PU, explicitly requested MMA4-H lowering for one Hopper warpgroup.
 * Reuses the verified MMA3 SIMT source and standard extended-assembly WNs;
 * never changes default selection or binary representation.
 * Design: doc/AI-COMPILER-MMA4-H-HOPPER-LOWERING.md. */
#ifndef dsl_wgmma_opt_INCLUDED
#define dsl_wgmma_opt_INCLUDED

#include "dsl_typed_simt_opt.h"
#include "dsl_compute_realization.h"

enum {
    VHO_DSL_WGMMA_ACCEPT_ORDER = 1,
    VHO_DSL_WGMMA_ACCEPT_ROUNDING = 2,
    VHO_DSL_WGMMA_ACCEPT_SUBNORMAL = 4,
    VHO_DSL_WGMMA_ACCEPT_NONFINITE = 8,
    VHO_DSL_WGMMA_ACCEPT_CONTRACTION = 16,
    VHO_DSL_WGMMA_ACCEPT_ISA = 31
};

typedef struct {
    UINT32 enable;
    UINT32 profile_id;
    UINT32 arch_id;
    UINT32 ptx;
    UINT32 toolkit;
    UINT32 acceptance_flags;
    UINT32 shared_limit_bytes;
    UINT32 reserved;
} VHO_DSL_WGMMA_CONTROL;

typedef struct {
    VHO_DSL_TYPED_SIMT_BINDING simt;
    ST *shared_a, *shared_b, *fragment;
    ST *tid, *copy, *base, *row, *column;
    ST *block_dim, *grid_dim, *barrier;
} VHO_DSL_WGMMA_BINDING;

/* Explicit O3-only transformation in the current PU. Verify the borrowed
 * MMA3 GRID source and exact target/acceptance before allocation. Caller owns
 * all bound symbols, launch/dispatch and insertion of the detached body.
 * Disabled/rejected requests preserve *body and the source; no fallback is
 * silently launched. Launch exactly 128x1x1 threads and ceil(N/64)x(M/64)x1
 * blocks on sm_90a with >=4096 shared bytes and aligned disjoint buffers. */
extern BOOL VHO_DSL_wgmma_materialize(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_WGMMA_CONTROL *control,
    const VHO_DSL_WGMMA_BINDING *binding, WN *simt_body, WN **body, FILE *diagnostic);

/* Revalidate exact body/target/geometry/acceptance/effects without mutating
 * caller trees, symbols or tables. Reject stale or changed executable work. */
extern BOOL VHO_DSL_wgmma_verify(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_WGMMA_CONTROL *control,
    const VHO_DSL_WGMMA_BINDING *binding, WN *body, FILE *diagnostic);

/* Report executable operations only after full structural verification.
 * Failure emits nothing; native GPU correctness/resources/timing stay unknown. */
extern BOOL VHO_DSL_wgmma_print(FILE *file, const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_WGMMA_CONTROL *control,
    const VHO_DSL_WGMMA_BINDING *binding, WN *body);

#endif
