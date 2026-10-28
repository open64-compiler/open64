/* Active-PU MMA3 typed scalar baseline. Caller-owned bindings, canonical WN
 * construction and postchecks; no frontend, target MMA or selector changes.
 * Design: doc/AI-COMPILER-MMA3-TYPED-SIMT-BASELINE.md. */
#ifndef dsl_typed_simt_opt_INCLUDED
#define dsl_typed_simt_opt_INCLUDED

#include "dsl_typed_simt.h"
#include "srcpos.h"
class WN;
struct ST;

typedef enum {
    VHO_DSL_TYPED_SIMT_SERIAL = 1,
    VHO_DSL_TYPED_SIMT_GRID = 2
} VHO_DSL_TYPED_SIMT_EXECUTION;

typedef struct {
    ST *input_a, *input_b, *output;
    ST *row, *column, *index;
    ST *raw, *fraction, *exponent, *wide;
    ST *a_value, *b_value, *product, *accumulator;
    ST *thread_idx, *block_idx;
    SRCPOS source_position;
} VHO_DSL_TYPED_SIMT_BINDING;

/* Construct an owned detached serial reference or direct-SIMT body in the
 * current PU. U2 input pointers represent the accepted F16/BF16 source;
 * output is F4. Private product/accumulator symbols must be volatile F4.
 * Caller guarantees naturally aligned, sufficiently sized buffers, disjoint
 * output and round-to-nearest-even/gradual-underflow execution. GRID launch
 * uses exactly threads_per_cta X threads, Y/Z thread dimensions one, and Z
 * grid dimension one; X/Y blocks cover ceil(N/threads_per_cta) by M outputs.
 * Insertion/publication belongs to the caller; failure preserves *block. */
extern BOOL VHO_DSL_typed_simt_materialize(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_TYPED_SIMT_BINDING *binding,
    UINT32 execution, WN **block, FILE *diagnostic);

/* Revalidate exact geometry, numerical steps and source/storage identities
 * after edits. Borrow the body; never rewrite it or caller-owned symbols. */
extern BOOL VHO_DSL_typed_simt_verify(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_TYPED_SIMT_BINDING *binding,
    UINT32 execution, WN *block, FILE *diagnostic);

/* Print versioned source/action evidence only after structural verification;
 * bad inputs produce no stream. No numerical/GPU timing claim is made. */
extern BOOL VHO_DSL_typed_simt_print(FILE *file, const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_TYPED_SIMT_BINDING *binding,
    UINT32 execution, WN *block);

#endif
