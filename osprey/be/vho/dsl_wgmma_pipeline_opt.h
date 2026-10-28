/* VHO active-PU MMA5-H two-slot completion and joint-candidate contracts.
 * Explicit lowering only; SIMT selection and binary WHIRL remain unchanged.
 * Design: doc/AI-COMPILER-MMA5-H-HOPPER-PIPELINE.md. */
#ifndef dsl_wgmma_pipeline_opt_INCLUDED
#define dsl_wgmma_pipeline_opt_INCLUDED
#include "dsl_wgmma_opt.h"
#include "dsl_compute_model.h"

typedef enum {
    VHO_DSL_WGMMA_COOPERATIVE = 1,
    VHO_DSL_WGMMA_TMA_BULK = 2
} VHO_DSL_WGMMA_DELIVERY;

typedef struct {
    VHO_DSL_WGMMA_CONTROL compute;
    UINT32 delivery;
    UINT32 outstanding_groups;
    UINT32 reserved;
} VHO_DSL_WGMMA_PIPELINE_CONTROL;

typedef struct {
    VHO_DSL_WGMMA_BINDING compute;
    ST *raw_a, *raw_b, *ready;
} VHO_DSL_WGMMA_PIPELINE_BINDING;

typedef struct {
    UINT32 family, delivery, slots, groups, shared_bytes, known, eligible;
    uint64_t rounded_registers, rounded_shared, resident, limiting;
    uint64_t kernel[3], invocation[3];
    uint64_t terms[3][8];
} VHO_DSL_WGMMA_PIPELINE_CANDIDATE;

typedef struct {
    VHO_DSL_WGMMA_PIPELINE_CANDIDATE candidates[4];
    UINT32 nominal_candidate, bounded_candidate, emitted_family;
} VHO_DSL_WGMMA_PIPELINE_RESULT;

/* Explicit O3 request in the active PU; borrowed SIMT source is verified first.
 * Shared A/B each hold 2048 U2 values, fragment holds 32 F4 values. TMA also
 * requires disjoint raw A/B (2048 U2 each) and ready (two U8), all aligned 16.
 * Every failure preserves *body. No launch or automatic selection is implied. */
extern BOOL VHO_DSL_wgmma_pipeline_materialize(const DSL_TYPED_SIMT_RECORD *,
    const DSL_NUMERICAL_CONTRACT_RECORD *, const VHO_DSL_WGMMA_PIPELINE_CONTROL *,
    const VHO_DSL_WGMMA_PIPELINE_BINDING *, WN *simt_body, WN **body, FILE *);

/* Check the exact executable tree, effects, identities and completion protocol
 * without changing it or creating symbols; stale or altered bodies reject. */
extern BOOL VHO_DSL_wgmma_pipeline_verify(const DSL_TYPED_SIMT_RECORD *,
    const DSL_NUMERICAL_CONTRACT_RECORD *, const VHO_DSL_WGMMA_PIPELINE_CONTROL *,
    const VHO_DSL_WGMMA_PIPELINE_BINDING *, WN *, FILE *);

/* Print actual action/lifetime evidence only after structural verification. */
extern BOOL VHO_DSL_wgmma_pipeline_print(FILE *, const DSL_TYPED_SIMT_RECORD *,
    const DSL_NUMERICAL_CONTRACT_RECORD *, const VHO_DSL_WGMMA_PIPELINE_CONTROL *,
    const VHO_DSL_WGMMA_PIPELINE_BINDING *, WN *);

/* Analyze one complete same-contract tuple family (one-slot cooperative,
 * two-slot cooperative, two-slot bulk TMA) using MMA2 explicit model units and
 * the existing occupancy ledger. Synthetic costs never change emitted SIMT.
 * Bad input/overflow preserves output; absent timing is known=0, not zero cost.
 * register_bytes is a caller-supplied conservative per-thread reservation,
 * including accumulator, epilogue, addressing and copy/preparation temporaries. */
extern BOOL VHO_DSL_wgmma_pipeline_analyze(const DSL_TYPED_SIMT_RECORD *,
    const DSL_NUMERICAL_CONTRACT_RECORD *, const VHO_DSL_WGMMA_PIPELINE_CONTROL *,
    const DSL_COMPUTE_MODEL_RECORD *, uint64_t register_bytes,
    VHO_DSL_WGMMA_PIPELINE_RESULT *, FILE *);
#endif
