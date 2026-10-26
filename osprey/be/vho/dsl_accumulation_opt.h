/* Copyright (C) 2026 Open64 Project */

/* Active-PU numerical planning and transactional bounded-reduction lowering.
 * Reuses standard WHIRL; no target FP8/FP4 opcode or emitter is invented here.
 * Design: doc/AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md. */
#ifndef dsl_accumulation_opt_INCLUDED
#define dsl_accumulation_opt_INCLUDED

#include "dsl_accumulation.h"
#include "srcpos.h"

class WN;
struct ST;
struct VHO_DSL_ACCUMULATION;

typedef enum {
    VHO_DSL_ACC_SCALAR_F32 = 1,
    VHO_DSL_ACC_HOPPER_WGMMA_FP8 = 2,
    VHO_DSL_ACC_BLACKWELL_SCALED_FP4 = 3
} VHO_DSL_ACCUMULATION_INSTRUCTION;

typedef struct {
    ST *a;
    ST *b;
    ST *output;
    ST *index;
    ST *partial;
    ST *outer;
    SRCPOS source_position;
} VHO_DSL_ACCUMULATION_BINDING;

/* At -O3, plan instruction-aligned segments in the caller's current PU. The request
 * contains an accepted contract, not a request to infer model accuracy.
 * Blackwell is rejected until its independent scaled profile exists. */
extern VHO_DSL_ACCUMULATION *VHO_DSL_accumulation_create
    (const DSL_NUMERICAL_CONTRACT_RECORD &contract,
     const DSL_ACCUMULATION_PLAN_RECORD &request, FILE *diagnostic);

/* Release runtime planning state; no WN or symbol ownership is transferred. */
extern void VHO_DSL_accumulation_destroy(VHO_DSL_ACCUMULATION *analysis);

/* Copy immutable selected records for trace/audit without exposing storage. */
extern BOOL VHO_DSL_accumulation_records(const VHO_DSL_ACCUMULATION *analysis,
    DSL_NUMERICAL_CONTRACT_RECORD *contract, DSL_ACCUMULATION_PLAN_RECORD *plan);

/* Fetch one segment; reject an out-of-range ordinal without changing output. */
extern BOOL VHO_DSL_accumulation_segment(const VHO_DSL_ACCUMULATION *analysis,
    UINT32 ordinal, DSL_ACCUMULATION_SEGMENT_RECORD *segment);

/* Publish a separately versioned planning trace only under caller option
 * control; null output disables it. Native execution is never implied. */
extern void VHO_DSL_accumulation_print(FILE *file,
    const VHO_DSL_ACCUMULATION *analysis);

/* Build a detached, synchronous scalar F32 dot block with explicit reset and
 * outer merges. Caller supplies typed active-PU symbols and distinct private
 * accumulator/index symbols; pointer formals may alias. The caller owns
 * insertion after verification; output is unchanged on failure. Hopper/FP4
 * requests fail closed rather than substituting scalar F32 for target MMA. */
extern BOOL VHO_DSL_accumulation_materialize_f32
    (const VHO_DSL_ACCUMULATION *analysis,
     const VHO_DSL_ACCUMULATION_BINDING &binding, WN **block, FILE *diagnostic);

/* Revalidate exact segment/loop/address/reset/merge order after edits. This
 * is an explicit phase-boundary check, not a new binary-WHIRL annotation. */
extern BOOL VHO_DSL_accumulation_verify_f32
    (const VHO_DSL_ACCUMULATION *analysis,
     const VHO_DSL_ACCUMULATION_BINDING &binding, WN *block, FILE *diagnostic);

#endif
