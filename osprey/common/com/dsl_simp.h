/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com exposes process-local bridges from eligible scalar projections
 * and integral tensor binary expressions to the existing WN simplifier.
 * Callers supply disposable scalar
 * projections, compiler table/pool context and any tensor result publication;
 * this interface neither owns a PU nor changes mapped-image representation.
 * Design: doc/WHIRL-DSL-SIMPLIFICATION-PLAN.md, traditional-engine reuse.
 */

#ifndef dsl_simp_INCLUDED
#define dsl_simp_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opcode.h"
#include "opcode.h"
#include "symtab_idx.h"
#include "dsl_scalar.h"

class WN;

/* Disposable, independently owned scalar leaves for one element of an already
 * checked tensor operation. The owner supplies broadcast indexing and canonical
 * I4 predicates; this is not permission to replace an entire broadcast tensor
 * with a scalar or an unbroadcast operand. No source operand is consumed. */
typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    TYPE_ID element_mtype;
    UINT32 kid_count;
    WN *projected_kid[3];
} DSL_SIMP_SCALAR_CANDIDATE;

/*
 * Select the scalar projection accepted by this bridge. The historical
 * TEST_SCALAR spelling is used by the focused bridge fixture and the builder's
 * compact tensor-constant adapter; it is not a general tensor projection or a
 * promise that this service publishes a folded tensor value.
 */
typedef enum {
    DSL_SIMP_PROJECTION_NONE = 0,
    DSL_SIMP_PROJECTION_TEST_SCALAR = 1
} DSL_SIMP_PROJECTION_KIND;

typedef enum {
    DSL_SIMP_NOT_APPLICABLE = 0,
    DSL_SIMP_UNCHANGED = 1,
    DSL_SIMP_ENGINE_REWRITE = 2,
    DSL_SIMP_REJECT_DISABLED = 3,
    DSL_SIMP_REJECT_MALFORMED = 4,
    DSL_SIMP_REJECT_UNSUPPORTED_OPERATOR = 5,
    DSL_SIMP_REJECT_EFFECTFUL_OPERATOR = 6,
    DSL_SIMP_REJECT_DESCRIPTOR = 7,
    DSL_SIMP_REJECT_NUMERIC_POLICY = 8,
    DSL_SIMP_REJECT_PROJECTION = 9,
    DSL_SIMP_INVALID = 10
} DSL_SIMP_STATUS;

/* Caller-owned result. On rewrite, only engine_result is retained from the WN
 * pool; projected leaves may have been consumed. Rejection and unchanged leave
 * their ownership with the caller. This record is never mapped into .B. */
typedef struct {
    DSL_SIMP_STATUS status;
    OPCODE projected_opcode;
    WN *engine_result;
} DSL_SIMP_SCALAR_RESULT;

/* Reuse WN_SimplifyExp1/2/3 for one checked element, never whole-tensor rules.
 * Require distinct disposable constants or pure local/formal/PREG loads,
 * in the caller's WN pool,
 * initialized compiler context and nonoverlapping candidate/result. Rejection
 * consumes nothing and clears result first. FP arithmetic/comparisons require
 * strict compiler controls (including disabled aggressive folding and division
 * splitting) and nonvolatile local/formal operands; constants,
 * PREGs (which may have constant homes), and nested FP
 * expressions remain unresolved, avoiding unsafe zero/NaN/rounding rewrites.
 * SELECT may forward FP leaves bit-for-bit only with a literal condition and
 * must not drop source evaluation.
 * Unbound intrinsic projections are rejected, including EXP/LOG; lookup alone
 * does not select their source numerical binding. Success can consume leaves;
 * callers retain only engine_result and own all tensor/metadata publication.
 * Hidden linkage adds no backend dynamic export or foreign library dependency. */
extern DSL_SIMP_STATUS DSL_simp_scalar
                                (const DSL_SIMP_SCALAR_CANDIDATE *candidate,
                                 DSL_SIMP_SCALAR_RESULT *result)
    __attribute__((visibility("hidden")));

typedef enum {
    DSL_SIMP_REPLACEMENT_NONE = 0,
    DSL_SIMP_REPLACEMENT_KID0 = 1,
    DSL_SIMP_REPLACEMENT_KID1 = 2,
    DSL_SIMP_REPLACEMENT_NEW_WN = 3
} DSL_SIMP_REPLACEMENT_KIND;

/* Borrowed logical types and two disposable scalar expression trees. Callers
 * establish valid table indices, compatible projection meaning and normal WN
 * pool/strict-tree ownership; the bridge does not inspect tensor payloads or
 * independently prove projected operand purity, aliasing or provenance. */
typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    TY_IDX result_ty;
    TY_IDX operand_ty[2];
    DSL_SIMP_PROJECTION_KIND projection_kind;
    WN *projected_kid[2];
} DSL_SIMP_BINARY_CANDIDATE;

/* Caller-owned stage state, zeroed by Prepare. Projected pointers record input
 * identity and can become stale after Apply consumes trees; only the retained
 * engine_result may be used after a rewrite. No stack-view pointer is stored. */
typedef struct {
    DSL_SIMP_STATUS status;
    DSL_SIMP_STATUS rejection;
    DSL_SIMP_REPLACEMENT_KIND replacement;
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    OPCODE projected_opcode;
    WN *projected_kid[2];
    WN *engine_result;
} DSL_SIMP_BINARY_RESULT;

/* Return a static status spelling, or "unknown" for an out-of-range value;
 * the caller must not free it. No registry or result mutation. */
extern const char *DSL_Simp_Status_Name (DSL_SIMP_STATUS status);
/* Return a static replacement spelling, or "unknown" outside the enum;
 * the caller must not free it. No WN inspection or ownership change. */
extern const char *DSL_Simp_Replacement_Name
                                (DSL_SIMP_REPLACEMENT_KIND replacement);

/*
 * Validate a stable, nonoverlapping candidate/result and writable stack WN
 * storage in the caller's initialized compiler context. Zero nonnull result
 * first; snapshot logical identity/kid pointers, then check Enable_WN_Simp,
 * registered binary algebraic/pure identity, equivalent canonical descriptors,
 * integral element type and the supported scalar ADD/MPY projection. Accepted
 * preparation returns NOT_APPLICABLE and initializes stack_view; every rejection
 * leaves stack_view and the caller's projected trees unchanged. Result reports
 * rejection but may retain copied candidate fields. No tensor/image writes,
 * allocation-exception recovery or independent operand-effect/ownership proof.
 */
extern DSL_SIMP_STATUS DSL_Simp_Prepare_Binary
                                (const DSL_SIMP_BINARY_CANDIDATE *candidate,
                                 WN *stack_view,
                                 DSL_SIMP_BINARY_RESULT *result);
/* Apply once to the same unmodified prepared view/result. Check stage/opcode,
 * then let WN_SimplifyExp2 use the traditional controls and consume/delete
 * convention. ENGINE_REWRITE retains a pool-backed result (possibly a kid);
 * UNCHANGED leaves both kids caller-owned and engine_result NULL. Never delete
 * or publish stack_view; after rewriting its kid pointers may be stale.
 * Replacement classification is deferred to Postprocess. Malformed input sets
 * status/rejection where possible without clearing other fields or undoing a
 * prior engine call; no general rollback or tensor publication is provided. */
extern DSL_SIMP_STATUS DSL_Simp_Apply_Binary
                                (WN *stack_view,
                                 DSL_SIMP_BINARY_RESULT *result);
/* Classify an unmodified Apply result as none, original kid0/kid1 identity or
 * new WN without dereferencing potentially consumed kids. Preserve accepted
 * status and engine_result; do not rebuild DSL symbols, metadata or images.
 * Invalid stage/NULL input rejects, updating status/rejection when possible,
 * but supplies no pointer-liveness validation, cleanup or engine rollback. */
extern DSL_SIMP_STATUS DSL_Simp_Postprocess_Binary
                                (DSL_SIMP_BINARY_RESULT *result);
/* Run Prepare, Apply and Postprocess with a stack-local view, stopping at the
 * first rejected stage. Candidate/result and projected-tree contracts above
 * apply; retain only engine_result on rewrite and dispose unchanged/rejected
 * projections as caller-owned trees. Result publication into DSL IR remains
 * with the caller; this convenience API adds no rollback or diagnostics. */
extern DSL_SIMP_STATUS DSL_Simp_Binary
                                (const DSL_SIMP_BINARY_CANDIDATE *candidate,
                                 DSL_SIMP_BINARY_RESULT *result);
/* Print logical operator/version, status and classified replacement to a
 * borrowed stream; NULL stream/result is a no-op. Use fallback spellings for
 * unknown metadata, without validating or changing result/WNs; I/O errors are
 * not returned. No physical DSL escape tag or mapped record is emitted. */
extern void DSL_Simp_Trace_Binary
                                (FILE *file,
                                 const DSL_SIMP_BINARY_RESULT *result);

#endif /* dsl_simp_INCLUDED */
