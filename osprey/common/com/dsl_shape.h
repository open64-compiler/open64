/*
 * Copyright (C) 2026 Open64 Project
 */

/* Common ingestion-callable local tensor schema checks and shape inference.
 * PU-wide fixed-point analysis is separately owned by be/vho; this header
 * carries no solver lifetime or refinement callback. LN-REF-1 admission is in
 * doc/AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md; no target or
 * optimization choice belongs in these predicates. Local facts are transient,
 * not new .B records; existing TY/image/string tables remain caller-active
 * and borrowed only during a call. Scratch strings/vectors may allocate, but
 * these APIs never create TYs, mutate WHIRL or activate a PU. See also
 * doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md and
 * doc/WHIRL-DSL-INFRASTRUCTURE.md. */

#ifndef dsl_shape_INCLUDED
#define dsl_shape_INCLUDED

#include <stddef.h>
#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "symtab_idx.h"

#define DSL_SHAPE_MAX_RANK 16
#define DSL_SHAPE_DIMENSION_TEXT_MAX 96

typedef enum {
    DSL_SHAPE_CHECK_UNREGISTERED = 0,
    DSL_SHAPE_CHECK_VALID = 1,
    DSL_SHAPE_CHECK_INVALID = 2
} DSL_SHAPE_CHECK_RESULT;

/* Borrow the node and operand array for one check; the caller keeps their
 * image/string/type context live and supplies valid nonzero TY handles. */
typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    const DSL_IR_NODE_RECORD *node;
    const TY_IDX *operand_types;
    UINT32 operand_count;
    TY_IDX result_ty;
} DSL_SHAPE_OPERATOR_INPUT;

typedef enum {
    DSL_SHAPE_FACT_PENDING = 0,
    DSL_SHAPE_FACT_COMPLETE = 1,
    DSL_SHAPE_FACT_CONTRADICTION = 2
} DSL_SHAPE_FACT_STATE;

typedef enum {
    DSL_SHAPE_DIMENSION_PENDING = 0,
    DSL_SHAPE_DIMENSION_STATIC = 1,
    DSL_SHAPE_DIMENSION_ANONYMOUS_DYNAMIC = 2,
    DSL_SHAPE_DIMENSION_SYMBOL = 3,
    DSL_SHAPE_DIMENSION_EXPRESSION = 4
} DSL_SHAPE_DIMENSION_KIND;

/* Caller-owned scratch evidence, not persisted tensor identity. Rank is -1
 * or 0..16; text slots are NUL-terminated and canonical, numeric caches agree
 * with STATIC dimensions. COMPLETE can still contain anonymous '?' dimensions;
 * it means no pending dimensions, not a proof of equality or static storage. */
typedef struct {
    UINT32 state;
    INT32 rank;
    UINT8 dimension_kind[DSL_SHAPE_MAX_RANK];
    UINT8 dimension_known[DSL_SHAPE_MAX_RANK];
    UINT64 dimension[DSL_SHAPE_MAX_RANK];
    char dimension_text[DSL_SHAPE_MAX_RANK]
                       [DSL_SHAPE_DIMENSION_TEXT_MAX];
} DSL_SHAPE_FACT;

typedef enum {
    DSL_SHAPE_INFERENCE_UNREGISTERED = 0,
    DSL_SHAPE_INFERENCE_PENDING = 1,
    DSL_SHAPE_INFERENCE_COMPLETE = 2,
    DSL_SHAPE_INFERENCE_CONTRADICTION = 3
} DSL_SHAPE_INFERENCE_RESULT;

/* Borrow parallel type/fact arrays and node in the caller's active context.
 * Facts obey the bounded canonical scratch contract; output must not alias
 * this input or its fact array because inference initializes output first. */
typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    const DSL_IR_NODE_RECORD *node;
    const TY_IDX *operand_types;
    const DSL_SHAPE_FACT *operand_facts;
    UINT32 operand_count;
    TY_IDX result_ty;
} DSL_SHAPE_INFERENCE_INPUT;

/* Test that a live tensor TY has kind/dtype/rank/shape attributes present.
 * Read-only presence check, not parsing or complete descriptor validation. */
extern BOOL DSL_Shape_Tensor_Core_Complete(TY_IDX ty);
/* Check static F4/float32 [R,N] -> [R], R,N>0, with matching element and
 * representation attributes; require axis="1", keepdim="false",
 * accum_dtype="float32", reassociate="false". Scratch parsing may allocate;
 * no descriptor mutation, order selection or target permission results.
 * See doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md. */
extern BOOL DSL_Shape_Reduce_Sum_Types_Valid
                                (TY_IDX operand_ty, TY_IDX result_ty,
                                 const char *axis, const char *keepdim,
                                 const char *accum_dtype,
                                 const char *reassociate);

/* Require exactly ascending_left_fold, 00000000, rne_gradual_no_traps and
 * class_only, respectively; NULL fails. Borrowed strings are not retained.
 * This v2 source-binding predicate makes no target capability or policy choice.
 * Design: doc/AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md. */
extern BOOL DSL_ordered_reduce_sum_binding_valid
    (const char *order, const char *identity_bits,
     const char *fp_environment, const char *nan_policy);

/* Check identical positive static F4/float32 [R,N] descriptors with matching
 * representation and axis="1", accum_dtype="float32", reassociate="false",
 * state_reformulation="false", exp_semantics="source_unresolved". Scratch
 * parsing may allocate; no exp implementation or exceptional-value rule is
 * chosen. See doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md. */
extern BOOL DSL_Shape_Stable_Softmax_Types_Valid
    (TY_IDX operand_ty, TY_IDX result_ty, const char *axis,
     const char *accum_dtype, const char *reassociate,
     const char *state_reformulation, const char *exp_semantics);

/* Canonical eight-lowercase-hex positive finite IEEE binary32 epsilon bits.
 * No host floating-point parsing/rounding or target FTZ policy; output unchanged
 * on failure; bits must be non-NULL. Accept subnormals, reject zero, sign bit,
 * infinity and NaN. See doc/AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md. */
extern BOOL DSL_positive_f32_bits_valid(const char *text, UINT32 *bits);

/* Policy-free FP32 [R,N], [N], [N] -> [R,N] centered population contract.
 * Require axis="1", accum_dtype="float32", variance_kind="population_centered",
 * canonical positive epsilon bits, all three permissions="false", and
 * rsqrt_semantics="source_unresolved". Borrow live TYs/strings, parse using
 * scratch allocations, change no tables; width has no v2 count restriction. */
extern BOOL DSL_mean_variance_norm_types_valid
    (TY_IDX input_ty, TY_IDX gamma_ty, TY_IDX beta_ty, TY_IDX result_ty,
     const char *axis, const char *accum_dtype, const char *variance_kind,
     const char *epsilon_bits, const char *reassociate,
     const char *state_reformulation, const char *contract,
     const char *rsqrt_semantics);

/* Validate explicit LN-REF-1 arithmetic tokens, without upgrading v1 or
 * granting target/accuracy permission. Require, in parameter order,
 * sqrt_rne_then_div_rne_f32, ascending_left_fold, 00000000, sum_then_div_rne,
 * exact_f32_from_static_n, rne_gradual_no_traps, class_only, ln_ref_1.
 * NULL/mismatch returns FALSE without mutation or retaining strings. */
extern BOOL DSL_mean_variance_norm_ref_binding_valid
    (const char *rsqrt_semantics, const char *mean_order,
     const char *mean_identity_bits, const char *mean_finalize,
     const char *count_conversion, const char *fp_environment,
     const char *nan_policy, const char *reference_binding);

/* Validate the v2 descriptor/centered-recipe subset and exact count domain.
 * Require the shared recipe above but rsqrt=sqrt_rne_then_div_rne_f32,
 * 1<=N<=2^24 and R*N*4 representable in UINT64. No tables change; scratch
 * parsing may allocate. Callers must also validate the complete v2 binding. */
extern BOOL DSL_mean_variance_norm_ref_types_valid
    (TY_IDX input_ty, TY_IDX gamma_ty, TY_IDX beta_ty, TY_IDX result_ty,
     const char *axis, const char *accum_dtype, const char *variance_kind,
     const char *epsilon_bits, const char *reassociate,
     const char *state_reformulation, const char *contract,
     const char *rsqrt_semantics);

/* Derive exact count bits using integer arithmetic for 1 <= N <= 2^24.
 * Failure leaves caller output unchanged; this is not a cost/target check. */
extern BOOL DSL_mean_variance_norm_ref_count_bits(UINT64 count, UINT32 *bits);

/* Parse bracketed positive UINT64 extents (including rank-zero []), borrowing
 * shape and allocating scratch. On parse failure outputs stay unchanged;
 * after parsing, rank is written even if capacity is insufficient. Dimensions
 * are copied only on success; NULL dimensions is a rank query iff capacity=0.
 * Whitespace and a trailing comma before ']' are accepted by this parser. */
extern BOOL DSL_Shape_Parse_Static_Dimensions
                                (const char *shape,
                                 UINT64 *dimensions,
                                 UINT32 capacity,
                                 UINT32 *rank);
/* Format borrowed positive extents as [d0,d1,...], or [] at rank zero, into
 * caller storage. No allocation; FALSE can leave a partial/truncated buffer.
 * The extent array must contain rank entries when rank is nonzero. */
extern BOOL DSL_Shape_Format_Static_Dimensions
                                (const UINT64 *dimensions,
                                 UINT32 rank,
                                 char *buffer,
                                 size_t buffer_size);
/* Parse rank -1/<pending> or rank 0..16 logical dimensions, qualifying bare
 * symbols with nonzero owner_pu_st and canonicalizing explicit @puXXXXXXXX.
 * Accept positive extents, ?, <pending>, symbol and symbol +/- unsigned offset.
 * No PU activation or table mutation. Parse failure preserves buffer; format
 * failure may partially write it. Borrow input; temporary strings may allocate. */
extern BOOL DSL_Shape_Normalize_Logical_Shape
                                (ST_IDX owner_pu_st,
                                 const char *shape,
                                 INT32 expected_rank,
                                 char *buffer,
                                 size_t buffer_size);
/* Format a borrowed bounded fact's text (or <pending> for rank -1), independent
 * of its state flag. Caller supplies terminated dimension strings and output
 * storage; FALSE may leave partial/truncated text, with no table mutation. */
extern BOOL DSL_Shape_Format_Fact (const DSL_SHAPE_FACT *fact,
                                   char *buffer,
                                   size_t buffer_size);
/* Compare live core-complete tensor TYs by element TY, dtype, representation
 * keys and rank text, optionally shape text. Read-only exact spelling equality;
 * not algebraic shape equality or complete descriptor/alias validation. */
extern BOOL DSL_Shape_Tensor_Compatible
                                (TY_IDX ty0,
                                 TY_IDX ty1,
                                 BOOL require_shape);
/* Test the static zero-motion view subset: equal element TY/alignment/size,
 * kind/dtype/layout and optional sharding/placement/memory/quantization, plus
 * equal checked extent products. Rank/shape may differ; strides/runtime state
 * are not compared here. No mutation or lowering/alias-policy permission. */
extern BOOL DSL_Shape_Representation_Preserving_View
                                (TY_IDX source_ty, TY_IDX view_ty);
/* Recognize only OPR_DSLFLATTEN version 2 as the zero-motion operator spelling;
 * no descriptor, storage, or use legality is established by this predicate. */
extern BOOL DSL_Shape_Zero_Motion_View_Operator
                                (UINT32 dsl_operator, UINT16 version);
/* Test exact operator/version membership in the immutable local rule table;
 * no fallback version, input validation, registration or policy mutation. */
extern BOOL DSL_Shape_Has_Operator_Rule
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version);
/* Copy logical shape evidence from a live core-complete TY into caller scratch.
 * Rank text must match TY rank and symbols must already be PU-qualified.
 * Failure before shape parsing preserves fact; parsing failure leaves reset or
 * partial evidence. Success may still be PENDING; no TY/image mutation. */
extern BOOL DSL_Shape_Fact_From_Type (TY_IDX ty, DSL_SHAPE_FACT *fact);
/* Infer one registered local rule into distinct caller scratch, using borrowed
 * bounded facts and the active type/image/string context. Initialize result
 * before validation; early CONTRADICTION returns can leave PENDING state,
 * while rule rejection marks contradiction and may retain partial fields.
 * UNREGISTERED leaves initialized scratch. COMPLETE is shape evidence only:
 * full schema/type/numerical legality still requires Check_Operator/gatekeeper.
 * Temporary containers may allocate; no TY mutation, traversal or PU solving. */
extern DSL_SHAPE_INFERENCE_RESULT DSL_Shape_Infer_Operator
                                (const DSL_SHAPE_INFERENCE_INPUT *input,
                                 DSL_SHAPE_FACT *result);
/* Check one node with the exact registered version and live operand/result TYs.
 * Borrow inputs for this call, allocating temporary facts only; never mutate
 * tables. NULL/malformed inputs give INVALID; absent opcode/rule gives
 * UNREGISTERED. Symbolic matmul/attention/broadcast require equality proofs;
 * anonymous '?' is not such proof. This is no optimization/target admission. */
extern DSL_SHAPE_CHECK_RESULT DSL_Shape_Check_Operator
                                (const DSL_SHAPE_OPERATOR_INPUT *input);

#endif /* dsl_shape_INCLUDED */
