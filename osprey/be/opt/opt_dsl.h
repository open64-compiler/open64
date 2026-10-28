/*
 * Copyright (C) 2026 Open64 Project
 */

/* PU-local semantic bridge and transient canonical-WHIRL characterization.
 * Contracts: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md and
 * doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md. No persisted fact layout. */

#ifndef opt_dsl_INCLUDED
#define opt_dsl_INCLUDED

#include "defs.h"
#include "dsl_opcode.h"
#include "symtab_idx.h"

class WN;

/* Transient R1 facts derived only from canonical active-PU WHIRL. These are
 * neither mapped-image records nor executable realization choices. The sum
 * recipe has one FP32 state per row, zero identity, add update, identity
 * finalize and a real-arithmetic add merge that is NOT authorized for FP32.
 * Accesses are logical X[r,k] reads/Y[r] result definitions, not traffic or
 * allocation facts. See AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md. */
enum {
  WOPT_DSL_REDUCTION_INPUT_READ_ONLY = 1U << 0,
  WOPT_DSL_REDUCTION_REASSOC_FORBIDDEN = 1U << 1,
  WOPT_DSL_REDUCTION_REFORMULATION_NOT_GRANTED = 1U << 2,
  WOPT_DSL_REDUCTION_ORDER_UNRESOLVED = 1U << 3,
  WOPT_DSL_REDUCTION_FINITE_UNPROVEN = 1U << 4,
  WOPT_DSL_REDUCTION_OVERFLOW_UNPROVEN = 1U << 5,
  WOPT_DSL_REDUCTION_ACCURACY_UNPROVEN = 1U << 6,
  WOPT_DSL_REDUCTION_ALIAS_UNPROVEN = 1U << 7,
  WOPT_DSL_REDUCTION_STORAGE_UNPLANNED = 1U << 8,
  WOPT_DSL_REDUCTION_OUTPUT_OWNER_PER_ROW = 1U << 9,
  WOPT_DSL_REDUCTION_SUM_F32_ROWS_V1 = 1
};

typedef struct {
  UINT32 recipe;
  UINT32 flags;
  ST_IDX owner_pu;
  UINT32 node_id;
  UINT32 input_value_id;
  UINT32 result_value_id;
  TY_IDX input_ty;
  TY_IDX result_ty;
  UINT64 rows;
  UINT64 width;
  UINT32 source_line;
} WOPT_DSL_REDUCTION_CHARACTERIZATION;

/* Query a member assignment of the driver-selected active whole PU. Validate
 * native meaning/ownership first; leave output and WHIRL unchanged on failure.
 * Facts are recomputed on demand; no Python metadata, cached FX fact or sample
 * value is consulted. REGION/cross-PU scopes remain unsupported. */
extern BOOL WOPT_DSL_characterize_reduction
    (WN *scope, WN *assignment, WOPT_DSL_REDUCTION_CHARACTERIZATION *output,
     FILE *diagnostic);

/* Verify the published reduction form within the active whole-PU PREOPT
 * scope, without rewriting or enlarging scope. Empty/non-reduction scopes
 * are no-ops; unsupported REGION reduction scope or malformed IR fails.
 * Optional trace is supplied only under existing AI trace controls.
 * See doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md. */
extern BOOL WOPT_DSL_verify_reduction_forms
    (WN *tree, const char *phase, FILE *diagnostic, FILE *trace);

/* Transient PU-local stable-Softmax facts; no storage or algorithm choice.
 * The v1 recipe is max/subtract/exp/sum/divide, not an online (m,l) merge.
 * See doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md. */
enum {
  WOPT_DSL_SOFTMAX_INPUT_READ_ONLY = 1U << 0,
  WOPT_DSL_SOFTMAX_REASSOC_FORBIDDEN = 1U << 1,
  WOPT_DSL_SOFTMAX_REFORMULATION_FORBIDDEN = 1U << 2,
  WOPT_DSL_SOFTMAX_ORDER_UNRESOLVED = 1U << 3,
  WOPT_DSL_SOFTMAX_FINITE_UNPROVEN = 1U << 4,
  WOPT_DSL_SOFTMAX_OVERFLOW_UNPROVEN = 1U << 5,
  WOPT_DSL_SOFTMAX_ACCURACY_UNPROVEN = 1U << 6,
  WOPT_DSL_SOFTMAX_ALIAS_UNPROVEN = 1U << 7,
  WOPT_DSL_SOFTMAX_STORAGE_UNPLANNED = 1U << 8,
  WOPT_DSL_SOFTMAX_OUTPUT_OWNER_PER_ROW = 1U << 9,
  WOPT_DSL_SOFTMAX_EXP_UNRESOLVED = 1U << 10,
  WOPT_DSL_SOFTMAX_EXCEPTION_UNRESOLVED = 1U << 11,
  WOPT_DSL_SOFTMAX_STABLE_F32_ROWS_V1 = 1
};

typedef struct {
  UINT32 recipe;
  UINT32 flags;
  ST_IDX owner_pu;
  UINT32 node_id;
  UINT32 input_value_id;
  UINT32 result_value_id;
  TY_IDX input_ty;
  TY_IDX result_ty;
  UINT64 rows;
  UINT64 width;
  UINT32 source_line;
} WOPT_DSL_SOFTMAX_CHARACTERIZATION;

/* Validate active whole-PU membership and native joins; never publish partial
 * facts or consult Python/debug metadata. REGION/cross-PU queries fail. */
extern BOOL WOPT_DSL_characterize_softmax
    (WN *scope, WN *assignment, WOPT_DSL_SOFTMAX_CHARACTERIZATION *output,
     FILE *diagnostic);

/* PREOPT verifies before import and after emission even with tracing off. */
extern BOOL WOPT_DSL_verify_softmax_forms
    (WN *tree, const char *phase, FILE *diagnostic, FILE *trace);

typedef UINT32 WOPT_DSL_SEMANTIC_INFO_ID;
/* Transient active-PU centered population normalization, not storage or an
 * executable algorithm. See doc/AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md.
 * Operand order is X, gamma, beta; epsilon is canonical IEEE binary32 bits. */
enum {
  WOPT_DSL_NORM_INPUTS_READ_ONLY = 1U << 0,
  WOPT_DSL_NORM_REASSOC_FORBIDDEN = 1U << 1,
  WOPT_DSL_NORM_REFORMULATION_FORBIDDEN = 1U << 2,
  WOPT_DSL_NORM_ORDER_UNRESOLVED = 1U << 3,
  WOPT_DSL_NORM_FINITE_UNPROVEN = 1U << 4,
  WOPT_DSL_NORM_OVERFLOW_UNPROVEN = 1U << 5,
  WOPT_DSL_NORM_ACCURACY_UNPROVEN = 1U << 6,
  WOPT_DSL_NORM_ALIAS_UNPROVEN = 1U << 7,
  WOPT_DSL_NORM_STORAGE_UNPLANNED = 1U << 8,
  WOPT_DSL_NORM_OUTPUT_OWNER_PER_ROW = 1U << 9,
  WOPT_DSL_NORM_RSQRT_UNRESOLVED = 1U << 10,
  WOPT_DSL_NORM_EXCEPTION_UNRESOLVED = 1U << 11,
  WOPT_DSL_NORM_CONTRACTION_FORBIDDEN = 1U << 12,
  WOPT_DSL_NORM_CENTERED_POPULATION_F32_ROWS_V1 = 1
};

typedef struct {
  UINT32 recipe;
  UINT32 flags;
  ST_IDX owner_pu;
  UINT32 node_id;
  UINT32 operand_value_ids[3];
  UINT32 result_value_id;
  TY_IDX operand_types[3];
  TY_IDX result_ty;
  UINT64 rows;
  UINT64 width;
  UINT32 epsilon_bits;
  UINT32 source_line;
} WOPT_DSL_NORMALIZATION_CHARACTERIZATION;

/* Derive a snapshot only after whole active-PU/native/membership validation.
 * Failure leaves output unchanged; no Python, sample or debug facts are read. */
extern BOOL WOPT_DSL_characterize_normalization
    (WN *scope, WN *assignment, WOPT_DSL_NORMALIZATION_CHARACTERIZATION *output,
     FILE *diagnostic);

/* Verify before PREOPT import and after emission, even when trace is disabled.
 * REGION normalization scopes are not supported by this bounded contract. */
extern BOOL WOPT_DSL_verify_normalization_forms
    (WN *tree, const char *phase, FILE *diagnostic, FILE *trace);

#define WOPT_DSL_SEMANTIC_INFO_INVALID_ID 0

enum {
  WOPT_DSL_SEMANTIC_PURE = 1U << 0,
  WOPT_DSL_SEMANTIC_PROJECTABLE = 1U << 1,
  WOPT_DSL_SEMANTIC_PROJECTABLE_ROOT = 1U << 2,
  WOPT_DSL_SEMANTIC_PROJECTION = 1U << 3
};

typedef enum {
  WOPT_DSL_PROJECTION_NONE = 0,
  WOPT_DSL_PROJECTION_QUOTIENT = 1,
  WOPT_DSL_PROJECTION_REMAINDER = 2
} WOPT_DSL_PROJECTION_KIND;

typedef struct {
  BOOL lowering_capability;
  BOOL profitable;
} WOPT_DSL_DIVREM_TARGET_POLICY;

/*
 * Runtime-only semantic identity for a logical DSL CODEREP.  Source
 * positions, diagnostics, profiling, and reconstruction provenance do not
 * participate in this record because they must not inhibit WOPT value
 * equivalence.
 */
typedef struct {
  WOPT_DSL_SEMANTIC_INFO_ID id;
  DSL_OPERATOR logical_operator;
  UINT32 version;
  UINT32 flags;
  TY_IDX result_ty;
  TY_IDX second_result_ty;
  TCON_IDX tensor_tcon_idx;
  UINT32 projection_kind;
  UINT32 origin_node_id;
  UINT32 origin_result_value_id;
  UINT64 canonical_attribute_hash;
  UINT64 operand_descriptor_hash;
  UINT64 effect_identity;
} WOPT_DSL_SEMANTIC_INFO;

typedef char WOPT_DSL_Semantic_Info_Id_Size_Check
    [sizeof(WOPT_DSL_SEMANTIC_INFO_ID) == 4 ? 1 : -1];
typedef char WOPT_DSL_Semantic_Info_Size_Check
    [sizeof(WOPT_DSL_SEMANTIC_INFO) == 64 ? 1 : -1];

extern void WOPT_DSL_semantic_info_reset(void);
extern WOPT_DSL_SEMANTIC_INFO_ID WOPT_DSL_semantic_info_intern
    (const WOPT_DSL_SEMANTIC_INFO *info);
extern BOOL WOPT_DSL_semantic_info_get
    (WOPT_DSL_SEMANTIC_INFO_ID id, WOPT_DSL_SEMANTIC_INFO *info);
extern UINT32 WOPT_DSL_semantic_info_hash
    (WOPT_DSL_SEMANTIC_INFO_ID id);
extern UINT32 WOPT_DSL_semantic_info_count(void);
extern BOOL WOPT_DSL_algebraic_safety_allows
    (DSL_ALGEBRAIC_SAFETY safety, BOOL floating_point,
     BOOL reassociation_enabled);
extern void WOPT_DSL_reset_DIVREM_target_policy(void);
extern BOOL WOPT_DSL_set_DIVREM_target_policy
    (const WOPT_DSL_DIVREM_TARGET_POLICY *policy);
extern BOOL WOPT_DSL_get_DIVREM_target_policy
    (WOPT_DSL_DIVREM_TARGET_POLICY *policy);
extern BOOL WOPT_DSL_DIVREM_combination_enabled
    (BOOL option_enabled);
extern BOOL WOPT_DSL_create_DIVREM_semantics
    (const WOPT_DSL_SEMANTIC_INFO *standalone,
     WOPT_DSL_SEMANTIC_INFO *combined,
     WOPT_DSL_SEMANTIC_INFO *projection);
extern BOOL WOPT_DSL_uncombine_projection_semantics
    (const WOPT_DSL_SEMANTIC_INFO *projection,
     WOPT_DSL_SEMANTIC_INFO *standalone);
extern BOOL WOPT_DSL_projectable_info
    (const WOPT_DSL_SEMANTIC_INFO *info);
extern BOOL WOPT_DSL_projection_info
    (const WOPT_DSL_SEMANTIC_INFO *info);
extern BOOL WOPT_DSL_import_semantic_info
    (const WN *wn, ST_IDX result_st, const char *owner_pu,
     WOPT_DSL_SEMANTIC_INFO *info, FILE *diagnostic);
extern BOOL WOPT_DSL_create_folded_tensor_info
    (const WOPT_DSL_SEMANTIC_INFO *origin, TCON_IDX result_tcon_idx,
     WOPT_DSL_SEMANTIC_INFO *result);
extern BOOL WOPT_DSL_fold_compact_tensors
    (const WOPT_DSL_SEMANTIC_INFO *origin,
     const TCON_IDX *operand_tcon_idx, UINT32 operand_count,
     WOPT_DSL_SEMANTIC_INFO *result, FILE *diagnostic);
extern WN *WOPT_DSL_emit_WN
    (const WOPT_DSL_SEMANTIC_INFO *info, const WN *original,
     ST_IDX result_st, WN **kids, UINT32 kid_count, FILE *diagnostic);

#endif /* opt_dsl_INCLUDED */
