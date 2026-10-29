/* Copyright (C) 2026 Open64 Project */

/* VHO-owned, runtime-only numerical and bounded-reduction contracts.
 * These records support backend planning and verification, not persisted IR;
 * they neither select a target nor change binary WHIRL fields.
 * Design: doc/AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md. */
#ifndef dsl_accumulation_INCLUDED
#define dsl_accumulation_INCLUDED

#include <stdio.h>
#include "defs.h"
#include "symtab_idx.h"

typedef enum {
    DSL_ACC_FORMAT_UNKNOWN = 0,
    DSL_ACC_FORMAT_F32 = 1,
    DSL_ACC_FORMAT_F8_E4M3 = 2,
    DSL_ACC_FORMAT_F8_E5M2 = 3,
    DSL_ACC_FORMAT_F4_E2M1 = 4,
    /* Numerical format vocabulary shared by VHO compute contracts. The
     * segmented accumulation API below still rejects F16/BF16; the separate
     * typed-SIMT verifier accepts their explicit FP32 accumulation contract.
     * doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md. */
    DSL_ACC_FORMAT_F16 = 5,
    DSL_ACC_FORMAT_BF16 = 6
} DSL_ACCUMULATION_FORMAT;

typedef enum {
    DSL_ACC_EVIDENCE_UNKNOWN = 0,
    DSL_ACC_EVIDENCE_ISA = 1,
    DSL_ACC_EVIDENCE_PUBLISHED = 2,
    DSL_ACC_EVIDENCE_NATIVE = 3
} DSL_ACCUMULATION_EVIDENCE;

enum {
    DSL_ACC_PERMISSION_REASSOCIATE = 1,
    DSL_ACC_PERMISSION_NARROW = 2
};

typedef struct {
    UINT32 id;
    UINT32 source_format;
    UINT32 input_format;
    UINT32 product_format;
    UINT32 partial_format;
    UINT32 outer_format;
    UINT32 output_format;
    UINT32 permissions;
    UINT32 maximum_span;
    UINT32 evidence_kind;
    UINT32 evidence_id;
    UINT32 reserved;
} DSL_NUMERICAL_CONTRACT_RECORD;

typedef struct {
    UINT32 id;
    UINT32 contract_id;
    ST_IDX owner_pu_st;
    UINT32 target_profile_id;
    UINT32 instruction_id;
    UINT32 instruction_k;
    UINT32 total_k;
    UINT32 staging_k;
    UINT32 requested_span;
    UINT32 effective_max_span;
    UINT32 scale_group_k;
    UINT32 segment_count;
    UINT32 internal_precision_bits;
    UINT32 internal_evidence_kind;
    UINT32 internal_evidence_id;
    UINT32 reserved;
} DSL_ACCUMULATION_PLAN_RECORD;

typedef struct {
    UINT32 ordinal;
    UINT32 begin_k;
    UINT32 valid_k;
    UINT32 issued_k;
    UINT32 reserved;
} DSL_ACCUMULATION_SEGMENT_RECORD;

/* Check exact coverage, permitted reassociation/narrowing, scale boundaries,
 * instruction alignment and evidence identity without selecting a target.
 * F16/BF16 remain rejected by this segmented accumulation API; typed-SIMT
 * validation is separate. Inputs are borrowed and never mutated; invalid
 * input returns FALSE with an optional diagnostic. */
extern BOOL DSL_accumulation_verify
    (const DSL_NUMERICAL_CONTRACT_RECORD *contract,
     const DSL_ACCUMULATION_PLAN_RECORD *plan,
     const DSL_ACCUMULATION_SEGMENT_RECORD *segments, UINT32 count,
     FILE *diagnostic);

/* Print a separate versioned compact stream; do not silently extend AI_OPT v7.
 * This records planning only, never an executable/native-accuracy claim.
 * Borrow all inputs; a null stream or invalid contract emits nothing. */
extern void DSL_accumulation_print
    (FILE *file, const DSL_NUMERICAL_CONTRACT_RECORD *contract,
     const DSL_ACCUMULATION_PLAN_RECORD *plan,
     const DSL_ACCUMULATION_SEGMENT_RECORD *segments, UINT32 count);

#endif
