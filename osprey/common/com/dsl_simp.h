/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_simp_INCLUDED
#define dsl_simp_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opcode.h"
#include "opcode.h"
#include "symtab_idx.h"

class WN;

/*
 * M1 bridge between logical DSL expressions and the traditional WN
 * simplifier. The scalar projection is a test-only proof vehicle until tensor
 * constant storage and result publication have a reviewed representation.
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

typedef enum {
    DSL_SIMP_REPLACEMENT_NONE = 0,
    DSL_SIMP_REPLACEMENT_KID0 = 1,
    DSL_SIMP_REPLACEMENT_KID1 = 2,
    DSL_SIMP_REPLACEMENT_NEW_WN = 3
} DSL_SIMP_REPLACEMENT_KIND;

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    TY_IDX result_ty;
    TY_IDX operand_ty[2];
    DSL_SIMP_PROJECTION_KIND projection_kind;
    WN *projected_kid[2];
} DSL_SIMP_BINARY_CANDIDATE;

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

extern const char *DSL_Simp_Status_Name (DSL_SIMP_STATUS status);
extern const char *DSL_Simp_Replacement_Name
                                (DSL_SIMP_REPLACEMENT_KIND replacement);

/*
 * The caller owns projected_kid until Apply is called. On ENGINE_REWRITE the
 * traditional simplifier has applied its normal consume/delete convention and
 * engine_result is the retained pool-backed result. On UNCHANGED, the caller
 * still owns both projected kids.
 */
extern DSL_SIMP_STATUS DSL_Simp_Prepare_Binary
                                (const DSL_SIMP_BINARY_CANDIDATE *candidate,
                                 WN *stack_view,
                                 DSL_SIMP_BINARY_RESULT *result);
extern DSL_SIMP_STATUS DSL_Simp_Apply_Binary
                                (WN *stack_view,
                                 DSL_SIMP_BINARY_RESULT *result);
extern DSL_SIMP_STATUS DSL_Simp_Postprocess_Binary
                                (DSL_SIMP_BINARY_RESULT *result);
extern DSL_SIMP_STATUS DSL_Simp_Binary
                                (const DSL_SIMP_BINARY_CANDIDATE *candidate,
                                 DSL_SIMP_BINARY_RESULT *result);
extern void DSL_Simp_Trace_Binary
                                (FILE *file,
                                 const DSL_SIMP_BINARY_RESULT *result);

#endif /* dsl_simp_INCLUDED */
