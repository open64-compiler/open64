/*
 * Copyright (C) 2026 Open64 Project
 *
 * Checked construction of ordinary WHIRL calls used by FHE runtime lowering.
 * ABI names and semantic argument selection remain FHE-consumer policy.
 */

#ifndef fhe_standard_whirl_INCLUDED
#define fhe_standard_whirl_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "symtab.h"

class WN;

typedef enum {
    VHO_FHE_STANDARD_PARM_INVALID = 0,
    VHO_FHE_STANDARD_PARM_BY_VALUE = 1,
    VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY = 2,
    VHO_FHE_STANDARD_PARM_BORROWED_MUTABLE = 3,
    VHO_FHE_STANDARD_PARM_OUTPUT_SLOT = 4
} VHO_FHE_STANDARD_PARM_POLICY;

typedef struct {
    TY_IDX formal_ty;
    TY_IDX actual_ty;
    WN *actual;
    VHO_FHE_STANDARD_PARM_POLICY policy;
    const char *output_name;
    TY_IDX output_ty;
} VHO_FHE_STANDARD_PARM;

typedef WN *(*VHO_FHE_STANDARD_FAILURE_BUILDER)
                                (ST_IDX status_st,
                                 ST_IDX output_st,
                                 void *context,
                                 FILE *diagnostic);

typedef struct {
    const char *function_name;
    TY_IDX status_ty;
    const VHO_FHE_STANDARD_PARM *parameters;
    UINT32 parameter_count;
    const char *status_name;
    SRCPOS source_position;
    VHO_FHE_STANDARD_FAILURE_BUILDER build_failure;
    void *failure_context;
} VHO_FHE_STANDARD_CALL_SPEC;

typedef struct {
    WN *block;
    ST_IDX function_st;
    ST_IDX status_st;
    ST_IDX output_st;
    UINT32 parameter_count;
} VHO_FHE_STANDARD_CALL_RESULT;

extern void VHO_FHE_Standard_Call_Result_Init
                                (VHO_FHE_STANDARD_CALL_RESULT *result);
extern BOOL VHO_FHE_Build_Standard_Call
                                (const VHO_FHE_STANDARD_CALL_SPEC *spec,
                                 FILE *diagnostic,
                                 VHO_FHE_STANDARD_CALL_RESULT *result);
extern BOOL VHO_FHE_Commit_Standard_Call
                                (WN *parent_block,
                                 WN *before,
                                 VHO_FHE_STANDARD_CALL_RESULT *result,
                                 FILE *diagnostic);

/* Validate the physical shapes used by the v1 void/hidden-result call ABI. */
extern BOOL VHO_FHE_Standard_Function_Body_Valid
                                (const WN *entry, FILE *diagnostic);
extern BOOL VHO_FHE_Standard_Null_Initializer_Valid
                                (const WN *initialization, ST_IDX result_st,
                                 FILE *diagnostic);
extern BOOL VHO_FHE_Standard_Null_Guard_Valid
                                (const WN *check, ST_IDX result_st,
                                 SRCPOS source_position, FILE *diagnostic);

#endif /* fhe_standard_whirl_INCLUDED */
