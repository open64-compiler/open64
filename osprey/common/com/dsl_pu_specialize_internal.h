/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private managed-image construction used by a program-scope PU clone
 * transaction. This is not a supported persistent-edit API.
 */

#ifndef dsl_pu_specialize_internal_INCLUDED
#define dsl_pu_specialize_internal_INCLUDED

#include "dsl_ir_image.h"

typedef struct {
    DSL_IR_VALUE_ID source_value_id;
    DSL_IR_VALUE_ID clone_value_id;
} DSL_PU_CLONE_VALUE_PAIR;

typedef struct {
    UINT32 node_count;
    UINT32 attribute_count;
    UINT32 value_count;
    UINT32 reference_count;
    UINT32 formal_count;
    UINT32 pu_identity_count;
} DSL_PU_CLONE_IMAGE_SAVEPOINT;

extern BOOL DSL_IR_Image_Clone_PU_Values
                (ST_IDX source_pu_st, const char *source_pu_name,
                 ST_IDX clone_pu_st, const char *clone_pu_name,
                 DSL_PU_CLONE_VALUE_PAIR *pairs, UINT32 pair_capacity,
                 UINT32 *pair_count,
                 DSL_PU_CLONE_IMAGE_SAVEPOINT *savepoint);
extern void DSL_IR_Image_Clone_PU_Restore
                (const DSL_PU_CLONE_IMAGE_SAVEPOINT *savepoint);
extern BOOL DSL_Call_Image_Replace_Call_WN
                (DSL_CALLSITE_METADATA_ID id, const WN *expected,
                 WN *replacement);
extern BOOL DSL_PU_Interface_Image_Shift_Formals
                (ST_IDX owner_pu_st, UINT32 first_ordinal,
                 UINT32 count);
extern BOOL DSL_Call_ABI_Image_Shift_Arguments
                (DSL_CALLSITE_METADATA_ID callsite_id,
                 UINT32 first_ordinal, UINT32 count);
extern BOOL DSL_Call_Image_Retarget_Call_WN
                (DSL_CALLSITE_METADATA_ID callsite_id,
                 const WN *expected, WN *replacement,
                 ST_IDX new_callee);

#endif /* dsl_pu_specialize_internal_INCLUDED */
