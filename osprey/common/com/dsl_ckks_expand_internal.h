/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private savepoints for one owner-PU CKKS expansion transaction. These are
 * not persistent-edit or frontend builder APIs.
 */

#ifndef dsl_ckks_expand_internal_INCLUDED
#define dsl_ckks_expand_internal_INCLUDED

#include "dsl_ir_image.h"

typedef struct {
    void *table;
    STR_IDX next_index;
} DSL_CKKS_STRTAB_SAVEPOINT;

typedef struct {
    UINT32 opcode_count;
    UINT32 node_count;
    UINT32 attribute_count;
    UINT32 value_count;
    UINT32 reference_count;
    DSL_IR_NODE_ID source_node_id;
    DSL_IR_VALUE_ID source_value_id;
    UINT32 source_node_flags;
    UINT32 source_value_flags;
} DSL_CKKS_IMAGE_SAVEPOINT;

extern BOOL DSL_CKKS_Strtab_Save (DSL_CKKS_STRTAB_SAVEPOINT *savepoint);
extern BOOL DSL_CKKS_Strtab_Restore
                     (const DSL_CKKS_STRTAB_SAVEPOINT *savepoint);

extern BOOL DSL_IR_Image_CKKS_Save
                (DSL_IR_VALUE_ID source_value_id,
                 DSL_CKKS_IMAGE_SAVEPOINT *savepoint);
extern void DSL_IR_Image_CKKS_Restore
                (const DSL_CKKS_IMAGE_SAVEPOINT *savepoint,
                 DSL_IR_VALUE_ID replacement_value_id);
extern BOOL DSL_IR_Image_Redirect_And_Lower_Value
                (DSL_IR_VALUE_ID source_value_id,
                 DSL_IR_VALUE_ID replacement_value_id);

extern void DSL_CKKS_Event_Image_Trim (UINT32 record_count);

/* Focused transaction fault injection; zero disables the test hook. */
extern void DSL_CKKS_Expand_Set_Test_Fault (UINT32 stage);

#endif /* dsl_ckks_expand_internal_INCLUDED */
