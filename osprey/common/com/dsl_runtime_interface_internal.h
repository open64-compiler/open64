/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private runtime-interface services shared with program-interface
 * reconstruction and generic DSL rewrite validation. These declarations are
 * not frontend or mapped-image APIs. See
 * doc/FHE-SYNC5-RUNTIME-ENTRY-BINDING-GAP.md and
 * doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md.
 */

#ifndef dsl_runtime_interface_internal_INCLUDED
#define dsl_runtime_interface_internal_INCLUDED

#include <vector>

#include "dsl_ir_image.h"

/* Journal one tensor return store and its source/result projection contracts. */
typedef struct {
    WN *parent_block;
    WN *store;
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *result_formal;
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *source_value;
} DSL_RUNTIME_RETURN_SITE;

/* Prove that a logical value is owned by the named PU and stable value row. */
extern BOOL DSL_Runtime_Interface_Value_Owner_Valid
                                (ST_IDX owner_pu_st,
                                 const DSL_IR_VALUE_RECORD &value);

/* Find the physical BLOCK that directly or recursively contains target. */
extern WN *DSL_Runtime_Interface_Parent_Block (WN *tree, const WN *target);

/*
 * Preflight all projected return stores without mutation and append their
 * physical/logical relationships to sites.
 */
extern BOOL DSL_Runtime_Interface_Collect_Returns
                                (WN *tree,
                                 WN *parent_block,
                                 ST_IDX owner_pu_st,
                                 const DSL_RUNTIME_INTERFACE_PLAN *plan,
                                 std::vector<DSL_RUNTIME_RETURN_SITE> *sites,
                                 FILE *diagnostic);

/* Create the owner-local runtime handle symbol described by request. */
extern ST_IDX DSL_Runtime_Interface_Create_Handle_ST
                                (const DSL_RUNTIME_VALUE_PROJECTION_REQUEST
                                     &request);

/* Detect executable uses of canonical tensor source symbols for one PU. */
extern BOOL DSL_Runtime_Interface_Tree_Uses_Source_ST
                                (WN *tree, ST_IDX owner_pu_st);

/* Validate one persisted runtime-input record against tensor/TCON contracts. */
extern BOOL DSL_Program_Interface_Runtime_Input_Contract_Valid
                                (const DSL_RUNTIME_INPUT_RECORD *record);

#endif /* dsl_runtime_interface_internal_INCLUDED */

