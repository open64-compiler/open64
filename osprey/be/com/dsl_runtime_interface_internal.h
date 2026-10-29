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
#include "dsl_runtime_interface.h"

#include "dsl_ir_image.h"

/* Journal one tensor return store and its source/result projection contracts. */
typedef struct {
    WN *parent_block;
    WN *store;
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *result_formal;
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *source_value;
} DSL_RUNTIME_RETURN_SITE;

/* Match value ID/ST/name through unique exact owner-name metadata with valid
 * shared rows/strings and a live global PU ST. No local symbol/tree residency
 * proof, mutation or PU activation; FALSE leaves caller storage untouched. */
extern BOOL DSL_Runtime_Interface_Value_Owner_Valid
                                (ST_IDX owner_pu_st,
                                 const DSL_IR_VALUE_RECORD &value);

/* Find pointer-equal target as a BLOCK statement in a caller-selected live tree;
 * return the borrowed containing BLOCK, or NULL for absence/NULL tree. Does not
 * validate owner/context, attach WNs, mutate the tree or extend its lifetime. */
extern WN *DSL_Runtime_Interface_Parent_Block (WN *tree, const WN *target);

/*
 * Under a validated readable plan and matching owner locals/shared rows, append
 * borrowed projected-return relationships to nonnull caller sites scratch.
 * NULL tree succeeds. FALSE diagnoses unsupported stores/type relations and
 * may retain earlier scratch entries, not tree/image changes. Caller keeps
 * request/WN lifetimes stable through commit; allocation exceptions escape.
 */
extern BOOL DSL_Runtime_Interface_Collect_Returns
                                (WN *tree,
                                 WN *parent_block,
                                 ST_IDX owner_pu_st,
                                 const DSL_RUNTIME_INTERFACE_PLAN *plan,
                                 std::vector<DSL_RUNTIME_RETURN_SITE> *sites,
                                 FILE *diagnostic);

/* Commit-only symbol/string allocation under selected resident locals: derive
 * AUTO/FORMAL/FORMAL_REF class from a validated binding request, assign handle
 * TY and copy source-ST position. Caller owns tree placement/lifetime and
 * wider failure handling; no canonical-TY edit or allocation rollback. */
extern ST_IDX DSL_Runtime_Interface_Create_Handle_ST
                                (const DSL_RUNTIME_VALUE_PROJECTION_REQUEST
                                     &request);

/* Scan borrowed WNs for any symbolic reference to a stored projection's source
 * ST under the supplied owner, including retired rows. NULL is empty; no PU
 * activation, type/ownership proof, row mutation or distinction between reads
 * and definitions. Caller supplies coherent projection rows/tree lifetime. */
extern BOOL DSL_Runtime_Interface_Tree_Uses_Source_ST
                                (WN *tree, ST_IDX owner_pu_st);

/* Read-only input profile/source-TCON/owner check with valid shared tables;
 * only consult external reference/local ST facts when the source owner is
 * already active. Require that matching local context in that case; never
 * switch PUs. Role/row ID/flags/reserved, physical ABI and payload authenticity
 * remain outer-validator/caller obligations. NULL/mismatch returns FALSE. */
extern BOOL DSL_Program_Interface_Runtime_Input_Contract_Valid
                                (const DSL_RUNTIME_INPUT_RECORD *record);

#endif /* dsl_runtime_interface_internal_INCLUDED */

