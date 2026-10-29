/*
 * Private backend-free physical PU queries shared by external tensors, native
 * rewrite and CKKS expansion. One shared header definition gives each consumer
 * an internal-linkage helper; no tables, mutation authority or policy is exported.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR / service separation.
 */

#ifndef DSL_IR_REWRITE_QUERIES_INTERNAL_INCLUDED
#define DSL_IR_REWRITE_QUERIES_INTERNAL_INCLUDED

#include <string>
#include <string.h>
#include "../../common/ir_image/dsl_image_service.h"
#include "dsl_ir_transaction_internal.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"

/*
 * Match a value ID through exact owner-name/ST/name metadata before local ST
 * access. Require valid shared row/string state and a global function owner;
 * uniqueness comes from Find_PU_Value, not a physical tree/ST residency proof.
 * Read-only; FALSE leaves caller data unchanged and no PU is activated.
 */
static BOOL
DSL_IR_Image_Value_Belongs_To_PU
        (const DSL_IR_VALUE_RECORD &value, ST_IDX owner_pu_st)
{
    DSL_IR_VALUE_RECORD owned_value;

    return DSL_IR_Image_PU_ST_Valid(owner_pu_st) &&
           value.name != STR_IDX_ZERO &&
           DSL_IR_Image_Find_PU_Value
               (value.st, Index_To_Str(value.name),
                ST_name(St_Table[owner_pu_st]), &owned_value) &&
           owned_value.id == value.id;
}

/* Search a caller-selected tree for pointer-equal target as a BLOCK statement;
 * return its borrowed containing BLOCK, or NULL for missing/NULL inputs. No PU
 * context is checked or changed, and nonstatement kids have no BLOCK result. */
static WN *
DSL_IR_Find_Containing_Block (WN *tree, const WN *target)
{
    if (tree == NULL || target == NULL)
        return NULL;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *current = WN_first(tree); current != NULL;
             current = WN_next(current)) {
            if (current == target)
                return tree;
            WN *containing = DSL_IR_Find_Containing_Block(current, target);
            if (containing != NULL)
                return containing;
        }
        return NULL;
    }
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
        WN *containing = DSL_IR_Find_Containing_Block
                             (WN_kid(tree, kid), target);
        if (containing != NULL)
            return containing;
    }
    return NULL;
}

/*
 * Resolve a readonly, passed-not-saved by-reference LDA actual through its
 * associated callsite owner and resident local ST/name. Caller supplies the
 * active owner's valid shared/local context and readable WNs. This does not
 * compare a call-ABI argument row, PARM/address TYs or the actual's source TY.
 * Find_PU_Value copies on success (NULL output probes); FALSE preserves output.
 * No mutation or PU activation; later materialization preflight proves types.
 */
static BOOL
DSL_IR_Value_From_Call_Actual
        (ST_IDX owner_pu_st,
         const WN *call,
         UINT32 ordinal,
         DSL_IR_VALUE_RECORD *value)
{
    DSL_CALLSITE_METADATA_RECORD callsite;
    if (call == NULL || WN_operator(call) != OPR_CALL ||
        ordinal >= (UINT32)WN_kid_count(call) ||
        !DSL_Call_Image_Find_Callsite(call, &callsite) ||
        callsite.owner_pu_st != owner_pu_st)
        return FALSE;

    const WN *parm = WN_kid(call, ordinal);
    const WN *address = parm == NULL || WN_operator(parm) != OPR_PARM ?
                        NULL : WN_kid0(parm);
    ST_IDX actual_st = address == NULL ? ST_IDX_ZERO : WN_st_idx(address);
    if (address == NULL || !WN_Parm_By_Reference(parm) ||
        !WN_Parm_Read_Only(parm) || !WN_Parm_Passed_Not_Saved(parm) ||
        WN_operator(address) != OPR_LDA ||
        ST_IDX_level(actual_st) != CURRENT_SYMTAB ||
        ST_IDX_index(actual_st) == 0 ||
        ST_IDX_index(actual_st) >= ST_Table_Size(CURRENT_SYMTAB))
        return FALSE;
    return DSL_IR_Image_Find_PU_Value
               (actual_st, ST_name(St_Table[actual_st]),
                ST_name(St_Table[owner_pu_st]), value);
}

/* Test exact owner-name metadata and value spelling across stored rows, including
 * terminal provenance. Require valid global owner/name/shared strings; no local
 * ST or tree proof, interning or mutation. Scratch allocation failures escape. */
static BOOL
DSL_IR_PU_Value_Name_Exists (ST_IDX owner_pu_st, const char *name)
{
    std::string owner = "owner_pu=";
    owner += ST_name(St_Table[owner_pu_st]);
    for (DSL_IR_VALUE_ID id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(id, &value) ||
            value.name == STR_IDX_ZERO || value.metadata == STR_IDX_ZERO)
            continue;
        if (strcmp(Index_To_Str(value.name), name) == 0 &&
            owner == Index_To_Str(value.metadata))
            return TRUE;
    }
    return FALSE;
}

/*
 * Reject any retiring-symbol reference outside its containing BLOCK, enforcing
 * the v1 same-block dominance rule through a read-only PU traversal.
 */
static BOOL
DSL_IR_Retire_Has_Use_Outside_Block
        (const WN *wn, const WN *containing_block, ST_IDX retiring_st)
{
    if (wn == NULL || wn == containing_block)
        return FALSE;
    if (WN_has_sym(wn) && WN_st_idx(wn) == retiring_st)
        return TRUE;
    if (WN_operator(wn) == OPR_BLOCK) {
        for (const WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement)) {
            if (DSL_IR_Retire_Has_Use_Outside_Block
                    (statement, containing_block, retiring_st))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(wn); ++kid) {
        if (DSL_IR_Retire_Has_Use_Outside_Block
                (WN_kid(wn, kid), containing_block, retiring_st))
            return TRUE;
    }
    return FALSE;
}

#endif /* DSL_IR_REWRITE_QUERIES_INTERNAL_INCLUDED */
