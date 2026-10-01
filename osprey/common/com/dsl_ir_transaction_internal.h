/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private ownership predicates shared by DSL transaction implementations.
 * These helpers are not producer APIs and do not define mapped-image state.
 * See doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md.
 */

#ifndef dsl_ir_transaction_internal_INCLUDED
#define dsl_ir_transaction_internal_INCLUDED

#include "pu_info.h"
#include "symtab.h"

/*
 * Validate that an ST_IDX names a global function with a live PU entry.
 * Transaction preflight uses this before consulting owner-local values.
 */
static inline BOOL
DSL_IR_Image_PU_ST_Valid (ST_IDX st)
{
    return ST_IDX_level(st) == GLOBAL_SYMTAB && ST_IDX_index(st) != 0 &&
           ST_IDX_index(st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(st) == CLASS_FUNC && ST_pu(St_Table[st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[st]) < PU_Table_Size();
}

/*
 * Validate that owner_pu_st is the PU whose local symtab and WHIRL tree are
 * active. Callers must establish this before reading or mutating local ST_IDX.
 */
static inline BOOL
DSL_IR_Image_Current_PU_Is (ST_IDX owner_pu_st)
{
    return DSL_IR_Image_PU_ST_Valid(owner_pu_st) && Current_pu != NULL &&
           Current_pu == &Pu_Table[ST_pu(St_Table[owner_pu_st])];
}

#endif /* dsl_ir_transaction_internal_INCLUDED */

