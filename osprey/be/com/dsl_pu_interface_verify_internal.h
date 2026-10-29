/*
 * Private structural TY predicates and formal ordering for backend-free
 * PU-interface verification and external runtime-input checks. The inline
 * comparator only orders borrowed rows; no image or PU state is owned here.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR / service separation.
 */

#ifndef DSL_PU_INTERFACE_VERIFY_INTERNAL_INCLUDED
#define DSL_PU_INTERFACE_VERIFY_INTERNAL_INCLUDED

#include "../../common/com/dsl_ir_image.h"

/* Check a nonzero packed TY and its index bound without local symbol access. */
extern BOOL DSL_Program_Interface_TY_Contract_Valid (TY_IDX ty);

/* Require an existing pointer TY; do not validate its pointee or activate a PU. */
extern BOOL DSL_Program_Interface_Pointer_TY_Contract_Valid (TY_IDX ty);

/* Require an existing tensor-extension TY without changing canonical types. */
extern BOOL DSL_Program_Interface_Tensor_TY_Contract_Valid (TY_IDX ty);

/* Require a nonzero in-range TCON index; no payload authentication is done. */
extern BOOL DSL_Program_Interface_TCON_Contract_Valid (TCON_IDX tcon);

struct DSL_PU_FORMAL_ORDINAL_LESS {
    /* Order borrowed formal rows by ordinal; equal ordinals compare false. */
    inline BOOL operator()(const DSL_PU_FORMAL_RECORD &kid0,
                    const DSL_PU_FORMAL_RECORD &kid1) const
    { return kid0.formal_ordinal < kid1.formal_ordinal; }
};

#endif /* DSL_PU_INTERFACE_VERIFY_INTERNAL_INCLUDED */
