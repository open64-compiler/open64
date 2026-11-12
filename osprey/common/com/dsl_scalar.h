/* Shared, typed scalar meaning for elementwise tensor wrappers. This header
 * maps existing logical IDs to ordinary WHIRL operations, not a second rule
 * engine or a new persisted schema. Callers retain tensor shape/broadcast,
 * numerical binding and phase ownership. No WN, table or target is selected.
 * Design: doc/AI-COMPILER-NATIVE-TENSOR-EXPRESSION-CONTRACT-v0.1.md. */

#ifndef DSL_SCALAR_INCLUDED
#define DSL_SCALAR_INCLUDED

#include "dsl_opcode.h"
#include "opcode.h"
#include "wintrinsic.h"

typedef struct {
    OPCODE opcode;
    INTRINSIC intrinsic;
    UINT32 arity;
    BOOL requires_fp_binding;
} DSL_SCALAR_OPERATION;

/* Return the established operation family without inventing a scalar opcode.
 * Target constant folding keeps its own element-width handling; a narrow tensor
 * storage mtype need not have a legal same-width WHIRL register opcode. Misses
 * preserve optional output and never change a registry or compiler table. */
static inline BOOL
DSL_scalar_operator (DSL_OPERATOR logical_operator, UINT16 version,
                     OPERATOR *scalar_operator)
{
    if (version != 1)
        return FALSE;
    OPERATOR result;
    switch (logical_operator) {
    case OPR_DSLADD: result = OPR_ADD; break;
    case OPR_DSLSUB: result = OPR_SUB; break;
    case OPR_DSLMUL: result = OPR_MPY; break;
    case OPR_DSLDIV: result = OPR_DIV; break;
    case OPR_DSLREM: result = OPR_REM; break;
    case OPR_DSLGREATER: result = OPR_GT; break;
    case OPR_DSLLOGICALNOT: result = OPR_LNOT; break;
    case OPR_DSLSELECT: result = OPR_SELECT; break;
    case OPR_DSLEXP:
    case OPR_DSLLOG:
    case OPR_DSLISFINITE:
        result = OPR_INTRINSIC_OP;
        break;
    default:
        return FALSE;
    }
    if (scalar_operator != NULL)
        *scalar_operator = result;
    return TRUE;
}

/* Describe one already type-checked element operation without admitting a
 * whole tensor schema or emitting an intrinsic placeholder. Predicate results
 * use WHIRL's register-sized I4, not the tensor bool U1 storage carrier.
 * Misses preserve output; a NULL output probes support. EXP/LOG describe the
 * established intrinsics; EXP/LOG's source implementation and ISFINITE's
 * exception behavior require a reviewed binding before simplifier dispatch. */
static inline BOOL
DSL_scalar_operation (DSL_OPERATOR logical_operator, UINT16 version,
                      TYPE_ID element_mtype, DSL_SCALAR_OPERATION *operation)
{
    OPERATOR scalar_operator;
    if (!DSL_scalar_operator(logical_operator, version, &scalar_operator) ||
        (UINT32)element_mtype <= MTYPE_UNKNOWN ||
        (UINT32)element_mtype > MTYPE_LAST ||
        (!MTYPE_is_integral(element_mtype) &&
         element_mtype != MTYPE_F4 && element_mtype != MTYPE_F8))
        return FALSE;

    DSL_SCALAR_OPERATION result;
    result.intrinsic = INTRINSIC_NONE;
    result.arity = 2;
    result.requires_fp_binding = FALSE;
    TYPE_ID result_mtype = element_mtype;
    TYPE_ID descriptor_mtype = MTYPE_V;
    switch (logical_operator) {
    case OPR_DSLADD:
    case OPR_DSLSUB:
    case OPR_DSLMUL:
    case OPR_DSLDIV:
        break;
    case OPR_DSLREM:
        if (!MTYPE_is_integral(element_mtype))
            return FALSE;
        break;
    case OPR_DSLGREATER:
        result_mtype = MTYPE_I4;
        descriptor_mtype = element_mtype;
        break;
    case OPR_DSLLOGICALNOT:
        if (element_mtype != MTYPE_I4)
            return FALSE;
        result.arity = 1;
        break;
    case OPR_DSLSELECT:
        result.arity = 3;
        break;
    case OPR_DSLEXP:
    case OPR_DSLLOG:
    case OPR_DSLISFINITE:
        if (element_mtype != MTYPE_F4 && element_mtype != MTYPE_F8)
            return FALSE;
        result.arity = 1;
        result.requires_fp_binding = TRUE;
        if (logical_operator == OPR_DSLEXP)
            result.intrinsic = element_mtype == MTYPE_F4 ?
                INTRN_F4EXP : INTRN_F8EXP;
        else if (logical_operator == OPR_DSLLOG)
            result.intrinsic = element_mtype == MTYPE_F4 ?
                INTRN_F4LOG : INTRN_F8LOG;
        else {
            result.intrinsic = element_mtype == MTYPE_F4 ?
                INTRN_F4FINITE : INTRN_F8FINITE;
            result_mtype = MTYPE_I4;
        }
        break;
    default:
        return FALSE;
    }
    result.opcode = OPCODE_make_op_MACRO(scalar_operator, result_mtype,
                                         descriptor_mtype);
    if (!Is_Valid_Opcode(result.opcode))
        return FALSE;
    if (operation != NULL)
        *operation = result;
    return TRUE;
}

#endif
