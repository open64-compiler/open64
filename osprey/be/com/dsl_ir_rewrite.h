/*
 * Backend-free native WHIRL definition lookup, rewrite and retirement.
 * The caller owns legality and active-PU scope; requests borrow all inputs.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_IR_REWRITE_INCLUDED
#define DSL_IR_REWRITE_INCLUDED

#include <stdio.h>
#include "../../common/com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

/*
 * Runtime-only request for atomically replacing a native DSL definition and
 * its logical image record. Operand templates and arrays are borrowed for the
 * call. Stable node, value, result symbol/type, and source identities remain
 * unchanged.
 */
typedef struct {
    DSL_OPERATOR expected_operator;
    UINT16 expected_version;
    UINT16 replacement_version;
    DSL_OPERATOR replacement_operator;
    const WN *const *operand_templates;
    const DSL_IR_VALUE_ID *operand_value_ids;
    UINT32 operand_count;
    const DSL_IR_ATTRIBUTE_RECORD *attributes;
    UINT32 attribute_count;
    STR_IDX payload;
    UINT32 result_value_kind;
} DSL_IR_NATIVE_VALUE_REWRITE_REQUEST;

/*
 * Runtime-only redirect/retire request. PU_Info supplied to the transaction is
 * the sole tree/owner authority; the common containing BLOCK is derived from
 * the two definitions during preflight.
 */
typedef struct {
    WN *replacement_definition;
    DSL_IR_VALUE_ID replacement_value_id;
    WN *retiring_definition;
    DSL_IR_VALUE_ID retiring_value_id;
    DSL_OPERATOR expected_retiring_operator;
    UINT16 expected_retiring_version;
    UINT16 replacement_operand_ordinal;
} DSL_IR_NATIVE_VALUE_RETIRE_REQUEST;

/*
 * Resolve one native STID definition to its stable logical result value in the
 * exact active PU. PU_Info is the sole owner authority; the query rejects an
 * inactive PU, ambiguous owner-qualified rows and physical/logical opcode
 * disagreement. Caller supplies valid shared/local tables; this does not prove
 * definition membership in the tree or uniqueness of its physical STID. Optional
 * output is a scalar row copy (NULL probes); FALSE preserves output. No retained
 * WN/row pointer, table mutation or PU activation.
 */
extern BOOL DSL_IR_Image_Find_Definition_Value
                                (PU_Info *pu_info,
                                 const WN *definition,
                                 DSL_IR_VALUE_RECORD *value_record);

/*
 * Atomically replace one native/logical operation in the exact active PU.
 * Preflight proves the expected operator, schema, operands, result ST/TY,
 * owner, and source identity. Commit preserves stable node/value identity and
 * updates the physical WN plus logical image together. Rejection leaves the
 * original physical definition unchanged, but temporary WNs, strings and an
 * ensured opcode row may remain allocated. No general allocation rollback.
 */
extern BOOL DSL_IR_Rewrite_Native_Value
                                (PU_Info *pu_info,
                                 WN *definition,
                                 DSL_IR_VALUE_ID value_id,
                                 const DSL_IR_NATIVE_VALUE_REWRITE_REQUEST
                                     *request);

/*
 * Atomically redirect owner-safe uses to a dominating replacement and retire
 * one pure definition. The transaction derives the PU tree and common parent
 * BLOCK from PU_Info, preflights WHIRL, image, call-ABI, effect, and REGION
 * uses, then commits without accepting caller-supplied physical ownership.
 * FALSE is preflight-only and leaves IR unchanged; post-commit image/runtime/
 * REGION failures assert rather than roll back. No general allocation recovery
 * or canonical-TY mutation. Caller supplies resident locals/shared tables and
 * treats TRUE as completion of this selected-PU mechanism, not domain proof.
 */
extern BOOL DSL_IR_Redirect_And_Retire_Native_Value
                                (PU_Info *pu_info,
                                 const DSL_IR_NATIVE_VALUE_RETIRE_REQUEST
                                     *request);

/* Use the same selected-PU retirement preflight/commit boundary for distinct
 * representation-preserving zero-motion view TYs. Also require unused retired
 * projection handles and compatible replacement projection; redirect LDID TYs
 * and record view retirement, leaving canonical TYs unchanged. FALSE precedes
 * writes; postcheck failures assert with no rollback journal or PU switch. */
extern BOOL DSL_IR_Redirect_And_Retire_Native_View
                                (PU_Info *pu_info,
                                 const DSL_IR_NATIVE_VALUE_RETIRE_REQUEST
                                     *request);

#endif /* DSL_IR_REWRITE_INCLUDED */
