/*
 * VHO-owned executable CKKS expansion of caller-selected native value groups.
 * Preflight is read-only; commit coordinates physical WHIRL and existing image,
 * string, symbol and event checkpoints. No optimization selection or PU switch.
 * Design: doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md and
 * doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR / service separation.
 */

#include <ctype.h>
#include "../../common/ir_image/dsl_image_service.h"
#include "../com/dsl_pu_interface_verify.h"
#include "../com/dsl_ir_rewrite.h"
#include "../com/dsl_external_tensor.h"
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <algorithm>
#include <set>
#include <string>
#include <vector>

#include "dsl_memory_behavior.h"
#include "../vho/dsl_ckks_expand.h"
#include "../vho/dsl_ckks_expand_internal.h"
#include "dsl_ckks_event_internal.h"
#include "dsl_tensor_fold.h"
#include "dsl_ir_image.h"
#include "../com/dsl_ir_transaction_internal.h"
#include "../com/dsl_runtime_interface_internal.h"
#include "dsl_region.h"
#include "dsl_shape.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"

#include "../com/dsl_ir_rewrite_queries_internal.h"

/* Compare/replace one associated call-ABI value after owner preflight; FALSE
 * changes nothing and a missing argument is a TRUE no-op. No physical actual
 * edit or broader rollback; the expansion journal coordinates those. */
extern BOOL DSL_Call_ABI_Image_Update_Argument_Value
                                (const WN *, UINT32, DSL_IR_VALUE_ID,
                                 DSL_IR_VALUE_ID);
/* Commit a preflighted logical redirect/retirement over image rows. Caller
 * proves physical, owner and consumer legality and coordinates wider
 * rollback. */
extern BOOL DSL_IR_Image_Redirect_And_Retire_Value
                                (DSL_IR_VALUE_ID, DSL_IR_VALUE_ID, UINT32,
                                 BOOL);
/* Check image projection compatibility before a cross-TY view retirement; no
 * row mutation. */
extern BOOL DSL_Runtime_Interface_Image_Can_Retire_View
                                (ST_IDX, DSL_IR_VALUE_ID, DSL_IR_VALUE_ID);
/* Commit compatible view-projection retirement after owner preflight; no
 * physical WHIRL or canonical TY mutation. */
extern BOOL DSL_Runtime_Interface_Image_Retire_View
                                (ST_IDX, DSL_IR_VALUE_ID, DSL_IR_VALUE_ID);
/* Commit a checked projection source redirect over image rows; the physical
 * transaction owns preflight and combined rollback. */
extern BOOL DSL_Runtime_Interface_Image_Redirect_Value
                                (ST_IDX, DSL_IR_VALUE_ID, DSL_IR_VALUE_ID);
/* Count selected-PU REGION uses without activating another PU or modifying
 * its interface. */
extern UINT32 DSL_Region_Symbol_Use_Count (PU_Info *, ST_IDX);
/* Read-only REGION preflight for the caller-selected PU and
 * source/replacement symbols; no PU activation. */
extern BOOL DSL_Region_Can_Redirect_Symbol (PU_Info *, ST_IDX, ST_IDX);
/* Apply the previously checked REGION symbol redirect in the selected PU. The
 * owning transaction coordinates physical/image edits and rollback. */
extern BOOL DSL_Region_Redirect_Symbol (PU_Info *, ST_IDX, ST_IDX);

/* Report the first expansion preflight/verification error without mutation. */
static BOOL
DSL_IR_CKKS_Report (FILE *diagnostic, const char *message, UINT32 index)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL CKKS expansion error: %s index=%u\n",
                message, index);
    return FALSE;
}

/* Match every registry schema name exactly once with no extra step attributes,
 * then require nonempty values without forbidden payload separators. Require
 * readable count-sized step arrays; no semantic value/numerical proof, string
 * interning or mutation. FALSE rejects only this static admission contract. */
static BOOL
DSL_IR_CKKS_Attributes_Match
        (const DSL_OPERATOR_INFO &info,
         const DSL_CKKS_EXPANSION_STEP &step)
{
    const char *schema = info.attribute_schema == NULL ? "" :
                         info.attribute_schema;
    UINT32 required_count = 0;
    const char *cursor = schema;
    while (*cursor != '\0') {
        const char *end = strchr(cursor, ';');
        size_t length = end == NULL ? strlen(cursor) :
                                     (size_t)(end - cursor);
        if (length == 0)
            return FALSE;
        ++required_count;
        UINT32 matches = 0;
        for (UINT32 i = 0; i < step.attribute_count; ++i) {
            const DSL_CKKS_EXPANSION_ATTRIBUTE &attribute =
                step.attributes[i];
            if (attribute.name != NULL &&
                strlen(attribute.name) == length &&
                strncmp(attribute.name, cursor, length) == 0)
                ++matches;
        }
        if (matches != 1)
            return FALSE;
        if (end == NULL)
            break;
        cursor = end + 1;
    }
    if (required_count != step.attribute_count)
        return FALSE;
    for (UINT32 i = 0; i < step.attribute_count; ++i) {
        const DSL_CKKS_EXPANSION_ATTRIBUTE &attribute = step.attributes[i];
        if (attribute.name == NULL || attribute.value == NULL ||
            attribute.value[0] == '\0' ||
            strpbrk(attribute.name, ";=\n\r") != NULL ||
            strpbrk(attribute.value, ";\n\r") != NULL)
            return FALSE;
    }
    return TRUE;
}

/* Build copied legacy payload text from existing-value or prior-step operand
 * names and step attributes. Require preflighted indices/strings; no IR
 * mutation, allocation exceptions propagate. */
static std::string
DSL_IR_CKKS_Payload
        (const DSL_CKKS_EXPANSION_REQUEST &request,
         UINT32 step_index)
{
    const DSL_CKKS_EXPANSION_STEP &step = request.steps[step_index];
    std::string payload;
    for (UINT32 i = 0; i < step.operand_count; ++i) {
        const DSL_CKKS_EXPANSION_OPERAND &operand = step.operands[i];
        const char *name = NULL;
        DSL_IR_VALUE_RECORD value;
        if (operand.kind == DSL_CKKS_EXPANSION_EXISTING_VALUE &&
            DSL_IR_Image_Get_Value(operand.value_id, &value))
            name = Index_To_Str(value.name);
        else if (operand.kind == DSL_CKKS_EXPANSION_PRIOR_STEP)
            name = request.steps[operand.step_index].result_name;
        char ordinal[32];
        snprintf(ordinal, sizeof(ordinal), "%u", i);
        if (!payload.empty())
            payload += ";";
        payload += "kid";
        payload += ordinal;
        payload += "=";
        payload += name == NULL ? "" : name;
    }
    for (UINT32 i = 0; i < step.attribute_count; ++i) {
        if (!payload.empty())
            payload += ";";
        payload += step.attributes[i].name;
        payload += "=";
        payload += step.attributes[i].value;
    }
    return payload;
}

typedef struct {
    WN *call;
    WN *address; /* NULL for an exact by-value runtime projection. */
    UINT32 actual_ordinal;
} DSL_IR_CKKS_CALL_USE;

typedef struct {
    ST_IDX source_st;
    WN *source_definition;
    const std::vector<DSL_IR_CKKS_CALL_USE> *calls;
    BOOL source_seen;
    BOOL valid;
    UINT32 definition_count;
    std::vector<WN *> reads;
    std::vector<WN *> addresses;
} DSL_IR_CKKS_USE_SCAN;

/* Census one source definition plus subsequent LDID reads and only preflighted
 * call-actual LDA pointers in caller scratch. Reject other definitions, early
 * uses and escapes by clearing valid; lists/counts may already be partial.
 * Require initialized scan/live WNs; no tree writes or general CFG proof. */
static void
DSL_IR_CKKS_Scan_Tree (WN *wn, DSL_IR_CKKS_USE_SCAN *scan)
{
    if (wn == NULL || scan == NULL || !scan->valid)
        return;
    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement)) {
            if (statement == scan->source_definition) {
                if (scan->source_seen) {
                    scan->valid = FALSE;
                    return;
                }
                for (INT32 kid = 0; kid < WN_kid_count(statement); ++kid)
                    DSL_IR_CKKS_Scan_Tree(WN_kid(statement, kid), scan);
                ++scan->definition_count;
                scan->source_seen = TRUE;
            } else {
                DSL_IR_CKKS_Scan_Tree(statement, scan);
            }
        }
        return;
    }
    if (WN_has_sym(wn) && WN_st_idx(wn) == scan->source_st) {
        if (WN_operator(wn) == OPR_STID) {
            ++scan->definition_count;
            if (wn != scan->source_definition)
                scan->valid = FALSE;
        } else if (WN_operator(wn) == OPR_LDID) {
            if (scan->source_seen)
                scan->reads.push_back(wn);
            else
                scan->valid = FALSE;
        } else if (WN_operator(wn) == OPR_LDA) {
            BOOL allowed = FALSE;
            for (UINT32 i = 0; i < scan->calls->size(); ++i) {
                if ((*scan->calls)[i].address == wn) {
                    allowed = TRUE;
                    break;
                }
            }
            if (allowed && scan->source_seen)
                scan->addresses.push_back(wn);
            else
                scan->valid = FALSE;
        } else {
            scan->valid = FALSE;
        }
    }
    for (INT32 kid = 0; scan->valid && kid < WN_kid_count(wn); ++kid)
        DSL_IR_CKKS_Scan_Tree(WN_kid(wn, kid), scan);
}

/* Collect permitted source call uses after checking owner, effective ordinal,
 * physical actual and runtime projection contracts. FALSE may leave scratch
 * uses partial, but changes no WN/image rows. */
static BOOL
DSL_IR_CKKS_Preflight_Calls
        (ST_IDX owner_pu_st, const DSL_IR_VALUE_RECORD &source,
         std::vector<DSL_IR_CKKS_CALL_USE> *uses, FILE *diagnostic)
{
    for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_ABI_Image_Get_Argument(i, &argument))
            return DSL_IR_CKKS_Report
                       (diagnostic, "missing call argument", i);
        if (argument.argument_value_id != source.id)
            continue;
        DSL_RETIRED_CALL_ARGUMENT_RECORD retired;
        if (DSL_Program_Interface_Image_Find_Retired_Call
                (argument.id, &retired))
            continue;
        DSL_CALLSITE_METADATA_RECORD callsite;
        const WN *call = DSL_Call_Image_Get_Call_WN(argument.callsite_id);
        if (!DSL_Call_Image_Get_Callsite
                (argument.callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st || call == NULL)
            return DSL_IR_CKKS_Report
                       (diagnostic, "call owner mismatch", i);
        UINT32 effective_ordinal = argument.actual_ordinal;
        for (UINT32 j = 1;
             j <= DSL_Program_Interface_Image_Retired_Call_Count(); ++j) {
            DSL_RETIRED_CALL_ARGUMENT_RECORD retired_call;
            if (DSL_Program_Interface_Image_Get_Retired_Call
                    (j, &retired_call) &&
                retired_call.callsite_id == callsite.id &&
                retired_call.old_actual_ordinal < argument.actual_ordinal) {
                if (effective_ordinal == 0)
                    return DSL_IR_CKKS_Report
                               (diagnostic, "invalid call ordinal", i);
                --effective_ordinal;
            }
        }
        DSL_IR_VALUE_RECORD actual;
        if (WN_operator(call) != OPR_CALL ||
            WN_st_idx(call) != callsite.callee_pu_st ||
            effective_ordinal >= (UINT32)WN_kid_count(call))
            return DSL_IR_CKKS_Report
                       (diagnostic, "call actual mismatch", i);
        const WN *parm = WN_kid(call, effective_ordinal);
        const WN *address = parm == NULL || WN_operator(parm) != OPR_PARM ?
                            NULL : WN_kid0(parm);
        DSL_RUNTIME_CALL_PROJECTION_RECORD runtime_call;
        BOOL projected = DSL_Runtime_Interface_Image_Find_Call
                             (callsite.id, argument.actual_ordinal,
                              &runtime_call);
        if (projected) {
            DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
            if (runtime_call.owner_pu_st != owner_pu_st ||
                runtime_call.source_value_id != source.id ||
                runtime_call.actual_ordinal != argument.actual_ordinal ||
                runtime_call.callee_formal_ordinal !=
                    argument.callee_formal_ordinal ||
                runtime_call.direction != DSL_RUNTIME_CALL_INPUT ||
                !DSL_Runtime_Interface_Image_Get_Value
                    (runtime_call.value_projection_id, &projection) ||
                projection.owner_pu_st != owner_pu_st ||
                projection.source_value_id != source.id ||
                projection.source_st != source.st ||
                projection.source_ty != source.ty ||
                projection.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE ||
                projection.formal_ordinal !=
                    DSL_RUNTIME_INTERFACE_INVALID_ORDINAL ||
                ST_IDX_level(projection.handle_st) != CURRENT_SYMTAB ||
                ST_IDX_index(projection.handle_st) == 0 ||
                ST_IDX_index(projection.handle_st) >=
                    ST_Table_Size(CURRENT_SYMTAB) ||
                ST_type(St_Table[projection.handle_st]) !=
                    projection.handle_ty ||
                address == NULL || WN_operator(address) != OPR_LDID ||
                WN_st_idx(address) != projection.handle_st ||
                WN_ty(address) != projection.handle_ty ||
                WN_ty(parm) != projection.handle_ty ||
                WN_parm_flag(parm) !=
                    (WN_PARM_BY_VALUE | WN_PARM_READ_ONLY |
                     WN_PARM_PASSED_NOT_SAVED))
                return DSL_IR_CKKS_Report
                           (diagnostic, "runtime call actual mismatch", i);
            address = NULL;
        } else {
            if (!DSL_IR_Value_From_Call_Actual
                    (owner_pu_st, call, effective_ordinal, &actual) ||
                actual.id != source.id || address == NULL ||
                WN_Parm_Out(parm) ||
                TY_kind(WN_ty(parm)) != KIND_POINTER ||
                TY_pointed(WN_ty(parm)) != source.ty ||
                WN_st_idx(address) != source.st)
                return DSL_IR_CKKS_Report
                           (diagnostic, "call actual is not read-only", i);
        }
        for (UINT32 j = 0; j < uses->size(); ++j) {
            if (((*uses)[j].call == call &&
                 (*uses)[j].actual_ordinal == argument.actual_ordinal) ||
                (address != NULL && (*uses)[j].address == address))
                return DSL_IR_CKKS_Report
                           (diagnostic, "duplicate call actual", i);
        }
        DSL_IR_CKKS_CALL_USE use;
        use.call = const_cast<WN *>(call);
        use.address = const_cast<WN *>(address);
        use.actual_ordinal = argument.actual_ordinal;
        uses->push_back(use);
    }
    return TRUE;
}

/* With validated resident value ST and a known containing BLOCK/source STID,
 * accept FORMAL/FORMAL_REF immediately or an earlier same-ST STID in that BLOCK.
 * Read-only restricted ordering check, not unique-def or general CFG dominance;
 * caller keeps symbols/tree alive and proves source membership separately. */
static BOOL
DSL_IR_CKKS_Operand_Dominates
        (WN *block, WN *source_definition,
         const DSL_IR_VALUE_RECORD &value)
{
    ST_SCLASS sclass = ST_sclass(St_Table[value.st]);
    if (sclass == SCLASS_FORMAL || sclass == SCLASS_FORMAL_REF)
        return TRUE;
    for (WN *statement = WN_first(block); statement != NULL;
         statement = WN_next(statement)) {
        if (statement == source_definition)
            break;
        if (WN_operator(statement) == OPR_STID &&
            WN_st_idx(statement) == value.st)
            return TRUE;
    }
    return FALSE;
}

struct DSL_IR_CKKS_PHYSICAL_OPERAND {
    ST_IDX st;
    TY_IDX ty;
};

/* Read-only recursive LDA-use test for one handle; NULL has no address use. */
static BOOL
DSL_IR_CKKS_Has_Handle_Address_Use (const WN *wn, ST_IDX handle_st)
{
    if (wn == NULL)
        return FALSE;
    if (WN_operator(wn) == OPR_LDA && WN_st_idx(wn) == handle_st)
        return TRUE;
    if (WN_operator(wn) == OPR_BLOCK) {
        for (const WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement)) {
            if (DSL_IR_CKKS_Has_Handle_Address_Use(statement, handle_st))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(wn); ++kid) {
        if (DSL_IR_CKKS_Has_Handle_Address_Use
                (WN_kid(wn, kid), handle_st))
            return TRUE;
    }
    return FALSE;
}

/* Resolve a validated existing value to its semantic ST/TY or one exact projected
 * result handle with a preceding initialized by-reference CALL in this BLOCK.
 * Require nonnull physical scratch and resident owner/shared context; initialize
 * scratch before checks, so FALSE can leave it filled. No row/WN writes or PU
 * switch; handle initialization checks STID type/order, not its full RHS meaning. */
static BOOL
DSL_IR_CKKS_Resolve_Existing_Operand
        (WN *block, WN *source_definition, ST_IDX owner_pu_st,
         const DSL_IR_VALUE_RECORD &value,
         DSL_IR_CKKS_PHYSICAL_OPERAND *physical)
{
    DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
    physical->st = value.st;
    physical->ty = value.ty;
    if (!DSL_Runtime_Interface_Image_Find_Value
             (owner_pu_st, value.id, &projection))
        return DSL_IR_CKKS_Operand_Dominates
                   (block, source_definition, value);

    DSL_RUNTIME_CALL_PROJECTION_RECORD result_call;
    UINT32 result_count = 0;
    for (UINT32 i = 1; i <= DSL_Runtime_Interface_Image_Call_Count(); ++i) {
        DSL_RUNTIME_CALL_PROJECTION_RECORD current;
        if (!DSL_Runtime_Interface_Image_Get_Call(i, &current))
            return FALSE;
        if (current.source_value_id == value.id &&
            current.value_projection_id == projection.id &&
            current.direction == DSL_RUNTIME_CALL_RESULT) {
            result_call = current;
            ++result_count;
        }
    }
    if (result_count == 0)
        return DSL_IR_CKKS_Operand_Dominates
                   (block, source_definition, value);
    if (result_count != 1 || projection.flags != 0 ||
        projection.owner_pu_st != owner_pu_st ||
        projection.source_value_id != value.id ||
        projection.source_st != value.st ||
        projection.source_ty != value.ty ||
        projection.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE ||
        projection.formal_ordinal !=
            DSL_RUNTIME_INTERFACE_INVALID_ORDINAL ||
        ST_IDX_level(projection.handle_st) != CURRENT_SYMTAB ||
        ST_IDX_index(projection.handle_st) == 0 ||
        ST_IDX_index(projection.handle_st) >=
            ST_Table_Size(CURRENT_SYMTAB) ||
        ST_type(St_Table[projection.handle_st]) != projection.handle_ty ||
        ST_sclass(St_Table[projection.handle_st]) != SCLASS_AUTO ||
        result_call.owner_pu_st != owner_pu_st ||
        result_call.source_value_id != value.id ||
        result_call.value_projection_id != projection.id ||
        result_call.actual_ordinal != result_call.callee_formal_ordinal)
        return FALSE;

    DSL_CALLSITE_METADATA_RECORD callsite;
    const WN *call = DSL_Call_Image_Get_Call_WN(result_call.callsite_id);
    UINT32 effective_ordinal = result_call.actual_ordinal;
    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Retired_Call_Count(); ++i) {
        DSL_RETIRED_CALL_ARGUMENT_RECORD retired;
        if (!DSL_Program_Interface_Image_Get_Retired_Call(i, &retired))
            return FALSE;
        if (retired.callsite_id == result_call.callsite_id &&
            retired.old_actual_ordinal < result_call.actual_ordinal) {
            if (effective_ordinal == 0)
                return FALSE;
            --effective_ordinal;
        }
    }
    if (!DSL_Call_Image_Get_Callsite
             (result_call.callsite_id, &callsite) ||
        callsite.owner_pu_st != owner_pu_st || call == NULL ||
        WN_operator(call) != OPR_CALL ||
        WN_st_idx(call) != callsite.callee_pu_st ||
        effective_ordinal >= (UINT32)WN_kid_count(call))
        return FALSE;
    const WN *parm = WN_kid(call, effective_ordinal);
    const WN *address = parm == NULL || WN_operator(parm) != OPR_PARM ?
                        NULL : WN_kid0(parm);
    if (address == NULL || WN_operator(address) != OPR_LDA ||
        WN_st_idx(address) != projection.handle_st ||
        WN_ty(parm) != WN_ty(address) ||
        TY_kind(WN_ty(parm)) != KIND_POINTER ||
        TY_pointed(WN_ty(parm)) != projection.handle_ty ||
        WN_parm_flag(parm) !=
            (WN_PARM_BY_REFERENCE | WN_PARM_OUT |
             WN_PARM_PASSED_NOT_SAVED))
        return FALSE;

    BOOL initialized = FALSE;
    BOOL called = FALSE;
    for (WN *statement = WN_first(block); statement != NULL;
         statement = WN_next(statement)) {
        if (statement == source_definition)
            break;
        if (WN_operator(statement) == OPR_STID &&
            WN_st_idx(statement) == projection.handle_st) {
            if (called || WN_ty(statement) != projection.handle_ty)
                return FALSE;
            initialized = TRUE;
        }
        if (statement == call) {
            if (!initialized || called)
                return FALSE;
            called = TRUE;
        } else if (called &&
                   DSL_IR_CKKS_Has_Handle_Address_Use
                       (statement, projection.handle_st)) {
            return FALSE;
        }
    }
    if (!called)
        return FALSE;
    physical->st = projection.handle_st;
    physical->ty = projection.handle_ty;
    return TRUE;
}

/* Validate the complete caller-selected expansion against the exact active
 * PU, schema, groups, contexts, physical uses and operands. Fill borrowed-
 * pointer scratch only; rejection may leave partial scratch but does not mutate
 * IR or consume inputs. Caller supplies safe shared references/local context;
 * scratch allocation escapes and this is not a numerical/cryptographic proof. */
static BOOL
DSL_IR_CKKS_Preflight
        (PU_Info *pu_info, const DSL_CKKS_EXPANSION_REQUEST *request,
         DSL_IR_VALUE_RECORD *source_value, WN **source_block,
         std::vector<DSL_IR_CKKS_CALL_USE> *calls,
         DSL_IR_CKKS_USE_SCAN *scan,
         std::vector<DSL_IR_CKKS_PHYSICAL_OPERAND> *physical_operands,
         FILE *diagnostic)
{
    ST_IDX owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
                         PU_Info_proc_sym(pu_info);
    WN *pu_root = pu_info == NULL ? NULL : PU_Info_tree_ptr(pu_info);
    if (pu_info == NULL || request == NULL || source_value == NULL ||
        source_block == NULL || calls == NULL || scan == NULL ||
        pu_info != Current_PU_Info || pu_root == NULL ||
        !DSL_IR_Image_Current_PU_Is(owner_pu_st) ||
        request->source_definition == NULL ||
        request->groups == NULL || request->group_count == 0 ||
        request->steps == NULL || request->step_count == 0 ||
        request->contexts == NULL || request->context_count == 0 ||
        request->final_step_index >= request->step_count ||
        request->step_count >
            (~(UINT32)0 - DSL_CKKS_Event_Image_Count()) /
                request->context_count ||
        physical_operands == NULL || !DSL_IR_Image_Validate(NULL) ||
        !DSL_CKKS_Event_Image_Validate(NULL) ||
        (DSL_Runtime_Interface_Image_Has_Records() &&
         !DSL_Runtime_Interface_Image_Validate(NULL)))
        return DSL_IR_CKKS_Report
                   (diagnostic, "invalid request or PU", 0);
    *source_block = DSL_IR_Find_Containing_Block
                        (pu_root, request->source_definition);
    DSL_IR_NODE_RECORD source_node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD source_opcode;
    if (*source_block == NULL ||
        !DSL_IR_Image_Find_Definition_Value
            (pu_info, request->source_definition, source_value) ||
        source_value->id != request->source_value_id ||
        source_value->flags != DSL_IR_VALUE_FLAG_NONE ||
        !DSL_IR_Image_Value_Belongs_To_PU
            (*source_value, owner_pu_st) ||
        !DSL_Tensor_Has_Unique_Ownership(source_value->st) ||
        !DSL_IR_Image_Get_Node
            (source_value->producer_node_id, &source_node) ||
        source_node.flags != DSL_IR_NODE_FLAG_NONE ||
        !DSL_IR_Image_Get_Opcode_Descriptor
            (source_node.opcode_descriptor_id, &source_opcode) ||
        source_opcode.logical_operator !=
            request->expected_source_operator ||
        source_opcode.version != request->expected_source_version ||
        source_opcode.effect_model != DSL_EFFECT_MODEL_PURE ||
        DSL_CKKS_Event_Image_Has_Source(source_value->id))
        return DSL_IR_CKKS_Report
                   (diagnostic, "source definition mismatch", 0);
    for (UINT32 i = 1; i <= DSL_Effect_Image_State_Effect_Count(); ++i) {
        DSL_STATE_EFFECT_RECORD effect;
        if (!DSL_Effect_Image_Get_State_Effect(i, &effect) ||
            effect.owner_node_id == source_node.id)
            return DSL_IR_CKKS_Report
                       (diagnostic, "source has state effect", i);
    }
    for (UINT32 i = 1;
         i <= DSL_Runtime_Interface_Image_Value_Count(); ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
        if (!DSL_Runtime_Interface_Image_Get_Value(i, &projection))
            return DSL_IR_CKKS_Report
                       (diagnostic, "invalid runtime projection", i);
        if (projection.source_value_id == source_value->id &&
            (projection.owner_pu_st != owner_pu_st ||
             projection.source_st != source_value->st ||
             projection.source_ty != source_value->ty ||
             projection.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE))
            return DSL_IR_CKKS_Report
                       (diagnostic, "source runtime projection mismatch", i);
    }
    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Runtime_Input_Count(); ++i) {
        DSL_RUNTIME_INPUT_RECORD input;
        if (!DSL_Program_Interface_Image_Get_Runtime_Input(i, &input) ||
            input.source_value_id == source_value->id)
            return DSL_IR_CKKS_Report
                       (diagnostic, "source has runtime input", i);
    }

    UINT32 next_step = 0;
    physical_operands->clear();
    UINT32 prior_static_ordinal = 0;
    for (UINT32 i = 0; i < request->group_count; ++i) {
        const DSL_CKKS_EXPANSION_GROUP &group = request->groups[i];
        if (group.source_static_ordinal == 0 ||
            group.origin_static_ordinal == 0 ||
            group.source_static_ordinal <= prior_static_ordinal ||
            group.first_step != next_step || group.step_count == 0 ||
            group.step_count > request->step_count - next_step)
            return DSL_IR_CKKS_Report
                       (diagnostic, "invalid event group", i);
        prior_static_ordinal = group.source_static_ordinal;
        next_step += group.step_count;
    }
    if (next_step != request->step_count ||
        request->steps[request->final_step_index].result_ty !=
            source_value->ty)
        return DSL_IR_CKKS_Report
                   (diagnostic, "incomplete groups or final TY", 0);

    for (UINT32 i = 0; i < request->step_count; ++i) {
        const DSL_CKKS_EXPANSION_STEP &step = request->steps[i];
        DSL_OPERATOR_INFO info;
        if (step.dsl_operator < OPR_DSLCKKSADD ||
            step.dsl_operator > OPR_DSLCKKSBOOTSTRAP ||
            !DSL_Operator_Get_Info_Version
                (step.dsl_operator, step.version, &info) ||
            info.nkids < 0 ||
            (UINT32)info.nkids != step.operand_count ||
            info.effect_model != DSL_EFFECT_MODEL_PURE ||
            (step.operand_count != 0 && step.operands == NULL) ||
            (step.attribute_count != 0 && step.attributes == NULL) ||
            !DSL_IR_CKKS_Attributes_Match(info, step) ||
            step.result_name == NULL || step.result_name[0] == '\0' ||
            strpbrk(step.result_name, ";=\n\r") != NULL ||
            !TY_is_tensor_extension(step.result_ty) ||
            !TY_tensor_is_canonical(step.result_ty) ||
            SRCPOS_linenum(step.source_position) == 0 ||
            DSL_IR_PU_Value_Name_Exists
                (owner_pu_st, step.result_name))
            return DSL_IR_CKKS_Report
                       (diagnostic, "invalid CKKS step", i);
        for (UINT32 prior = 0; prior < i; ++prior) {
            if (strcmp(request->steps[prior].result_name,
                       step.result_name) == 0)
                return DSL_IR_CKKS_Report
                           (diagnostic, "duplicate result name", i);
        }
        for (UINT32 kid = 0; kid < step.operand_count; ++kid) {
            const DSL_CKKS_EXPANSION_OPERAND &operand = step.operands[kid];
            if (operand.kind == DSL_CKKS_EXPANSION_PRIOR_STEP) {
                if (operand.step_index >= i || operand.value_id != 0)
                    return DSL_IR_CKKS_Report
                               (diagnostic, "forward step operand", i);
                DSL_IR_CKKS_PHYSICAL_OPERAND physical =
                    { ST_IDX_ZERO, TY_IDX_ZERO };
                physical_operands->push_back(physical);
                continue;
            }
            DSL_IR_VALUE_RECORD value;
            DSL_IR_CKKS_PHYSICAL_OPERAND physical;
            if (operand.kind != DSL_CKKS_EXPANSION_EXISTING_VALUE ||
                operand.step_index != 0 ||
                operand.value_id == source_value->id ||
                !DSL_IR_Image_Get_Value(operand.value_id, &value) ||
                value.flags != DSL_IR_VALUE_FLAG_NONE ||
                (!TY_is_tensor_extension(value.ty) &&
                 (value.value_kind != DSL_IR_VALUE_SYMBOL ||
                  TY_mtype(value.ty) != MTYPE_F8 || kid != 1 ||
                  (step.dsl_operator != OPR_DSLCKKSADD &&
                   step.dsl_operator != OPR_DSLCKKSSUB &&
                   step.dsl_operator != OPR_DSLCKKSMUL))) ||
                !DSL_IR_Image_Value_Belongs_To_PU
                    (value, owner_pu_st) ||
                ST_IDX_level(value.st) != CURRENT_SYMTAB ||
                ST_IDX_index(value.st) == 0 ||
                ST_IDX_index(value.st) >= ST_Table_Size(CURRENT_SYMTAB) ||
                ST_type(St_Table[value.st]) != value.ty ||
                !DSL_IR_CKKS_Resolve_Existing_Operand
                     (*source_block, request->source_definition,
                      owner_pu_st, value, &physical))
                return DSL_IR_CKKS_Report
                           (diagnostic, "invalid existing operand", i);
            physical_operands->push_back(physical);
        }
    }

    for (UINT32 i = 0; i < request->context_count; ++i) {
        const DSL_CKKS_EXPANSION_CONTEXT &context = request->contexts[i];
        DSL_PU_SOURCE_IDENTITY_RECORD identity;
        DSL_IR_VALUE_RECORD origin;
        DSL_IR_NODE_RECORD origin_node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD origin_opcode;
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_PU_Identity
                (context.context_pu_identity_id, &identity) ||
            identity.owner_pu_st != owner_pu_st ||
            !DSL_IR_Image_PU_ST_Valid(context.origin_owner_pu_st) ||
            !DSL_IR_Image_Get_Value
                (context.origin_source_value_id, &origin) ||
            !DSL_IR_Image_Value_Belongs_To_PU
                (origin, context.origin_owner_pu_st) ||
            !DSL_IR_Image_Get_Node
                (origin.producer_node_id, &origin_node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                (origin_node.opcode_descriptor_id, &origin_opcode) ||
            origin_opcode.logical_operator !=
                request->expected_source_operator ||
            origin_opcode.version != request->expected_source_version ||
            (context.origin_owner_pu_st == owner_pu_st &&
             context.origin_source_value_id != source_value->id) ||
            (context.context_callsite_id == 0 &&
             (context.origin_owner_pu_st != owner_pu_st ||
              context.origin_source_value_id != source_value->id)) ||
            (context.context_callsite_id != 0 &&
             (!DSL_Call_Image_Get_Callsite
                  (context.context_callsite_id, &callsite) ||
              callsite.callee_pu_st != owner_pu_st ||
              !DSL_IR_Image_PU_ST_Valid(callsite.owner_pu_st))))
            return DSL_IR_CKKS_Report
                       (diagnostic, "invalid source context", i);
        for (UINT32 prior = 0; prior < i; ++prior) {
            if (request->contexts[prior].context_pu_identity_id ==
                    context.context_pu_identity_id &&
                request->contexts[prior].context_callsite_id ==
                    context.context_callsite_id)
                return DSL_IR_CKKS_Report
                       (diagnostic, "duplicate source context", i);
        }
    }
    BOOL unspecialized = TRUE;
    for (UINT32 i = 0; i < request->context_count; ++i) {
        if (request->contexts[i].origin_owner_pu_st != owner_pu_st ||
            request->contexts[i].origin_source_value_id != source_value->id) {
            unspecialized = FALSE;
            break;
        }
    }
    if (unspecialized) {
        for (UINT32 i = 0; i < request->group_count; ++i) {
            if (request->groups[i].origin_static_ordinal !=
                request->groups[i].source_static_ordinal)
                return DSL_IR_CKKS_Report
                           (diagnostic, "unspecialized origin mismatch", i);
        }
    }
    if (!DSL_Region_Verify_PU(pu_info, diagnostic) ||
        !DSL_Call_ABI_Image_Validate_PU(pu_info, diagnostic) ||
        !DSL_IR_CKKS_Preflight_Calls
            (owner_pu_st, *source_value, calls, diagnostic))
        return FALSE;
    scan->source_st = source_value->st;
    scan->source_definition = request->source_definition;
    scan->calls = calls;
    scan->source_seen = FALSE;
    scan->valid = TRUE;
    scan->definition_count = 0;
    DSL_IR_CKKS_Scan_Tree(*source_block, scan);
    UINT32 direct_calls = 0;
    for (UINT32 i = 0; i < calls->size(); ++i) {
        if ((*calls)[i].address != NULL)
            ++direct_calls;
    }
    if (!scan->valid || !scan->source_seen ||
        scan->definition_count != 1 ||
        scan->addresses.size() != direct_calls ||
        DSL_IR_Retire_Has_Use_Outside_Block
            (pu_root, *source_block, source_value->st))
        return DSL_IR_CKKS_Report
                   (diagnostic, "source use is not redirectable", 0);
    return TRUE;
}

/* Run complete read-only expansion preflight with private scratch, without
 * committing or consuming the request. FALSE may diagnose; no PU activation
 * or IR mutation. */
BOOL
DSL_IR_Can_Expand_Native_Value_To_CKKS_Events
        (PU_Info *pu_info, const DSL_CKKS_EXPANSION_REQUEST *request,
         FILE *diagnostic)
{
    DSL_IR_VALUE_RECORD source_value;
    WN *source_block = NULL;
    std::vector<DSL_IR_CKKS_CALL_USE> calls;
    DSL_IR_CKKS_USE_SCAN scan;
    std::vector<DSL_IR_CKKS_PHYSICAL_OPERAND> physical_operands;
    return DSL_IR_CKKS_Preflight
               (pu_info, request, &source_value, &source_block,
                &calls, &scan, &physical_operands, diagnostic);
}

static UINT32 DSL_ckks_expand_test_fault_stage = 0;

/* Select the process-local expansion fault stage for focused rollback tests;
 * zero disables injection. No persisted or physical state changes. */
void
DSL_CKKS_Expand_Set_Test_Fault (UINT32 stage)
{
    DSL_ckks_expand_test_fault_stage = stage;
}

/* Append one preflighted CKKS step as managed node/value/operand/attribute
 * rows, deriving links from earlier step results. Caller owns capacity and
 * complete rollback on failure; no physical insertion here. Require nonnull
 * result and valid preflighted strings/operands/ST; FALSE may leave interned
 * strings and appended row prefixes, while result is published only on TRUE.
 * No whole-image validation or exception recovery is supplied by this helper. */
static BOOL
DSL_IR_CKKS_Add_Image_Step
        (ST_IDX owner_pu_st, const DSL_CKKS_EXPANSION_STEP &step,
         const std::string &payload,
         const std::vector<DSL_IR_VALUE_ID> &operand_ids,
         ST_IDX result_st, DSL_CKKS_EXPANSION_STEP_RESULT *result)
{
    DSL_IR_OPCODE_DESCRIPTOR_ID descriptor_id =
        DSL_IR_Image_Ensure_Opcode_Descriptor
            (step.dsl_operator, step.version);
    if (descriptor_id == DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID)
        return FALSE;
    DSL_IR_NODE_RECORD node;
    DSL_IR_Node_Record_Init(&node);
    node.opcode_descriptor_id = descriptor_id;
    node.payload = Save_Str(payload.c_str());
    DSL_IR_NODE_ID node_id = DSL_IR_Image_Add_Node(&node);
    if (node_id == DSL_IR_NODE_INVALID_ID)
        return FALSE;

    DSL_IR_VALUE_REFERENCE_ID first_reference =
        DSL_IR_VALUE_REFERENCE_INVALID_ID;
    for (UINT32 i = 0; i < operand_ids.size(); ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        DSL_IR_Value_Reference_Record_Init(&reference);
        reference.owner_node_id = node_id;
        reference.ordinal = i;
        reference.value_id = operand_ids[i];
        DSL_IR_VALUE_REFERENCE_ID id =
            DSL_IR_Image_Add_Value_Reference(&reference);
        if (id == DSL_IR_VALUE_REFERENCE_INVALID_ID)
            return FALSE;
        if (i == 0)
            first_reference = id;
    }

    DSL_IR_ATTRIBUTE_ID first_attribute = DSL_IR_ATTRIBUTE_INVALID_ID;
    for (UINT32 i = 0; i < step.attribute_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD attribute;
        DSL_IR_Attribute_Record_Init(&attribute);
        attribute.owner_node_id = node_id;
        attribute.value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
        attribute.name = Save_Str(step.attributes[i].name);
        attribute.value = Save_Str(step.attributes[i].value);
        DSL_IR_ATTRIBUTE_ID id = DSL_IR_Image_Add_Attribute(&attribute);
        if (id == DSL_IR_ATTRIBUTE_INVALID_ID)
            return FALSE;
        if (i == 0)
            first_attribute = id;
    }

    DSL_IR_VALUE_RECORD value;
    DSL_IR_Value_Record_Init(&value);
    value.value_kind = DSL_IR_VALUE_OPERATOR_RESULT;
    value.producer_node_id = node_id;
    value.ty = step.result_ty;
    value.st = result_st;
    value.name = Save_Str(step.result_name);
    std::string owner = "owner_pu=";
    owner += ST_name(St_Table[owner_pu_st]);
    value.metadata = Save_Str(owner.c_str());
    DSL_IR_VALUE_ID value_id = DSL_IR_Image_Add_Value(&value);
    if (value_id == DSL_IR_VALUE_INVALID_ID ||
        !DSL_IR_Image_Set_Node_Links
            (node_id, first_reference, operand_ids.size(),
             first_attribute, step.attribute_count, value_id))
        return FALSE;
    result->node_id = node_id;
    result->value_id = value_id;
    result->result_st = result_st;
    return TRUE;
}

/*
 * Atomically replace one owner-qualified native DSL value with its complete
 * CKKS event expansion. The transaction redirects physical reads, call ABI,
 * REGION, optional runtime projections, and managed-image references together;
 * every reported late failure attempts coordinated restoration and verifies it
 * before returning FALSE with cleared count-sized results. Earlier preflight/
 * savepoint rejection preserves output; failed restoration stops compilation.
 * Allocation exceptions are not caught and have no journal-unwind guarantee.
 * Success deletes the source definition and publishes step results; no PU switch.
 */
BOOL
DSL_IR_Expand_Native_Value_To_CKKS_Events
        (PU_Info *pu_info, const DSL_CKKS_EXPANSION_REQUEST *request,
         FILE *diagnostic, DSL_CKKS_EXPANSION_STEP_RESULT *results)
{
    DSL_IR_VALUE_RECORD source;
    WN *source_block = NULL;
    std::vector<DSL_IR_CKKS_CALL_USE> calls;
    DSL_IR_CKKS_USE_SCAN scan;
    std::vector<DSL_IR_CKKS_PHYSICAL_OPERAND> physical_operands;
    if (results == NULL ||
        !DSL_IR_CKKS_Preflight
            (pu_info, request, &source, &source_block,
             &calls, &scan, &physical_operands, diagnostic))
        return FALSE;

    /* Couple append-only image/string savepoints with the physical mutation
     * journal below; neither savepoint alone can restore an expansion. */
    DSL_CKKS_IMAGE_SAVEPOINT image_savepoint;
    DSL_CKKS_STRTAB_SAVEPOINT string_savepoint;
    if (!DSL_IR_Image_CKKS_Save(source.id, &image_savepoint) ||
        !DSL_CKKS_Strtab_Save(&string_savepoint))
        return DSL_IR_CKKS_Report
                   (diagnostic, "could not establish savepoints", 0);
    UINT32 event_count = DSL_CKKS_Event_Image_Count();
    UINT32 symbol_count = ST_Table_Size(CURRENT_SYMTAB);
    UINT32 metadata_count = St_tensor_metadata.Size();
    UINT32 kv_count = Tensor_dsl_kv_table.Size();
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu_info);
    BOOL has_runtime_interface =
        DSL_Runtime_Interface_Image_Has_Records();
    std::vector<std::string> payloads;
    std::vector<DSL_CKKS_EXPANSION_STEP_RESULT> produced
        (request->step_count);
    std::vector<WN *> definitions;
    payloads.reserve(request->step_count);
    definitions.reserve(request->step_count);
    for (UINT32 i = 0; i < request->step_count; ++i)
        payloads.push_back(DSL_IR_CKKS_Payload(*request, i));

    UINT32 updated_calls = 0;
    BOOL region_redirected = FALSE;
    BOOL source_extracted = FALSE;
    BOOL physical_redirected = FALSE;
    BOOL runtime_redirected = FALSE;
    BOOL committed = FALSE;
    UINT32 physical_cursor = 0;
    do {
        for (UINT32 i = 0; i < request->step_count; ++i) {
            const DSL_CKKS_EXPANSION_STEP &step = request->steps[i];
            std::vector<WN *> kids;
            std::vector<DSL_IR_VALUE_ID> operand_ids;
            kids.reserve(step.operand_count);
            operand_ids.reserve(step.operand_count);
            for (UINT32 kid = 0; kid < step.operand_count; ++kid) {
                const DSL_CKKS_EXPANSION_OPERAND &operand =
                    step.operands[kid];
                DSL_IR_VALUE_RECORD value;
                if (operand.kind == DSL_CKKS_EXPANSION_PRIOR_STEP) {
                    if (!DSL_IR_Image_Get_Value
                            (produced[operand.step_index].value_id,
                             &value))
                        break;
                } else if (!DSL_IR_Image_Get_Value
                               (operand.value_id, &value)) {
                    break;
                }
                DSL_IR_CKKS_PHYSICAL_OPERAND physical =
                    physical_operands[physical_cursor++];
                ST_IDX load_st = operand.kind ==
                    DSL_CKKS_EXPANSION_EXISTING_VALUE ? physical.st :
                    value.st;
                TY_IDX load_ty = operand.kind ==
                    DSL_CKKS_EXPANSION_EXISTING_VALUE ? physical.ty :
                    value.ty;
                TYPE_ID mtype = TY_is_tensor_extension(load_ty) ?
                                MTYPE_M : TY_mtype(load_ty);
                WN *load = WN_CreateLdid
                    (OPR_LDID, mtype, mtype, 0, load_st, load_ty);
                if (load == NULL)
                    break;
                kids.push_back(load);
                operand_ids.push_back(value.id);
            }
            if (kids.size() != step.operand_count) {
                for (UINT32 kid = 0; kid < kids.size(); ++kid)
                    WN_DELETE_Tree(kids[kid]);
                break;
            }
            WN *expression = DSL_WN_Create_Native
                (step.dsl_operator, step.version, payloads[i].c_str(),
                 kids.empty() ? NULL : &kids[0], kids.size());
            if (expression == NULL) {
                for (UINT32 kid = 0; kid < kids.size(); ++kid)
                    WN_DELETE_Tree(kids[kid]);
                break;
            }
            ST_IDX result_st = DSL_Tensor_Create_Result_Symbol
                (step.result_name, step.result_ty,
                 SCLASS_AUTO, EXPORT_LOCAL);
            if (result_st == ST_IDX_ZERO) {
                WN_DELETE_Tree(expression);
                break;
            }
            Set_ST_Srcpos(St_Table[result_st], step.source_position);
            WN *definition = WN_CreateStid
                (OPR_STID, MTYPE_V, MTYPE_M, 0, result_st,
                 step.result_ty, expression);
            if (definition == NULL) {
                WN_DELETE_Tree(expression);
                break;
            }
            WN_Set_Linenum(definition, step.source_position);
            if (!DSL_IR_CKKS_Add_Image_Step
                    (owner_pu_st, step, payloads[i], operand_ids,
                     result_st, &produced[i])) {
                WN_DELETE_Tree(definition);
                break;
            }
            WN_INSERT_BlockBefore
                (source_block, request->source_definition, definition);
            definitions.push_back(definition);
            if (DSL_ckks_expand_test_fault_stage == 1 ||
                (DSL_ckks_expand_test_fault_stage == 4 && i == 3))
                break;
        }
        if (definitions.size() != request->step_count ||
            DSL_ckks_expand_test_fault_stage == 1)
            break;

        for (UINT32 context_index = 0;
             context_index < request->context_count; ++context_index) {
            const DSL_CKKS_EXPANSION_CONTEXT &context =
                request->contexts[context_index];
            for (UINT32 group_index = 0;
                 group_index < request->group_count; ++group_index) {
                const DSL_CKKS_EXPANSION_GROUP &group =
                    request->groups[group_index];
                for (UINT32 ordinal = 0;
                     ordinal < group.step_count; ++ordinal) {
                    UINT32 step_index = group.first_step + ordinal;
                    DSL_CKKS_EVENT_RECORD event;
                    memset(&event, 0, sizeof(event));
                    event.owner_pu_st = owner_pu_st;
                    event.source_value_id = source.id;
                    event.source_node_id = source.producer_node_id;
                    event.context_pu_identity_id =
                        context.context_pu_identity_id;
                    event.context_callsite_id =
                        context.context_callsite_id;
                    event.source_static_ordinal =
                        group.source_static_ordinal;
                    event.step_ordinal = ordinal;
                    event.result_value_id = produced[step_index].value_id;
                    event.result_node_id = produced[step_index].node_id;
                    event.origin_owner_pu_st =
                        context.origin_owner_pu_st;
                    event.origin_source_value_id =
                        context.origin_source_value_id;
                    event.origin_static_ordinal =
                        group.origin_static_ordinal;
                    if (step_index == request->final_step_index)
                        event.flags = DSL_CKKS_EVENT_FINAL_RESULT;
                    if (DSL_CKKS_Event_Image_Add(&event) ==
                        DSL_CKKS_EVENT_INVALID_ID)
                        break;
                }
            }
        }
        if (DSL_CKKS_Event_Image_Count() !=
            event_count + request->context_count * request->step_count)
            break;

        const DSL_CKKS_EXPANSION_STEP_RESULT &final_result =
            produced[request->final_step_index];
        UINT32 region_uses = DSL_Region_Symbol_Use_Count
                                 (pu_info, source.st);
        if (region_uses != 0 &&
            !DSL_Region_Can_Redirect_Symbol
                (pu_info, source.st, final_result.result_st))
            break;
        for (UINT32 i = 0; i < scan.reads.size(); ++i)
            WN_st_idx(scan.reads[i]) = final_result.result_st;
        for (UINT32 i = 0; i < scan.addresses.size(); ++i)
            WN_st_idx(scan.addresses[i]) = final_result.result_st;
        physical_redirected = TRUE;
        for (UINT32 i = 0; i < calls.size(); ++i) {
            if (!DSL_Call_ABI_Image_Update_Argument_Value
                    (calls[i].call, calls[i].actual_ordinal,
                     source.id, final_result.value_id))
                break;
            ++updated_calls;
        }
        if (updated_calls != calls.size())
            break;
        if (region_uses != 0) {
            if (!DSL_Region_Redirect_Symbol
                    (pu_info, source.st, final_result.result_st))
                break;
            region_redirected = TRUE;
        }
        if (has_runtime_interface) {
            if (!DSL_Runtime_Interface_Image_Redirect_Value(
                    owner_pu_st, source.id, final_result.value_id))
                break;
            runtime_redirected = TRUE;
        }
        if (DSL_ckks_expand_test_fault_stage == 2)
            break;
        if (!DSL_IR_Image_Redirect_And_Lower_Value
                (source.id, final_result.value_id))
            break;
        WN *removed = WN_EXTRACT_FromBlock
                          (source_block, request->source_definition);
        if (removed != request->source_definition)
            break;
        source_extracted = TRUE;
        if (DSL_ckks_expand_test_fault_stage == 3 ||
            !DSL_IR_Image_Validate(diagnostic) ||
            !DSL_CKKS_Event_Image_Validate(diagnostic) ||
            (has_runtime_interface &&
             !DSL_Runtime_Interface_Image_Validate(diagnostic)) ||
            !DSL_Region_Verify_PU(pu_info, diagnostic) ||
            !DSL_Call_ABI_Image_Validate_PU(pu_info, diagnostic))
            break;
        committed = TRUE;
    } while (FALSE);

    if (committed) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "DSL CKKS expansion: owner=%s source=value%u "
                    "replacement=value%u redirected_reads=%u "
                    "redirected_addresses=%u\n",
                    ST_name(St_Table[owner_pu_st]), source.id,
                    produced[request->final_step_index].value_id,
                    (UINT32)scan.reads.size(),
                    (UINT32)scan.addresses.size());
        WN_DELETE_Tree(request->source_definition);
        for (UINT32 i = 0; i < request->step_count; ++i)
            results[i] = produced[i];
        return TRUE;
    }

    /* Undo only the stages reached, then validate the coordinated restoration.
     * A failed restore is fatal rather than a recoverable transaction FALSE. */
    const DSL_CKKS_EXPANSION_STEP_RESULT &final_result =
        produced[request->final_step_index];
    BOOL restored = TRUE;
    BOOL region_restored = TRUE;
    BOOL runtime_restored = TRUE;
    BOOL calls_restored = TRUE;
    BOOL definitions_restored = TRUE;
    BOOL strings_restored = TRUE;
    BOOL image_valid = TRUE;
    BOOL events_valid = TRUE;
    BOOL runtime_valid = TRUE;
    BOOL region_valid = TRUE;
    BOOL calls_valid = TRUE;
    if (source_extracted && !definitions.empty())
        WN_INSERT_BlockAfter
            (source_block, definitions.back(), request->source_definition);
    if (region_redirected) {
        region_restored = DSL_Region_Redirect_Symbol
                              (pu_info, final_result.result_st, source.st);
        restored = region_restored && restored;
    }
    if (runtime_redirected) {
        runtime_restored = DSL_Runtime_Interface_Image_Redirect_Value(
                               owner_pu_st, final_result.value_id, source.id);
        restored = runtime_restored && restored;
    }
    for (UINT32 i = updated_calls; i != 0; --i) {
        BOOL call_restored = DSL_Call_ABI_Image_Update_Argument_Value
                                 (calls[i - 1].call,
                                  calls[i - 1].actual_ordinal,
                                  final_result.value_id, source.id);
        calls_restored = call_restored && calls_restored;
        restored = call_restored && restored;
    }
    if (physical_redirected) {
        for (UINT32 i = 0; i < scan.reads.size(); ++i)
            WN_st_idx(scan.reads[i]) = source.st;
        for (UINT32 i = 0; i < scan.addresses.size(); ++i)
            WN_st_idx(scan.addresses[i]) = source.st;
    }
    for (UINT32 i = 0; i < definitions.size(); ++i) {
        BOOL definition_restored = WN_EXTRACT_FromBlock
                                       (source_block, definitions[i]) ==
                                   definitions[i];
        definitions_restored = definition_restored && definitions_restored;
        restored = definition_restored && restored;
        WN_DELETE_Tree(definitions[i]);
    }
    DSL_CKKS_Event_Image_Trim(event_count);
    DSL_IR_Image_CKKS_Restore
        (&image_savepoint, final_result.value_id);
    St_tensor_metadata.Delete_down_to(metadata_count);
    Tensor_dsl_kv_table.Delete_down_to(kv_count);
    while (ST_Table_Size(CURRENT_SYMTAB) > symbol_count)
        Scope_tab[CURRENT_SYMTAB].st_tab->Delete_last();
    strings_restored = DSL_CKKS_Strtab_Restore(&string_savepoint);
    restored = strings_restored && restored;
    image_valid = DSL_IR_Image_Validate(NULL);
    events_valid = DSL_CKKS_Event_Image_Validate(NULL);
    runtime_valid = !has_runtime_interface ||
                    DSL_Runtime_Interface_Image_Validate(NULL);
    if (!runtime_valid)
        DSL_Runtime_Interface_Image_Validate
            (diagnostic != NULL ? diagnostic : stderr);
    region_valid = DSL_Region_Verify_PU(pu_info, NULL);
    calls_valid = DSL_Call_ABI_Image_Validate_PU(pu_info, NULL);
    restored = image_valid && events_valid && runtime_valid && region_valid &&
               calls_valid && restored;
    if (!restored)
        Fail_FmtAssertion
            ("CKKS expansion rollback failed: region=%u runtime=%u "
             "calls=%u definitions=%u strings=%u image=%u events=%u "
             "runtime_image=%u region_verify=%u call_abi=%u; "
             "compilation must stop",
             region_restored, runtime_restored, calls_restored,
             definitions_restored, strings_restored, image_valid,
             events_valid, runtime_valid, region_valid, calls_valid);
    memset(results, 0, request->step_count * sizeof(*results));
    return DSL_IR_CKKS_Report
               (diagnostic, "transaction rolled back", 0);
}
