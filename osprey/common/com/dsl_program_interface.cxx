/*
 * Copyright (C) 2026 Open64 Project
 *
 * Program-interface ABI retirement, runtime-input promotion, reconstruction,
 * and verification for native DSL PUs. See
 * doc/FHE-SYNC5-RUNTIME-ENTRY-BINDING-GAP.md,
 * doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md, and
 * doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md.
 */

#include <string.h>
#include <algorithm>
#include <sstream>
#include <string>
#include <vector>

#include "dsl_ir_image.h"
#include "dsl_ir_transaction_internal.h"
#include "dsl_program_interface_internal.h"
#include "dsl_runtime_interface_internal.h"
#include "dsl_region.h"
#include "dsl_region_internal.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"

/* Commit-only image/tree services used after complete program-plan preflight. */
extern DSL_RETIRED_FORMAL_ID
DSL_Program_Interface_Image_Add_Retired_Formal
                                (const DSL_RETIRED_FORMAL_RECORD *);
extern DSL_RETIRED_CALL_ARGUMENT_ID
DSL_Program_Interface_Image_Add_Retired_Call
                                (const DSL_RETIRED_CALL_ARGUMENT_RECORD *);
extern DSL_RUNTIME_INPUT_ID DSL_Program_Interface_Image_Add_Runtime_Input
                                (const DSL_RUNTIME_INPUT_RECORD *);
extern DSL_RUNTIME_INPUT_BINDING_ID
DSL_Program_Interface_Image_Add_Runtime_Binding
                                (const DSL_RUNTIME_INPUT_BINDING_RECORD *);
extern DSL_RUNTIME_INPUT_CALL_ID
DSL_Program_Interface_Image_Add_Runtime_Call
                                (const DSL_RUNTIME_INPUT_CALL_RECORD *);
extern DSL_RUNTIME_VALUE_PROJECTION_ID
DSL_Runtime_Interface_Image_Add_Value
                                (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *);
extern DSL_RUNTIME_CALL_PROJECTION_ID
DSL_Runtime_Interface_Image_Add_Call
                                (const DSL_RUNTIME_CALL_PROJECTION_RECORD *);
extern BOOL DSL_Call_Image_Replace_Call_WN
                                (DSL_CALLSITE_METADATA_ID, const WN *, WN *);

/* Emit the stable program-interface diagnostic shape and return FALSE. */
static BOOL
DSL_Program_Interface_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL program interface error: %s id=%u\n",
                message, id);
    return FALSE;
}

/* Accept only a nonempty stable string used in persisted semantic roles. */
static BOOL
DSL_Program_Interface_String_Valid (const char *text)
{
    return text != NULL && text[0] != '\0';
}

/* Find the unique retirement request for one persisted PU formal row. */
static const DSL_RETIRED_FORMAL_REQUEST *
DSL_Program_Interface_Find_Retired_Formal_Request
        (const DSL_PROGRAM_INTERFACE_PLAN *plan,
         DSL_PU_FORMAL_ID formal_id)
{
    for (UINT32 i = 0; plan != NULL && i < plan->retired_formal_count; ++i) {
        if (plan->retired_formals[i].pu_formal_id == formal_id)
            return &plan->retired_formals[i];
    }
    return NULL;
}

/* Find the unique retirement request for one persisted call-argument row. */
static const DSL_RETIRED_CALL_ARGUMENT_REQUEST *
DSL_Program_Interface_Find_Retired_Call_Request
        (const DSL_PROGRAM_INTERFACE_PLAN *plan,
         DSL_CALL_ARGUMENT_ID argument_id)
{
    for (UINT32 i = 0;
         plan != NULL && i < plan->retired_call_argument_count; ++i) {
        if (plan->retired_call_arguments[i].call_argument_id == argument_id)
            return &plan->retired_call_arguments[i];
    }
    return NULL;
}

/*
 * Count retired physical formals before an original ordinal so validation can
 * map old interface positions to the rebuilt ABI deterministically.
 */
static UINT32
DSL_Program_Interface_Retired_Formals_Before
        (const DSL_PROGRAM_INTERFACE_PLAN *plan, ST_IDX owner_pu_st,
         UINT32 ordinal)
{
    UINT32 count = 0;
    for (UINT32 i = 0; i < plan->retired_formal_count; ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (DSL_PU_Interface_Image_Get_Formal
                (plan->retired_formals[i].pu_formal_id, &formal) &&
            formal.owner_pu_st == owner_pu_st &&
            formal.formal_ordinal < ordinal)
            ++count;
    }
    return count;
}

/* Compute final legacy-formal count after applying the retirement set. */
static UINT32
DSL_Program_Interface_Live_Formal_Count
        (const DSL_PROGRAM_INTERFACE_PLAN *plan, ST_IDX owner_pu_st)
{
    UINT32 count = 0;
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (DSL_PU_Interface_Image_Get_Formal(i, &formal) &&
            formal.owner_pu_st == owner_pu_st &&
            DSL_Program_Interface_Find_Retired_Formal_Request
                (plan, formal.id) == NULL)
            ++count;
    }
    return count;
}

/* Find one runtime value projection request by exact owner and value ID. */
static const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *
DSL_Program_Interface_Find_Runtime_Value
        (const DSL_RUNTIME_INTERFACE_PLAN *plan, ST_IDX owner_pu_st,
         DSL_IR_VALUE_ID value_id)
{
    for (UINT32 i = 0; plan != NULL && i < plan->value_count; ++i) {
        if (plan->values[i].owner_pu_st == owner_pu_st &&
            plan->values[i].source_value_id == value_id)
            return &plan->values[i];
    }
    return NULL;
}

/* Find one runtime call projection request by callsite and old ordinal. */
static const DSL_RUNTIME_CALL_PROJECTION_REQUEST *
DSL_Program_Interface_Find_Runtime_Call
        (const DSL_RUNTIME_INTERFACE_PLAN *plan,
         DSL_CALLSITE_METADATA_ID callsite_id, UINT32 actual_ordinal)
{
    for (UINT32 i = 0; plan != NULL && i < plan->call_count; ++i) {
        if (plan->calls[i].callsite_id == callsite_id &&
            plan->calls[i].actual_ordinal == actual_ordinal)
            return &plan->calls[i];
    }
    return NULL;
}

/*
 * Define deterministic global runtime-input ordering by stable role and source
 * identity. Plan fingerprints and persisted IDs depend on this order.
 */
static BOOL
DSL_Program_Interface_Input_Less
        (const DSL_RUNTIME_INPUT_REQUEST *left,
         const DSL_RUNTIME_INPUT_REQUEST *right)
{
    if (left->input_kind != right->input_kind)
        return left->input_kind < right->input_kind;
    INT role_order = strcmp(left->stable_role, right->stable_role);
    if (role_order != 0)
        return role_order < 0;
    if (left->source_owner_pu_st != right->source_owner_pu_st)
        return left->source_owner_pu_st < right->source_owner_pu_st;
    if (left->source_value_id != right->source_value_id)
        return left->source_value_id < right->source_value_id;
    return left->source_tcon < right->source_tcon;
}

/*
 * Define deterministic per-PU binding ordering independent of request-array
 * order, preserving reproducible formal ordinals and image rows.
 */
static BOOL
DSL_Program_Interface_Binding_Less
        (const DSL_RUNTIME_INPUT_BINDING_REQUEST *left,
         const DSL_RUNTIME_INPUT_BINDING_REQUEST *right,
         const DSL_PROGRAM_INTERFACE_PLAN *plan)
{
    if (left->binding_kind != right->binding_kind)
        return left->binding_kind < right->binding_kind;
    INT role_order = strcmp(left->semantic_role, right->semantic_role);
    if (role_order != 0)
        return role_order < 0;
    if (left->runtime_input_index ==
            DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX ||
        right->runtime_input_index ==
            DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX)
        return left->runtime_input_index < right->runtime_input_index;
    return DSL_Program_Interface_Input_Less
        (&plan->runtime_inputs[left->runtime_input_index],
         &plan->runtime_inputs[right->runtime_input_index]);
}

/* Derive one binding's final formal ordinal in deterministic sorted order. */
static UINT32
DSL_Program_Interface_Binding_Ordinal
        (const DSL_PROGRAM_INTERFACE_PLAN *plan, UINT32 binding_index)
{
    const DSL_RUNTIME_INPUT_BINDING_REQUEST &binding =
        plan->runtime_input_bindings[binding_index];
    UINT32 ordinal = DSL_Program_Interface_Live_Formal_Count
                         (plan, binding.owner_pu_st);
    for (UINT32 i = 0; i < plan->runtime_input_binding_count; ++i) {
        if (plan->runtime_input_bindings[i].owner_pu_st ==
                binding.owner_pu_st &&
            DSL_Program_Interface_Binding_Less
                (&plan->runtime_input_bindings[i], &binding, plan))
            ++ordinal;
    }
    return ordinal;
}

/*
 * Validate that a runtime projection request names an owner-local tensor value
 * and a canonical handle type suitable for ABI reconstruction.
 */
static BOOL
DSL_Program_Interface_Runtime_Value_Valid
        (const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request)
{
    DSL_IR_VALUE_RECORD source;
    return DSL_IR_Image_PU_ST_Valid(request.owner_pu_st) &&
           request.source_value_id != DSL_IR_VALUE_INVALID_ID &&
           DSL_IR_Image_Get_Value(request.source_value_id, &source) &&
           source.st == request.expected_source_st &&
           source.ty == request.expected_source_ty &&
           DSL_Runtime_Interface_Value_Owner_Valid
               (request.owner_pu_st, source) &&
           TY_is_tensor_extension(request.expected_source_ty) &&
           request.handle_ty != TY_IDX_ZERO &&
           TY_IDX_index(request.handle_ty) < TY_Table_Size() &&
           TY_kind(request.handle_ty) == KIND_POINTER &&
           request.binding_kind >= DSL_RUNTIME_BINDING_LOCAL_VALUE &&
           request.binding_kind <= DSL_RUNTIME_BINDING_RESULT_FORMAL;
}

static std::string DSL_program_interface_prepared_plan;
static std::vector<ST_IDX> DSL_program_interface_committed_pus;
static BOOL DSL_program_interface_mapped_committed = FALSE;

/* Clear process-local plan and PU commit evidence before a new program. */
void
DSL_Program_Interface_Reset_Commit_State (void)
{
    DSL_program_interface_prepared_plan.clear();
    DSL_program_interface_committed_pus.clear();
    DSL_program_interface_mapped_committed = FALSE;
}

/* A complete mapped image is eligible for strict per-PU reader validation. */
void
DSL_Program_Interface_Mark_Mapped_Committed (void)
{
    DSL_program_interface_mapped_committed = TRUE;
}

/* Query process-local eligibility without interpreting local PU state. */
BOOL
DSL_Program_Interface_PU_Is_Committed (ST_IDX owner_pu_st)
{
    if (DSL_program_interface_mapped_committed)
        return TRUE;
    return std::find(DSL_program_interface_committed_pus.begin(),
                     DSL_program_interface_committed_pus.end(),
                     owner_pu_st) !=
           DSL_program_interface_committed_pus.end();
}

/* Append one length-delimited text field to the canonical plan fingerprint. */
static void
DSL_Program_Interface_Append_Text
        (std::ostringstream *stream, const char *text)
{
    size_t length = text == NULL ? 0 : strlen(text);
    *stream << length << ':';
    if (length != 0)
        stream->write(text, length);
    *stream << ';';
}

/* Build the stable semantic key used to order one runtime-input request. */
static std::string
DSL_Program_Interface_Input_Request_Key
        (const DSL_RUNTIME_INPUT_REQUEST &request)
{
    std::ostringstream stream;
    stream << request.input_kind << ';' << request.source_owner_pu_st << ';'
           << request.source_value_id << ';' << request.source_ty << ';'
           << request.source_tcon << ';' << request.handle_ty << ';';
    DSL_Program_Interface_Append_Text(&stream, request.stable_role);
    return stream.str();
}

/* Build the stable semantic key used to order one PU binding request. */
static std::string
DSL_Program_Interface_Binding_Request_Key
        (const DSL_PROGRAM_INTERFACE_PLAN *plan, UINT32 index)
{
    const DSL_RUNTIME_INPUT_BINDING_REQUEST &request =
        plan->runtime_input_bindings[index];
    std::ostringstream stream;
    stream << request.owner_pu_st << ';' << request.handle_ty << ';'
           << request.binding_kind << ';'
           << (unsigned long long)request.source_position << ';';
    DSL_Program_Interface_Append_Text(&stream, request.semantic_role);
    if (request.runtime_input_index !=
        DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX)
        stream << DSL_Program_Interface_Input_Request_Key
                      (plan->runtime_inputs[request.runtime_input_index]);
    return stream.str();
}

/* Append a sorted request-key set to the canonical plan fingerprint. */
static void
DSL_Program_Interface_Append_Keys
        (std::ostringstream *stream, const char *category,
         std::vector<std::string> *keys)
{
    std::sort(keys->begin(), keys->end());
    *stream << category << ':' << keys->size() << ';';
    for (UINT32 i = 0; i < keys->size(); ++i) {
        *stream << (*keys)[i].size() << ':';
        stream->write((*keys)[i].data(), (*keys)[i].size());
        *stream << ';';
    }
}

/*
 * Canonicalize all program/runtime requests into an order-independent plan
 * fingerprint. Apply_PU requires this exact prevalidated fingerprint.
 */
static std::string
DSL_Program_Interface_Plan_Fingerprint
        (const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
         const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan)
{
    std::ostringstream result;
    std::vector<std::string> keys;
    for (UINT32 i = 0; i < program_plan->retired_formal_count; ++i) {
        std::ostringstream key;
        key << program_plan->retired_formals[i].pu_formal_id << ';';
        DSL_Program_Interface_Append_Text
            (&key, program_plan->retired_formals[i].semantic_role);
        keys.push_back(key.str());
    }
    DSL_Program_Interface_Append_Keys(&result, "retired_formal", &keys);
    keys.clear();
    for (UINT32 i = 0;
         i < program_plan->retired_call_argument_count; ++i) {
        std::ostringstream key;
        key << program_plan->retired_call_arguments[i].call_argument_id
            << ';';
        DSL_Program_Interface_Append_Text
            (&key, program_plan->retired_call_arguments[i].semantic_role);
        keys.push_back(key.str());
    }
    DSL_Program_Interface_Append_Keys(&result, "retired_call", &keys);
    keys.clear();
    for (UINT32 i = 0; i < program_plan->runtime_input_count; ++i)
        keys.push_back(DSL_Program_Interface_Input_Request_Key
                           (program_plan->runtime_inputs[i]));
    DSL_Program_Interface_Append_Keys(&result, "input", &keys);
    keys.clear();
    for (UINT32 i = 0;
         i < program_plan->runtime_input_binding_count; ++i)
        keys.push_back(DSL_Program_Interface_Binding_Request_Key
                           (program_plan, i));
    DSL_Program_Interface_Append_Keys(&result, "binding", &keys);
    keys.clear();
    for (UINT32 i = 0;
         i < program_plan->runtime_input_call_count; ++i) {
        const DSL_RUNTIME_INPUT_CALL_REQUEST &request =
            program_plan->runtime_input_calls[i];
        std::ostringstream key;
        key << request.callsite_id << ';'
            << DSL_Program_Interface_Binding_Request_Key
                   (program_plan, request.caller_binding_index)
            << DSL_Program_Interface_Binding_Request_Key
                   (program_plan, request.callee_binding_index);
        keys.push_back(key.str());
    }
    DSL_Program_Interface_Append_Keys(&result, "input_call", &keys);
    keys.clear();
    for (UINT32 i = 0; i < runtime_plan->value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request =
            runtime_plan->values[i];
        std::ostringstream key;
        key << request.owner_pu_st << ';' << request.source_value_id << ';'
            << request.expected_source_st << ';'
            << request.expected_source_ty << ';' << request.handle_ty << ';'
            << request.binding_kind << ';' << request.formal_ordinal << ';';
        keys.push_back(key.str());
    }
    DSL_Program_Interface_Append_Keys(&result, "projection", &keys);
    keys.clear();
    for (UINT32 i = 0; i < runtime_plan->call_count; ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request =
            runtime_plan->calls[i];
        std::ostringstream key;
        key << request.owner_pu_st << ';' << request.callsite_id << ';'
            << request.source_value_id << ';' << request.actual_ordinal << ';'
            << request.callee_formal_ordinal << ';' << request.direction
            << ';';
        keys.push_back(key.str());
    }
    DSL_Program_Interface_Append_Keys(&result, "call_projection", &keys);
    return result.str();
}

/* Initialize caller-owned program-interface result counters to zero. */
void
DSL_Program_Interface_Result_Init (DSL_PROGRAM_INTERFACE_RESULT *result)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
}

/*
 * Validate the complete program-wide ABI transformation before any PU commit.
 * It proves retirement liveness, runtime-input/TCON identity, binding/call
 * coverage, deterministic ordering, and cross-plan consistency, then records
 * only an in-memory fingerprint; failure mutates no WHIRL or image rows.
 */
BOOL
DSL_Program_Interface_Plan_Validate
        (const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
         const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan,
         FILE *diagnostic)
{
    if (program_plan == NULL || runtime_plan == NULL ||
        (program_plan->retired_formal_count != 0 &&
         program_plan->retired_formals == NULL) ||
        (program_plan->retired_call_argument_count != 0 &&
         program_plan->retired_call_arguments == NULL) ||
        (program_plan->runtime_input_count != 0 &&
         program_plan->runtime_inputs == NULL) ||
        (program_plan->runtime_input_binding_count != 0 &&
         program_plan->runtime_input_bindings == NULL) ||
        (program_plan->runtime_input_call_count != 0 &&
         program_plan->runtime_input_calls == NULL) ||
        (runtime_plan->value_count != 0 && runtime_plan->values == NULL) ||
        (runtime_plan->call_count != 0 && runtime_plan->calls == NULL) ||
        DSL_Program_Interface_Image_Has_Records())
        return DSL_Program_Interface_Report
                   (diagnostic, "plan is incomplete or already applied", 0);

    for (UINT32 i = 0; i < program_plan->retired_formal_count; ++i) {
        const DSL_RETIRED_FORMAL_REQUEST &request =
            program_plan->retired_formals[i];
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(request.pu_formal_id, &formal) ||
            !DSL_Program_Interface_String_Valid(request.semantic_role))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid retired formal request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (program_plan->retired_formals[j].pu_formal_id ==
                request.pu_formal_id)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate retired formal request",
                            i + 1);
        }
        UINT32 incoming = 0;
        UINT32 covered = 0;
        for (UINT32 j = 1; j <= DSL_Call_ABI_Image_Argument_Count(); ++j) {
            DSL_CALL_ARGUMENT_RECORD argument;
            DSL_CALLSITE_METADATA_RECORD callsite;
            if (!DSL_Call_ABI_Image_Get_Argument(j, &argument) ||
                !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite))
                return DSL_Program_Interface_Report
                           (diagnostic, "missing canonical call", j);
            if (callsite.callee_pu_st == formal.owner_pu_st &&
                argument.callee_formal_ordinal == formal.formal_ordinal) {
                ++incoming;
                const DSL_RETIRED_CALL_ARGUMENT_REQUEST *retired =
                    DSL_Program_Interface_Find_Retired_Call_Request
                        (program_plan, argument.id);
                if (retired != NULL &&
                    strcmp(retired->semantic_role, request.semantic_role) == 0)
                    ++covered;
            }
        }
        if (incoming != covered)
            return DSL_Program_Interface_Report
                       (diagnostic, "incomplete retired formal coverage",
                        i + 1);
    }

    for (UINT32 i = 0;
         i < program_plan->retired_call_argument_count; ++i) {
        const DSL_RETIRED_CALL_ARGUMENT_REQUEST &request =
            program_plan->retired_call_arguments[i];
        DSL_CALL_ARGUMENT_RECORD argument;
        DSL_CALLSITE_METADATA_RECORD callsite;
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_Call_ABI_Image_Get_Argument
                (request.call_argument_id, &argument) ||
            !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite) ||
            !DSL_PU_Interface_Image_Find_Formal
                (callsite.callee_pu_st, argument.callee_formal_ordinal,
                 &formal) ||
            DSL_Program_Interface_Find_Retired_Formal_Request
                (program_plan, formal.id) == NULL ||
            !DSL_Program_Interface_String_Valid(request.semantic_role))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid retired call request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (program_plan->retired_call_arguments[j].call_argument_id ==
                request.call_argument_id)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate retired call request",
                            i + 1);
        }
    }

    for (UINT32 i = 0; i < runtime_plan->value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request =
            runtime_plan->values[i];
        DSL_PU_FORMAL_RECORD formal;
        BOOL is_formal = request.binding_kind ==
                             DSL_RUNTIME_BINDING_INPUT_FORMAL ||
                         request.binding_kind ==
                             DSL_RUNTIME_BINDING_RESULT_FORMAL;
        if (!DSL_Program_Interface_Runtime_Value_Valid(request) ||
            (is_formal &&
             (!DSL_PU_Interface_Image_Find_Formal
                  (request.owner_pu_st, request.formal_ordinal, &formal) ||
              formal.formal_value_id != request.source_value_id ||
              DSL_Program_Interface_Find_Retired_Formal_Request
                  (program_plan, formal.id) != NULL)) ||
            (!is_formal && request.formal_ordinal !=
                               DSL_RUNTIME_INTERFACE_INVALID_ORDINAL))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid live projection request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (runtime_plan->values[j].owner_pu_st == request.owner_pu_st &&
                runtime_plan->values[j].source_value_id ==
                    request.source_value_id)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate live projection", i + 1);
        }
    }

    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal))
            return DSL_Program_Interface_Report
                       (diagnostic, "missing canonical formal", i);
        BOOL retired = DSL_Program_Interface_Find_Retired_Formal_Request
                           (program_plan, formal.id) != NULL;
        BOOL projected = DSL_Program_Interface_Find_Runtime_Value
                             (runtime_plan, formal.owner_pu_st,
                              formal.formal_value_id) != NULL;
        if (retired == projected)
            return DSL_Program_Interface_Report
                       (diagnostic, "formal retirement/projection mismatch",
                        i);
    }

    for (UINT32 i = 0; i < runtime_plan->call_count; ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request =
            runtime_plan->calls[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_Image_Get_Callsite(request.callsite_id, &callsite) ||
            callsite.owner_pu_st != request.owner_pu_st ||
            request.actual_ordinal != request.callee_formal_ordinal ||
            request.direction < DSL_RUNTIME_CALL_INPUT ||
            request.direction > DSL_RUNTIME_CALL_RESULT ||
            DSL_Program_Interface_Find_Runtime_Value
                (runtime_plan, request.owner_pu_st,
                 request.source_value_id) == NULL ||
            (request.direction == DSL_RUNTIME_CALL_INPUT &&
             (!DSL_Call_ABI_Image_Find_Argument_By_Id
                  (request.callsite_id, request.actual_ordinal, &argument) ||
              argument.argument_value_id != request.source_value_id ||
              DSL_Program_Interface_Find_Retired_Call_Request
                  (program_plan, argument.id) != NULL)))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid live call projection", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (runtime_plan->calls[j].callsite_id == request.callsite_id &&
                runtime_plan->calls[j].actual_ordinal ==
                    request.actual_ordinal)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate live call projection",
                            i + 1);
        }
    }
    for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_ABI_Image_Get_Argument(i, &argument))
            return DSL_Program_Interface_Report
                       (diagnostic, "missing canonical call argument", i);
        BOOL retired = DSL_Program_Interface_Find_Retired_Call_Request
                           (program_plan, argument.id) != NULL;
        BOOL projected = DSL_Program_Interface_Find_Runtime_Call
                             (runtime_plan, argument.callsite_id,
                              argument.actual_ordinal) != NULL;
        if (retired == projected)
            return DSL_Program_Interface_Report
                       (diagnostic, "call retirement/projection mismatch", i);
    }

    for (UINT32 i = 0; i < program_plan->runtime_input_count; ++i) {
        const DSL_RUNTIME_INPUT_REQUEST &input =
            program_plan->runtime_inputs[i];
        DSL_RUNTIME_INPUT_RECORD record;
        memset(&record, 0, sizeof(record));
        record.input_kind = input.input_kind;
        record.source_owner_pu_st = input.source_owner_pu_st;
        record.source_value_id = input.source_value_id;
        record.source_ty = input.source_ty;
        record.source_tcon = input.source_tcon;
        record.handle_ty = input.handle_ty;
        if (input.input_kind == DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR) {
            DSL_IR_VALUE_RECORD value;
            if (DSL_IR_Image_Get_Value(input.source_value_id, &value))
                record.source_st = value.st;
        }
        if (!DSL_Program_Interface_Runtime_Input_Contract_Valid(&record) ||
            !DSL_Program_Interface_String_Valid(input.stable_role) ||
            input.handle_ty == TY_IDX_ZERO)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime input request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_RUNTIME_INPUT_REQUEST &previous =
                program_plan->runtime_inputs[j];
            if ((input.input_kind ==
                     DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
                 previous.input_kind == input.input_kind &&
                 previous.source_owner_pu_st == input.source_owner_pu_st &&
                 previous.source_value_id == input.source_value_id) ||
                (input.input_kind !=
                     DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
                 previous.input_kind !=
                     DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
                 strcmp(previous.stable_role, input.stable_role) == 0))
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate runtime input request",
                            i + 1);
        }
    }

    for (UINT32 i = 0;
         i < program_plan->runtime_input_binding_count; ++i) {
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &binding =
            program_plan->runtime_input_bindings[i];
        BOOL root = binding.binding_kind ==
                        DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE ||
                    binding.binding_kind ==
                        DSL_RUNTIME_INPUT_BINDING_ROOT_RESOURCE;
        if (!DSL_IR_Image_PU_ST_Valid(binding.owner_pu_st) ||
            binding.handle_ty == TY_IDX_ZERO ||
            TY_IDX_index(binding.handle_ty) >= TY_Table_Size() ||
            TY_kind(binding.handle_ty) != KIND_POINTER ||
            !DSL_Program_Interface_String_Valid(binding.semantic_role) ||
            binding.source_position == 0 ||
            binding.binding_kind <
                DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE ||
            binding.binding_kind >
                DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL ||
            (root &&
             (binding.runtime_input_index >=
                  program_plan->runtime_input_count ||
              program_plan->runtime_inputs[binding.runtime_input_index].
                  handle_ty != binding.handle_ty ||
              (binding.binding_kind ==
                   DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE) !=
                  (program_plan->runtime_inputs
                       [binding.runtime_input_index].input_kind ==
                   DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR))) ||
            (!root && binding.runtime_input_index !=
                          DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime binding request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_RUNTIME_INPUT_BINDING_REQUEST &previous =
                program_plan->runtime_input_bindings[j];
            if (previous.owner_pu_st == binding.owner_pu_st &&
                strcmp(previous.semantic_role, binding.semantic_role) == 0)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate runtime binding request",
                            i + 1);
        }
    }

    std::vector<std::vector<BOOL> > edges
        (program_plan->runtime_input_binding_count,
         std::vector<BOOL>(program_plan->runtime_input_binding_count, FALSE));
    for (UINT32 i = 0; i < program_plan->runtime_input_call_count; ++i) {
        const DSL_RUNTIME_INPUT_CALL_REQUEST &request =
            program_plan->runtime_input_calls[i];
        if (request.caller_binding_index >=
                program_plan->runtime_input_binding_count ||
            request.callee_binding_index >=
                program_plan->runtime_input_binding_count)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime call request", i + 1);
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &caller =
            program_plan->runtime_input_bindings
                [request.caller_binding_index];
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &callee =
            program_plan->runtime_input_bindings
                [request.callee_binding_index];
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(request.callsite_id, &callsite) ||
            callsite.owner_pu_st != caller.owner_pu_st ||
            callsite.callee_pu_st != callee.owner_pu_st ||
            callee.binding_kind !=
                DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL ||
            caller.handle_ty != callee.handle_ty)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime call request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (program_plan->runtime_input_calls[j].callsite_id ==
                    request.callsite_id &&
                program_plan->runtime_input_calls[j].callee_binding_index ==
                    request.callee_binding_index)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate runtime call request",
                            i + 1);
        }
        edges[request.caller_binding_index]
             [request.callee_binding_index] = TRUE;
    }
    for (UINT32 k = 0; k < edges.size(); ++k)
        for (UINT32 i = 0; i < edges.size(); ++i)
            for (UINT32 j = 0; j < edges.size(); ++j)
                edges[i][j] = edges[i][j] ||
                              (edges[i][k] && edges[k][j]);
    for (UINT32 i = 0; i < edges.size(); ++i) {
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &binding =
            program_plan->runtime_input_bindings[i];
        BOOL root = binding.binding_kind !=
                        DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL;
        UINT32 incoming_calls = 0;
        UINT32 represented_calls = 0;
        for (UINT32 c = 1; c <= DSL_Call_Image_Callsite_Count(); ++c) {
            DSL_CALLSITE_METADATA_RECORD callsite;
            if (!DSL_Call_Image_Get_Callsite(c, &callsite))
                return DSL_Program_Interface_Report
                           (diagnostic, "missing callsite", c);
            if (callsite.callee_pu_st == binding.owner_pu_st) {
                ++incoming_calls;
                for (UINT32 r = 0;
                     r < program_plan->runtime_input_call_count; ++r) {
                    if (program_plan->runtime_input_calls[r].callsite_id ==
                            callsite.id &&
                        program_plan->runtime_input_calls[r].
                            callee_binding_index == i)
                        ++represented_calls;
                }
            }
        }
        if ((root && incoming_calls != 0) ||
            (!root && incoming_calls != represented_calls))
            return DSL_Program_Interface_Report
                       (diagnostic, "incomplete rooted runtime flow", i + 1);
        if (edges[i][i])
            return DSL_Program_Interface_Report
                       (diagnostic, "cyclic runtime flow", i + 1);
        BOOL reachable = root;
        for (UINT32 r = 0; r < edges.size() && !reachable; ++r) {
            if (program_plan->runtime_input_bindings[r].binding_kind !=
                    DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL &&
                edges[r][i])
                reachable = TRUE;
        }
        if (!reachable)
            return DSL_Program_Interface_Report
                       (diagnostic, "unrooted runtime binding", i + 1);
    }

    for (UINT32 i = 0; i < program_plan->runtime_input_count; ++i) {
        UINT32 roots = 0;
        for (UINT32 j = 0;
             j < program_plan->runtime_input_binding_count; ++j) {
            if (program_plan->runtime_input_bindings[j].runtime_input_index == i)
                ++roots;
        }
        if (roots != 1)
            return DSL_Program_Interface_Report
                       (diagnostic, "runtime input root coverage", i + 1);
    }
    return TRUE;
}

typedef struct {
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *request;
    ST_IDX handle_st;
    DSL_RUNTIME_VALUE_PROJECTION_ID projection_id;
} DSL_PROGRAM_CREATED_VALUE;

typedef struct {
    UINT32 request_index;
    ST_IDX handle_st;
    UINT32 final_formal_ordinal;
} DSL_PROGRAM_CREATED_BINDING;

struct DSL_PROGRAM_RUNTIME_VALUE_INDEX_LESS {
    const DSL_RUNTIME_INTERFACE_PLAN *plan;
    /* Borrow the immutable plan used to compare request indexes. */
    explicit DSL_PROGRAM_RUNTIME_VALUE_INDEX_LESS
        (const DSL_RUNTIME_INTERFACE_PLAN *value) : plan(value) {}
    /* Order runtime values by owner, source value, then request index. */
    BOOL operator() (UINT32 left, UINT32 right) const {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &a = plan->values[left];
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &b = plan->values[right];
        if (a.owner_pu_st != b.owner_pu_st)
            return a.owner_pu_st < b.owner_pu_st;
        if (a.source_value_id != b.source_value_id)
            return a.source_value_id < b.source_value_id;
        return a.binding_kind < b.binding_kind;
    }
};

struct DSL_PROGRAM_BINDING_INDEX_LESS {
    const DSL_PROGRAM_INTERFACE_PLAN *plan;
    /* Borrow the immutable plan used to compare binding indexes. */
    explicit DSL_PROGRAM_BINDING_INDEX_LESS
        (const DSL_PROGRAM_INTERFACE_PLAN *value) : plan(value) {}
    /* Order bindings by owner and their deterministic semantic key. */
    BOOL operator() (UINT32 left, UINT32 right) const {
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &a =
            plan->runtime_input_bindings[left];
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &b =
            plan->runtime_input_bindings[right];
        if (a.owner_pu_st != b.owner_pu_st)
            return a.owner_pu_st < b.owner_pu_st;
        UINT32 a_ordinal = DSL_Program_Interface_Binding_Ordinal(plan, left);
        UINT32 b_ordinal = DSL_Program_Interface_Binding_Ordinal(plan, right);
        return a_ordinal != b_ordinal ? a_ordinal < b_ordinal : left < right;
    }
};

struct DSL_PROGRAM_RETIRED_FORMAL_INDEX_LESS {
    const DSL_PROGRAM_INTERFACE_PLAN *plan;
    /* Borrow the immutable plan used to compare retirement indexes. */
    explicit DSL_PROGRAM_RETIRED_FORMAL_INDEX_LESS
        (const DSL_PROGRAM_INTERFACE_PLAN *value) : plan(value) {}
    /* Persist formal retirements in stable original-formal ID order. */
    BOOL operator() (UINT32 left, UINT32 right) const {
        return plan->retired_formals[left].pu_formal_id <
               plan->retired_formals[right].pu_formal_id;
    }
};

struct DSL_PROGRAM_RETIRED_CALL_INDEX_LESS {
    const DSL_PROGRAM_INTERFACE_PLAN *plan;
    /* Borrow the immutable plan used to compare retirement indexes. */
    explicit DSL_PROGRAM_RETIRED_CALL_INDEX_LESS
        (const DSL_PROGRAM_INTERFACE_PLAN *value) : plan(value) {}
    /* Persist call retirements in stable original-argument ID order. */
    BOOL operator() (UINT32 left, UINT32 right) const {
        return plan->retired_call_arguments[left].call_argument_id <
               plan->retired_call_arguments[right].call_argument_id;
    }
};

struct DSL_PROGRAM_RUNTIME_CALL_INDEX_LESS {
    const DSL_RUNTIME_INTERFACE_PLAN *plan;
    /* Borrow the immutable plan used to compare call projection indexes. */
    explicit DSL_PROGRAM_RUNTIME_CALL_INDEX_LESS
        (const DSL_RUNTIME_INTERFACE_PLAN *value) : plan(value) {}
    /* Order projected calls by owner, callsite, ordinal, then index. */
    BOOL operator() (UINT32 left, UINT32 right) const {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &a = plan->calls[left];
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &b = plan->calls[right];
        if (a.owner_pu_st != b.owner_pu_st)
            return a.owner_pu_st < b.owner_pu_st;
        if (a.callsite_id != b.callsite_id)
            return a.callsite_id < b.callsite_id;
        if (a.actual_ordinal != b.actual_ordinal)
            return a.actual_ordinal < b.actual_ordinal;
        return a.direction < b.direction;
    }
};

struct DSL_PROGRAM_INPUT_CALL_INDEX_LESS {
    const DSL_PROGRAM_INTERFACE_PLAN *plan;
    /* Borrow the immutable plan used to compare threaded-input calls. */
    explicit DSL_PROGRAM_INPUT_CALL_INDEX_LESS
        (const DSL_PROGRAM_INTERFACE_PLAN *value) : plan(value) {}
    /* Order threaded-input calls by callsite and binding identities. */
    BOOL operator() (UINT32 left, UINT32 right) const {
        const DSL_RUNTIME_INPUT_CALL_REQUEST &a =
            plan->runtime_input_calls[left];
        const DSL_RUNTIME_INPUT_CALL_REQUEST &b =
            plan->runtime_input_calls[right];
        if (a.callsite_id != b.callsite_id)
            return a.callsite_id < b.callsite_id;
        UINT32 a_ordinal = DSL_Program_Interface_Binding_Ordinal
                               (plan, a.callee_binding_index);
        UINT32 b_ordinal = DSL_Program_Interface_Binding_Ordinal
                               (plan, b.callee_binding_index);
        return a_ordinal != b_ordinal ? a_ordinal < b_ordinal : left < right;
    }
};

/* Find a runtime handle created for one exact owner/source value pair. */
static const DSL_PROGRAM_CREATED_VALUE *
DSL_Program_Interface_Find_Created_Value
        (const std::vector<DSL_PROGRAM_CREATED_VALUE> &created,
         DSL_IR_VALUE_ID value_id)
{
    for (UINT32 i = 0; i < created.size(); ++i) {
        if (created[i].request->source_value_id == value_id)
            return &created[i];
    }
    return NULL;
}

/* Find the committed handle/formal journal for one binding request index. */
static const DSL_PROGRAM_CREATED_BINDING *
DSL_Program_Interface_Find_Created_Binding
        (const std::vector<DSL_PROGRAM_CREATED_BINDING> &created,
         UINT32 request_index)
{
    for (UINT32 i = 0; i < created.size(); ++i) {
        if (created[i].request_index == request_index)
            return &created[i];
    }
    return NULL;
}

/* Recursively detect any executable use of one owner-local symbol. */
static BOOL
DSL_Program_Interface_Tree_Uses_ST (const WN *tree, ST_IDX st)
{
    if (tree == NULL)
        return FALSE;
    if (WN_has_sym(tree) && WN_st_idx(tree) == st)
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (DSL_Program_Interface_Tree_Uses_ST(stmt, st))
                return TRUE;
        }
        return FALSE;
    }
    for (UINT32 i = 0; i < WN_kid_count(tree); ++i) {
        if (DSL_Program_Interface_Tree_Uses_ST(WN_kid(tree, i), st))
            return TRUE;
    }
    return FALSE;
}

/* Map a request-array runtime input index to its deterministic persisted ID. */
static UINT32
DSL_Program_Interface_Input_Id
        (const DSL_PROGRAM_INTERFACE_PLAN *plan, UINT32 request_index)
{
    UINT32 id = 1;
    for (UINT32 i = 0; i < plan->runtime_input_count; ++i) {
        if (DSL_Program_Interface_Input_Less
                (&plan->runtime_inputs[i],
                 &plan->runtime_inputs[request_index]))
            ++id;
    }
    return id;
}

/*
 * Create one owner-local formal handle for a validated runtime-input binding,
 * preserving semantic-role naming and source position during commit.
 */
static ST_IDX
DSL_Program_Interface_Create_Binding_ST
        (const DSL_RUNTIME_INPUT_BINDING_REQUEST &request)
{
    std::string name("__dsl_input_");
    for (const char *cursor = request.semantic_role; *cursor != '\0';
         ++cursor) {
        unsigned char ch = (unsigned char)*cursor;
        name += isalnum(ch) ? (char)ch : '_';
    }
    ST *handle = New_ST(CURRENT_SYMTAB);
    ST_Init(handle, Save_Str(name.c_str()), CLASS_VAR, SCLASS_FORMAL,
            EXPORT_LOCAL, request.handle_ty);
    Set_ST_is_value_parm(*handle);
    Set_ST_Srcpos(*handle, request.source_position);
    return ST_st_idx(handle);
}

/*
 * Rebuild and canonicalize the PU function prototype from final formal order.
 * The helper allocates TY/TYLIST state only during an accepted PU commit.
 */
static TY_IDX
DSL_Program_Interface_Function_TY
        (const std::vector<ST_IDX> &formals)
{
    TY_IDX function_ty;
    TY &function = New_TY(function_ty);
    TY_Init(function, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    Set_TY_align(function_ty, 1);
    TYLIST_IDX tylist_idx;
    Set_TYLIST_type(New_TYLIST(tylist_idx), MTYPE_To_TY(MTYPE_V));
    Set_TY_tylist(function_ty, tylist_idx);
    for (UINT32 i = 0; i < formals.size(); ++i) {
        ST &formal = St_Table[formals[i]];
        TY_IDX formal_ty = ST_sclass(formal) == SCLASS_FORMAL_REF ?
                           Make_Pointer_Type(ST_type(formal)) :
                           ST_type(formal);
        Set_TYLIST_type(New_TYLIST(tylist_idx), formal_ty);
    }
    Set_TYLIST_type(New_TYLIST(tylist_idx), TY_IDX_ZERO);
    return TY_is_unique(function_ty);
}

/*
 * Validate one active PU's old formals, calls, retirement liveness, runtime
 * bindings, and projected returns against the prepared global plan. It only
 * fills journals; failure leaves the PU and images unchanged.
 */
static BOOL
DSL_Program_Interface_Preflight_PU
        (PU_Info *pu, const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
         const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan,
         std::vector<ST_IDX> *region_prune_symbols,
         std::vector<DSL_RUNTIME_RETURN_SITE> *returns,
         FILE *diagnostic)
{
    if (pu == NULL || PU_Info_tree_ptr(pu) == NULL ||
        !DSL_IR_Image_Current_PU_Is(PU_Info_proc_sym(pu)))
        return DSL_Program_Interface_Report
                   (diagnostic, "program unit is not active", 0);
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *entry = PU_Info_tree_ptr(pu);
    if (WN_operator(entry) != OPR_FUNC_ENTRY)
        return DSL_Program_Interface_Report
                   (diagnostic, "program unit has no FUNC_ENTRY", 0);

    UINT32 canonical_formals = 0;
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal))
            return DSL_Program_Interface_Report
                       (diagnostic, "missing canonical formal", i);
        if (formal.owner_pu_st != owner_pu_st)
            continue;
        if (formal.formal_ordinal != canonical_formals ||
            formal.formal_ordinal >= WN_num_formals(entry) ||
            WN_st_idx(WN_formal(entry, formal.formal_ordinal)) !=
                formal.formal_st)
            return DSL_Program_Interface_Report
                       (diagnostic, "physical formal mismatch", i);
        const DSL_RETIRED_FORMAL_REQUEST *retired =
            DSL_Program_Interface_Find_Retired_Formal_Request
                (program_plan, formal.id);
        if (retired != NULL) {
            if (DSL_Program_Interface_Tree_Uses_ST
                    (WN_func_body(entry), formal.formal_st))
                return DSL_Program_Interface_Report
                           (diagnostic,
                            "retired formal remains executable", i);
            if (DSL_Region_Symbol_Use_Count(pu, formal.formal_st) != 0)
                region_prune_symbols->push_back(formal.formal_st);
        }
        ++canonical_formals;
    }
    if (canonical_formals != WN_num_formals(entry))
        return DSL_Program_Interface_Report
                   (diagnostic, "incomplete canonical formal interface", 0);
    if (!region_prune_symbols->empty() &&
        !DSL_Region_Can_Prune_Input_Symbols
             (pu, &(*region_prune_symbols)[0], region_prune_symbols->size()))
        return DSL_Program_Interface_Report
                   (diagnostic,
                    "retired formal REGION interface is not prunable", 0);

    for (UINT32 i = 0; i < runtime_plan->value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request =
            runtime_plan->values[i];
        if (request.owner_pu_st != owner_pu_st)
            continue;
        if (ST_IDX_level(request.expected_source_st) != CURRENT_SYMTAB ||
            ST_IDX_index(request.expected_source_st) == 0 ||
            ST_IDX_index(request.expected_source_st) >=
                ST_Table_Size(CURRENT_SYMTAB) ||
            ST_type(St_Table[request.expected_source_st]) !=
                request.expected_source_ty)
            return DSL_Program_Interface_Report
                       (diagnostic, "active projection source mismatch",
                        i + 1);
    }

    for (UINT32 i = 0; i < program_plan->runtime_input_count; ++i) {
        const DSL_RUNTIME_INPUT_REQUEST &input =
            program_plan->runtime_inputs[i];
        if (input.input_kind == DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
            input.source_owner_pu_st == owner_pu_st) {
            DSL_IR_EXTERNAL_TENSOR_REFERENCE reference;
            if (!DSL_IR_Image_Get_External_Tensor_Reference
                    (owner_pu_st, input.source_value_id, &reference) ||
                reference.descriptor_ty != input.source_ty ||
                reference.st == ST_IDX_ZERO)
                return DSL_Program_Interface_Report
                           (diagnostic, "external runtime input mismatch",
                            i + 1);
        }
    }

    std::vector<UINT32> runtime_call_indexes;
    for (UINT32 i = 0; i < runtime_plan->call_count; ++i) {
        if (runtime_plan->calls[i].owner_pu_st == owner_pu_st)
            runtime_call_indexes.push_back(i);
    }
    std::sort(runtime_call_indexes.begin(), runtime_call_indexes.end(),
              DSL_PROGRAM_RUNTIME_CALL_INDEX_LESS(runtime_plan));
    for (UINT32 order = 0; order < runtime_call_indexes.size(); ++order) {
        UINT32 i = runtime_call_indexes[order];
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request =
            runtime_plan->calls[i];
        const WN *call = DSL_Call_Image_Get_Call_WN(request.callsite_id);
        if (call == NULL || WN_operator(call) != OPR_CALL ||
            request.actual_ordinal >= WN_kid_count(call) ||
            DSL_Runtime_Interface_Parent_Block(entry, call) == NULL)
            return DSL_Program_Interface_Report
                       (diagnostic, "physical call is unavailable", i + 1);
    }
    return DSL_Runtime_Interface_Collect_Returns
               (entry, NULL, owner_pu_st, runtime_plan, returns, diagnostic);
}

/* Create a projected input/result PARM with the exact runtime handle ABI. */
static WN *
DSL_Program_Interface_Create_Call_Parm
        (const DSL_PROGRAM_CREATED_VALUE &value, UINT32 direction)
{
    TYPE_ID mtype = TY_mtype(value.request->handle_ty);
    if (direction == DSL_RUNTIME_CALL_INPUT) {
        WN *load = WN_CreateLdid
                       (OPR_LDID, mtype, mtype, 0, value.handle_st,
                        value.request->handle_ty);
        return WN_CreateParm
                   (mtype, load, value.request->handle_ty,
                    WN_PARM_BY_VALUE | WN_PARM_READ_ONLY |
                    WN_PARM_PASSED_NOT_SAVED);
    }
    TY_IDX pointer_ty = Make_Pointer_Type(value.request->handle_ty);
    WN *address = WN_CreateLda
                      (OPR_LDA, Pointer_Mtype, MTYPE_V, 0, pointer_ty,
                       value.handle_st);
    return WN_CreateParm
               (Pointer_Mtype, address, pointer_ty,
                WN_PARM_BY_REFERENCE | WN_PARM_OUT |
                WN_PARM_PASSED_NOT_SAVED);
}

/* Create a threaded runtime-input PARM from one caller binding handle. */
static WN *
DSL_Program_Interface_Create_Binding_Parm
        (const DSL_PROGRAM_CREATED_BINDING &binding,
         const DSL_PROGRAM_INTERFACE_PLAN *plan)
{
    const DSL_RUNTIME_INPUT_BINDING_REQUEST &request =
        plan->runtime_input_bindings[binding.request_index];
    TYPE_ID mtype = TY_mtype(request.handle_ty);
    WN *load = WN_CreateLdid
                   (OPR_LDID, mtype, mtype, 0, binding.handle_st,
                    request.handle_ty);
    return WN_CreateParm
               (mtype, load, request.handle_ty,
                WN_PARM_BY_VALUE | WN_PARM_READ_ONLY |
                WN_PARM_PASSED_NOT_SAVED);
}

/*
 * Publish globally ordered runtime-input rows once, after plan validation and
 * before per-PU binding rows. Existing rows must match exactly.
 */
static BOOL
DSL_Program_Interface_Install_Inputs
        (const DSL_PROGRAM_INTERFACE_PLAN *plan)
{
    if (DSL_Program_Interface_Image_Runtime_Input_Count() != 0)
        return DSL_Program_Interface_Image_Runtime_Input_Count() ==
               plan->runtime_input_count;
    for (UINT32 expected_id = 1;
         expected_id <= plan->runtime_input_count; ++expected_id) {
        UINT32 request_index = DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX;
        for (UINT32 i = 0; i < plan->runtime_input_count; ++i) {
            if (DSL_Program_Interface_Input_Id(plan, i) == expected_id) {
                request_index = i;
                break;
            }
        }
        if (request_index == DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX)
            return FALSE;
        const DSL_RUNTIME_INPUT_REQUEST &request =
            plan->runtime_inputs[request_index];
        DSL_RUNTIME_INPUT_RECORD record;
        memset(&record, 0, sizeof(record));
        record.input_kind = request.input_kind;
        record.source_owner_pu_st = request.source_owner_pu_st;
        record.source_value_id = request.source_value_id;
        record.source_ty = request.source_ty;
        record.source_tcon = request.source_tcon;
        record.stable_role = Save_Str(request.stable_role);
        record.handle_ty = request.handle_ty;
        if (request.input_kind ==
            DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR) {
            DSL_IR_VALUE_RECORD value;
            if (!DSL_IR_Image_Get_Value(request.source_value_id, &value))
                return FALSE;
            record.source_st = value.st;
        }
        if (DSL_Program_Interface_Image_Add_Runtime_Input(&record) !=
            expected_id)
            return FALSE;
    }
    return TRUE;
}

/*
 * Commit the prepared program-interface plan for one active PU. It rebuilds
 * formals/prototype/calls/returns, retires verified-dead ABI slots, installs
 * runtime handles, and finally publishes matching image rows and counters.
 */
BOOL
DSL_Program_Interface_Apply_PU
        (PU_Info *pu, const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
         const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan,
         FILE *diagnostic, DSL_PROGRAM_INTERFACE_RESULT *result)
{
    DSL_Program_Interface_Result_Init(result);
    std::vector<ST_IDX> region_prune_symbols;
    std::vector<DSL_RUNTIME_RETURN_SITE> returns;
    if (result == NULL || program_plan == NULL || runtime_plan == NULL)
        return FALSE;
    std::string fingerprint = DSL_Program_Interface_Plan_Fingerprint
                                  (program_plan, runtime_plan);
    BOOL already_applied = DSL_Program_Interface_Image_Has_Records();
    if ((already_applied &&
         (DSL_program_interface_prepared_plan.empty() ||
          DSL_program_interface_prepared_plan != fingerprint)) ||
        (!already_applied &&
         !DSL_Program_Interface_Plan_Validate
              (program_plan, runtime_plan, diagnostic)) ||
        !DSL_Program_Interface_Preflight_PU
             (pu, program_plan, runtime_plan, &region_prune_symbols,
              &returns, diagnostic)) {
        if (already_applied &&
            DSL_program_interface_prepared_plan != fingerprint)
            DSL_Program_Interface_Report
                (diagnostic, "prepared plan does not match", 0);
        return FALSE;
    }
    if (!already_applied)
        DSL_program_interface_prepared_plan = fingerprint;
    if (!DSL_Program_Interface_Install_Inputs(program_plan))
        return DSL_Program_Interface_Report
                   (diagnostic, "could not install runtime inputs", 0);

    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *old_entry = PU_Info_tree_ptr(pu);
    std::vector<DSL_PROGRAM_CREATED_VALUE> created_values;
    std::vector<DSL_PROGRAM_CREATED_BINDING> created_bindings;
    std::vector<UINT32> value_indexes;
    std::vector<UINT32> binding_indexes;

    for (UINT32 i = 0; i < runtime_plan->value_count; ++i) {
        if (runtime_plan->values[i].owner_pu_st == owner_pu_st)
            value_indexes.push_back(i);
    }
    std::sort(value_indexes.begin(), value_indexes.end(),
              DSL_PROGRAM_RUNTIME_VALUE_INDEX_LESS(runtime_plan));
    for (UINT32 order = 0; order < value_indexes.size(); ++order) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request =
            runtime_plan->values[value_indexes[order]];
        DSL_PROGRAM_CREATED_VALUE value;
        value.request = &request;
        value.handle_st = DSL_Runtime_Interface_Create_Handle_ST(request);
        value.projection_id = DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID;
        created_values.push_back(value);
    }
    for (UINT32 i = 0;
         i < program_plan->runtime_input_binding_count; ++i) {
        if (program_plan->runtime_input_bindings[i].owner_pu_st ==
            owner_pu_st)
            binding_indexes.push_back(i);
    }
    std::sort(binding_indexes.begin(), binding_indexes.end(),
              DSL_PROGRAM_BINDING_INDEX_LESS(program_plan));
    for (UINT32 order = 0; order < binding_indexes.size(); ++order) {
        UINT32 i = binding_indexes[order];
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &request =
            program_plan->runtime_input_bindings[i];
        DSL_PROGRAM_CREATED_BINDING binding;
        binding.request_index = i;
        binding.handle_st =
            DSL_Program_Interface_Create_Binding_ST(request);
        binding.final_formal_ordinal =
            DSL_Program_Interface_Binding_Ordinal(program_plan, i);
        created_bindings.push_back(binding);
    }

    UINT32 final_formal_count =
        DSL_Program_Interface_Live_Formal_Count(program_plan, owner_pu_st) +
        created_bindings.size();
    std::vector<ST_IDX> formal_sts(final_formal_count, ST_IDX_ZERO);
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal) ||
            formal.owner_pu_st != owner_pu_st ||
            DSL_Program_Interface_Find_Retired_Formal_Request
                (program_plan, formal.id) != NULL)
            continue;
        const DSL_PROGRAM_CREATED_VALUE *value =
            DSL_Program_Interface_Find_Created_Value
                (created_values, formal.formal_value_id);
        UINT32 ordinal = formal.formal_ordinal -
            DSL_Program_Interface_Retired_Formals_Before
                (program_plan, owner_pu_st, formal.formal_ordinal);
        FmtAssert(value != NULL && ordinal < formal_sts.size(),
                  ("preflighted live formal is missing"));
        formal_sts[ordinal] = value->handle_st;
    }
    for (UINT32 i = 0; i < created_bindings.size(); ++i) {
        FmtAssert(created_bindings[i].final_formal_ordinal <
                      formal_sts.size(),
                  ("preflighted runtime binding ordinal is invalid"));
        formal_sts[created_bindings[i].final_formal_ordinal] =
            created_bindings[i].handle_st;
    }
    for (UINT32 i = 0; i < formal_sts.size(); ++i)
        FmtAssert(formal_sts[i] != ST_IDX_ZERO,
                  ("preflighted final formal is missing"));

    WN *entry = WN_CreateEntry
                    ((INT16)formal_sts.size(), owner_pu_st,
                     WN_func_body(old_entry), WN_func_pragmas(old_entry),
                     WN_func_varrefs(old_entry));
    for (UINT32 i = 0; i < formal_sts.size(); ++i)
        WN_formal(entry, i) = WN_CreateIdname(0, formal_sts[i]);
    TY_IDX function_ty = DSL_Program_Interface_Function_TY(formal_sts);
    FmtAssert(function_ty != TY_IDX_ZERO,
              ("preflighted program interface prototype failed"));
    Set_PU_prototype(Pu_Table[ST_pu(St_Table[owner_pu_st])], function_ty);
    Set_PU_Info_tree_ptr(pu, entry);

    for (UINT32 callsite_id = 1;
         callsite_id <= DSL_Call_Image_Callsite_Count(); ++callsite_id) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            continue;
        WN *old_call = const_cast<WN *>
                           (DSL_Call_Image_Get_Call_WN(callsite_id));
        UINT32 final_count =
            DSL_Program_Interface_Live_Formal_Count
                (program_plan, callsite.callee_pu_st);
        for (UINT32 i = 0;
             i < program_plan->runtime_input_binding_count; ++i) {
            if (program_plan->runtime_input_bindings[i].owner_pu_st ==
                callsite.callee_pu_st)
                ++final_count;
        }
        WN *new_call = WN_Create
                           (OPR_CALL, WN_rtype(old_call), WN_desc(old_call),
                            final_count);
        WN_st_idx(new_call) = WN_st_idx(old_call);
        WN_call_flag(new_call) = WN_call_flag(old_call);
        WN_Set_Linenum(new_call, WN_Get_Linenum(old_call));

        for (UINT32 old_ordinal = 0;
             old_ordinal < (UINT32)WN_kid_count(old_call); ++old_ordinal) {
            DSL_CALL_ARGUMENT_RECORD argument;
            BOOL has_argument = DSL_Call_ABI_Image_Find_Argument_By_Id
                                    (callsite_id, old_ordinal, &argument);
            if (has_argument &&
                DSL_Program_Interface_Find_Retired_Call_Request
                    (program_plan, argument.id) != NULL)
                continue;
            const DSL_RUNTIME_CALL_PROJECTION_REQUEST *request =
                DSL_Program_Interface_Find_Runtime_Call
                    (runtime_plan, callsite_id, old_ordinal);
            FmtAssert(request != NULL,
                      ("preflighted live call projection is missing"));
            const DSL_PROGRAM_CREATED_VALUE *value =
                DSL_Program_Interface_Find_Created_Value
                    (created_values, request->source_value_id);
            FmtAssert(value != NULL,
                      ("preflighted call value is missing"));
            UINT32 final_ordinal = old_ordinal -
                DSL_Program_Interface_Retired_Formals_Before
                    (program_plan, callsite.callee_pu_st, old_ordinal);
            WN_kid(new_call, final_ordinal) =
                DSL_Program_Interface_Create_Call_Parm
                    (*value, request->direction);
        }
        for (UINT32 i = 0;
             i < program_plan->runtime_input_call_count; ++i) {
            const DSL_RUNTIME_INPUT_CALL_REQUEST &request =
                program_plan->runtime_input_calls[i];
            if (request.callsite_id != callsite_id)
                continue;
            const DSL_PROGRAM_CREATED_BINDING *caller =
                DSL_Program_Interface_Find_Created_Binding
                    (created_bindings, request.caller_binding_index);
            UINT32 ordinal = DSL_Program_Interface_Binding_Ordinal
                                 (program_plan,
                                  request.callee_binding_index);
            FmtAssert(caller != NULL && ordinal < final_count,
                      ("preflighted runtime call binding is missing"));
            WN_kid(new_call, ordinal) =
                DSL_Program_Interface_Create_Binding_Parm
                    (*caller, program_plan);
        }
        for (UINT32 i = 0; i < final_count; ++i)
            FmtAssert(WN_kid(new_call, i) != NULL,
                      ("preflighted final call actual is missing"));
        WN *parent = DSL_Runtime_Interface_Parent_Block(entry, old_call);
        FmtAssert(parent != NULL,
                  ("preflighted call parent is missing"));
        WN_INSERT_BlockBefore(parent, old_call, new_call);
        FmtAssert(DSL_Call_Image_Replace_Call_WN
                      (callsite_id, old_call, new_call),
                  ("callsite runtime association update failed"));
        WN_DELETE_FromBlock(parent, old_call);
        ++result->rewritten_call_count;
    }

    for (UINT32 i = 0; i < returns.size(); ++i) {
        const DSL_PROGRAM_CREATED_VALUE *target =
            DSL_Program_Interface_Find_Created_Value
                (created_values,
                 returns[i].result_formal->source_value_id);
        const DSL_PROGRAM_CREATED_VALUE *source =
            DSL_Program_Interface_Find_Created_Value
                (created_values, returns[i].source_value->source_value_id);
        FmtAssert(target != NULL && source != NULL,
                  ("preflighted return projection is missing"));
        TYPE_ID mtype = TY_mtype(source->request->handle_ty);
        WN *load = WN_CreateLdid
                       (OPR_LDID, mtype, mtype, 0, source->handle_st,
                        source->request->handle_ty);
        WN *store = WN_CreateStid
                        (OPR_STID, MTYPE_V, mtype, 0, target->handle_st,
                         target->request->handle_ty, load);
        WN_Set_Linenum(store, WN_Get_Linenum(returns[i].store));
        WN_INSERT_BlockBefore
            (returns[i].parent_block, returns[i].store, store);
        WN_DELETE_FromBlock(returns[i].parent_block, returns[i].store);
        ++result->rewritten_return_count;
    }

    if (!region_prune_symbols.empty())
        FmtAssert(DSL_Region_Prune_Input_Symbols
                      (pu, &region_prune_symbols[0],
                       region_prune_symbols.size()),
                  ("preflighted REGION input pruning failed"));

    for (UINT32 i = 0; i < created_values.size(); ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD record;
        memset(&record, 0, sizeof(record));
        record.owner_pu_st = owner_pu_st;
        record.source_value_id = created_values[i].request->source_value_id;
        record.source_st = created_values[i].request->expected_source_st;
        record.source_ty = created_values[i].request->expected_source_ty;
        record.handle_st = created_values[i].handle_st;
        record.handle_ty = created_values[i].request->handle_ty;
        record.binding_kind = created_values[i].request->binding_kind;
        record.formal_ordinal = created_values[i].request->formal_ordinal;
        created_values[i].projection_id =
            DSL_Runtime_Interface_Image_Add_Value(&record);
        ++result->canonical_projection_count;
    }
    std::vector<UINT32> projected_call_indexes;
    for (UINT32 i = 0; i < runtime_plan->call_count; ++i) {
        if (runtime_plan->calls[i].owner_pu_st == owner_pu_st)
            projected_call_indexes.push_back(i);
    }
    std::sort(projected_call_indexes.begin(), projected_call_indexes.end(),
              DSL_PROGRAM_RUNTIME_CALL_INDEX_LESS(runtime_plan));
    for (UINT32 order = 0; order < projected_call_indexes.size(); ++order) {
        UINT32 i = projected_call_indexes[order];
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request =
            runtime_plan->calls[i];
        const DSL_PROGRAM_CREATED_VALUE *value =
            DSL_Program_Interface_Find_Created_Value
                (created_values, request.source_value_id);
        DSL_RUNTIME_CALL_PROJECTION_RECORD record;
        memset(&record, 0, sizeof(record));
        record.owner_pu_st = owner_pu_st;
        record.callsite_id = request.callsite_id;
        record.value_projection_id = value->projection_id;
        record.source_value_id = request.source_value_id;
        record.actual_ordinal = request.actual_ordinal;
        record.callee_formal_ordinal = request.callee_formal_ordinal;
        record.direction = request.direction;
        DSL_Runtime_Interface_Image_Add_Call(&record);
    }

    std::vector<UINT32> retired_formal_indexes;
    for (UINT32 i = 0; i < program_plan->retired_formal_count; ++i)
        retired_formal_indexes.push_back(i);
    std::sort(retired_formal_indexes.begin(), retired_formal_indexes.end(),
              DSL_PROGRAM_RETIRED_FORMAL_INDEX_LESS(program_plan));
    for (UINT32 order = 0; order < retired_formal_indexes.size(); ++order) {
        UINT32 i = retired_formal_indexes[order];
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal
                (program_plan->retired_formals[i].pu_formal_id, &formal) ||
            formal.owner_pu_st != owner_pu_st)
            continue;
        DSL_RETIRED_FORMAL_RECORD record;
        memset(&record, 0, sizeof(record));
        record.pu_formal_id = formal.id;
        record.owner_pu_st = owner_pu_st;
        record.formal_value_id = formal.formal_value_id;
        record.formal_st = formal.formal_st;
        record.formal_ty = formal.formal_ty;
        record.old_formal_ordinal = formal.formal_ordinal;
        record.retirement_reason =
            DSL_INTERFACE_RETIREMENT_VERIFIED_DEAD_INPUT;
        record.semantic_role =
            Save_Str(program_plan->retired_formals[i].semantic_role);
        DSL_Program_Interface_Image_Add_Retired_Formal(&record);
        ++result->retired_formal_count;
    }
    std::vector<UINT32> retired_call_indexes;
    for (UINT32 i = 0;
         i < program_plan->retired_call_argument_count; ++i)
        retired_call_indexes.push_back(i);
    std::sort(retired_call_indexes.begin(), retired_call_indexes.end(),
              DSL_PROGRAM_RETIRED_CALL_INDEX_LESS(program_plan));
    for (UINT32 order = 0; order < retired_call_indexes.size(); ++order) {
        UINT32 i = retired_call_indexes[order];
        DSL_CALL_ARGUMENT_RECORD argument;
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_ABI_Image_Get_Argument
                (program_plan->retired_call_arguments[i].call_argument_id,
                 &argument) ||
            !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            continue;
        DSL_RETIRED_CALL_ARGUMENT_RECORD record;
        memset(&record, 0, sizeof(record));
        record.call_argument_id = argument.id;
        record.callsite_id = argument.callsite_id;
        record.argument_value_id = argument.argument_value_id;
        record.old_actual_ordinal = argument.actual_ordinal;
        record.old_callee_formal_ordinal = argument.callee_formal_ordinal;
        record.retirement_reason =
            DSL_INTERFACE_RETIREMENT_VERIFIED_DEAD_INPUT;
        record.semantic_role = Save_Str
            (program_plan->retired_call_arguments[i].semantic_role);
        DSL_Program_Interface_Image_Add_Retired_Call(&record);
        ++result->retired_call_argument_count;
    }
    for (UINT32 i = 0; i < created_bindings.size(); ++i) {
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &request =
            program_plan->runtime_input_bindings
                [created_bindings[i].request_index];
        DSL_RUNTIME_INPUT_BINDING_RECORD record;
        memset(&record, 0, sizeof(record));
        record.owner_pu_st = owner_pu_st;
        record.runtime_input_id = request.runtime_input_index ==
            DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX ? 0 :
            DSL_Program_Interface_Input_Id
                (program_plan, request.runtime_input_index);
        record.handle_st = created_bindings[i].handle_st;
        record.handle_ty = request.handle_ty;
        record.final_formal_ordinal =
            created_bindings[i].final_formal_ordinal;
        record.binding_kind = request.binding_kind;
        record.semantic_role = Save_Str(request.semantic_role);
        DSL_Program_Interface_Image_Add_Runtime_Binding(&record);
        ++result->runtime_binding_count;
    }
    std::vector<UINT32> input_call_indexes;
    for (UINT32 i = 0;
         i < program_plan->runtime_input_call_count; ++i)
        input_call_indexes.push_back(i);
    std::sort(input_call_indexes.begin(), input_call_indexes.end(),
              DSL_PROGRAM_INPUT_CALL_INDEX_LESS(program_plan));
    for (UINT32 order = 0; order < input_call_indexes.size(); ++order) {
        UINT32 i = input_call_indexes[order];
        const DSL_RUNTIME_INPUT_CALL_REQUEST &request =
            program_plan->runtime_input_calls[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(request.callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            continue;
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &caller =
            program_plan->runtime_input_bindings
                [request.caller_binding_index];
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &callee =
            program_plan->runtime_input_bindings
                [request.callee_binding_index];
        DSL_RUNTIME_INPUT_CALL_RECORD record;
        memset(&record, 0, sizeof(record));
        record.callsite_id = request.callsite_id;
        record.caller_owner_pu_st = caller.owner_pu_st;
        record.caller_final_formal_ordinal =
            DSL_Program_Interface_Binding_Ordinal
                (program_plan, request.caller_binding_index);
        record.callee_owner_pu_st = callee.owner_pu_st;
        record.callee_final_formal_ordinal =
            DSL_Program_Interface_Binding_Ordinal
                (program_plan, request.callee_binding_index);
        record.final_actual_ordinal =
            record.callee_final_formal_ordinal;
        record.final_callee_formal_ordinal =
            record.callee_final_formal_ordinal;
        record.handle_ty = callee.handle_ty;
        record.semantic_role = Save_Str(callee.semantic_role);
        DSL_Program_Interface_Image_Add_Runtime_Call(&record);
        ++result->runtime_call_count;
    }
    result->runtime_input_count = program_plan->runtime_input_count;
    DSL_program_interface_committed_pus.push_back(owner_pu_st);
    return TRUE;
}

/* Count persisted retired formals before an original PU ordinal. */
static UINT32
DSL_Program_Interface_Image_Retired_Before
        (ST_IDX owner_pu_st, UINT32 old_ordinal)
{
    UINT32 count = 0;
    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Retired_Formal_Count(); ++i) {
        DSL_RETIRED_FORMAL_RECORD retired;
        if (DSL_Program_Interface_Image_Get_Retired_Formal(i, &retired) &&
            retired.owner_pu_st == owner_pu_st &&
            retired.old_formal_ordinal < old_ordinal)
            ++count;
    }
    return count;
}

/* Count persisted retired call arguments before an original call ordinal. */
static UINT32
DSL_Program_Interface_Image_Retired_Call_Before
        (DSL_CALLSITE_METADATA_ID callsite_id, UINT32 old_ordinal)
{
    UINT32 count = 0;
    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Retired_Call_Count(); ++i) {
        DSL_RETIRED_CALL_ARGUMENT_RECORD retired;
        if (DSL_Program_Interface_Image_Get_Retired_Call(i, &retired) &&
            retired.callsite_id == callsite_id &&
            retired.old_actual_ordinal < old_ordinal)
            ++count;
    }
    return count;
}

/* Verify one final physical PARM against its handle TY and direction ABI. */
static BOOL
DSL_Program_Interface_Parm_Matches_Handle
        (const WN *parm, ST_IDX handle_st, TY_IDX handle_ty,
         UINT32 direction)
{
    const WN *actual = parm == NULL || WN_operator(parm) != OPR_PARM ?
                       NULL : WN_kid0(parm);
    if (direction == DSL_RUNTIME_CALL_INPUT)
        return actual != NULL && WN_operator(actual) == OPR_LDID &&
               WN_st_idx(actual) == handle_st && WN_ty(parm) == handle_ty &&
               WN_ty(actual) == handle_ty &&
               WN_parm_flag(parm) ==
                   (WN_PARM_BY_VALUE | WN_PARM_READ_ONLY |
                    WN_PARM_PASSED_NOT_SAVED);
    return actual != NULL && WN_operator(actual) == OPR_LDA &&
           WN_st_idx(actual) == handle_st && WN_ty(parm) == WN_ty(actual) &&
           TY_kind(WN_ty(parm)) == KIND_POINTER &&
           TY_pointed(WN_ty(parm)) == handle_ty &&
           WN_parm_flag(parm) ==
               (WN_PARM_BY_REFERENCE | WN_PARM_OUT |
                WN_PARM_PASSED_NOT_SAVED);
}

/*
 * Verify a committed PU's final formals, calls, retirement rows, and threaded
 * runtime-input bindings against physical WHIRL and active local symbols.
 */
BOOL
DSL_Program_Interface_Validate_PU (PU_Info *pu, FILE *diagnostic)
{
    if (pu == NULL || PU_Info_tree_ptr(pu) == NULL ||
        !DSL_IR_Image_Current_PU_Is(PU_Info_proc_sym(pu)))
        return DSL_Program_Interface_Report
                   (diagnostic, "program unit is not active", 0);
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *entry = PU_Info_tree_ptr(pu);
    if (WN_operator(entry) != OPR_FUNC_ENTRY)
        return DSL_Program_Interface_Report
                   (diagnostic, "program unit has no FUNC_ENTRY", 0);

    UINT32 expected_formals = 0;
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal) ||
            formal.owner_pu_st != owner_pu_st)
            continue;
        DSL_RETIRED_FORMAL_RECORD retired;
        if (DSL_Program_Interface_Image_Find_Retired_Formal
                (formal.id, &retired)) {
            if (DSL_Program_Interface_Tree_Uses_ST
                    (WN_func_body(entry), formal.formal_st) ||
                DSL_Region_Symbol_Use_Count(pu, formal.formal_st) != 0)
                return DSL_Program_Interface_Report
                           (diagnostic, "retired formal remains executable",
                            formal.id);
            continue;
        }
        DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
        UINT32 ordinal = formal.formal_ordinal -
            DSL_Program_Interface_Image_Retired_Before
                (owner_pu_st, formal.formal_ordinal);
        if (!DSL_Runtime_Interface_Image_Find_Value
                (owner_pu_st, formal.formal_value_id, &projection) ||
            ordinal >= WN_num_formals(entry) ||
            WN_st_idx(WN_formal(entry, ordinal)) != projection.handle_st)
            return DSL_Program_Interface_Report
                       (diagnostic, "live formal projection mismatch",
                        formal.id);
        ++expected_formals;
    }

    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Runtime_Binding_Count(); ++i) {
        DSL_RUNTIME_INPUT_BINDING_RECORD binding;
        if (!DSL_Program_Interface_Image_Get_Runtime_Binding(i, &binding) ||
            binding.owner_pu_st != owner_pu_st)
            continue;
        if (binding.final_formal_ordinal >= WN_num_formals(entry) ||
            WN_st_idx(WN_formal(entry, binding.final_formal_ordinal)) !=
                binding.handle_st ||
            ST_IDX_level(binding.handle_st) != CURRENT_SYMTAB ||
            ST_IDX_index(binding.handle_st) == 0 ||
            ST_IDX_index(binding.handle_st) >=
                ST_Table_Size(CURRENT_SYMTAB) ||
            ST_type(St_Table[binding.handle_st]) != binding.handle_ty ||
            ST_sclass(St_Table[binding.handle_st]) != SCLASS_FORMAL ||
            ST_Srcpos(St_Table[binding.handle_st]) == 0)
            return DSL_Program_Interface_Report
                       (diagnostic, "runtime binding formal mismatch", i);
        ++expected_formals;
    }
    if (expected_formals != WN_num_formals(entry))
        return DSL_Program_Interface_Report
                   (diagnostic, "incomplete final formal interface", 0);

    for (UINT32 callsite_id = 1;
         callsite_id <= DSL_Call_Image_Callsite_Count(); ++callsite_id) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            continue;
        const WN *call = DSL_Call_Image_Get_Call_WN(callsite_id);
        if (call == NULL || WN_operator(call) != OPR_CALL)
            return DSL_Program_Interface_Report
                       (diagnostic, "final call is unavailable", callsite_id);
        UINT32 expected_actuals = 0;
        for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
            DSL_CALL_ARGUMENT_RECORD argument;
            if (!DSL_Call_ABI_Image_Get_Argument(i, &argument) ||
                argument.callsite_id != callsite_id)
                continue;
            DSL_RETIRED_CALL_ARGUMENT_RECORD retired;
            if (DSL_Program_Interface_Image_Find_Retired_Call
                    (argument.id, &retired))
                continue;
            DSL_RUNTIME_CALL_PROJECTION_RECORD runtime_call;
            DSL_RUNTIME_VALUE_PROJECTION_RECORD value;
            UINT32 ordinal = argument.actual_ordinal -
                DSL_Program_Interface_Image_Retired_Call_Before
                    (callsite_id, argument.actual_ordinal);
            if (!DSL_Runtime_Interface_Image_Find_Call
                    (callsite_id, argument.actual_ordinal, &runtime_call) ||
                !DSL_Runtime_Interface_Image_Get_Value
                    (runtime_call.value_projection_id, &value) ||
                ordinal >= WN_kid_count(call) ||
                !DSL_Program_Interface_Parm_Matches_Handle
                    (WN_kid(call, ordinal), value.handle_st,
                     value.handle_ty, runtime_call.direction))
                return DSL_Program_Interface_Report
                           (diagnostic, "live call projection mismatch",
                            argument.id);
            ++expected_actuals;
        }
        for (UINT32 i = 1;
             i <= DSL_Program_Interface_Image_Runtime_Call_Count(); ++i) {
            DSL_RUNTIME_INPUT_CALL_RECORD runtime_call;
            if (!DSL_Program_Interface_Image_Get_Runtime_Call
                    (i, &runtime_call) ||
                runtime_call.callsite_id != callsite_id)
                continue;
            DSL_RUNTIME_INPUT_BINDING_RECORD caller;
            if (!DSL_Program_Interface_Image_Find_Runtime_Binding
                    (runtime_call.caller_owner_pu_st,
                     runtime_call.caller_final_formal_ordinal, &caller) ||
                runtime_call.final_actual_ordinal >= WN_kid_count(call) ||
                !DSL_Program_Interface_Parm_Matches_Handle
                    (WN_kid(call, runtime_call.final_actual_ordinal),
                     caller.handle_st, caller.handle_ty,
                     DSL_RUNTIME_CALL_INPUT))
                return DSL_Program_Interface_Report
                           (diagnostic, "threaded runtime call mismatch", i);
            ++expected_actuals;
        }
        /* Hidden result projections are not represented in call-ABI rows. */
        for (UINT32 i = 1;
             i <= DSL_Runtime_Interface_Image_Call_Count(); ++i) {
            DSL_RUNTIME_CALL_PROJECTION_RECORD runtime_call;
            if (!DSL_Runtime_Interface_Image_Get_Call(i, &runtime_call) ||
                runtime_call.callsite_id != callsite_id ||
                runtime_call.direction != DSL_RUNTIME_CALL_RESULT)
                continue;
            DSL_RUNTIME_VALUE_PROJECTION_RECORD value;
            UINT32 ordinal = runtime_call.actual_ordinal -
                DSL_Program_Interface_Image_Retired_Call_Before
                    (callsite_id, runtime_call.actual_ordinal);
            if (!DSL_Runtime_Interface_Image_Get_Value
                    (runtime_call.value_projection_id, &value) ||
                ordinal >= WN_kid_count(call) ||
                !DSL_Program_Interface_Parm_Matches_Handle
                    (WN_kid(call, ordinal), value.handle_st,
                     value.handle_ty, DSL_RUNTIME_CALL_RESULT))
                return DSL_Program_Interface_Report
                           (diagnostic, "runtime result call mismatch", i);
            ++expected_actuals;
        }
        if (expected_actuals != WN_kid_count(call))
            return DSL_Program_Interface_Report
                       (diagnostic, "incomplete final call interface",
                        callsite_id);
    }
    return TRUE;
}

/*
 * Extend final interface verification with the lowering postcondition: no
 * promoted external tensor or canonical source ST may remain executable.
 */
BOOL
DSL_Program_Interface_Validate_Lowered_PU
        (PU_Info *pu, FILE *diagnostic)
{
    if (!DSL_Program_Interface_Validate_PU(pu, diagnostic))
        return FALSE;
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *entry = PU_Info_tree_ptr(pu);
    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Runtime_Input_Count(); ++i) {
        DSL_RUNTIME_INPUT_RECORD input;
        if (!DSL_Program_Interface_Image_Get_Runtime_Input(i, &input) ||
            input.input_kind != DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR ||
            input.source_owner_pu_st != owner_pu_st)
            continue;
        if (DSL_Program_Interface_Tree_Uses_ST(entry, input.source_st))
            return DSL_Program_Interface_Report
                       (diagnostic,
                        "promoted source definition remains executable", i);
    }
    if (DSL_Runtime_Interface_Tree_Uses_Source_ST(entry, owner_pu_st))
        return DSL_Program_Interface_Report
                   (diagnostic, "canonical tensor source remains executable",
                    0);
    return TRUE;
}
