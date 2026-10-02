/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: derive the complete SecureResNet FHE program/runtime interface
 * plans from persisted DSL identities. This is the FHE policy adapter between
 * the model census and common/com's generic owner-safe transaction.
 *
 * Compilation scope: VHO FHE semantic runtime lowering, whole program.
 * The unique root PU must be active because typed external-tensor lookup and
 * call-result recovery interpret its local symbol table.
 *
 * Design references:
 *   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-D
 *   doc/FHE-SYNC5-PROGRAM-INTERFACE-CONTRACT.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 */

#include <string.h>

#include <deque>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "dsl_tensor_fold.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "fhe_runtime_interface_plan.h"
#include "fhe_semantic_convert.h"
#include "fhe_semantic_runtime_lower.h"
#include "pu_info.h"
#include "srcpos.h"
#include "symtab.h"
#include "wn.h"

typedef std::pair<ST_IDX, UINT32> VHO_FHE_FORMAL_KEY;
typedef std::pair<ST_IDX, DSL_IR_VALUE_ID> VHO_FHE_VALUE_KEY;
typedef std::pair<ST_IDX, std::string> VHO_FHE_BINDING_KEY;

static std::vector<DSL_RETIRED_FORMAL_REQUEST>
    VHO_FHE_retired_formals;
static std::vector<DSL_RETIRED_CALL_ARGUMENT_REQUEST>
    VHO_FHE_retired_call_arguments;
static std::vector<DSL_RUNTIME_INPUT_REQUEST> VHO_FHE_runtime_inputs;
static std::vector<DSL_RUNTIME_INPUT_BINDING_REQUEST>
    VHO_FHE_runtime_input_bindings;
static std::vector<DSL_RUNTIME_INPUT_CALL_REQUEST>
    VHO_FHE_runtime_input_calls;
static std::vector<DSL_RUNTIME_VALUE_PROJECTION_REQUEST>
    VHO_FHE_value_projections;
static std::vector<DSL_RUNTIME_CALL_PROJECTION_REQUEST>
    VHO_FHE_call_projections;
static std::deque<std::string> VHO_FHE_interface_role_storage;
static DSL_PROGRAM_INTERFACE_PLAN VHO_FHE_program_plan;
static DSL_RUNTIME_INTERFACE_PLAN VHO_FHE_runtime_plan;
static BOOL VHO_FHE_interface_plans_prepared;
static BOOL VHO_FHE_interface_plans_failed;
static std::set<ST_IDX> VHO_FHE_interface_applied_owners;

/* Hash semantic fields in a fixed order, never struct padding or pointers. */
static void
VHO_FHE_Interface_Plan_Hash_Word (UINT64 *hash, UINT64 value)
{
    for (UINT32 byte = 0; byte < 8; ++byte) {
        *hash ^= (value >> (byte * 8)) & 0xff;
        *hash *= 1099511628211ULL;
    }
}

/* Include the complete role text and its terminator in the semantic hash. */
static void
VHO_FHE_Interface_Plan_Hash_Role (UINT64 *hash, const char *role)
{
    if (role == NULL) {
        VHO_FHE_Interface_Plan_Hash_Word(hash, 0);
        return;
    }
    VHO_FHE_Interface_Plan_Hash_Word(hash, strlen(role) + 1);
    for (const unsigned char *p =
             reinterpret_cast<const unsigned char *>(role);
         *p != '\0'; ++p) {
        *hash ^= *p;
        *hash *= 1099511628211ULL;
    }
}

/* Fingerprint every validated request field for later all-PU equality checks. */
static UINT64
VHO_FHE_Interface_Plan_Fingerprint (void)
{
    UINT64 hash = 14695981039346656037ULL;
#define HASH_WORD(value) VHO_FHE_Interface_Plan_Hash_Word(&hash, (value))
#define HASH_ROLE(value) VHO_FHE_Interface_Plan_Hash_Role(&hash, (value))
    HASH_WORD(VHO_FHE_retired_formals.size());
    for (UINT32 i = 0; i < VHO_FHE_retired_formals.size(); ++i) {
        const DSL_RETIRED_FORMAL_REQUEST &r = VHO_FHE_retired_formals[i];
        HASH_WORD(r.pu_formal_id);
        HASH_ROLE(r.semantic_role);
    }
    HASH_WORD(VHO_FHE_retired_call_arguments.size());
    for (UINT32 i = 0; i < VHO_FHE_retired_call_arguments.size(); ++i) {
        const DSL_RETIRED_CALL_ARGUMENT_REQUEST &r =
            VHO_FHE_retired_call_arguments[i];
        HASH_WORD(r.call_argument_id);
        HASH_ROLE(r.semantic_role);
    }
    HASH_WORD(VHO_FHE_runtime_inputs.size());
    for (UINT32 i = 0; i < VHO_FHE_runtime_inputs.size(); ++i) {
        const DSL_RUNTIME_INPUT_REQUEST &r = VHO_FHE_runtime_inputs[i];
        HASH_WORD(r.input_kind);
        HASH_WORD(r.source_owner_pu_st);
        HASH_WORD(r.source_value_id);
        HASH_WORD(r.source_ty);
        HASH_WORD(r.source_tcon);
        HASH_ROLE(r.stable_role);
        HASH_WORD(r.handle_ty);
    }
    HASH_WORD(VHO_FHE_runtime_input_bindings.size());
    for (UINT32 i = 0; i < VHO_FHE_runtime_input_bindings.size(); ++i) {
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &r =
            VHO_FHE_runtime_input_bindings[i];
        HASH_WORD(r.owner_pu_st);
        HASH_WORD(r.runtime_input_index);
        HASH_WORD(r.handle_ty);
        HASH_WORD(r.binding_kind);
        HASH_ROLE(r.semantic_role);
        HASH_WORD(r.source_position);
    }
    HASH_WORD(VHO_FHE_runtime_input_calls.size());
    for (UINT32 i = 0; i < VHO_FHE_runtime_input_calls.size(); ++i) {
        const DSL_RUNTIME_INPUT_CALL_REQUEST &r =
            VHO_FHE_runtime_input_calls[i];
        HASH_WORD(r.callsite_id);
        HASH_WORD(r.caller_binding_index);
        HASH_WORD(r.callee_binding_index);
    }
    HASH_WORD(VHO_FHE_value_projections.size());
    for (UINT32 i = 0; i < VHO_FHE_value_projections.size(); ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &r =
            VHO_FHE_value_projections[i];
        HASH_WORD(r.owner_pu_st);
        HASH_WORD(r.source_value_id);
        HASH_WORD(r.expected_source_st);
        HASH_WORD(r.expected_source_ty);
        HASH_WORD(r.handle_ty);
        HASH_WORD(r.binding_kind);
        HASH_WORD(r.formal_ordinal);
    }
    HASH_WORD(VHO_FHE_call_projections.size());
    for (UINT32 i = 0; i < VHO_FHE_call_projections.size(); ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &r =
            VHO_FHE_call_projections[i];
        HASH_WORD(r.owner_pu_st);
        HASH_WORD(r.callsite_id);
        HASH_WORD(r.source_value_id);
        HASH_WORD(r.actual_ordinal);
        HASH_WORD(r.callee_formal_ordinal);
        HASH_WORD(r.direction);
    }
#undef HASH_WORD
#undef HASH_ROLE
    return hash;
}

/* Emit the stable FHE semantic planning diagnostic and return FALSE. */
static BOOL
VHO_FHE_Interface_Plan_Report (FILE *diagnostic, const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "CFHELOWER-SEM-002: %s\n", message);
    return FALSE;
}

/* Keep one process-lifetime copy for every role pointer stored in a request. */
static const char *
VHO_FHE_Interface_Plan_Copy_Role (const char *role)
{
    if (role == NULL || role[0] == '\0')
        return NULL;
    VHO_FHE_interface_role_storage.push_back(std::string(role));
    return VHO_FHE_interface_role_storage.back().c_str();
}

/* Publish borrowed plan views over the current process-owned vectors. */
static void
VHO_FHE_Interface_Plan_Refresh_Views (void)
{
    memset(&VHO_FHE_program_plan, 0, sizeof(VHO_FHE_program_plan));
    memset(&VHO_FHE_runtime_plan, 0, sizeof(VHO_FHE_runtime_plan));
    VHO_FHE_program_plan.retired_formals = VHO_FHE_retired_formals.empty() ?
        NULL : &VHO_FHE_retired_formals[0];
    VHO_FHE_program_plan.retired_formal_count =
        VHO_FHE_retired_formals.size();
    VHO_FHE_program_plan.retired_call_arguments =
        VHO_FHE_retired_call_arguments.empty() ? NULL :
        &VHO_FHE_retired_call_arguments[0];
    VHO_FHE_program_plan.retired_call_argument_count =
        VHO_FHE_retired_call_arguments.size();
    VHO_FHE_program_plan.runtime_inputs = VHO_FHE_runtime_inputs.empty() ?
        NULL : &VHO_FHE_runtime_inputs[0];
    VHO_FHE_program_plan.runtime_input_count = VHO_FHE_runtime_inputs.size();
    VHO_FHE_program_plan.runtime_input_bindings =
        VHO_FHE_runtime_input_bindings.empty() ? NULL :
        &VHO_FHE_runtime_input_bindings[0];
    VHO_FHE_program_plan.runtime_input_binding_count =
        VHO_FHE_runtime_input_bindings.size();
    VHO_FHE_program_plan.runtime_input_calls =
        VHO_FHE_runtime_input_calls.empty() ? NULL :
        &VHO_FHE_runtime_input_calls[0];
    VHO_FHE_program_plan.runtime_input_call_count =
        VHO_FHE_runtime_input_calls.size();
    VHO_FHE_runtime_plan.values = VHO_FHE_value_projections.empty() ? NULL :
        &VHO_FHE_value_projections[0];
    VHO_FHE_runtime_plan.value_count = VHO_FHE_value_projections.size();
    VHO_FHE_runtime_plan.calls = VHO_FHE_call_projections.empty() ? NULL :
        &VHO_FHE_call_projections[0];
    VHO_FHE_runtime_plan.call_count = VHO_FHE_call_projections.size();
}

/* Release every borrowed request view at a program or checkpoint boundary. */
void
VHO_FHE_Runtime_Interface_Plans_Reset (void)
{
    VHO_FHE_retired_formals.clear();
    VHO_FHE_retired_call_arguments.clear();
    VHO_FHE_runtime_inputs.clear();
    VHO_FHE_runtime_input_bindings.clear();
    VHO_FHE_runtime_input_calls.clear();
    VHO_FHE_value_projections.clear();
    VHO_FHE_call_projections.clear();
    VHO_FHE_interface_role_storage.clear();
    VHO_FHE_interface_plans_prepared = FALSE;
    VHO_FHE_interface_plans_failed = FALSE;
    VHO_FHE_interface_applied_owners.clear();
    VHO_FHE_Interface_Plan_Refresh_Views();
}

/* Match the closed BatchNorm-only role set certified dead by SYNC-3. */
static BOOL
VHO_FHE_Interface_Plan_Is_Dead_BN_Role (STR_IDX role)
{
    static const char *const roles[] = {
        "cnn.basic_block.bn1.scale",
        "cnn.basic_block.bn1.bias",
        "cnn.basic_block.bn1.mean",
        "cnn.basic_block.bn1.variance",
        "cnn.basic_block.bn2.scale",
        "cnn.basic_block.bn2.bias",
        "cnn.basic_block.bn2.mean",
        "cnn.basic_block.bn2.variance",
        "cnn.basic_block.downsample.bn.scale",
        "cnn.basic_block.downsample.bn.bias",
        "cnn.basic_block.downsample.bn.mean",
        "cnn.basic_block.downsample.bn.variance"
    };
    if (role == STR_IDX_ZERO)
        return FALSE;
    const char *name = Index_To_Str(role);
    for (UINT32 i = 0; i < sizeof(roles) / sizeof(roles[0]); ++i) {
        if (strcmp(name, roles[i]) == 0)
            return TRUE;
    }
    return FALSE;
}

/* Match live plaintext Conv slots without interpreting source symbol names. */
static BOOL
VHO_FHE_Interface_Plan_Is_Conv_Plain_Role (STR_IDX role)
{
    static const char *const roles[] = {
        "cnn.basic_block.conv1.weight",
        "cnn.basic_block.conv1.bias",
        "cnn.basic_block.conv2.weight",
        "cnn.basic_block.conv2.bias",
        "cnn.basic_block.downsample.conv.weight",
        "cnn.basic_block.downsample.conv.bias"
    };
    if (role == STR_IDX_ZERO)
        return FALSE;
    const char *name = Index_To_Str(role);
    for (UINT32 i = 0; i < sizeof(roles) / sizeof(roles[0]); ++i) {
        if (strcmp(name, roles[i]) == 0)
            return TRUE;
    }
    return FALSE;
}

/* Find or create the canonical pointer to one opaque public ABI handle. */
static TY_IDX
VHO_FHE_Interface_Plan_Opaque_Handle_TY (const char *name)
{
    for (UINT32 index = 1; index < TY_Table_Size(); ++index) {
        TY_IDX candidate = make_TY_IDX(index);
        if (TY_kind(candidate) != KIND_POINTER)
            continue;
        TY_IDX pointee = TY_pointed(candidate);
        if (pointee != TY_IDX_ZERO && TY_IDX_index(pointee) < TY_Table_Size() &&
            TY_kind(pointee) == KIND_STRUCT && TY_name(pointee) != NULL &&
            strcmp(TY_name(pointee), name) == 0)
            return Make_Pointer_Type(pointee);
    }
    TY_IDX opaque_ty;
    TY &opaque = New_TY(opaque_ty);
    TY_Init(opaque, 0, KIND_STRUCT, MTYPE_M, Save_Str(name));
    Set_TY_align(opaque_ty, 1);
    return Make_Pointer_Type(opaque_ty);
}

/* Resolve one value's structured PU owner without opening foreign symtabs. */
static BOOL
VHO_FHE_Interface_Plan_Value_Owner
        (const DSL_IR_VALUE_RECORD &value, ST_IDX *owner_pu_st)
{
    if (value.name == STR_IDX_ZERO || owner_pu_st == NULL)
        return FALSE;
    UINT32 matches = 0;
    ST_IDX owner = ST_IDX_ZERO;
    for (UINT32 id = 1; id <= DSL_Call_Image_PU_Identity_Count(); ++id) {
        DSL_PU_SOURCE_IDENTITY_RECORD identity;
        DSL_IR_VALUE_RECORD candidate;
        if (!DSL_Call_Image_Get_PU_Identity(id, &identity) ||
            !DSL_IR_Image_Find_PU_Value
                 (value.st, Index_To_Str(value.name),
                  ST_name(St_Table[identity.owner_pu_st]), &candidate) ||
            candidate.id != value.id)
            continue;
        owner = identity.owner_pu_st;
        ++matches;
    }
    if (matches != 1)
        return FALSE;
    *owner_pu_st = owner;
    return TRUE;
}

/* Read one canonical node operand through its persisted reference row. */
static BOOL
VHO_FHE_Interface_Plan_Node_Operand
        (const DSL_IR_NODE_RECORD &node, UINT32 ordinal,
         DSL_IR_VALUE_ID *value_id)
{
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    if (value_id == NULL || ordinal >= node.operand_count ||
        !DSL_IR_Image_Get_Value_Reference
             (node.first_operand_reference_id + ordinal, &reference) ||
        reference.owner_node_id != node.id || reference.ordinal != ordinal ||
        reference.value_id == DSL_IR_VALUE_INVALID_ID)
        return FALSE;
    *value_id = reference.value_id;
    return TRUE;
}

/* Find the unique root PU and require it to be the active physical owner. */
static BOOL
VHO_FHE_Interface_Plan_Active_Root
        (PU_Info *root_pu, ST_IDX *root_owner, SRCPOS *fallback_position)
{
    if (root_pu == NULL || Current_PU_Info != root_pu ||
        PU_Info_tree_ptr(root_pu) == NULL || root_owner == NULL ||
        fallback_position == NULL)
        return FALSE;
    ST_IDX owner = PU_Info_proc_sym(root_pu);
    UINT32 incoming = 0;
    for (UINT32 id = 1; id <= DSL_Call_Image_Callsite_Count(); ++id) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(id, &callsite))
            return FALSE;
        if (callsite.callee_pu_st == owner)
            ++incoming;
    }
    if (incoming != 0 || ST_IDX_level(owner) != GLOBAL_SYMTAB ||
        ST_IDX_index(owner) == 0 ||
        ST_IDX_index(owner) >= ST_Table_Size(GLOBAL_SYMTAB) ||
        ST_class(St_Table[owner]) != CLASS_FUNC)
        return FALSE;
    SRCPOS position = WN_Get_Linenum(PU_Info_tree_ptr(root_pu));
    if (position == 0) {
        for (UINT32 id = 1; id <= DSL_Call_Image_Callsite_Count(); ++id) {
            const WN *call = DSL_Call_Image_Get_Call_WN(id);
            if (call != NULL && WN_Get_Linenum(call) != 0) {
                position = WN_Get_Linenum(call);
                break;
            }
        }
    }
    if (position == 0)
        return FALSE;
    *root_owner = owner;
    *fallback_position = position;
    return TRUE;
}

/* Add one owner/value projection or prove an existing request is identical. */
static BOOL
VHO_FHE_Interface_Plan_Add_Value_Projection
        (ST_IDX owner, DSL_IR_VALUE_ID value_id, TY_IDX handle_ty,
         UINT32 binding_kind, UINT32 formal_ordinal,
         std::map<VHO_FHE_VALUE_KEY, UINT32> *indexes, FILE *diagnostic)
{
    DSL_IR_VALUE_RECORD value;
    ST_IDX actual_owner = ST_IDX_ZERO;
    if (indexes == NULL || !DSL_IR_Image_Get_Value(value_id, &value) ||
        !VHO_FHE_Interface_Plan_Value_Owner(value, &actual_owner) ||
        actual_owner != owner) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHELOWER-SEM-002: value projection owner mismatch "
                    "owner=%u value=%u actual_owner=%u\n",
                    (UINT32)owner, value_id, (UINT32)actual_owner);
        return FALSE;
    }
    VHO_FHE_VALUE_KEY key(owner, value_id);
    std::map<VHO_FHE_VALUE_KEY, UINT32>::const_iterator found =
        indexes->find(key);
    if (found != indexes->end()) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request =
            VHO_FHE_value_projections[found->second];
        BOOL identical = request.handle_ty == handle_ty &&
                         request.binding_kind == binding_kind &&
                         request.formal_ordinal == formal_ordinal;
        if (!identical && diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHELOWER-SEM-002: conflicting projection for owner=%u "
                    "value=%u old_kind=%u new_kind=%u\n", (UINT32)owner,
                    value_id, request.binding_kind, binding_kind);
        return identical;
    }
    DSL_RUNTIME_VALUE_PROJECTION_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.owner_pu_st = owner;
    request.source_value_id = value_id;
    request.expected_source_st = value.st;
    request.expected_source_ty = value.ty;
    request.handle_ty = handle_ty;
    request.binding_kind = binding_kind;
    request.formal_ordinal = formal_ordinal;
    (*indexes)[key] = VHO_FHE_value_projections.size();
    VHO_FHE_value_projections.push_back(request);
    return TRUE;
}

/* Return the stable source position for one root-owned external tensor. */
static SRCPOS
VHO_FHE_Interface_Plan_Source_Position
        (const DSL_IR_EXTERNAL_TENSOR_REFERENCE &reference,
         SRCPOS fallback)
{
    if (ST_IDX_level(reference.st) == CURRENT_SYMTAB &&
        ST_IDX_index(reference.st) != 0 &&
        ST_IDX_index(reference.st) < ST_Table_Size(CURRENT_SYMTAB) &&
        ST_Srcpos(St_Table[reference.st]) != 0)
        return ST_Srcpos(St_Table[reference.st]);
    return fallback;
}

/* Find the one approved ACE composite profile used by all three resources. */
static BOOL
VHO_FHE_Interface_Plan_Coefficient_Stages
        (DSL_FHE_APPROX_STAGE_RECORD stages[3], FILE *diagnostic)
{
    DSL_FHE_COMPOSITE_PROFILE_RECORD profile;
    UINT32 matches = 0;
    for (UINT32 id = 1; id <= DSL_FHE_Approx_Profile_Count(); ++id) {
        DSL_FHE_COMPOSITE_PROFILE_RECORD candidate;
        if (!DSL_FHE_Approx_Profile_Get(id, &candidate) ||
            candidate.profile_name == STR_IDX_ZERO)
            return FALSE;
        if (strcmp(Index_To_Str(candidate.profile_name),
                   VHO_FHE_ACE_RELU_PROFILE_NAME) == 0 &&
            candidate.profile_version == VHO_FHE_ACE_RELU_PROFILE_VERSION) {
            profile = candidate;
            ++matches;
        }
    }
    if (matches != 1 || profile.stage_count != 3) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHELOWER-SEM-002: expected one ACE profile with "
                    "three stages, found profiles=%u stages=%u\n",
                    matches, matches == 1 ? profile.stage_count : 0);
        return FALSE;
    }
    for (UINT32 ordinal = 0; ordinal < 3; ++ordinal) {
        if (!DSL_FHE_Approx_Stage_Find(profile.id, ordinal,
                                      &stages[ordinal]) ||
            stages[ordinal].coefficient_tensor_tcon == TCON_IDX_ZERO) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "CFHELOWER-SEM-002: ACE coefficient stage %u is "
                        "missing or has no tensor TCON\n", ordinal);
            return FALSE;
        }
    }
    return TRUE;
}

/* Recover one root caller's hidden-result value from the canonical call WN. */
static BOOL
VHO_FHE_Interface_Plan_Call_Result_Value
        (ST_IDX root_owner, DSL_CALLSITE_METADATA_ID callsite_id,
         UINT32 result_ordinal, DSL_IR_VALUE_ID *value_id)
{
    const WN *call = DSL_Call_Image_Get_Call_WN(callsite_id);
    if (call == NULL || WN_operator(call) != OPR_CALL ||
        result_ordinal >= (UINT32)WN_kid_count(call) || value_id == NULL)
        return FALSE;
    const WN *parm = WN_kid(call, result_ordinal);
    const WN *address = parm == NULL || WN_operator(parm) != OPR_PARM ?
        NULL : WN_kid0(parm);
    if (address == NULL || WN_operator(address) != OPR_LDA ||
        ST_IDX_level(WN_st_idx(address)) != CURRENT_SYMTAB ||
        ST_IDX_index(WN_st_idx(address)) == 0 ||
        ST_IDX_index(WN_st_idx(address)) >= ST_Table_Size(CURRENT_SYMTAB))
        return FALSE;
    ST_IDX st = WN_st_idx(address);
    DSL_IR_VALUE_RECORD value;
    if (!DSL_IR_Image_Find_PU_Value
            (st, ST_name(St_Table[st]), ST_name(St_Table[root_owner]),
             &value))
        return FALSE;
    *value_id = value.id;
    return TRUE;
}

/* Add the exact external tensor and root promoted-source binding requests. */
static BOOL
VHO_FHE_Interface_Plan_Add_External_Inputs
        (ST_IDX root_owner, const std::set<DSL_IR_VALUE_ID> &source_values,
         TY_IDX plaintext_ty, SRCPOS fallback_position,
         std::map<DSL_IR_VALUE_ID, UINT32> *input_indexes,
         std::map<DSL_IR_VALUE_ID, UINT32> *binding_indexes,
         FILE *diagnostic)
{
    for (std::set<DSL_IR_VALUE_ID>::const_iterator it = source_values.begin();
         it != source_values.end(); ++it) {
        DSL_IR_EXTERNAL_TENSOR_REFERENCE reference;
        if (!DSL_IR_Image_Get_External_Tensor_Reference
                (root_owner, *it, &reference) ||
            reference.tensor_tcon == TCON_IDX_ZERO ||
            reference.tensor_key == NULL || reference.tensor_key[0] == '\0') {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "CFHELOWER-SEM-002: external tensor value %u has "
                        "no validated tensor TCON\n", *it);
            return FALSE;
        }
        const char *role = VHO_FHE_Interface_Plan_Copy_Role
                               (reference.tensor_key);
        if (role == NULL)
            return FALSE;
        DSL_RUNTIME_INPUT_REQUEST input;
        memset(&input, 0, sizeof(input));
        input.input_kind = DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR;
        input.source_owner_pu_st = root_owner;
        input.source_value_id = *it;
        input.source_ty = reference.descriptor_ty;
        input.source_tcon = reference.tensor_tcon;
        input.stable_role = role;
        input.handle_ty = plaintext_ty;
        UINT32 input_index = VHO_FHE_runtime_inputs.size();
        VHO_FHE_runtime_inputs.push_back(input);
        (*input_indexes)[*it] = input_index;

        DSL_RUNTIME_INPUT_BINDING_REQUEST binding;
        memset(&binding, 0, sizeof(binding));
        binding.owner_pu_st = root_owner;
        binding.runtime_input_index = input_index;
        binding.handle_ty = plaintext_ty;
        binding.binding_kind =
            DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE;
        binding.semantic_role = role;
        binding.source_position = VHO_FHE_Interface_Plan_Source_Position
                                      (reference, fallback_position);
        (*binding_indexes)[*it] = VHO_FHE_runtime_input_bindings.size();
        VHO_FHE_runtime_input_bindings.push_back(binding);
    }
    return TRUE;
}

/* Add model/coefficient inputs and one binding per program unit. */
static BOOL
VHO_FHE_Interface_Plan_Add_Resources
        (ST_IDX root_owner, TY_IDX model_ty, TY_IDX plaintext_ty,
         SRCPOS source_position,
         std::map<VHO_FHE_BINDING_KEY, UINT32> *binding_indexes,
         FILE *diagnostic)
{
    static const char *const roles[4] = {
        "fhe.model",
        "fhe.relu.coefficient.stage0",
        "fhe.relu.coefficient.stage1",
        "fhe.relu.coefficient.stage2"
    };
    DSL_FHE_APPROX_STAGE_RECORD stages[3];
    if (!VHO_FHE_Interface_Plan_Coefficient_Stages(stages, diagnostic))
        return FALSE;
    UINT32 input_indexes[4];
    for (UINT32 i = 0; i < 4; ++i) {
        DSL_RUNTIME_INPUT_REQUEST input;
        memset(&input, 0, sizeof(input));
        input.input_kind = i == 0 ? DSL_RUNTIME_INPUT_OPAQUE_RESOURCE :
            DSL_RUNTIME_INPUT_TENSOR_TCON_RESOURCE;
        input.stable_role = roles[i];
        input.handle_ty = i == 0 ? model_ty : plaintext_ty;
        if (i != 0) {
            DSL_TENSOR_TCON_RECORD tensor;
            input.source_tcon = stages[i - 1].coefficient_tensor_tcon;
            if (!DSL_Tensor_TCON_Get(input.source_tcon, &tensor)) {
                if (diagnostic != NULL)
                    fprintf(diagnostic,
                            "CFHELOWER-SEM-002: coefficient resource %u "
                            "has an invalid tensor TCON %u\n", i,
                            (UINT32)input.source_tcon);
                return FALSE;
            }
            input.source_ty = tensor.descriptor_ty;
        }
        input_indexes[i] = VHO_FHE_runtime_inputs.size();
        VHO_FHE_runtime_inputs.push_back(input);
    }

    for (UINT32 owner_id = 1;
         owner_id <= DSL_Call_Image_PU_Identity_Count(); ++owner_id) {
        DSL_PU_SOURCE_IDENTITY_RECORD identity;
        if (!DSL_Call_Image_Get_PU_Identity(owner_id, &identity)) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "CFHELOWER-SEM-002: PU identity %u is missing\n",
                        owner_id);
            return FALSE;
        }
        for (UINT32 i = 0; i < 4; ++i) {
            DSL_RUNTIME_INPUT_BINDING_REQUEST binding;
            memset(&binding, 0, sizeof(binding));
            binding.owner_pu_st = identity.owner_pu_st;
            binding.runtime_input_index = identity.owner_pu_st == root_owner ?
                input_indexes[i] : DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX;
            binding.handle_ty = i == 0 ? model_ty : plaintext_ty;
            binding.binding_kind = identity.owner_pu_st == root_owner ?
                DSL_RUNTIME_INPUT_BINDING_ROOT_RESOURCE :
                DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL;
            binding.semantic_role = roles[i];
            binding.source_position = source_position;
            VHO_FHE_BINDING_KEY key(identity.owner_pu_st, roles[i]);
            (*binding_indexes)[key] = VHO_FHE_runtime_input_bindings.size();
            VHO_FHE_runtime_input_bindings.push_back(binding);
        }
    }
    return TRUE;
}

/* Derive formal retirements and all canonical call-argument classifications. */
static BOOL
VHO_FHE_Interface_Plan_Classify_Call_ABI
        (std::map<VHO_FHE_FORMAL_KEY, const char *> *retired_formals,
         std::set<DSL_IR_VALUE_ID> *source_values,
         std::map<VHO_FHE_FORMAL_KEY, STR_IDX> *live_formal_roles)
{
    if (retired_formals == NULL || source_values == NULL ||
        live_formal_roles == NULL)
        return FALSE;
    for (UINT32 id = 1; id <= DSL_Call_ABI_Image_Argument_Count(); ++id) {
        DSL_CALL_ARGUMENT_RECORD argument;
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_ABI_Image_Get_Argument(id, &argument) ||
            !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite))
            return FALSE;
        VHO_FHE_FORMAL_KEY key(callsite.callee_pu_st,
                               argument.callee_formal_ordinal);
        if (VHO_FHE_Interface_Plan_Is_Dead_BN_Role(argument.semantic_role)) {
            const char *role = VHO_FHE_Interface_Plan_Copy_Role
                                   (Index_To_Str(argument.semantic_role));
            std::map<VHO_FHE_FORMAL_KEY, const char *>::const_iterator found =
                retired_formals->find(key);
            if (role == NULL ||
                (found != retired_formals->end() &&
                 strcmp(found->second, role) != 0))
                return FALSE;
            (*retired_formals)[key] = role;
            DSL_RETIRED_CALL_ARGUMENT_REQUEST retired;
            memset(&retired, 0, sizeof(retired));
            retired.call_argument_id = argument.id;
            retired.semantic_role = role;
            VHO_FHE_retired_call_arguments.push_back(retired);
            continue;
        }
        std::map<VHO_FHE_FORMAL_KEY, STR_IDX>::const_iterator found =
            live_formal_roles->find(key);
        if (found != live_formal_roles->end() &&
            found->second != argument.semantic_role)
            return FALSE;
        (*live_formal_roles)[key] = argument.semantic_role;
        if (VHO_FHE_Interface_Plan_Is_Conv_Plain_Role
                (argument.semantic_role))
            source_values->insert(argument.argument_value_id);
    }
    for (std::map<VHO_FHE_FORMAL_KEY, const char *>::const_iterator it =
             retired_formals->begin(); it != retired_formals->end(); ++it) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Find_Formal
                (it->first.first, it->first.second, &formal))
            return FALSE;
        DSL_RETIRED_FORMAL_REQUEST retired;
        memset(&retired, 0, sizeof(retired));
        retired.pu_formal_id = formal.id;
        retired.semantic_role = it->second;
        VHO_FHE_retired_formals.push_back(retired);
    }
    return TRUE;
}

/* Add root-only folded stem and classifier plaintext sources. */
static BOOL
VHO_FHE_Interface_Plan_Add_Root_Plain_Sources
        (std::set<DSL_IR_VALUE_ID> *source_values)
{
    if (source_values == NULL)
        return FALSE;
    for (UINT32 id = 1;
         id <= DSL_FHE_Plan_BN_Fold_Provenance_Count(); ++id) {
        DSL_FHE_BN_FOLD_PROVENANCE_RECORD fold;
        DSL_IR_NODE_RECORD conv;
        DSL_IR_VALUE_ID value_id;
        if (!DSL_FHE_Plan_Get_BN_Fold_Provenance(id, &fold) ||
            fold.context_callsite_id != DSL_CALLSITE_METADATA_INVALID_ID)
            continue;
        if (!DSL_IR_Image_Get_Node(fold.conv_node_id, &conv) ||
            !VHO_FHE_Interface_Plan_Node_Operand(conv, 1, &value_id))
            return FALSE;
        source_values->insert(value_id);
        if (!VHO_FHE_Interface_Plan_Node_Operand(conv, 2, &value_id))
            return FALSE;
        source_values->insert(value_id);
    }
    for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
        DSL_IR_VALUE_ID value_id;
        if (!DSL_IR_Image_Get_Node(id, &node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                 (node.opcode_descriptor_id, &opcode))
            return FALSE;
        if ((node.flags & (DSL_IR_NODE_FLAG_RETIRED |
                           DSL_IR_NODE_FLAG_LOWERED)) != 0 ||
            opcode.logical_operator != OPR_DSLLINEAR)
            continue;
        if (!VHO_FHE_Interface_Plan_Node_Operand(node, 1, &value_id))
            return FALSE;
        source_values->insert(value_id);
        if (!VHO_FHE_Interface_Plan_Node_Operand(node, 2, &value_id))
            return FALSE;
        source_values->insert(value_id);
    }
    return TRUE;
}

/* Add all live formal and local tensor values required by later lowering. */
static BOOL
VHO_FHE_Interface_Plan_Add_Value_Projections
        (ST_IDX root_owner,
         const std::map<VHO_FHE_FORMAL_KEY, const char *> &retired_formals,
         const std::map<VHO_FHE_FORMAL_KEY, STR_IDX> &live_formal_roles,
         const std::set<DSL_IR_VALUE_ID> &source_values,
         TY_IDX ciphertext_ty, TY_IDX plaintext_ty,
         std::map<VHO_FHE_VALUE_KEY, UINT32> *projection_indexes,
         FILE *diagnostic)
{
    for (UINT32 id = 1; id <= DSL_PU_Interface_Image_Formal_Count(); ++id) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(id, &formal)) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "CFHELOWER-SEM-002: PU formal row %u is missing\n",
                        id);
            return FALSE;
        }
        VHO_FHE_FORMAL_KEY key(formal.owner_pu_st, formal.formal_ordinal);
        if (retired_formals.find(key) != retired_formals.end())
            continue;
        UINT32 binding_kind = DSL_RUNTIME_BINDING_RESULT_FORMAL;
        TY_IDX handle_ty = ciphertext_ty;
        std::map<VHO_FHE_FORMAL_KEY, STR_IDX>::const_iterator role =
            live_formal_roles.find(key);
        if (role != live_formal_roles.end()) {
            binding_kind = DSL_RUNTIME_BINDING_INPUT_FORMAL;
            if (VHO_FHE_Interface_Plan_Is_Conv_Plain_Role(role->second))
                handle_ty = plaintext_ty;
        } else if (formal.owner_pu_st == root_owner) {
            DSL_FHE_ENTRY_VALUE_RECORD entry_value;
            UINT32 matches = 0;
            for (UINT32 value_id = 1;
                 value_id <= DSL_FHE_Entry_Value_Count(); ++value_id) {
                DSL_FHE_ENTRY_VALUE_RECORD candidate;
                if (DSL_FHE_Get_Entry_Value(value_id, &candidate) &&
                    candidate.value_id == formal.formal_value_id) {
                    entry_value = candidate;
                    ++matches;
                }
            }
            if (matches == 0) {
                UINT32 root_formal_count = 0;
                UINT32 output_matches = 0;
                for (UINT32 formal_id = 1;
                     formal_id <= DSL_PU_Interface_Image_Formal_Count();
                     ++formal_id) {
                    DSL_PU_FORMAL_RECORD root_formal;
                    if (!DSL_PU_Interface_Image_Get_Formal
                             (formal_id, &root_formal))
                        return FALSE;
                    if (root_formal.owner_pu_st == root_owner)
                        ++root_formal_count;
                }
                for (UINT32 value_id = 1;
                     value_id <= DSL_FHE_Entry_Value_Count(); ++value_id) {
                    DSL_FHE_ENTRY_VALUE_RECORD candidate;
                    DSL_IR_VALUE_RECORD output;
                    if (DSL_FHE_Get_Entry_Value(value_id, &candidate) &&
                        candidate.role == DSL_FHE_ENTRY_VALUE_OUTPUT &&
                        DSL_IR_Image_Get_Value(candidate.value_id, &output) &&
                        output.ty == formal.formal_ty)
                        ++output_matches;
                }
                if (formal.formal_ordinal + 1 == root_formal_count &&
                    output_matches == 1) {
                    binding_kind = DSL_RUNTIME_BINDING_RESULT_FORMAL;
                    matches = 1;
                    entry_value.role = DSL_FHE_ENTRY_VALUE_OUTPUT;
                }
            }
            if (matches != 1 ||
                (entry_value.role != DSL_FHE_ENTRY_VALUE_INPUT &&
                 entry_value.role != DSL_FHE_ENTRY_VALUE_OUTPUT)) {
                if (diagnostic != NULL)
                    fprintf(diagnostic,
                            "CFHELOWER-SEM-002: root formal %u value %u has "
                            "entry matches=%u role=%u\n",
                            formal.formal_ordinal, formal.formal_value_id,
                            matches, matches == 1 ? entry_value.role : 0);
                return FALSE;
            }
            binding_kind = entry_value.role == DSL_FHE_ENTRY_VALUE_INPUT ?
                DSL_RUNTIME_BINDING_INPUT_FORMAL :
                DSL_RUNTIME_BINDING_RESULT_FORMAL;
        }
        if (!VHO_FHE_Interface_Plan_Add_Value_Projection
                (formal.owner_pu_st, formal.formal_value_id, handle_ty,
                 binding_kind, formal.formal_ordinal, projection_indexes,
                 diagnostic))
            return FALSE;
    }

    for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
        if (!DSL_IR_Image_Get_Node(id, &node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                 (node.opcode_descriptor_id, &opcode)) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "CFHELOWER-SEM-002: executable node row %u is "
                        "malformed\n", id);
            return FALSE;
        }
        if ((node.flags & (DSL_IR_NODE_FLAG_RETIRED |
                           DSL_IR_NODE_FLAG_LOWERED)) != 0)
            continue;
        BOOL admitted = opcode.logical_operator == OPR_DSLTENSORCONST ||
            opcode.logical_operator == OPR_DSLCONV2D ||
            opcode.logical_operator == OPR_DSLRESIDUALADD ||
            opcode.logical_operator == OPR_DSLRELU ||
            opcode.logical_operator == OPR_DSLGLOBALAVGPOOL2D ||
            opcode.logical_operator == OPR_DSLFLATTEN ||
            opcode.logical_operator == OPR_DSLLINEAR ||
            opcode.logical_operator == OPR_DSLOUTPUTLOGITS;
        if (!admitted)
            continue;
        DSL_IR_VALUE_RECORD result;
        ST_IDX owner = ST_IDX_ZERO;
        if (!DSL_IR_Image_Get_Value(node.result_value_id, &result) ||
            !VHO_FHE_Interface_Plan_Value_Owner(result, &owner)) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "CFHELOWER-SEM-002: node %u result value %u has no "
                        "unique owner\n", node.id, node.result_value_id);
            return FALSE;
        }
        TY_IDX handle_ty = source_values.find(result.id) !=
            source_values.end() ? plaintext_ty : ciphertext_ty;
        if (!VHO_FHE_Interface_Plan_Add_Value_Projection
                (owner, result.id, handle_ty, DSL_RUNTIME_BINDING_LOCAL_VALUE,
                 DSL_RUNTIME_INTERFACE_INVALID_ORDINAL,
                 projection_indexes, diagnostic))
            return FALSE;
        for (UINT32 ordinal = 0; ordinal < node.operand_count; ++ordinal) {
            DSL_IR_VALUE_ID operand_id;
            DSL_IR_VALUE_RECORD operand;
            ST_IDX operand_owner = ST_IDX_ZERO;
            if (!VHO_FHE_Interface_Plan_Node_Operand
                    (node, ordinal, &operand_id) ||
                !DSL_IR_Image_Get_Value(operand_id, &operand) ||
                !VHO_FHE_Interface_Plan_Value_Owner
                    (operand, &operand_owner)) {
                if (diagnostic != NULL)
                    fprintf(diagnostic,
                            "CFHELOWER-SEM-002: node %u operand %u value %u "
                            "has no unique owner\n", node.id, ordinal,
                            operand_id);
                return FALSE;
            }
            TY_IDX operand_handle = source_values.find(operand_id) !=
                source_values.end() ? plaintext_ty : ciphertext_ty;
            VHO_FHE_VALUE_KEY operand_key(operand_owner, operand_id);
            if (projection_indexes->find(operand_key) ==
                    projection_indexes->end() &&
                !VHO_FHE_Interface_Plan_Add_Value_Projection
                    (operand_owner, operand_id, operand_handle,
                     DSL_RUNTIME_BINDING_LOCAL_VALUE,
                     DSL_RUNTIME_INTERFACE_INVALID_ORDINAL,
                     projection_indexes, diagnostic))
                return FALSE;
        }
    }
    return TRUE;
}

/* Add canonical live call projections and the explicit runtime input flow. */
static BOOL
VHO_FHE_Interface_Plan_Add_Call_Flow
        (ST_IDX root_owner,
         const std::set<DSL_IR_VALUE_ID> &source_values,
         TY_IDX ciphertext_ty, TY_IDX plaintext_ty,
         const std::map<DSL_IR_VALUE_ID, UINT32> &source_bindings,
         std::map<VHO_FHE_BINDING_KEY, UINT32> *binding_indexes,
         std::map<VHO_FHE_VALUE_KEY, UINT32> *projection_indexes,
         FILE *diagnostic)
{
    if (binding_indexes == NULL || projection_indexes == NULL)
        return FALSE;
    for (UINT32 id = 1; id <= DSL_Call_ABI_Image_Argument_Count(); ++id) {
        DSL_CALL_ARGUMENT_RECORD argument;
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_ABI_Image_Get_Argument(id, &argument) ||
            !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite))
            return FALSE;
        if (VHO_FHE_Interface_Plan_Is_Dead_BN_Role(argument.semantic_role))
            continue;
        TY_IDX handle_ty = VHO_FHE_Interface_Plan_Is_Conv_Plain_Role
                               (argument.semantic_role) ? plaintext_ty :
                               ciphertext_ty;
        VHO_FHE_VALUE_KEY value_key(callsite.owner_pu_st,
                                    argument.argument_value_id);
        if (projection_indexes->find(value_key) ==
                projection_indexes->end() &&
            !VHO_FHE_Interface_Plan_Add_Value_Projection
                (callsite.owner_pu_st, argument.argument_value_id, handle_ty,
                 DSL_RUNTIME_BINDING_LOCAL_VALUE,
                 DSL_RUNTIME_INTERFACE_INVALID_ORDINAL,
                 projection_indexes, diagnostic))
            return FALSE;
        DSL_RUNTIME_CALL_PROJECTION_REQUEST projection;
        memset(&projection, 0, sizeof(projection));
        projection.owner_pu_st = callsite.owner_pu_st;
        projection.callsite_id = callsite.id;
        projection.source_value_id = argument.argument_value_id;
        projection.actual_ordinal = argument.actual_ordinal;
        projection.callee_formal_ordinal = argument.callee_formal_ordinal;
        projection.direction = DSL_RUNTIME_CALL_INPUT;
        VHO_FHE_call_projections.push_back(projection);

        if (VHO_FHE_Interface_Plan_Is_Conv_Plain_Role
                (argument.semantic_role)) {
            std::map<DSL_IR_VALUE_ID, UINT32>::const_iterator caller =
                source_bindings.find(argument.argument_value_id);
            VHO_FHE_BINDING_KEY callee_key
                (callsite.callee_pu_st, Index_To_Str(argument.semantic_role));
            std::map<VHO_FHE_BINDING_KEY, UINT32>::const_iterator callee =
                binding_indexes->find(callee_key);
            if (caller == source_bindings.end() ||
                callee == binding_indexes->end())
                return FALSE;
            DSL_RUNTIME_INPUT_CALL_REQUEST runtime_call;
            memset(&runtime_call, 0, sizeof(runtime_call));
            runtime_call.callsite_id = callsite.id;
            runtime_call.caller_binding_index = caller->second;
            runtime_call.callee_binding_index = callee->second;
            VHO_FHE_runtime_input_calls.push_back(runtime_call);
        }
    }

    for (UINT32 callsite_id = 1;
         callsite_id <= DSL_Call_Image_Callsite_Count(); ++callsite_id) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(callsite_id, &callsite) ||
            callsite.owner_pu_st != root_owner)
            return FALSE;
        UINT32 result_ordinal = DSL_RUNTIME_INTERFACE_INVALID_ORDINAL;
        for (UINT32 formal_id = 1;
             formal_id <= DSL_PU_Interface_Image_Formal_Count(); ++formal_id) {
            DSL_PU_FORMAL_RECORD formal;
            if (!DSL_PU_Interface_Image_Get_Formal(formal_id, &formal) ||
                formal.owner_pu_st != callsite.callee_pu_st)
                continue;
            if (DSL_Call_ABI_Image_Callee_Formal_Count
                    (formal.owner_pu_st, formal.formal_ordinal) == 0) {
                if (result_ordinal != DSL_RUNTIME_INTERFACE_INVALID_ORDINAL)
                    return FALSE;
                result_ordinal = formal.formal_ordinal;
            }
        }
        DSL_IR_VALUE_ID result_value_id;
        if (result_ordinal == DSL_RUNTIME_INTERFACE_INVALID_ORDINAL ||
            !VHO_FHE_Interface_Plan_Call_Result_Value
                (root_owner, callsite.id, result_ordinal, &result_value_id))
            return FALSE;
        VHO_FHE_VALUE_KEY result_key(root_owner, result_value_id);
        if (projection_indexes->find(result_key) ==
                projection_indexes->end() &&
            !VHO_FHE_Interface_Plan_Add_Value_Projection
                (root_owner, result_value_id, ciphertext_ty,
                 DSL_RUNTIME_BINDING_LOCAL_VALUE,
                 DSL_RUNTIME_INTERFACE_INVALID_ORDINAL,
                 projection_indexes, diagnostic))
            return FALSE;
        DSL_RUNTIME_CALL_PROJECTION_REQUEST result_call;
        memset(&result_call, 0, sizeof(result_call));
        result_call.owner_pu_st = root_owner;
        result_call.callsite_id = callsite.id;
        result_call.source_value_id = result_value_id;
        result_call.actual_ordinal = result_ordinal;
        result_call.callee_formal_ordinal = result_ordinal;
        result_call.direction = DSL_RUNTIME_CALL_RESULT;
        VHO_FHE_call_projections.push_back(result_call);

        static const char *const resource_roles[4] = {
            "fhe.model",
            "fhe.relu.coefficient.stage0",
            "fhe.relu.coefficient.stage1",
            "fhe.relu.coefficient.stage2"
        };
        for (UINT32 role = 0; role < 4; ++role) {
            VHO_FHE_BINDING_KEY caller_key(root_owner, resource_roles[role]);
            VHO_FHE_BINDING_KEY callee_key(callsite.callee_pu_st,
                                           resource_roles[role]);
            std::map<VHO_FHE_BINDING_KEY, UINT32>::const_iterator caller =
                binding_indexes->find(caller_key);
            std::map<VHO_FHE_BINDING_KEY, UINT32>::const_iterator callee =
                binding_indexes->find(callee_key);
            if (caller == binding_indexes->end() ||
                callee == binding_indexes->end())
                return FALSE;
            DSL_RUNTIME_INPUT_CALL_REQUEST runtime_call;
            memset(&runtime_call, 0, sizeof(runtime_call));
            runtime_call.callsite_id = callsite.id;
            runtime_call.caller_binding_index = caller->second;
            runtime_call.callee_binding_index = callee->second;
            VHO_FHE_runtime_input_calls.push_back(runtime_call);
        }
    }
    return TRUE;
}

/* Add one threaded plaintext binding for every shared callee Conv slot. */
static BOOL
VHO_FHE_Interface_Plan_Add_Threaded_Plain_Bindings
        (const std::map<VHO_FHE_FORMAL_KEY, STR_IDX> &live_formal_roles,
         TY_IDX plaintext_ty, SRCPOS source_position,
         std::map<VHO_FHE_BINDING_KEY, UINT32> *binding_indexes)
{
    if (binding_indexes == NULL)
        return FALSE;
    for (std::map<VHO_FHE_FORMAL_KEY, STR_IDX>::const_iterator it =
             live_formal_roles.begin(); it != live_formal_roles.end(); ++it) {
        if (!VHO_FHE_Interface_Plan_Is_Conv_Plain_Role(it->second))
            continue;
        const char *role = VHO_FHE_Interface_Plan_Copy_Role
                               (Index_To_Str(it->second));
        if (role == NULL)
            return FALSE;
        DSL_RUNTIME_INPUT_BINDING_REQUEST binding;
        memset(&binding, 0, sizeof(binding));
        binding.owner_pu_st = it->first.first;
        binding.runtime_input_index =
            DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX;
        binding.handle_ty = plaintext_ty;
        binding.binding_kind = DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL;
        binding.semantic_role = role;
        binding.source_position = source_position;
        VHO_FHE_BINDING_KEY key(binding.owner_pu_st, role);
        if (binding_indexes->find(key) != binding_indexes->end())
            return FALSE;
        (*binding_indexes)[key] = VHO_FHE_runtime_input_bindings.size();
        VHO_FHE_runtime_input_bindings.push_back(binding);
    }
    return TRUE;
}

/* Construct and prevalidate one all-PU plan from the active root's evidence. */
BOOL
VHO_FHE_Runtime_Interface_Plans_Prepare
        (PU_Info *root_pu, FILE *diagnostic,
         VHO_FHE_RUNTIME_INTERFACE_PLAN_SUMMARY *summary)
{
    VHO_FHE_Runtime_Interface_Plans_Reset();
    if (summary != NULL)
        memset(summary, 0, sizeof(*summary));
    ST_IDX root_owner;
    SRCPOS source_position;
    if (summary == NULL ||
        !VHO_FHE_Interface_Plan_Active_Root
            (root_pu, &root_owner, &source_position))
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic, "unique root PU is not active");

    VHO_FHE_RUNTIME_INTERFACE_CENSUS census;
    if (!VHO_FHE_Runtime_Interface_Census_Prepare(diagnostic, &census))
        return FALSE;
    std::map<VHO_FHE_FORMAL_KEY, const char *> retired_formals;
    std::map<VHO_FHE_FORMAL_KEY, STR_IDX> live_formal_roles;
    std::set<DSL_IR_VALUE_ID> source_values;
    if (!VHO_FHE_Interface_Plan_Classify_Call_ABI
            (&retired_formals, &source_values, &live_formal_roles) ||
        !VHO_FHE_Interface_Plan_Add_Root_Plain_Sources(&source_values)) {
        VHO_FHE_Runtime_Interface_Plans_Reset();
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic, "could not classify the canonical interface");
    }

    TY_IDX model_ty = VHO_FHE_Interface_Plan_Opaque_Handle_TY
                          ("open64_fhe_model_v1_s");
    TY_IDX ciphertext_ty = VHO_FHE_Interface_Plan_Opaque_Handle_TY
                               ("open64_fhe_ciphertext_v1_s");
    TY_IDX plaintext_ty = VHO_FHE_Interface_Plan_Opaque_Handle_TY
                              ("open64_fhe_plain_tensor_v1_s");
    std::map<DSL_IR_VALUE_ID, UINT32> input_indexes;
    std::map<DSL_IR_VALUE_ID, UINT32> source_binding_indexes;
    std::map<VHO_FHE_BINDING_KEY, UINT32> binding_indexes;
    if (!VHO_FHE_Interface_Plan_Add_External_Inputs
            (root_owner, source_values, plaintext_ty, source_position,
             &input_indexes, &source_binding_indexes, diagnostic)) {
        VHO_FHE_Runtime_Interface_Plans_Reset();
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic, "external tensor input contract is invalid");
    }
    for (std::map<DSL_IR_VALUE_ID, UINT32>::const_iterator it =
             source_binding_indexes.begin();
         it != source_binding_indexes.end(); ++it) {
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &binding =
            VHO_FHE_runtime_input_bindings[it->second];
        binding_indexes[VHO_FHE_BINDING_KEY
                            (binding.owner_pu_st, binding.semantic_role)] =
            it->second;
    }
    if (!VHO_FHE_Interface_Plan_Add_Threaded_Plain_Bindings
            (live_formal_roles, plaintext_ty, source_position,
             &binding_indexes)) {
        VHO_FHE_Runtime_Interface_Plans_Reset();
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic,
                    "threaded plaintext binding flow is invalid");
    }
    if (!VHO_FHE_Interface_Plan_Add_Resources
            (root_owner, model_ty, plaintext_ty, source_position,
             &binding_indexes, diagnostic)) {
        VHO_FHE_Runtime_Interface_Plans_Reset();
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic, "model/coefficient resource flow is invalid");
    }

    std::map<VHO_FHE_VALUE_KEY, UINT32> projection_indexes;
    if (!VHO_FHE_Interface_Plan_Add_Value_Projections
            (root_owner, retired_formals, live_formal_roles, source_values,
             ciphertext_ty, plaintext_ty, &projection_indexes,
             diagnostic)) {
        VHO_FHE_Runtime_Interface_Plans_Reset();
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic, "runtime value projection flow is invalid");
    }
    if (!VHO_FHE_Interface_Plan_Add_Call_Flow
            (root_owner, source_values, ciphertext_ty, plaintext_ty,
             source_binding_indexes, &binding_indexes,
             &projection_indexes, diagnostic)) {
        VHO_FHE_Runtime_Interface_Plans_Reset();
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic, "runtime call projection flow is invalid");
    }

    VHO_FHE_Interface_Plan_Refresh_Views();
    if (VHO_FHE_program_plan.retired_formal_count !=
            census.retired_formal_count ||
        VHO_FHE_program_plan.retired_call_argument_count !=
            census.retired_call_argument_count ||
        VHO_FHE_program_plan.runtime_input_count !=
            census.source_external_input_count +
            census.runtime_resource_input_count ||
        VHO_FHE_program_plan.runtime_input_binding_count !=
            census.root_source_binding_count +
            census.threaded_source_binding_count +
            census.resource_binding_count ||
        VHO_FHE_program_plan.runtime_input_call_count !=
            census.runtime_input_call_count ||
        !DSL_Program_Interface_Plan_Validate
            (&VHO_FHE_program_plan, &VHO_FHE_runtime_plan, diagnostic)) {
        VHO_FHE_Runtime_Interface_Plans_Reset();
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic, "complete interface plan was rejected");
    }

    summary->retired_formal_count =
        VHO_FHE_program_plan.retired_formal_count;
    summary->retired_call_argument_count =
        VHO_FHE_program_plan.retired_call_argument_count;
    summary->runtime_input_count = VHO_FHE_program_plan.runtime_input_count;
    summary->runtime_input_binding_count =
        VHO_FHE_program_plan.runtime_input_binding_count;
    summary->runtime_input_call_count =
        VHO_FHE_program_plan.runtime_input_call_count;
    summary->value_projection_count = VHO_FHE_runtime_plan.value_count;
    summary->call_projection_count = VHO_FHE_runtime_plan.call_count;
    summary->fingerprint = VHO_FHE_Interface_Plan_Fingerprint();
    VHO_FHE_interface_plans_prepared = TRUE;
    return TRUE;
}

/* Borrow the validated arrays until the next Prepare or Reset. */
BOOL
VHO_FHE_Runtime_Interface_Plans_Get
        (DSL_PROGRAM_INTERFACE_PLAN *program_plan,
         DSL_RUNTIME_INTERFACE_PLAN *runtime_plan)
{
    if (!VHO_FHE_interface_plans_prepared || program_plan == NULL ||
        runtime_plan == NULL)
        return FALSE;
    *program_plan = VHO_FHE_program_plan;
    *runtime_plan = VHO_FHE_runtime_plan;
    return TRUE;
}

/* Apply one owner-qualified slice and verify its physical WN/ABI projection. */
BOOL
VHO_FHE_Runtime_Interface_Plans_Apply_PU
        (PU_Info *pu, FILE *diagnostic,
         DSL_PROGRAM_INTERFACE_RESULT *result)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
    if (!VHO_FHE_interface_plans_prepared ||
        VHO_FHE_interface_plans_failed || pu == NULL || result == NULL ||
        VHO_FHE_interface_applied_owners.find(PU_Info_proc_sym(pu)) !=
            VHO_FHE_interface_applied_owners.end())
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic, "PU interface apply order or owner is invalid");
    if (!DSL_Program_Interface_Apply_PU
             (pu, &VHO_FHE_program_plan, &VHO_FHE_runtime_plan,
              diagnostic, result) ||
        !DSL_Program_Interface_Validate_PU(pu, diagnostic)) {
        VHO_FHE_interface_plans_failed = TRUE;
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic, "PU interface application failed");
    }
    VHO_FHE_interface_applied_owners.insert(PU_Info_proc_sym(pu));
    return TRUE;
}

/* Check all-PU coverage and complete interface images before publication. */
BOOL
VHO_FHE_Runtime_Interface_Plans_Verify_Complete (FILE *diagnostic)
{
    if (!VHO_FHE_interface_plans_prepared ||
        VHO_FHE_interface_plans_failed ||
        VHO_FHE_interface_applied_owners.size() !=
            DSL_Call_Image_PU_Identity_Count() ||
        DSL_Program_Interface_Image_Retired_Formal_Count() !=
            VHO_FHE_program_plan.retired_formal_count ||
        DSL_Program_Interface_Image_Retired_Call_Count() !=
            VHO_FHE_program_plan.retired_call_argument_count ||
        DSL_Program_Interface_Image_Runtime_Input_Count() !=
            VHO_FHE_program_plan.runtime_input_count ||
        DSL_Program_Interface_Image_Runtime_Binding_Count() !=
            VHO_FHE_program_plan.runtime_input_binding_count ||
        DSL_Program_Interface_Image_Runtime_Call_Count() !=
            VHO_FHE_program_plan.runtime_input_call_count ||
        DSL_Runtime_Interface_Image_Value_Count() !=
            VHO_FHE_runtime_plan.value_count ||
        DSL_Runtime_Interface_Image_Call_Count() !=
            VHO_FHE_runtime_plan.call_count ||
        !DSL_Program_Interface_Image_Validate(diagnostic) ||
        !DSL_Runtime_Interface_Image_Validate(diagnostic))
        return VHO_FHE_Interface_Plan_Report
                   (diagnostic, "all-PU interface image is incomplete");
    return TRUE;
}
