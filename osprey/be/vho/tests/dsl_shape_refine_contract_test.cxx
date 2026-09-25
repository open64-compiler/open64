/*
 * Contract test for mandatory VHO DSL tensor shape refinement.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "stab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_gatekeeper.h"
#include "dsl_region.h"
#include "dsl_shape_refine.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

extern UINT32 DSL_Region_Symbol_Use_Count (PU_Info *, ST_IDX);

void
Signal_Cleanup(INT sig) {}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
    return "";
}

static void
Initialize_Test_Context(void)
{
    MEM_Initialize();
    Set_Error_Tables(Phases, host_errlist);
    Init_Error_Handler(10);
    Set_Error_File(NULL);
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    Preconfigure();
    Init_Controls_Tbl();
    ABI_Name = "n64";
    Configure();
    IR_reader_init();
    Initialize_Symbol_Tables(TRUE);
    DST_Init(NULL, 0);
}

static void
Initialize_Descriptor
        (DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor,
         const char *shape,
         const char *traits)
{
    memset(descriptor, 0, sizeof(*descriptor));
    descriptor->type_core.kind = "tensor";
    descriptor->type_core.dtype = "float32";
    descriptor->type_core.rank = 2;
    descriptor->type_core.logical_shape = shape;
    descriptor->traits.traits = traits;
    descriptor->representation.layout = "row_major";
    descriptor->representation.sharding = "replicated";
    descriptor->representation.placement = "host";
    descriptor->representation.memory = "contiguous";
    descriptor->representation.quantization = "none";
}

static TY_IDX
Create_Custom_Canonical_Tensor
        (const char *name,
         const char *shape,
         const char *shape_contract)
{
    TY_IDX ty = TY_Create_Tensor_Type(name, MTYPE_To_TY(MTYPE_F4), 2);
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_KIND, "tensor");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_DTYPE, "float32");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_RANK, "2");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_SHAPE, shape);
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_TRAITS,
                             "derived_activation");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_LAYOUT, "row_major");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_SHARDING, "replicated");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_PLACEMENT, "host");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_MEMORY, "contiguous");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_QUANTIZATION, "none");
    TY_tensor_bind_attribute(ty, "shape_contract", shape_contract);
    if (!TY_tensor_seal(ty))
        return TY_IDX_ZERO;
    return ty;
}

static int
Run_Custom_Identity_Retype_Test(BOOL qualifier_repro)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR input_descriptor;
    Initialize_Descriptor(&input_descriptor, "[2,3]", "activation");

    if (!DSL_Builder_Begin_Program())
        return 2;
    DSL_Opcode_Register_Common_Substrate();
    TY_IDX input_ty = DSL_Builder_Intern_Tensor_Type
                          ("identity_repro_input", MTYPE_To_TY(MTYPE_F4),
                           &input_descriptor);
    TY_IDX pending_ty = Create_Custom_Canonical_Tensor
                            ("identity_repro_tensor", "[2,<pending>]", "v1");
    TY_IDX wrong_refined_ty = Create_Custom_Canonical_Tensor
                                  ("identity_repro_tensor", "[2,3]",
                                   qualifier_repro ? "v1" : "v2");
    if (qualifier_repro)
        Set_TY_is_const(wrong_refined_ty);
    if (pending_ty == TY_IDX_ZERO || wrong_refined_ty == TY_IDX_ZERO)
        return 3;

    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("identity_repro");
    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("input", input_ty, 0);
    DSL_BUILDER_VALUE kids[2] = { input, input };
    DSL_BUILDER_VALUE add = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"),
                         DSL_OPCODE_COMMON_ADD, 1),
         1, kids, 2, &attribute, 1, "result", pending_ty);
    if (pu == NULL || input == NULL || add == NULL ||
        !DSL_Builder_Append_PU_Value(pu, input) ||
        !DSL_Builder_Append_PU_Value(pu, add) ||
        !DSL_Builder_Select_PU(pu))
        return 4;

    DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST request;
    request.owner_pu_st = PU_Info_proc_sym(pu);
    request.value_id = DSL_Builder_Get_Value_Image_Id(add);
    request.expected_old_ty = pending_ty;
    request.refined_ty = wrong_refined_ty;
    DSL_IR_VALUE_TYPE_REFINEMENT_RESULT result;
    memset(&result, 0, sizeof(result));
    BOOL accepted = DSL_IR_Refine_Native_Value_Types
                        (pu, PU_Info_tree_ptr(pu), &request, 1,
                         stderr, &result);
    ST_IDX result_st = DSL_Builder_Get_Value_Result_Symbol(add);
    DSL_IR_VALUE_RECORD value;
    BOOL unchanged = !accepted &&
                     WN_ty(add) == pending_ty &&
                     ST_type(St_Table[result_st]) == pending_ty &&
                     DSL_IR_Image_Get_Value(request.value_id, &value) &&
                     value.ty == pending_ty &&
                     result.updated_st_count == 0 &&
                     result.updated_wn_count == 0 &&
                     result.updated_value_count == 0 &&
                     result.rollback_count == 0;
    printf("%s accepted=%d writes=%u/%u/%u rollback_count=%u "
           "unchanged=%d old_contract=%s requested_contract=%s "
           "old_const=%d requested_const=%d\n",
           qualifier_repro ? "qualifier_repro" : "identity_repro",
           accepted, result.updated_st_count, result.updated_wn_count,
           result.updated_value_count, result.rollback_count, unchanged,
           TY_tensor_attribute(pending_ty, "shape_contract"),
           TY_tensor_attribute(wrong_refined_ty, "shape_contract"),
           TY_is_const(pending_ty) != 0,
           TY_is_const(wrong_refined_ty) != 0);
    return unchanged ? 0 : 5;
}

static BOOL
Value_Type_Is (DSL_BUILDER_VALUE value, TY_IDX expected)
{
    DSL_IR_VALUE_RECORD record;
    return value != NULL && WN_operator(value) == OPR_STID &&
           WN_ty(value) == expected &&
           ST_type(St_Table[WN_st_idx(value)]) == expected &&
           DSL_IR_Image_Get_Value
               (DSL_Builder_Get_Value_Image_Id(value), &record) &&
           record.ty == expected;
}

typedef enum {
    AUTH_CUSTOM_VALUE,
    AUTH_CUSTOM_ADDED,
    AUTH_CUSTOM_REMOVED,
    AUTH_DECLARED_TO_BOUND,
    AUTH_BOUND_TO_DECLARED,
    AUTH_DTYPE,
    AUTH_TRAITS,
    AUTH_LAYOUT,
    AUTH_SHARDING,
    AUTH_PLACEMENT,
    AUTH_MEMORY,
    AUTH_QUANTIZATION,
    AUTH_TY_SIZE,
    AUTH_TY_MTYPE,
    AUTH_TY_FLAGS,
    AUTH_NAME_IDX,
    AUTH_ALIGNMENT,
    AUTH_ELEMENT_TYPE,
    AUTH_SCHEMA_KIND,
    AUTH_CARRIER_KIND,
    AUTH_SEMANTIC_ROLE,
    AUTH_RUNTIME_STATE,
    AUTH_LINEAGE,
    AUTH_ENCRYPTION_REFERENCE,
    AUTH_UNKNOWN_VALUE,
    AUTH_RANK,
    AUTH_CONST_QUALIFIER,
    AUTH_VOLATILE_QUALIFIER,
    AUTH_RESTRICT_QUALIFIER,
    AUTH_USER_ALIGN_QUALIFIER,
    AUTH_FLAGS_EXT,
    AUTH_UNUSED_U1,
    AUTH_UNUSED_U2,
    AUTH_UNUSED_VTABLE,
    AUTH_INVALID_BIND_STATE,
    AUTH_VARIANT_COUNT
} AUTHORIZATION_VARIANT;

static const char *authorization_variant_name[AUTH_VARIANT_COUNT] = {
    "custom_value",
    "custom_added",
    "custom_removed",
    "declared_to_bound",
    "bound_to_declared",
    "dtype",
    "traits",
    "layout",
    "sharding",
    "placement",
    "memory",
    "quantization",
    "ty_size",
    "ty_mtype",
    "ty_flags",
    "name_idx",
    "alignment",
    "element_type",
    "schema_kind",
    "carrier_kind",
    "semantic_role",
    "runtime_state",
    "lineage",
    "encryption_reference",
    "unknown_value",
    "rank",
    "const_qualifier",
    "volatile_qualifier",
    "restrict_qualifier",
    "user_align_qualifier",
    "flags_ext",
    "unused_u1",
    "unused_u2",
    "unused_vtable",
    "invalid_bind_state"
};

static const char *
Authorization_Value
        (AUTHORIZATION_VARIANT variant,
         AUTHORIZATION_VARIANT field,
         BOOL refined,
         const char *stable,
         const char *changed)
{
    return variant == field && refined ? changed : stable;
}

static TY_IDX
Create_Authorization_Tensor
        (const char *name,
         const char *shape,
         AUTHORIZATION_VARIANT variant,
         BOOL refined)
{
    INT32 rank = variant == AUTH_RANK && refined ? 3 : 2;
    TY_IDX element_ty = variant == AUTH_ELEMENT_TYPE && refined ?
                            MTYPE_To_TY(MTYPE_F8) :
                            MTYPE_To_TY(MTYPE_F4);
    TY_IDX ty = variant == AUTH_CARRIER_KIND && refined ?
                    TY_Create_Tensor_Extension_Type(name, element_ty, rank) :
                    TY_Create_Tensor_Type(name, element_ty, rank);
    char rank_text[16];
    snprintf(rank_text, sizeof(rank_text), "%d", rank);

    TY_tensor_bind_attribute
        (ty, TY_TENSOR_SCHEMA_KIND,
         Authorization_Value(variant, AUTH_SCHEMA_KIND, refined,
                             "tensor", "foreign_tensor"));
    TY_tensor_bind_attribute
        (ty, TY_TENSOR_SCHEMA_DTYPE,
         Authorization_Value(variant, AUTH_DTYPE, refined,
                             "float32", "float64"));
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_RANK, rank_text);
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_SHAPE, shape);
    TY_tensor_bind_attribute
        (ty, TY_TENSOR_SCHEMA_TRAITS,
         Authorization_Value(variant, AUTH_TRAITS, refined,
                             "derived_activation", "foreign_trait"));
    TY_tensor_bind_attribute
        (ty, TY_TENSOR_SCHEMA_LAYOUT,
         Authorization_Value(variant, AUTH_LAYOUT, refined,
                             "row_major", "column_major"));
    TY_tensor_bind_attribute
        (ty, TY_TENSOR_SCHEMA_SHARDING,
         Authorization_Value(variant, AUTH_SHARDING, refined,
                             "replicated", "sharded"));
    TY_tensor_bind_attribute
        (ty, TY_TENSOR_SCHEMA_PLACEMENT,
         Authorization_Value(variant, AUTH_PLACEMENT, refined,
                             "host", "device"));
    TY_tensor_bind_attribute
        (ty, TY_TENSOR_SCHEMA_MEMORY,
         Authorization_Value(variant, AUTH_MEMORY, refined,
                             "contiguous", "strided"));
    TY_tensor_bind_attribute
        (ty, TY_TENSOR_SCHEMA_QUANTIZATION,
         Authorization_Value(variant, AUTH_QUANTIZATION, refined,
                             "none", "int8"));
    TY_tensor_bind_attribute
        (ty, TY_TENSOR_SCHEMA_RUNTIME_STATE,
         Authorization_Value(variant, AUTH_RUNTIME_STATE, refined,
                             "resident", "evicted"));
    TY_tensor_bind_attribute
        (ty, TY_TENSOR_SCHEMA_LINEAGE,
         Authorization_Value(variant, AUTH_LINEAGE, refined,
                             "lineage-a", "lineage-b"));

    if (variant != AUTH_CUSTOM_ADDED || refined)
        TY_tensor_bind_attribute
            (ty, "shape_contract",
             Authorization_Value(variant, AUTH_CUSTOM_VALUE, refined,
                                 "contract-v1", "contract-v2"));
    if (variant == AUTH_CUSTOM_REMOVED && !refined)
        TY_tensor_bind_attribute(ty, "removed_state", "present");
    if (variant == AUTH_DECLARED_TO_BOUND) {
        if (refined)
            TY_tensor_bind_attribute(ty, "binding_state", "bound");
        else
            TY_tensor_declare_attribute(ty, "binding_state");
    } else if (variant == AUTH_BOUND_TO_DECLARED) {
        if (refined)
            TY_tensor_declare_attribute(ty, "binding_state");
        else
            TY_tensor_bind_attribute(ty, "binding_state", "bound");
    }
    TY_tensor_bind_attribute
        (ty, "semantic_role",
         Authorization_Value(variant, AUTH_SEMANTIC_ROLE, refined,
                             "activation", "weight"));
    TY_tensor_bind_attribute
        (ty, "encryption_descriptor_ref",
         Authorization_Value(variant, AUTH_ENCRYPTION_REFERENCE, refined,
                             "none", "foreign-encryption-state"));
    TY_tensor_bind_attribute
        (ty, "future_unknown_state",
         Authorization_Value(variant, AUTH_UNKNOWN_VALUE, refined,
                             "future-v1", "future-v2"));

    if (!TY_tensor_seal(ty))
        return TY_IDX_ZERO;
    if (variant == AUTH_TY_SIZE && refined)
        Set_TY_size(ty, 1);
    if (variant == AUTH_TY_MTYPE && refined)
        Set_TY_mtype(ty, MTYPE_I4);
    if (variant == AUTH_TY_FLAGS && refined)
        Set_TY_is_character(ty);
    if (variant == AUTH_ALIGNMENT && refined)
        Set_TY_align(ty, 8);
    if (variant == AUTH_CONST_QUALIFIER && refined)
        Set_TY_is_const(ty);
    if (variant == AUTH_VOLATILE_QUALIFIER && refined)
        Set_TY_is_volatile(ty);
    if (variant == AUTH_RESTRICT_QUALIFIER && refined)
        Set_TY_is_restrict(ty);
    if (variant == AUTH_USER_ALIGN_QUALIFIER && refined)
        Set_TY_is_user_align(ty);
    if (variant == AUTH_FLAGS_EXT && refined)
        Set_TY_is_atomic(ty);
    if (variant == AUTH_UNUSED_U1 && refined)
        Ty_Table[ty].u1.fld = 1;
    if (variant == AUTH_UNUSED_U2 && refined)
        Ty_Table[ty].u2.etype = MTYPE_To_TY(MTYPE_F8);
    if (variant == AUTH_UNUSED_VTABLE && refined)
        Ty_Table[ty].vtable = make_ST_IDX(1, GLOBAL_SYMTAB);
    return ty;
}

static BOOL
Find_Tensor_Attribute_Entry
        (TY_IDX ty,
         const char *expected_key,
         TY_DSL_KV **found)
{
    if (found != NULL)
        *found = NULL;
    for (UINT32 index = 0; index < Ty_tensor_extensions.Size(); ++index) {
        TY_TENSOR_EXTENSION_STORE &ext = Ty_tensor_extensions[index];
        if (TY_IDX_index(ext.ty) != TY_IDX_index(ty))
            continue;
        for (UINT32 handle = ext.attribute_head; handle != 0;
             handle = Tensor_dsl_kv_table[handle - 1].next) {
            TY_DSL_KV *entry = &Tensor_dsl_kv_table[handle - 1];
            if (entry->key != 0 &&
                strcmp(&Str_Table[entry->key], expected_key) == 0) {
                if (found != NULL)
                    *found = entry;
                return TRUE;
            }
        }
    }
    return FALSE;
}

static BOOL
Run_Authorization_Rejection
        (const char *case_name,
         TY_IDX old_ty,
         TY_IDX refined_ty,
         BOOL corrupt_bind_state = FALSE)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR input_descriptor;
    Initialize_Descriptor(&input_descriptor, "[2,3]", "activation");
    if (!DSL_Builder_Begin_Program())
        return FALSE;
    DSL_Opcode_Register_Common_Substrate();
    TY_IDX input_ty = DSL_Builder_Intern_Tensor_Type
                          ("authorization_input", MTYPE_To_TY(MTYPE_F4),
                           &input_descriptor);
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU(case_name);
    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("input", input_ty, 0);
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    DSL_BUILDER_VALUE add_kids[2] = { input, input };
    DSL_BUILDER_VALUE add = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"),
                         DSL_OPCODE_COMMON_ADD, 1),
         1, add_kids, 2, &attribute, 1, "authorization_add", old_ty);
    DSL_BUILDER_VALUE relu_kids[1] = { add };
    DSL_BUILDER_VALUE relu = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"), "common.relu", 2),
         2, relu_kids, 1, NULL, 0, "authorization_relu", old_ty);
    DSL_BUILDER_REGION region = DSL_Builder_Create_Region
                                    (pu, NULL, "shape.authorization.v1", 1);
    if (pu == NULL || input == NULL || add == NULL || relu == NULL ||
        region == NULL || !DSL_Builder_Append_PU_Value(pu, input) ||
        !DSL_Builder_Append_Region_Value(region, add) ||
        !DSL_Builder_Append_Region_Value(region, relu) ||
        !DSL_Builder_Declare_Region_Value
             (region, add, DSL_REGION_VALUE_OUTPUT,
              1, DSL_REGION_INTERFACE_FLAG_NONE) ||
        !DSL_Builder_Declare_Region_Value
             (region, relu,
              DSL_REGION_VALUE_OUTPUT | DSL_REGION_VALUE_RESULT,
              0, DSL_REGION_INTERFACE_FLAG_NONE) ||
        !DSL_Builder_Append_PU_Region(pu, region) ||
        !DSL_Builder_Select_PU(pu) ||
        !DSL_Region_Verify_PU(pu, stderr))
        return FALSE;

    ST_IDX add_st = DSL_Builder_Get_Value_Result_Symbol(add);
    ST_tensor_bind_metadata(add_st, "wp1_context", "preserved");
    WN *direct_ldid = WN_kid0(WN_kid0(relu));
    BOOL current_before = VHO_DSL_Shape_Refinement_Is_Current
                              (pu, PU_Info_tree_ptr(pu), NULL);
    DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST request;
    request.owner_pu_st = PU_Info_proc_sym(pu);
    request.value_id = DSL_Builder_Get_Value_Image_Id(add);
    request.expected_old_ty = old_ty;
    request.refined_ty = refined_ty;
    DSL_IR_VALUE_TYPE_REFINEMENT_RESULT result;
    memset(&result, 0xff, sizeof(result));
    TY_DSL_KV *corrupted_entry = NULL;
    mUINT32 saved_state = TY_DSL_BIND_PENDING;
    if (corrupt_bind_state) {
        if (!Find_Tensor_Attribute_Entry
                 (refined_ty, "future_unknown_state", &corrupted_entry))
            return FALSE;
        saved_state = corrupted_entry->state;
        corrupted_entry->state = 2;
    }
    BOOL accepted = DSL_IR_Refine_Native_Value_Types
                        (pu, PU_Info_tree_ptr(pu), &request, 1,
                         stderr, &result);
    if (corrupted_entry != NULL)
        corrupted_entry->state = saved_state;
    BOOL current_after = VHO_DSL_Shape_Refinement_Is_Current
                             (pu, PU_Info_tree_ptr(pu), NULL);
    BOOL unchanged = !accepted &&
                     result.request_count == 0 &&
                     result.updated_st_count == 0 &&
                     result.updated_wn_count == 0 &&
                     result.updated_value_count == 0 &&
                     result.rollback_count == 0 &&
                     Value_Type_Is(add, old_ty) &&
                     Value_Type_Is(relu, old_ty) &&
                     direct_ldid != NULL && WN_ty(direct_ldid) == old_ty &&
                     strcmp(ST_tensor_metadata(add_st, "wp1_context"),
                            "preserved") == 0 &&
                     DSL_Region_Verify_PU(pu, stderr) &&
                     current_before == current_after;
    printf("authorization_case=%s accepted=%d requests=%u "
           "writes=%u/%u/%u rollback=%u unchanged=%d "
           "ldid=%d region=%d current_equal=%d\n",
           case_name, accepted, result.request_count,
           result.updated_st_count, result.updated_wn_count,
           result.updated_value_count, result.rollback_count, unchanged,
           direct_ldid != NULL && WN_ty(direct_ldid) == old_ty,
           DSL_Region_Verify_PU(pu, NULL),
           current_before == current_after);
    return unchanged;
}

static int
Run_Authorization_Matrix(void)
{
    UINT32 passed = 0;
    const char *selected = getenv("OPEN64_DSL_SHAPE_AUTH_CASE");
    for (UINT32 ordinal = 0; ordinal < AUTH_VARIANT_COUNT; ++ordinal) {
        AUTHORIZATION_VARIANT variant =
            (AUTHORIZATION_VARIANT)ordinal;
        if (selected != NULL &&
            strcmp(selected, authorization_variant_name[ordinal]) != 0)
            continue;
        char old_name[96];
        char refined_name[96];
        char shared_name[96];
        snprintf(old_name, sizeof(old_name), "wp1_%s_old",
                 authorization_variant_name[ordinal]);
        snprintf(refined_name, sizeof(refined_name), "wp1_%s_refined",
                 authorization_variant_name[ordinal]);
        snprintf(shared_name, sizeof(shared_name), "wp1_%s_tensor",
                 authorization_variant_name[ordinal]);
        TY_IDX old_ty = Create_Authorization_Tensor
                            (variant == AUTH_NAME_IDX ? old_name : shared_name,
                             "[2,<pending>]", variant, FALSE);
        TY_IDX refined_ty = Create_Authorization_Tensor
                                (variant == AUTH_NAME_IDX ?
                                     refined_name : shared_name,
                                 variant == AUTH_RANK ? "[2,3,1]" : "[2,3]",
                                 variant, TRUE);
        TY_IDX baseline_refined_ty = Create_Authorization_Tensor
                                         (variant == AUTH_NAME_IDX ?
                                              old_name : shared_name,
                                          "[2,3]", variant, FALSE);
        BOOL baseline_helper =
            TY_tensor_preserves_non_shape_state
                (old_ty, baseline_refined_ty);
        BOOL target_helper =
            TY_tensor_preserves_non_shape_state(old_ty, refined_ty);
        printf("authorization_isolation case=%s baseline_helper=%d "
               "target_helper=%d\n",
               authorization_variant_name[ordinal], baseline_helper,
               target_helper);
        if (old_ty == TY_IDX_ZERO || refined_ty == TY_IDX_ZERO ||
            baseline_refined_ty == TY_IDX_ZERO || !baseline_helper ||
            (variant != AUTH_INVALID_BIND_STATE && target_helper) ||
            !Run_Authorization_Rejection
                 (authorization_variant_name[ordinal], old_ty, refined_ty,
                  variant == AUTH_INVALID_BIND_STATE)) {
            fprintf(stderr, "WP1 authorization case failed: %s\n",
                    authorization_variant_name[ordinal]);
            return 10 + ordinal;
        }
        ++passed;
    }
    printf("WP1 authorization negative matrix passed: cases=%u "
           "preflight_zero_write=1 ldid_unchanged=1 region_valid=1\n",
           passed);
    return selected == NULL || passed == 1 ? 0 : 9;
}

static TY_IDX
Create_Preservation_Tensor
        (const char *name,
         const char *shape,
         const char *contract,
         const char *runtime_state,
         const char *lineage)
{
    TY_IDX ty = TY_Create_Tensor_Type(name, MTYPE_To_TY(MTYPE_F4), 2);
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_KIND, "tensor");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_DTYPE, "float32");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_RANK, "2");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_SHAPE, shape);
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_TRAITS,
                             "derived_activation");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_LAYOUT, "row_major");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_SHARDING, "replicated");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_PLACEMENT, "host");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_MEMORY, "contiguous");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_QUANTIZATION, "none");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_RUNTIME_STATE,
                             runtime_state);
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_LINEAGE, lineage);
    TY_tensor_bind_attribute(ty, "shape_contract", contract);
    TY_tensor_bind_attribute(ty, "semantic_role", "activation");
    TY_tensor_bind_attribute(ty, "encryption_descriptor_ref", "none");
    TY_tensor_bind_attribute(ty, "future_unknown_state", "future-v1");
    TY_tensor_declare_attribute(ty, "pending_auxiliary_state");
    return TY_tensor_seal(ty) ? ty : TY_IDX_ZERO;
}

static BOOL
Tensor_Attribute_Has_State
        (TY_IDX ty,
         const char *expected_key,
         const char *expected_value,
         TY_DSL_BIND_STATE expected_state)
{
    for (UINT32 ordinal = 0;
         ordinal < TY_tensor_attribute_count(ty); ++ordinal) {
        const char *key = NULL;
        const char *value = NULL;
        TY_DSL_BIND_STATE state = TY_DSL_BIND_PENDING;
        if (!TY_tensor_attribute_at(ty, ordinal, &key, &value, &state) ||
            key == NULL || strcmp(key, expected_key) != 0)
            continue;
        if (state != expected_state)
            return FALSE;
        return expected_state == TY_DSL_BIND_PENDING ? value == NULL :
               value != NULL && expected_value != NULL &&
               strcmp(value, expected_value) == 0;
    }
    return FALSE;
}

static BOOL
Run_Authorization_Success
        (TY_IDX old_ty,
         TY_IDX refined_ty)
{
    if (!DSL_Builder_Begin_Program())
        return FALSE;
    DSL_Opcode_Register_Common_Substrate();
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("authorization_positive");
    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("input", refined_ty, 0);
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    DSL_BUILDER_VALUE add_kids[2] = { input, input };
    DSL_BUILDER_VALUE add = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"),
                         DSL_OPCODE_COMMON_ADD, 1),
         1, add_kids, 2, &attribute, 1, "positive_add", old_ty);
    DSL_BUILDER_VALUE relu_kids[1] = { add };
    DSL_BUILDER_VALUE relu = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"), "common.relu", 2),
         2, relu_kids, 1, NULL, 0, "positive_relu", old_ty);
    DSL_BUILDER_REGION region = DSL_Builder_Create_Region
                                    (pu, NULL, "shape.positive.v1", 1);
    ST_IDX unrelated = DSL_Builder_Create_Tensor_Result_Symbol
                           ("positive_unrelated", old_ty,
                            SCLASS_AUTO, EXPORT_LOCAL);
    if (pu == NULL || input == NULL || add == NULL || relu == NULL ||
        region == NULL || ST_IDX_index(unrelated) == 0 ||
        !DSL_Builder_Append_PU_Value(pu, input) ||
        !DSL_Builder_Append_Region_Value(region, add) ||
        !DSL_Builder_Append_Region_Value(region, relu) ||
        !DSL_Builder_Declare_Region_Value
             (region, add, DSL_REGION_VALUE_OUTPUT,
              1, DSL_REGION_INTERFACE_FLAG_NONE) ||
        !DSL_Builder_Declare_Region_Value
             (region, relu,
              DSL_REGION_VALUE_OUTPUT | DSL_REGION_VALUE_RESULT,
              0, DSL_REGION_INTERFACE_FLAG_NONE) ||
        !DSL_Builder_Append_PU_Region(pu, region) ||
        !DSL_Builder_Select_PU(pu))
        return FALSE;

    ST_IDX add_st = DSL_Builder_Get_Value_Result_Symbol(add);
    ST_tensor_bind_metadata(add_st, "wp1_context", "preserved");
    DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST requests[2];
    requests[0].owner_pu_st = PU_Info_proc_sym(pu);
    requests[0].value_id = DSL_Builder_Get_Value_Image_Id(add);
    requests[0].expected_old_ty = old_ty;
    requests[0].refined_ty = refined_ty;
    requests[1].owner_pu_st = PU_Info_proc_sym(pu);
    requests[1].value_id = DSL_Builder_Get_Value_Image_Id(relu);
    requests[1].expected_old_ty = old_ty;
    requests[1].refined_ty = refined_ty;
    DSL_IR_VALUE_TYPE_REFINEMENT_RESULT result;
    memset(&result, 0, sizeof(result));
    UINT32 type_count_before = TY_Table_Size();
    BOOL accepted = DSL_IR_Refine_Native_Value_Types
                        (pu, PU_Info_tree_ptr(pu), requests, 2,
                         stderr, &result);
    WN *direct_ldid = WN_kid0(WN_kid0(relu));
    BOOL valid = accepted && result.request_count == 2 &&
                 result.updated_st_count == 2 &&
                 result.updated_wn_count == 3 &&
                 result.updated_value_count == 2 &&
                 result.rollback_count == 0 &&
                 Value_Type_Is(add, refined_ty) &&
                 Value_Type_Is(relu, refined_ty) &&
                 direct_ldid != NULL && WN_ty(direct_ldid) == refined_ty &&
                 ST_type(St_Table[unrelated]) == old_ty &&
                 strcmp(ST_tensor_metadata(add_st, "wp1_context"),
                        "preserved") == 0 &&
                 DSL_Region_Verify_PU(pu, stderr) &&
                 TY_Table_Size() == type_count_before;
    printf("authorization_positive accepted=%d requests=%u "
           "writes=%u/%u/%u rollback=%u valid=%d ldid=%d "
           "unrelated=%d metadata=%d ty_growth=%u\n",
           accepted, result.request_count, result.updated_st_count,
           result.updated_wn_count, result.updated_value_count,
           result.rollback_count, valid,
           direct_ldid != NULL && WN_ty(direct_ldid) == refined_ty,
           ST_type(St_Table[unrelated]) == old_ty,
           strcmp(ST_tensor_metadata(add_st, "wp1_context"),
                  "preserved") == 0,
           TY_Table_Size() - type_count_before);
    return valid;
}

typedef enum {
    PHYSICAL_PACKED = 0x01,
    PHYSICAL_ATOMIC = 0x02,
    PHYSICAL_CONST = 0x04,
    PHYSICAL_VOLATILE = 0x08,
    PHYSICAL_RESTRICT = 0x10,
    PHYSICAL_USER_ALIGN = 0x20,
    PHYSICAL_ALIGN_8 = 0x40
} PHYSICAL_STATE_MASK;

typedef struct {
    const char *name;
    UINT32 mask;
} PHYSICAL_STATE_CASE;

static const PHYSICAL_STATE_CASE physical_state_cases[] = {
    { "packed", PHYSICAL_PACKED },
    { "atomic", PHYSICAL_ATOMIC },
    { "const", PHYSICAL_CONST },
    { "volatile", PHYSICAL_VOLATILE },
    { "restrict", PHYSICAL_RESTRICT },
    { "user_align", PHYSICAL_USER_ALIGN },
    { "align_8", PHYSICAL_ALIGN_8 },
    { "combined", PHYSICAL_PACKED | PHYSICAL_ATOMIC |
                  PHYSICAL_CONST | PHYSICAL_VOLATILE |
                  PHYSICAL_RESTRICT | PHYSICAL_USER_ALIGN |
                  PHYSICAL_ALIGN_8 }
};

static void
Apply_Physical_State (TY_IDX *ty, UINT32 mask)
{
    if ((mask & PHYSICAL_PACKED) != 0)
        Set_TY_is_packed(*ty);
    if ((mask & PHYSICAL_ATOMIC) != 0)
        Set_TY_is_atomic(*ty);
    if ((mask & PHYSICAL_CONST) != 0)
        Set_TY_is_const(*ty);
    if ((mask & PHYSICAL_VOLATILE) != 0)
        Set_TY_is_volatile(*ty);
    if ((mask & PHYSICAL_RESTRICT) != 0)
        Set_TY_is_restrict(*ty);
    if ((mask & PHYSICAL_USER_ALIGN) != 0)
        Set_TY_is_user_align(*ty);
    if ((mask & PHYSICAL_ALIGN_8) != 0)
        Set_TY_align(*ty, 8);
}

static BOOL
Physical_State_Is_Preserved (TY_IDX base, TY_IDX refined)
{
    return TY_flags(base) == TY_flags(refined) &&
           Ty_Table[base].flags_ext == Ty_Table[refined].flags_ext &&
           TY_name_idx(base) == TY_name_idx(refined) &&
           TY_align_exp(base) == TY_align_exp(refined) &&
           TY_is_user_align(base) == TY_is_user_align(refined) &&
           TY_is_const(base) == TY_is_const(refined) &&
           TY_is_volatile(base) == TY_is_volatile(refined) &&
           TY_is_restrict(base) == TY_is_restrict(refined) &&
           Ty_Table[refined].u1.fld == 0 &&
           Ty_Table[refined].u2.etype == TY_IDX_ZERO &&
           Ty_Table[refined].vtable == ST_IDX_ZERO &&
           TY_tensor_preserves_non_shape_state(base, refined);
}

static BOOL
Run_Physical_Interner_Case (const PHYSICAL_STATE_CASE *physical)
{
    char tensor_name[96];
    char contract[96];
    snprintf(tensor_name, sizeof(tensor_name),
             "wp1_physical_%s_tensor", physical->name);
    snprintf(contract, sizeof(contract),
             "wp1-physical-%s-contract", physical->name);
    TY_IDX base = Create_Preservation_Tensor
                      (tensor_name, "[2,<pending>]", contract,
                       "resident-physical", "lineage-physical");
    Apply_Physical_State(&base, physical->mask);
    TY_TENSOR_TYPE_CORE_REFINEMENT refinement;
    refinement.rank = 2;
    refinement.logical_shape = "[2,3]";
    UINT32 before_create = TY_Table_Size();
    BOOL created = FALSE;
    TY_IDX refined = TY_Intern_Refined_Tensor_Type
                         (base, &refinement, &created);
    UINT32 after_create = TY_Table_Size();
    TY_Rebuild_Tensor_Type_Interner();
    BOOL reused_created = TRUE;
    TY_IDX reused = TY_Intern_Refined_Tensor_Type
                        (base, &refinement, &reused_created);
    UINT32 after_reuse = TY_Table_Size();
    TY_TENSOR_EXTENSION_INFO stable_info;
    BOOL stable_handle = TY_Get_Tensor_Extension_Info
                             (refined, &stable_info) &&
                         TY_IDX_index(stable_info.ty) ==
                             TY_IDX_index(refined) &&
                         TY_align(stable_info.ty) == 4 &&
                         !TY_is_const(stable_info.ty) &&
                         !TY_is_volatile(stable_info.ty) &&
                         !TY_is_restrict(stable_info.ty) &&
                         !TY_is_user_align(stable_info.ty);
    BOOL state_preserved = base != TY_IDX_ZERO &&
                           refined != TY_IDX_ZERO && created &&
                           after_create == before_create + 1 &&
                           reused == refined && !reused_created &&
                           after_reuse == after_create &&
                           stable_handle &&
                           Physical_State_Is_Preserved(base, refined);
    BOOL transaction = state_preserved &&
                       Run_Authorization_Success(base, refined);
    printf("interner_physical case=%s created=%d reused=%d "
           "ty_growth=%u state_preserved=%d transaction=%d "
           "flags=%u flags_ext=%u align=%u qualifiers=%d/%d/%d/%d "
           "name=%d zero_storage=%d stable_handle=%d\n",
           physical->name, created, reused == refined,
           after_create - before_create, state_preserved, transaction,
           (UINT32)TY_flags(refined),
           (UINT32)Ty_Table[refined].flags_ext, TY_align(refined),
           TY_is_const(refined) != 0, TY_is_volatile(refined) != 0,
           TY_is_restrict(refined) != 0,
           TY_is_user_align(refined) != 0,
           TY_name_idx(base) == TY_name_idx(refined),
           Ty_Table[refined].u1.fld == 0 &&
           Ty_Table[refined].u2.etype == TY_IDX_ZERO &&
           Ty_Table[refined].vtable == ST_IDX_ZERO,
           stable_handle);
    return transaction;
}

static BOOL
Run_Physical_Handle_Variant_Case(void)
{
    TY_IDX raw_base = Create_Preservation_Tensor
                          ("wp1_handle_variant_tensor", "[2,<pending>]",
                           "wp1-handle-variant-contract",
                           "resident-handle", "lineage-handle");
    Set_TY_is_packed(raw_base);
    Set_TY_is_atomic(raw_base);
    TY_IDX const_base = raw_base;
    Set_TY_is_const(const_base);
    Set_TY_align(const_base, 8);
    TY_IDX volatile_base = raw_base;
    Set_TY_is_volatile(volatile_base);
    Set_TY_is_user_align(volatile_base);

    TY_TENSOR_TYPE_CORE_REFINEMENT refinement;
    refinement.rank = 2;
    refinement.logical_shape = "[2,3]";
    UINT32 before_create = TY_Table_Size();
    BOOL const_created = FALSE;
    TY_IDX const_refined = TY_Intern_Refined_Tensor_Type
                               (const_base, &refinement, &const_created);
    UINT32 after_create = TY_Table_Size();
    TY_Rebuild_Tensor_Type_Interner();
    BOOL volatile_created = TRUE;
    TY_IDX volatile_refined = TY_Intern_Refined_Tensor_Type
                                  (volatile_base, &refinement,
                                   &volatile_created);
    UINT32 after_reuse = TY_Table_Size();
    TY_TENSOR_EXTENSION_INFO stable_info;
    BOOL stable_handle = TY_Get_Tensor_Extension_Info
                             (const_refined, &stable_info) &&
                         TY_IDX_index(stable_info.ty) ==
                             TY_IDX_index(const_refined) &&
                         TY_align(stable_info.ty) == 4 &&
                         !TY_is_const(stable_info.ty) &&
                         !TY_is_volatile(stable_info.ty) &&
                         !TY_is_restrict(stable_info.ty) &&
                         !TY_is_user_align(stable_info.ty);
    BOOL isolated_handles = const_created && !volatile_created &&
                            after_create == before_create + 1 &&
                            after_reuse == after_create &&
                            stable_handle &&
                            TY_IDX_index(const_refined) ==
                                TY_IDX_index(volatile_refined) &&
                            Physical_State_Is_Preserved
                                (const_base, const_refined) &&
                            Physical_State_Is_Preserved
                                (volatile_base, volatile_refined) &&
                            TY_is_const(const_refined) &&
                            !TY_is_volatile(const_refined) &&
                            TY_align(const_refined) == 8 &&
                            !TY_is_const(volatile_refined) &&
                            TY_is_volatile(volatile_refined) &&
                            TY_is_user_align(volatile_refined) &&
                            TY_align(volatile_refined) == 4;
    BOOL const_transaction = isolated_handles &&
        Run_Authorization_Success(const_base, const_refined);
    BOOL volatile_transaction = isolated_handles &&
        Run_Authorization_Success(volatile_base, volatile_refined);
    printf("interner_handle_variants created=%d reused=%d ty_growth=%u "
           "same_index=%d isolated=%d stable_handle=%d transactions=%d/%d "
           "const_align=%u volatile_align=%u\n",
           const_created, !volatile_created,
           after_create - before_create,
           TY_IDX_index(const_refined) == TY_IDX_index(volatile_refined),
           isolated_handles, stable_handle,
           const_transaction, volatile_transaction,
           TY_align(const_refined), TY_align(volatile_refined));
    return const_transaction && volatile_transaction;
}

typedef enum {
    PREEXISTING_NAME_MISMATCH,
    PREEXISTING_FLAGS_MISMATCH,
    PREEXISTING_FLAGS_EXT_MISMATCH,
    PREEXISTING_METADATA_MISMATCH,
    PREEXISTING_MISMATCH_COUNT
} PREEXISTING_MISMATCH_KIND;

static const char *preexisting_mismatch_name[PREEXISTING_MISMATCH_COUNT] = {
    "name",
    "flags",
    "flags_ext",
    "runtime_lineage"
};

static BOOL
Run_Preexisting_Mismatch_Case (PREEXISTING_MISMATCH_KIND kind)
{
    char shared_name[96];
    char candidate_name[96];
    char contract[96];
    snprintf(shared_name, sizeof(shared_name),
             "wp1_preexisting_%s_tensor", preexisting_mismatch_name[kind]);
    snprintf(candidate_name, sizeof(candidate_name),
             "wp1_preexisting_%s_candidate",
             preexisting_mismatch_name[kind]);
    snprintf(contract, sizeof(contract),
             "wp1-preexisting-%s-contract",
             preexisting_mismatch_name[kind]);
    TY_IDX base = Create_Preservation_Tensor
                      (shared_name, "[2,<pending>]", contract,
                       "resident-a", "lineage-a");
    TY_IDX candidate = Create_Preservation_Tensor
                           (kind == PREEXISTING_NAME_MISMATCH ?
                                candidate_name : shared_name,
                            "[2,3]", contract,
                            kind == PREEXISTING_METADATA_MISMATCH ?
                                "resident-b" : "resident-a",
                            kind == PREEXISTING_METADATA_MISMATCH ?
                                "lineage-b" : "lineage-a");
    if (kind == PREEXISTING_FLAGS_MISMATCH)
        Set_TY_is_packed(candidate);
    if (kind == PREEXISTING_FLAGS_EXT_MISMATCH)
        Set_TY_is_atomic(candidate);
    TY_Rebuild_Tensor_Type_Interner();

    TY_TENSOR_TYPE_CORE_REFINEMENT refinement;
    refinement.rank = 2;
    refinement.logical_shape = "[2,3]";
    UINT32 before_reuse = TY_Table_Size();
    BOOL created = TRUE;
    TY_IDX reused = TY_Intern_Refined_Tensor_Type
                        (base, &refinement, &created);
    UINT32 after_reuse = TY_Table_Size();
    BOOL owner_reuse = !created &&
                       TY_IDX_index(reused) == TY_IDX_index(candidate) &&
                       after_reuse == before_reuse &&
                       TY_tensor_attributes_are_equivalent
                           (reused, candidate) &&
                       !TY_tensor_preserves_non_shape_state(base, reused);
    BOOL rejected = owner_reuse &&
        Run_Authorization_Rejection
            (preexisting_mismatch_name[kind], base, reused);
    printf("interner_preexisting case=%s reused=%d ty_growth=%u "
           "authorization_preserves=%d rejected=%d\n",
           preexisting_mismatch_name[kind],
           TY_IDX_index(reused) == TY_IDX_index(candidate),
           after_reuse - before_reuse,
           TY_tensor_preserves_non_shape_state(base, reused), rejected);
    return rejected;
}

static int
Run_Interner_Authorization_Matrix(void)
{
    const char *selected_physical =
        getenv("OPEN64_DSL_SHAPE_INTERNER_PHYSICAL_CASE");
    if (selected_physical != NULL) {
        if (strcmp(selected_physical, "handle_variants") == 0)
            return Run_Physical_Handle_Variant_Case() ? 0 : 51;
        for (UINT32 i = 0;
             i < sizeof(physical_state_cases) /
                 sizeof(physical_state_cases[0]); ++i) {
            if (strcmp(selected_physical,
                       physical_state_cases[i].name) == 0)
                return Run_Physical_Interner_Case
                           (&physical_state_cases[i]) ? 0 : 43 + i;
        }
        return 42;
    }

    TY_IDX base = Create_Preservation_Tensor
                      ("wp1_preservation_base", "[2,<pending>]",
                       "wp1-preservation-contract", "resident",
                       "lineage-positive");
    TY_TENSOR_TYPE_CORE_REFINEMENT refinement;
    refinement.rank = 2;
    refinement.logical_shape = "[2,3]";
    UINT32 before_create = TY_Table_Size();
    BOOL created = FALSE;
    TY_IDX refined = TY_Intern_Refined_Tensor_Type
                         (base, &refinement, &created);
    UINT32 after_create = TY_Table_Size();
    BOOL reused_created = TRUE;
    TY_IDX reused = TY_Intern_Refined_Tensor_Type
                        (base, &refinement, &reused_created);
    UINT32 after_reuse = TY_Table_Size();
    BOOL preservation = base != TY_IDX_ZERO && refined != TY_IDX_ZERO &&
                        created && after_create == before_create + 1 &&
                        reused == refined && !reused_created &&
                        after_reuse == after_create &&
                        TY_tensor_preserves_non_shape_state(base, refined) &&
                        strcmp(TY_tensor_attribute
                                   (refined,
                                    TY_TENSOR_SCHEMA_RUNTIME_STATE),
                               "resident") == 0 &&
                        strcmp(TY_tensor_attribute
                                   (refined, TY_TENSOR_SCHEMA_LINEAGE),
                               "lineage-positive") == 0 &&
                        Tensor_Attribute_Has_State
                            (refined, "pending_auxiliary_state", NULL,
                             TY_DSL_BIND_PENDING);
    if (!preservation || !Run_Authorization_Success(base, refined)) {
        fprintf(stderr, "WP1 interner preservation case failed\n");
        return 40;
    }

    for (UINT32 i = 0;
         i < sizeof(physical_state_cases) /
             sizeof(physical_state_cases[0]); ++i) {
        if (!Run_Physical_Interner_Case(&physical_state_cases[i])) {
            fprintf(stderr, "WP1 physical interner case failed: %s\n",
                    physical_state_cases[i].name);
            return 42 + i;
        }
    }
    if (!Run_Physical_Handle_Variant_Case()) {
        fprintf(stderr, "WP1 physical handle variant case failed\n");
        return 51;
    }

    for (UINT32 i = 0; i < PREEXISTING_MISMATCH_COUNT; ++i) {
        if (!Run_Preexisting_Mismatch_Case
                 ((PREEXISTING_MISMATCH_KIND)i)) {
            fprintf(stderr, "WP1 preexisting mismatch case failed: %s\n",
                    preexisting_mismatch_name[i]);
            return 60 + i;
        }
    }

    printf("WP1 interner authorization matrix passed: created=1 "
           "reused=1 ty_growth=1 metadata_preserved=1 "
           "physical_cases=9 preexisting_mismatches=4\n");
    return 0;
}

static BOOL
Shape_Trigger_Names_Are_Stable (void)
{
    static const char *expected[VHO_DSL_SHAPE_TRIGGER_COUNT] = {
        "none",
        "pu_admission",
        "pu_identity_change",
        "seed_refinement",
        "operator_constraint_change",
        "value_relationship_change",
        "structural_transformation",
        "pu_region_restructuring",
        "symbolic_resolution",
        "dsl_wopt",
        "fhe_conversion",
        "vho_dsl_optimization"
    };
    for (UINT32 ordinal = 0;
         ordinal < VHO_DSL_SHAPE_TRIGGER_COUNT; ++ordinal) {
        if (strcmp(VHO_DSL_Shape_Trigger_Name
                       ((VHO_DSL_SHAPE_TRIGGER)ordinal),
                   expected[ordinal]) != 0)
            return FALSE;
    }
    return strcmp(VHO_DSL_Shape_Trigger_Name
                      ((VHO_DSL_SHAPE_TRIGGER)-1), "unknown") == 0 &&
           strcmp(VHO_DSL_Shape_Trigger_Name
                      (VHO_DSL_SHAPE_TRIGGER_COUNT), "unknown") == 0;
}

typedef struct {
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_VALUE result_value;
} WP2_SHAPE_FIXTURE;

static BOOL
Build_WP2_Shape_Fixture (const char *name, BOOL pending_result,
                         BOOL use_region, WP2_SHAPE_FIXTURE *fixture)
{
    if (fixture == NULL || !DSL_Builder_Begin_Program())
        return FALSE;
    DSL_Opcode_Register_Common_Substrate();
    DSL_BUILDER_TENSOR_DESCRIPTOR input_descriptor;
    DSL_BUILDER_TENSOR_DESCRIPTOR result_descriptor;
    Initialize_Descriptor(&input_descriptor, "[2,3]", "activation");
    Initialize_Descriptor
        (&result_descriptor, pending_result ? "[2,<pending>]" : "[2,3]",
         "derived_activation");
    TY_IDX input_ty = DSL_Builder_Intern_Tensor_Type
                          ("wp2_input_tensor", MTYPE_To_TY(MTYPE_F4),
                           &input_descriptor);
    TY_IDX result_ty = DSL_Builder_Intern_Tensor_Type
                           ("wp2_result_tensor", MTYPE_To_TY(MTYPE_F4),
                            &result_descriptor);
    fixture->pu = DSL_Builder_Create_Minimal_PU(name);
    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("wp2_input", input_ty, 0);
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    DSL_BUILDER_VALUE kids[2] = { input, input };
    fixture->result_value = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"),
                         DSL_OPCODE_COMMON_ADD, 1),
         1, kids, 2, &attribute, 1, "wp2_result", result_ty);
    if (input_ty == TY_IDX_ZERO || result_ty == TY_IDX_ZERO ||
        fixture->pu == NULL || input == NULL ||
        fixture->result_value == NULL ||
        !DSL_Builder_Append_PU_Value(fixture->pu, input))
        return FALSE;
    if (use_region) {
        DSL_BUILDER_REGION region = DSL_Builder_Create_Region
                                        (fixture->pu, NULL,
                                         "shape.wp2.region.v1", 1);
        if (region == NULL ||
            !DSL_Builder_Append_Region_Value
                 (region, fixture->result_value) ||
            !DSL_Builder_Declare_Region_Value
                 (region, fixture->result_value,
                  DSL_REGION_VALUE_OUTPUT | DSL_REGION_VALUE_RESULT,
                  0, DSL_REGION_INTERFACE_FLAG_NONE) ||
            !DSL_Builder_Append_PU_Region(fixture->pu, region))
            return FALSE;
    } else if (!DSL_Builder_Append_PU_Value
                    (fixture->pu, fixture->result_value)) {
        return FALSE;
    }
    return DSL_Builder_Select_PU(fixture->pu);
}

static BOOL
WP2_Success_Counters_Valid
        (const VHO_DSL_SHAPE_REFINE_RESULT &result, BOOL transaction)
{
    return result.boundary_admission_count == 1 &&
           result.boundary_success_exit_count == 1 &&
           result.retype_boundary_precheck_count == (transaction ? 1U : 0U) &&
           result.retype_boundary_postcheck_count == (transaction ? 1U : 0U);
}

static int
Run_WP2_Success_Test (const char *selected)
{
    if (selected == NULL)
        return 80;
    VHO_DSL_SHAPE_REFINE_RESULT result;
    memset(&result, 0, sizeof(result));
    BOOL accepted = FALSE;
    BOOL transaction = FALSE;
    DSL_BUILDER_PROGRAM_UNIT pu = NULL;
    WN *tree = NULL;

    if (strcmp(selected, "no_native") == 0) {
        if (!DSL_Builder_Begin_Program())
            return 81;
        DSL_Opcode_Register_Common_Substrate();
        pu = DSL_Builder_Create_Minimal_PU("wp2_no_native");
        if (pu == NULL || !DSL_Builder_Select_PU(pu))
            return 82;
        tree = PU_Info_tree_ptr(pu);
        accepted = VHO_DSL_Shape_Refine_Program_Unit
                       (pu, tree, TRUE, stderr, &result);
    } else {
        BOOL pending = strcmp(selected, "ordinary") == 0;
        BOOL region = strcmp(selected, "region") == 0;
        BOOL disabled = strcmp(selected, "disabled") == 0;
        BOOL already_complete = strcmp(selected, "already_complete") == 0;
        if (!pending && !region && !disabled && !already_complete &&
            strcmp(selected, "no_request") != 0)
            return 83;
        WP2_SHAPE_FIXTURE fixture;
        if (!Build_WP2_Shape_Fixture
                 (selected, pending, region, &fixture))
            return 84;
        pu = fixture.pu;
        tree = PU_Info_tree_ptr(pu);
        if (already_complete) {
            VHO_DSL_SHAPE_REFINE_RESULT first;
            memset(&first, 0, sizeof(first));
            if (!VHO_DSL_Shape_Refine_Program_Unit
                     (pu, tree, TRUE, stderr, &first) ||
                !WP2_Success_Counters_Valid(first, FALSE) ||
                !VHO_DSL_Shape_Refinement_Invalidate
                     (pu, tree, VHO_DSL_SHAPE_TRIGGER_DSL_WOPT,
                      stderr))
                return 85;
        }
        accepted = VHO_DSL_Shape_Refine_Program_Unit
                       (pu, tree, !disabled, stderr, &result);
        transaction = pending;
    }

    BOOL current = VHO_DSL_Shape_Refinement_Is_Current(pu, tree, NULL);
    BOOL valid = accepted && current && result.diagnostic_count == 0 &&
                 WP2_Success_Counters_Valid(result, transaction) &&
                 (transaction ? result.retyped_value_count == 1 &&
                                result.rollback_count == 0 :
                                result.retyped_value_count == 0 &&
                                result.rollback_count == 0);
    printf("WP2 success case=%s accepted=%d current=%d admission=%u "
           "exit=%u transaction=%u/%u retyped=%u rollback=%u valid=%d\n",
           selected, accepted, current, result.boundary_admission_count,
           result.boundary_success_exit_count,
           result.retype_boundary_precheck_count,
           result.retype_boundary_postcheck_count,
           result.retyped_value_count, result.rollback_count, valid);
    return valid ? 0 : 86;
}

typedef struct {
    DSL_BUILDER_PROGRAM_UNIT caller;
    DSL_BUILDER_PROGRAM_UNIT callee;
    DSL_BUILDER_CALL call;
    ST_IDX callee_input_st;
    ST_IDX callee_result_st;
    TY_IDX tensor_ty;
    TY_IDX wrong_ty;
} WP2_INTERFACE_FIXTURE;

static int Run_WP2_Region_Context_Red_Test(void);

static int
Run_WP2_Region_Owner_Negative_Test(void)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    Initialize_Descriptor(&descriptor, "[2,3]", "activation");
    if (!DSL_Builder_Begin_Program())
        return 97;
    DSL_Opcode_Register_Common_Substrate();
    TY_IDX tensor_ty = DSL_Builder_Intern_Tensor_Type
                           ("wp2_region_owner_tensor",
                            MTYPE_To_TY(MTYPE_F4), &descriptor);
    DSL_BUILDER_PROGRAM_UNIT owner = DSL_Builder_Create_Minimal_PU
                                         ("wp2_region_owner");
    DSL_BUILDER_VALUE owner_value = DSL_Builder_Create_Model_Input
                                        ("owner_value", tensor_ty, 0);
    DSL_BUILDER_REGION region = DSL_Builder_Create_Region
                                    (owner, NULL,
                                     "shape.wp2.owner.v1", 1);
    if (tensor_ty == TY_IDX_ZERO || owner == NULL || owner_value == NULL ||
        region == NULL ||
        !DSL_Builder_Append_PU_Value(owner, owner_value))
        return 98;

    DSL_BUILDER_PROGRAM_UNIT foreign = DSL_Builder_Create_Minimal_PU
                                           ("wp2_region_foreign");
    DSL_BUILDER_VALUE foreign_value = DSL_Builder_Create_Model_Input
                                          ("foreign_value", tensor_ty, 0);
    ST_IDX foreign_st = DSL_Builder_Get_Value_Result_Symbol(foreign_value);
    if (foreign == NULL || foreign_value == NULL ||
        ST_IDX_index(foreign_st) == 0 ||
        !DSL_Region_Declare_Symbol
             (region, foreign_st, DSL_REGION_VALUE_INPUT, 0,
              DSL_REGION_INTERFACE_FLAG_NONE) ||
        !DSL_Builder_Select_PU(owner) ||
        !DSL_Builder_Append_PU_Region(owner, region))
        return 99;

    VHO_DSL_SHAPE_REFINE_RESULT result;
    memset(&result, 0, sizeof(result));
    WN *tree = PU_Info_tree_ptr(owner);
    BOOL accepted = VHO_DSL_Shape_Refine_Program_Unit
                        (owner, tree, TRUE, stderr, &result);
    BOOL current = VHO_DSL_Shape_Refinement_Is_Current(owner, tree, NULL);
    BOOL valid = !accepted && !current &&
                 result.boundary_admission_count == 1 &&
                 result.boundary_success_exit_count == 0 &&
                 result.retype_boundary_precheck_count == 0 &&
                 result.retype_boundary_postcheck_count == 0 &&
                 result.updated_st_count == 0 &&
                 result.updated_wn_count == 0 &&
                 result.rollback_count == 0;
    printf("WP2 REGION owner negative accepted=%d current=%d "
           "admission=%u exit=%u writes=%u/%u rollback=%u valid=%d\n",
           accepted, current, result.boundary_admission_count,
           result.boundary_success_exit_count,
           result.updated_st_count, result.updated_wn_count,
           result.rollback_count, valid);
    return valid ? 0 : 100;
}

static int
Run_WP2_Region_Index_Negative_Test (const char *selected)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    Initialize_Descriptor(&descriptor, "[2,3]", "activation");
    if (!DSL_Builder_Begin_Program())
        return 101;
    DSL_Opcode_Register_Common_Substrate();
    TY_IDX tensor_ty = DSL_Builder_Intern_Tensor_Type
                           ("wp2_region_index_tensor",
                            MTYPE_To_TY(MTYPE_F4), &descriptor);
    DSL_BUILDER_PROGRAM_UNIT owner = DSL_Builder_Create_Minimal_PU
                                         ("wp2_region_index_owner");
    DSL_BUILDER_VALUE owner_value = DSL_Builder_Create_Model_Input
                                        ("owner_value", tensor_ty, 0);
    DSL_BUILDER_REGION region = DSL_Builder_Create_Region
                                    (owner, NULL,
                                     "shape.wp2.index.v1", 1);
    if (tensor_ty == TY_IDX_ZERO || owner == NULL || owner_value == NULL ||
        region == NULL ||
        !DSL_Builder_Append_PU_Value(owner, owner_value) ||
        !DSL_Builder_Select_PU(owner))
        return 102;
    ST_IDX invalid_st = strcmp(selected, "region_global") == 0 ?
                        PU_Info_proc_sym(owner) :
                        make_ST_IDX(ST_Table_Size(CURRENT_SYMTAB) + 4,
                                    CURRENT_SYMTAB);
    if (!DSL_Region_Declare_Symbol
             (region, invalid_st, DSL_REGION_VALUE_INPUT, 0,
              DSL_REGION_INTERFACE_FLAG_NONE) ||
        !DSL_Builder_Append_PU_Region(owner, region))
        return 103;

    VHO_DSL_SHAPE_REFINE_RESULT result;
    memset(&result, 0, sizeof(result));
    WN *tree = PU_Info_tree_ptr(owner);
    BOOL accepted = VHO_DSL_Shape_Refine_Program_Unit
                        (owner, tree, TRUE, stderr, &result);
    BOOL current = VHO_DSL_Shape_Refinement_Is_Current(owner, tree, NULL);
    BOOL valid = !accepted && !current &&
                 result.boundary_admission_count == 1 &&
                 result.boundary_success_exit_count == 0 &&
                 result.retype_boundary_precheck_count == 0 &&
                 result.retype_boundary_postcheck_count == 0 &&
                 result.updated_st_count == 0 &&
                 result.updated_wn_count == 0 &&
                 result.rollback_count == 0;
    printf("WP2 REGION index negative case=%s accepted=%d current=%d "
           "admission=%u exit=%u writes=%u/%u rollback=%u valid=%d\n",
           selected, accepted, current, result.boundary_admission_count,
           result.boundary_success_exit_count,
           result.updated_st_count, result.updated_wn_count,
           result.rollback_count, valid);
    return valid ? 0 : 104;
}

static int
Run_WP2_Region_Consume_Test (const char *selected)
{
    BOOL wrong_owner = selected != NULL &&
                       strcmp(selected, "wrong_owner") == 0;
    if (!wrong_owner &&
        (selected == NULL || strcmp(selected, "success") != 0))
        return 105;
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    Initialize_Descriptor(&descriptor, "[2,3]", "activation");
    if (!DSL_Builder_Begin_Program())
        return 106;
    DSL_Opcode_Register_Common_Substrate();
    TY_IDX tensor_ty = DSL_Builder_Intern_Tensor_Type
                           ("wp2_region_consume_tensor",
                            MTYPE_To_TY(MTYPE_F4), &descriptor);
    DSL_BUILDER_PROGRAM_UNIT owner = DSL_Builder_Create_Minimal_PU
                                         ("wp2_region_consume_owner");
    DSL_BUILDER_VALUE first_value = DSL_Builder_Create_Model_Input
                                        ("consume_first", tensor_ty, 0);
    DSL_BUILDER_VALUE second_value = DSL_Builder_Create_Model_Input
                                         ("consume_second", tensor_ty, 1);
    DSL_BUILDER_REGION first_region = DSL_Builder_Create_Region
                                          (owner, NULL,
                                           "shape.wp2.consume.first", 1);
    DSL_BUILDER_REGION second_region = DSL_Builder_Create_Region
                                           (owner, NULL,
                                            "shape.wp2.consume.second", 1);
    ST_IDX first_st = DSL_Builder_Get_Value_Result_Symbol(first_value);
    ST_IDX second_st = DSL_Builder_Get_Value_Result_Symbol(second_value);
    if (tensor_ty == TY_IDX_ZERO || owner == NULL || first_value == NULL ||
        second_value == NULL || first_region == NULL ||
        second_region == NULL || ST_IDX_index(first_st) == 0 ||
        ST_IDX_index(second_st) == 0 ||
        !DSL_Builder_Append_Region_Value(first_region, first_value) ||
        !DSL_Builder_Append_Region_Value(second_region, second_value) ||
        !DSL_Region_Declare_Symbol
             (first_region, first_st, DSL_REGION_VALUE_INPUT, 0,
              DSL_REGION_INTERFACE_FLAG_NONE))
        return 107;

    if (wrong_owner) {
        DSL_BUILDER_PROGRAM_UNIT foreign = DSL_Builder_Create_Minimal_PU
                                               ("wp2_region_consume_foreign");
        DSL_BUILDER_VALUE foreign_value = DSL_Builder_Create_Model_Input
                                              ("consume_foreign", tensor_ty,
                                               0);
        ST_IDX foreign_st =
            DSL_Builder_Get_Value_Result_Symbol(foreign_value);
        if (foreign == NULL || foreign_value == NULL ||
            ST_IDX_index(foreign_st) == 0 ||
            !DSL_Region_Declare_Symbol
                 (second_region, foreign_st, DSL_REGION_VALUE_INPUT, 1,
                  DSL_REGION_INTERFACE_FLAG_NONE) ||
            !DSL_Builder_Select_PU(owner))
            return 108;
    } else if (!DSL_Region_Declare_Symbol
                    (second_region, second_st, DSL_REGION_VALUE_INPUT, 1,
                     DSL_REGION_INTERFACE_FLAG_NONE)) {
        return 109;
    }

    WN *first_wn = DSL_Region_WN(first_region);
    WN *second_wn = DSL_Region_WN(second_region);
    if (!DSL_Builder_Append_PU_Region(owner, first_region) ||
        !DSL_Builder_Append_PU_Region(owner, second_region) ||
        !DSL_Region_Consume_WN(owner, first_wn))
        return 110;

    DSL_IR_ACTIVE_PU_BOUNDARY_CONTEXT boundary;
    boundary.pu_info = owner;
    boundary.tree = PU_Info_tree_ptr(owner);
    boundary.owner_pu_st = PU_Info_proc_sym(owner);
    BOOL store_valid = DSL_Region_Verify_PU(owner, stderr);
    BOOL boundary_valid = DSL_IR_Image_Validate_Active_PU_Boundaries
                              (&boundary, stderr);
    UINT32 first_uses = DSL_Region_Symbol_Use_Count(owner, first_st);
    UINT32 second_uses = DSL_Region_Symbol_Use_Count(owner, second_st);
    BOOL interface_valid = first_uses + second_uses == 1 &&
                           (wrong_owner ||
                            (first_uses == 0 && second_uses == 1));
    BOOL valid = store_valid &&
                 interface_valid &&
                 !DSL_Region_Is_Managed_WN(owner, first_wn) &&
                 DSL_Region_Is_Managed_WN(owner, second_wn) &&
                 boundary_valid == !wrong_owner;
    printf("WP2 REGION consume case=%s store_valid=%d boundary_valid=%d "
           "first_uses=%u second_uses=%u first_managed=%d "
           "second_managed=%d valid=%d\n",
           selected, store_valid, boundary_valid,
           first_uses, second_uses,
           DSL_Region_Is_Managed_WN(owner, first_wn),
           DSL_Region_Is_Managed_WN(owner, second_wn), valid);
    return valid ? 0 : 111;
}

static WN *
Find_WP2_Store (WN *tree, ST_IDX st)
{
    if (tree == NULL)
        return NULL;
    if (WN_operator(tree) == OPR_STID && WN_st_idx(tree) == st)
        return tree;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *statement = WN_first(tree); statement != NULL;
             statement = WN_next(statement)) {
            WN *found = Find_WP2_Store(statement, st);
            if (found != NULL)
                return found;
        }
        return NULL;
    }
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
        WN *found = Find_WP2_Store(WN_kid(tree, kid), st);
        if (found != NULL)
            return found;
    }
    return NULL;
}

static BOOL
Build_WP2_Interface_Fixture (WP2_INTERFACE_FIXTURE *fixture)
{
    if (fixture == NULL || !DSL_Builder_Begin_Program())
        return FALSE;
    memset(fixture, 0, sizeof(*fixture));
    DSL_Opcode_Register_Common_Substrate();
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    DSL_BUILDER_TENSOR_DESCRIPTOR wrong_descriptor;
    Initialize_Descriptor(&descriptor, "[2,3]", "activation");
    Initialize_Descriptor(&wrong_descriptor, "[2,4]", "activation");
    fixture->tensor_ty = DSL_Builder_Intern_Tensor_Type
                             ("wp2_interface_tensor",
                              MTYPE_To_TY(MTYPE_F4), &descriptor);
    fixture->wrong_ty = DSL_Builder_Intern_Tensor_Type
                            ("wp2_interface_wrong_tensor",
                             MTYPE_To_TY(MTYPE_F4), &wrong_descriptor);

    fixture->callee = DSL_Builder_Create_Minimal_PU("wp2_callee");
    UINT32 callee_file = DSL_Builder_Register_Source_File
                             (fixture->callee, __FILE__);
    DSL_BUILDER_SOURCE_POSITION position;
    memset(&position, 0, sizeof(position));
    position.file_id = callee_file;
    position.line = __LINE__ + 1;
    position.column = 1;
    position.statement_begin = 1;
    DSL_BUILDER_VALUE formal = DSL_Builder_Declare_PU_Formal
                                   (fixture->callee, "callee_input", 0,
                                    fixture->tensor_ty, &position);
    ++position.line;
    DSL_BUILDER_VALUE result = DSL_Builder_Declare_PU_Result
                                   (fixture->callee, "callee_result", 0,
                                    fixture->tensor_ty,
                                    DSL_PU_RESULT_TENSOR, &position);
    DSL_BUILDER_VALUE returned[1] = { formal };
    if (fixture->tensor_ty == TY_IDX_ZERO ||
        fixture->wrong_ty == TY_IDX_ZERO || fixture->callee == NULL ||
        callee_file == 0 || formal == NULL || result == NULL ||
        !DSL_Builder_Return_PU_Values(fixture->callee, returned, 1))
        return FALSE;
    WN *callee_entry = PU_Info_tree_ptr(fixture->callee);
    fixture->callee_input_st = WN_st_idx(WN_formal(callee_entry, 0));
    fixture->callee_result_st = WN_st_idx(WN_formal(callee_entry, 1));

    fixture->caller = DSL_Builder_Create_Minimal_PU("wp2_caller");
    UINT32 caller_file = DSL_Builder_Register_Source_File
                             (fixture->caller, __FILE__);
    DSL_BUILDER_VALUE actual = DSL_Builder_Create_Model_Input
                                   ("caller_input", fixture->tensor_ty, 0);
    DSL_BUILDER_CALLSITE_INFO callsite;
    memset(&callsite, 0, sizeof(callsite));
    callsite.canonical_class_name = "WP2Boundary";
    callsite.instance_path = "wp2.call";
    callsite.context_identity = "wp2.call.0";
    callsite.call_ordinal = 0;
    callsite.source_position.file_id = caller_file;
    callsite.source_position.line = __LINE__ + 1;
    callsite.source_position.column = 1;
    callsite.source_position.statement_begin = 1;
    const char *result_names[1] = { "caller_result" };
    fixture->call = DSL_Builder_Create_PU_Call
                        (fixture->caller, fixture->callee, &actual, 1,
                         result_names, 1, &callsite);
    return fixture->caller != NULL && caller_file != 0 && actual != NULL &&
           fixture->call != NULL &&
           DSL_Builder_Set_PU_Call_Argument_Role
               (fixture->call, 0, 0, "tensor.input");
}

static BOOL
WP2_Local_Symbol_Types_Unchanged (const TY_IDX *before, UINT32 count)
{
    if (before == NULL || ST_Table_Size(CURRENT_SYMTAB) != count)
        return FALSE;
    for (UINT32 i = 1; i < count; ++i) {
        ST_IDX st = make_ST_IDX(i, CURRENT_SYMTAB);
        if (ST_type(St_Table[st]) != before[i])
            return FALSE;
    }
    return TRUE;
}

static int
Run_WP2_Negative_Test (const char *selected)
{
    if (selected == NULL)
        return 90;
    if (strcmp(selected, "region") == 0)
        return Run_WP2_Region_Context_Red_Test();
    if (strcmp(selected, "region_owner") == 0)
        return Run_WP2_Region_Owner_Negative_Test();
    if (strcmp(selected, "region_global") == 0 ||
        strcmp(selected, "region_out_of_range") == 0)
        return Run_WP2_Region_Index_Negative_Test(selected);

    WP2_INTERFACE_FIXTURE fixture;
    if (!Build_WP2_Interface_Fixture(&fixture))
        return 91;
    BOOL caller_case = strcmp(selected, "actual") == 0 ||
                       strcmp(selected, "hidden_result") == 0;
    if (!caller_case && strcmp(selected, "formal") != 0 &&
        strcmp(selected, "return") != 0 &&
        strcmp(selected, "interface") != 0)
        return 92;
    DSL_BUILDER_PROGRAM_UNIT active = caller_case ? fixture.caller :
                                       fixture.callee;
    if (!DSL_Builder_Select_PU(active))
        return 93;

    if (caller_case) {
        UINT32 ordinal = strcmp(selected, "actual") == 0 ? 0 : 1;
        WN *parm = WN_kid(fixture.call, ordinal);
        WN *address = parm == NULL ? NULL : WN_kid0(parm);
        if (parm == NULL || address == NULL)
            return 94;
        TY_IDX wrong_pointer_ty = Make_Pointer_Type(fixture.wrong_ty);
        WN_set_ty(parm, wrong_pointer_ty);
        WN_set_ty(address, wrong_pointer_ty);
    } else {
        WN *entry = PU_Info_tree_ptr(fixture.callee);
        if (strcmp(selected, "formal") == 0) {
            WN_st_idx(WN_formal(entry, 0)) = fixture.callee_result_st;
        } else if (strcmp(selected, "interface") == 0) {
            Set_ST_type(St_Table[fixture.callee_input_st], fixture.wrong_ty);
        } else {
            WN *store = Find_WP2_Store(entry, fixture.callee_result_st);
            if (store == NULL)
                return 95;
            WN_set_ty(store, fixture.wrong_ty);
        }
    }

    UINT32 st_count = ST_Table_Size(CURRENT_SYMTAB);
    TY_IDX *st_types = new TY_IDX[st_count];
    for (UINT32 i = 1; i < st_count; ++i)
        st_types[i] = ST_type(St_Table[make_ST_IDX(i, CURRENT_SYMTAB)]);
    UINT32 type_count = TY_Table_Size();
    VHO_DSL_SHAPE_REFINE_RESULT result;
    memset(&result, 0, sizeof(result));
    WN *tree = PU_Info_tree_ptr(active);
    BOOL accepted = VHO_DSL_Shape_Refine_Program_Unit
                        (active, tree, TRUE, stderr, &result);
    BOOL current = VHO_DSL_Shape_Refinement_Is_Current(active, tree, NULL);
    BOOL unchanged = WP2_Local_Symbol_Types_Unchanged(st_types, st_count) &&
                     TY_Table_Size() == type_count;
    delete[] st_types;
    BOOL valid = !accepted && !current && unchanged &&
                 result.boundary_admission_count == 1 &&
                 result.boundary_success_exit_count == 0 &&
                 result.retype_boundary_precheck_count == 0 &&
                 result.retype_boundary_postcheck_count == 0 &&
                 result.retyped_value_count == 0 &&
                 result.updated_st_count == 0 &&
                 result.updated_wn_count == 0 &&
                 result.rollback_count == 0 && result.diagnostic_count == 1;
    printf("WP2 negative case=%s accepted=%d current=%d admission=%u "
           "exit=%u transaction=%u/%u writes=%u/%u rollback=%u "
           "unchanged=%d valid=%d\n",
           selected, accepted, current, result.boundary_admission_count,
           result.boundary_success_exit_count,
           result.retype_boundary_precheck_count,
           result.retype_boundary_postcheck_count,
           result.updated_st_count, result.updated_wn_count,
           result.rollback_count, unchanged, valid);
    return valid ? 0 : 96;
}

static int
Run_WP2_Region_Context_Red_Test(void)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    Initialize_Descriptor(&descriptor, "[2,3]", "activation");
    if (!DSL_Builder_Begin_Program())
        return 70;
    DSL_Opcode_Register_Common_Substrate();
    TY_IDX tensor_ty = DSL_Builder_Intern_Tensor_Type
                           ("wp2_region_context_tensor",
                            MTYPE_To_TY(MTYPE_F4), &descriptor);
    DSL_BUILDER_PROGRAM_UNIT region_pu =
        DSL_Builder_Create_Minimal_PU("wp2_region_context_owner");
    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("region_input", tensor_ty, 0);
    DSL_BUILDER_REGION region = DSL_Builder_Create_Region
                                    (region_pu, NULL,
                                     "shape.wp2.region.v1", 1);
    if (tensor_ty == TY_IDX_ZERO || region_pu == NULL || input == NULL ||
        region == NULL || !DSL_Builder_Append_Region_Value(region, input) ||
        !DSL_Builder_Declare_Region_Value
             (region, input, DSL_REGION_VALUE_INPUT, 0,
              DSL_REGION_INTERFACE_FLAG_NONE) ||
        !DSL_Builder_Append_PU_Region(region_pu, region))
        return 71;

    DSL_BUILDER_PROGRAM_UNIT foreign_tree_pu =
        DSL_Builder_Create_Minimal_PU("wp2_foreign_tree");
    if (foreign_tree_pu == NULL || !DSL_Builder_Select_PU(region_pu))
        return 72;

    WN *foreign_tree = PU_Info_tree_ptr(foreign_tree_pu);
    VHO_DSL_SHAPE_REFINE_RESULT result;
    memset(&result, 0, sizeof(result));
    BOOL accepted = VHO_DSL_Shape_Refine_Program_Unit
                        (region_pu, foreign_tree, TRUE, stderr, &result);
    BOOL current = VHO_DSL_Shape_Refinement_Is_Current
                       (region_pu, foreign_tree, NULL);
    BOOL valid = !accepted && !current && result.diagnostic_count == 1 &&
                 result.boundary_admission_count == 1 &&
                 result.boundary_success_exit_count == 0 &&
                 result.retype_boundary_precheck_count == 0 &&
                 result.retype_boundary_postcheck_count == 0 &&
                 result.retyped_value_count == 0 &&
                 result.updated_st_count == 0 &&
                 result.updated_wn_count == 0 &&
                 result.rollback_count == 0;
    printf("WP2 region context red: accepted=%d current=%d "
           "admission=%u exit=%u writes=%u/%u rollback=%u "
           "diagnostics=%u valid=%d\n",
           accepted, current, result.boundary_admission_count,
           result.boundary_success_exit_count,
           result.updated_st_count, result.updated_wn_count,
           result.rollback_count, result.diagnostic_count, valid);
    return valid ? 0 : 73;
}

int
main(void)
{
    Initialize_Test_Context();
    if (getenv("OPEN64_DSL_SHAPE_WP2_SUCCESS") != NULL)
        return Run_WP2_Success_Test
                   (getenv("OPEN64_DSL_SHAPE_WP2_SUCCESS"));
    if (getenv("OPEN64_DSL_SHAPE_WP2_NEGATIVE") != NULL)
        return Run_WP2_Negative_Test
                   (getenv("OPEN64_DSL_SHAPE_WP2_NEGATIVE"));
    if (getenv("OPEN64_DSL_SHAPE_WP2_REGION_CONSUME") != NULL)
        return Run_WP2_Region_Consume_Test
                   (getenv("OPEN64_DSL_SHAPE_WP2_REGION_CONSUME"));
    if (getenv("OPEN64_DSL_SHAPE_WP2_RED") != NULL)
        return Run_WP2_Region_Context_Red_Test();
    if (getenv("OPEN64_DSL_SHAPE_IDENTITY_REPRO") != NULL)
        return Run_Custom_Identity_Retype_Test(FALSE);
    if (getenv("OPEN64_DSL_SHAPE_QUALIFIER_REPRO") != NULL)
        return Run_Custom_Identity_Retype_Test(TRUE);
    if (getenv("OPEN64_DSL_SHAPE_AUTH_MATRIX") != NULL)
        return Run_Authorization_Matrix();
    if (getenv("OPEN64_DSL_SHAPE_INTERNER_MATRIX") != NULL)
        return Run_Interner_Authorization_Matrix();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();

    DSL_BUILDER_TENSOR_DESCRIPTOR input_descriptor;
    DSL_BUILDER_TENSOR_DESCRIPTOR pending_descriptor;
    Initialize_Descriptor(&input_descriptor, "[2,3]", "activation");
    Initialize_Descriptor
        (&pending_descriptor, "[2,<pending>]", "derived_activation");
    TY_IDX input_ty = DSL_Builder_Intern_Tensor_Type
                          ("sp5_input", MTYPE_To_TY(MTYPE_F4),
                           &input_descriptor);
    TY_IDX pending_ty = DSL_Builder_Intern_Tensor_Type
                            ("sp5_pending", MTYPE_To_TY(MTYPE_F4),
                             &pending_descriptor);
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("dsl_shape_refine_sp5");
    UINT32 file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("shape_input", input_ty, 0);
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    DSL_BUILDER_VALUE add_kids[2] = { input, input };
    DSL_BUILDER_VALUE add = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"),
                         DSL_OPCODE_COMMON_ADD, 1),
         1, add_kids, 2, &attribute, 1, "shape_add", pending_ty);
    DSL_BUILDER_VALUE relu_kids[1] = { add };
    DSL_BUILDER_VALUE relu = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"), "common.relu", 2),
         2, relu_kids, 1, NULL, 0, "shape_relu", pending_ty);
    DSL_BUILDER_SOURCE_POSITION position;
    memset(&position, 0, sizeof(position));
    position.file_id = file_id;
    position.line = 120;
    position.statement_begin = 1;
    DSL_BUILDER_REGION region = DSL_Builder_Create_Region
                                    (pu, NULL, "shape.refine.v1", 1);
    ST_IDX unrelated = DSL_Builder_Create_Tensor_Result_Symbol
                           ("unrelated_pending", pending_ty,
                            SCLASS_AUTO, EXPORT_LOCAL);
    if (input_ty == TY_IDX_ZERO || pending_ty == TY_IDX_ZERO || pu == NULL ||
        file_id == 0 || input == NULL || add == NULL || relu == NULL ||
        region == NULL || ST_IDX_index(unrelated) == 0 ||
        !DSL_Builder_Set_Tensor_Unique_Ownership(unrelated) ||
        !DSL_Builder_Set_Value_Source_Position(add, &position) ||
        (++position.line,
         !DSL_Builder_Set_Value_Source_Position(relu, &position)) ||
        !DSL_Builder_Append_PU_Value(pu, input) ||
        !DSL_Builder_Append_Region_Value(region, add) ||
        !DSL_Builder_Append_Region_Value(region, relu) ||
        !DSL_Builder_Declare_Region_Value
             (region, relu,
              DSL_REGION_VALUE_OUTPUT | DSL_REGION_VALUE_RESULT,
              0, DSL_REGION_INTERFACE_FLAG_NONE) ||
        !DSL_Builder_Append_PU_Region(pu, region)) {
        fprintf(stderr, "SP5 fixture creation failed\n");
        return 1;
    }

    DSL_GATEKEEPER_RESULT gatekeeper;
    if (!DSL_Gatekeeper_Verify_PU_Mode
             (pu, DSL_GATEKEEPER_ADMISSION, stderr, &gatekeeper) ||
        DSL_Gatekeeper_Verify_PU_Mode
             (pu, DSL_GATEKEEPER_STRICT, NULL, &gatekeeper)) {
        fprintf(stderr, "SP5 admission/strict split changed\n");
        return 1;
    }

    const char *before_artifact =
        getenv("OPEN64_DSL_SHAPE_SP10_BEFORE_ARTIFACT");
    if (before_artifact != NULL && before_artifact[0] != '\0') {
        DSL_BUILDER_MAPPED_IMAGE_REQUEST image;
        image.path = before_artifact;
        image.flags = 0;
        (void)unlink(before_artifact);
        if (!DSL_Builder_Finalize_Mapped_Image(&image) ||
            access(before_artifact, F_OK) != 0) {
            fprintf(stderr, "SP10 before-refinement image failed\n");
            return 1;
        }
        printf("SP10 before-refinement admission image passed\n");
        return 0;
    }

    VHO_DSL_SHAPE_REFINE_RESULT disabled;
    if (VHO_DSL_Shape_Refine_Program_Unit
            (pu, PU_Info_tree_ptr(pu), FALSE, NULL, &disabled) ||
        !Value_Type_Is(add, pending_ty) ||
        !Value_Type_Is(relu, pending_ty)) {
        fprintf(stderr, "SP5 disabled check-only mode mutated or accepted\n");
        return 1;
    }

    TY_TENSOR_TYPE_CORE_REFINEMENT refinement;
    refinement.rank = 2;
    refinement.logical_shape = "[2,3]";
    BOOL created = FALSE;
    TY_IDX refined_ty = TY_Intern_Refined_Tensor_Type
                            (pending_ty, &refinement, &created);
    DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST requests[2];
    requests[0].owner_pu_st = PU_Info_proc_sym(pu);
    requests[0].value_id = DSL_Builder_Get_Value_Image_Id(add);
    requests[0].expected_old_ty = pending_ty;
    requests[0].refined_ty = refined_ty;
    requests[1].owner_pu_st = PU_Info_proc_sym(pu);
    requests[1].value_id = DSL_Builder_Get_Value_Image_Id(relu);
    requests[1].expected_old_ty = pending_ty;
    requests[1].refined_ty = refined_ty;
    DSL_IR_VALUE_TYPE_REFINEMENT_RESULT forced_failure;
    setenv("OPEN64_DSL_SHAPE_RETYPE_TEST_POSTFAIL", "1", 1);
    BOOL unexpectedly_committed = DSL_IR_Refine_Native_Value_Types
        (pu, PU_Info_tree_ptr(pu), requests, 2, stderr, &forced_failure);
    unsetenv("OPEN64_DSL_SHAPE_RETYPE_TEST_POSTFAIL");
    if (TY_IDX_index(refined_ty) == 0 || !created || unexpectedly_committed ||
        forced_failure.rollback_count != 2 ||
        !Value_Type_Is(add, pending_ty) ||
        !Value_Type_Is(relu, pending_ty) ||
        ST_type(St_Table[unrelated]) != pending_ty ||
        !DSL_Region_Verify_PU(pu, stderr)) {
        fprintf(stderr, "SP5 late-failure rollback changed\n");
        return 1;
    }

    VHO_DSL_SHAPE_REFINE_RESULT refined;
    if (!VHO_DSL_Shape_Refine_Program_Unit
             (pu, PU_Info_tree_ptr(pu), TRUE, stderr, &refined) ||
        refined.solver.refinable_value_count != 2 ||
        refined.requested_value_count != 2 ||
        refined.retyped_value_count != 2 ||
        refined.reused_type_count != 2 ||
        refined.rollback_count != 0 ||
        !Value_Type_Is(add, refined_ty) ||
        !Value_Type_Is(relu, refined_ty) ||
        WN_ty(WN_kid0(WN_kid0(relu))) != refined_ty ||
        ST_type(St_Table[unrelated]) != pending_ty ||
        !DSL_Region_Verify_PU(pu, stderr) ||
        !DSL_Gatekeeper_Verify_PU_Mode
             (pu, DSL_GATEKEEPER_STRICT, stderr, &gatekeeper)) {
        fprintf(stderr,
                "SP5 shape refinement result changed: refinable=%u "
                "requested=%u retyped=%u reused=%u rollback=%u "
                "add_type=%d relu_type=%d kid_type=%d unrelated_type=%d\n",
                refined.solver.refinable_value_count,
                refined.requested_value_count, refined.retyped_value_count,
                refined.reused_type_count, refined.rollback_count,
                Value_Type_Is(add, refined_ty),
                Value_Type_Is(relu, refined_ty),
                WN_ty(WN_kid0(WN_kid0(relu))) == refined_ty,
                ST_type(St_Table[unrelated]) == pending_ty);
        return 1;
    }

    if (!Shape_Trigger_Names_Are_Stable() ||
        VHO_DSL_Shape_Refinement_Invalidate
            (pu, PU_Info_tree_ptr(pu),
             VHO_DSL_SHAPE_TRIGGER_COUNT, stderr) ||
        !VHO_DSL_Shape_Refinement_Is_Current
            (pu, PU_Info_tree_ptr(pu), stderr)) {
        fprintf(stderr, "SP10 shape trigger identity changed\n");
        return 1;
    }

    if (!VHO_DSL_Shape_Refinement_Is_Current
             (pu, PU_Info_tree_ptr(pu), stderr) ||
        !VHO_DSL_Shape_Refinement_Invalidate
             (pu, PU_Info_tree_ptr(pu),
              VHO_DSL_SHAPE_TRIGGER_OPERATOR_CONSTRAINT_CHANGE, stderr) ||
        VHO_DSL_Shape_Refinement_Is_Current
             (pu, PU_Info_tree_ptr(pu), NULL) ||
        !DSL_Gatekeeper_Verify_PU_Mode
             (pu, DSL_GATEKEEPER_STRICT, stderr, &gatekeeper)) {
        fprintf(stderr, "SP10 shape generation invalidation changed\n");
        return 1;
    }

    VHO_DSL_SHAPE_REFINE_RESULT revalidated;
    if (!VHO_DSL_Shape_Refine_Program_Unit
             (pu, PU_Info_tree_ptr(pu), TRUE, stderr, &revalidated) ||
        revalidated.solver.refinable_value_count != 0 ||
        revalidated.requested_value_count != 0 ||
        revalidated.retyped_value_count != 0 ||
        !VHO_DSL_Shape_Refinement_Is_Current
             (pu, PU_Info_tree_ptr(pu), stderr) ||
        !Value_Type_Is(add, refined_ty) ||
        !Value_Type_Is(relu, refined_ty)) {
        fprintf(stderr, "SP7 shape revalidation changed\n");
        return 1;
    }

    static const VHO_DSL_SHAPE_TRIGGER current_triggers[] = {
        VHO_DSL_SHAPE_TRIGGER_DSL_WOPT,
        VHO_DSL_SHAPE_TRIGGER_FHE_CONVERSION,
        VHO_DSL_SHAPE_TRIGGER_VHO_DSL_OPTIMIZATION
    };
    for (UINT32 index = 0;
         index < sizeof(current_triggers) / sizeof(current_triggers[0]);
         ++index) {
        if (!VHO_DSL_Shape_Refinement_Invalidate
                 (pu, PU_Info_tree_ptr(pu), current_triggers[index], stderr) ||
            VHO_DSL_Shape_Refinement_Is_Current
                 (pu, PU_Info_tree_ptr(pu), NULL) ||
            !VHO_DSL_Shape_Refine_Program_Unit
                 (pu, PU_Info_tree_ptr(pu), TRUE, stderr, &revalidated) ||
            !VHO_DSL_Shape_Refinement_Is_Current
                 (pu, PU_Info_tree_ptr(pu), stderr)) {
            fprintf(stderr,
                    "SP10 current trigger did not revalidate: %s\n",
                    VHO_DSL_Shape_Trigger_Name(current_triggers[index]));
            return 1;
        }
    }

    const char *artifact = getenv("OPEN64_DSL_SHAPE_SP10_ARTIFACT");
    if (artifact == NULL || artifact[0] == '\0')
        artifact = getenv("OPEN64_DSL_SHAPE_SP7_ARTIFACT");
    if (artifact == NULL || artifact[0] == '\0')
        artifact = getenv("OPEN64_DSL_SHAPE_SP5_ARTIFACT");
    if (artifact != NULL && artifact[0] != '\0') {
        DSL_BUILDER_MAPPED_IMAGE_REQUEST image;
        image.path = artifact;
        image.flags = 0;
        (void)unlink(artifact);
        if (!DSL_Builder_Finalize_Mapped_Image(&image) ||
            access(artifact, F_OK) != 0) {
            fprintf(stderr, "SP5 mapped-image finalization failed\n");
            return 1;
        }
    }

    printf("SP7 shape refinement contract passed: refinable=%u "
           "retyped=%u reused_types=%u rollback=%u revalidated=%u\n",
           refined.solver.refinable_value_count,
           refined.retyped_value_count, refined.reused_type_count,
           forced_failure.rollback_count,
           revalidated.solver.unchanged_value_count);
    printf("SP10 shape trigger contract passed: triggers=%u "
           "unknown_rejected=1 stale_revalidated=1 "
           "current_triggers=3\n",
           (UINT32)VHO_DSL_SHAPE_TRIGGER_COUNT - 1);
    return 0;
}
