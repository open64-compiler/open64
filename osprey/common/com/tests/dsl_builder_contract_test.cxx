/*
 * Native contract test for the minimal DSL builder boundary.
 *
 * This deliberately exercises the future Python-facing construction surface
 * without adding Python bindings or backend lowering dependencies.
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "stab.h"
#include "ir_reader.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "dsl_builder.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

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

    Initialize_Symbol_Tables(FALSE);
    New_Scope(GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
}

static int
Check_Tensor_Type_And_Descriptor(void)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    TY_IDX tensor_ty;
    TENSOR_DESCRIPTOR_RECORD record;
    int failed = 0;

    memset(&type_core, 0, sizeof(type_core));
    memset(&descriptor, 0, sizeof(descriptor));

    type_core.kind = "tensor";
    type_core.dtype = "int32";
    type_core.rank = 2;
    type_core.logical_shape = "[2,2]";

    tensor_ty = DSL_Builder_Create_Tensor_Type_Core("builder_tensor_type",
                                                    MTYPE_To_TY(MTYPE_I4),
                                                    &type_core);

    if (TY_kind(tensor_ty) != KIND_TENSOR ||
        !TY_is_tensor_extension(tensor_ty) ||
        TY_tensor_element_ty(tensor_ty) != MTYPE_To_TY(MTYPE_I4) ||
        TY_tensor_rank(tensor_ty) != 2) {
        fprintf(stderr, "builder did not create a native tensor type core\n");
        failed = 1;
    }

    descriptor.type_core = type_core;
    descriptor.traits.traits = "dense";
    descriptor.representation.layout = "row_major";
    descriptor.representation.sharding = "replicated";
    descriptor.representation.placement = "host";
    descriptor.representation.memory = "contiguous";
    descriptor.representation.quantization = "none";
    descriptor.representation.runtime_state = "static";
    descriptor.lineage.lineage = "builder_contract_test";

    if (!DSL_Builder_Attach_Tensor_Descriptor(tensor_ty, &descriptor)) {
        fprintf(stderr, "builder failed to attach tensor descriptor\n");
        failed = 1;
    }

    if (!TY_tensor_attribute_is_bound(tensor_ty, TY_TENSOR_SCHEMA_DTYPE) ||
        strcmp(TY_tensor_attribute(tensor_ty, TY_TENSOR_SCHEMA_DTYPE),
               "int32") != 0 ||
        !TY_tensor_attribute_is_bound(tensor_ty, TY_TENSOR_SCHEMA_LAYOUT) ||
        strcmp(TY_tensor_attribute(tensor_ty, TY_TENSOR_SCHEMA_LAYOUT),
               "row_major") != 0 ||
        !TY_tensor_attribute_is_bound(tensor_ty, TY_TENSOR_SCHEMA_LINEAGE) ||
        strcmp(TY_tensor_attribute(tensor_ty, TY_TENSOR_SCHEMA_LINEAGE),
               "builder_contract_test") != 0) {
        fprintf(stderr, "builder tensor descriptor attributes changed\n");
        failed = 1;
    }

    TENSOR_DESCRIPTOR_RECORD_Init(&record);
    if (!TY_get_tensor_descriptor_record(tensor_ty, &record) ||
        record.rank != 2 ||
        record.attribute_count != TY_tensor_attribute_count(tensor_ty)) {
        fprintf(stderr, "builder tensor descriptor record is inconsistent\n");
        failed = 1;
    }

    if (DSL_Builder_Attach_Tensor_Descriptor(TY_IDX_ZERO, &descriptor)) {
        fprintf(stderr, "builder accepted descriptor for non-tensor type\n");
        failed = 1;
    }

    return failed;
}

static int
Check_Symbol_Metadata(void)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_COMPILER_METADATA metadata[2];
    TY_IDX tensor_ty;
    ST_IDX st;
    int failed = 0;

    memset(&type_core, 0, sizeof(type_core));
    type_core.kind = "tensor";
    type_core.dtype = "int32";
    type_core.rank = 2;
    type_core.logical_shape = "[2,2]";

    tensor_ty = DSL_Builder_Create_Tensor_Type_Core("builder_symbol_type",
                                                    MTYPE_To_TY(MTYPE_I4),
                                                    &type_core);
    st = DSL_Builder_Create_Symbol("builder_tensor", tensor_ty, CLASS_VAR,
                                   SCLASS_UGLOBAL, EXPORT_LOCAL);

    metadata[0].name = TY_tensor_schema_key_name
                           (TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME);
    metadata[0].value = "builder_contract_test";
    metadata[1].name = TY_tensor_schema_key_name
                           (TY_TENSOR_SCHEMA_LOWERING_HINT);
    metadata[1].value = "native_builder";

    if (!DSL_Builder_Attach_Metadata(st, metadata, 2)) {
        fprintf(stderr, "builder failed to attach compiler metadata\n");
        failed = 1;
    }

    if (!ST_tensor_metadata_is_bound(st, TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME) ||
        strcmp(ST_tensor_metadata(st, TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME),
               "builder_contract_test") != 0 ||
        !ST_tensor_metadata_is_bound(st, TY_TENSOR_SCHEMA_LOWERING_HINT) ||
        strcmp(ST_tensor_metadata(st, TY_TENSOR_SCHEMA_LOWERING_HINT),
               "native_builder") != 0) {
        fprintf(stderr, "builder compiler metadata changed\n");
        failed = 1;
    }

    if (DSL_Builder_Attach_Metadata(ST_IDX_ZERO, metadata, 2) ||
        DSL_Builder_Attach_Metadata(st, NULL, 1)) {
        fprintf(stderr, "builder accepted invalid metadata request\n");
        failed = 1;
    }

    return failed;
}

static int
Check_Operator_Creation(void)
{
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID add_id;
    DSL_BUILDER_VALUE kids[2];
    DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[1];
    DSL_BUILDER_OPERATOR op;
    DSL_OPCODE_ANNOTATION annotation;
    int failed = 0;

    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");
    add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);

    kids[0] = DSL_WN_Create_Tensor_Const("kid_lhs", "int32", 2, "[2,2]",
                                         "splat", "0");
    kids[1] = DSL_WN_Create_Tensor_Const("kid_rhs", "int32", 2, "[2,2]",
                                         "splat", "1");
    attrs[0].name = "attr.broadcast_rule";
    attrs[0].value = "none";

    op = DSL_Builder_Create_Operator(add_id, 1, kids, 2, attrs, 1);

    if (op == NULL || !DSL_WN_Get_Opcode_Annotation(op, &annotation)) {
        fprintf(stderr, "builder failed to create DSL operator marker\n");
        return 1;
    }

    if (annotation.name_len != strlen(DSL_OPCODE_COMMON_ADD) ||
        strncmp(annotation.name, DSL_OPCODE_COMMON_ADD,
                annotation.name_len) != 0 ||
        annotation.version != 1 ||
        strcmp(annotation.payload,
               "kid0=kid_lhs;kid1=kid_rhs;attr.broadcast_rule=none") != 0) {
        fprintf(stderr, "builder common.add annotation changed\n");
        failed = 1;
    }

    if (DSL_Builder_Create_Operator(DSL_OPCODE_INVALID_ID, 1,
                                    kids, 2, attrs, 1) != NULL) {
        fprintf(stderr, "builder accepted invalid opcode id\n");
        failed = 1;
    }

    return failed;
}

static int
Check_Mapped_Image_Finalizer(void)
{
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    int failed = 0;

    request.path = "builder_contract_test.B";
    request.flags = 0;

    (void) unlink(request.path);

    if (!DSL_Builder_Finalize_Mapped_Image(&request)) {
        fprintf(stderr, "mapped image finalizer failed valid request\n");
        failed = 1;
    }

    if (access(request.path, F_OK) != 0) {
        fprintf(stderr, "mapped image finalizer did not create output file\n");
        failed = 1;
    }

    if (DSL_Builder_Finalize_Mapped_Image(NULL)) {
        fprintf(stderr, "mapped image finalizer accepted null request\n");
        failed = 1;
    }

    request.path = NULL;
    if (DSL_Builder_Finalize_Mapped_Image(&request)) {
        fprintf(stderr, "mapped image finalizer accepted null path\n");
        failed = 1;
    }

    request.path = "builder_contract_test.B";
    request.flags = 1;
    if (DSL_Builder_Finalize_Mapped_Image(&request)) {
        fprintf(stderr, "mapped image finalizer accepted unknown flags\n");
        failed = 1;
    }

    (void) unlink("builder_contract_test.B");

    return failed;
}

int
main(void)
{
    int failed = 0;

    Initialize_Test_Context();

    failed |= Check_Tensor_Type_And_Descriptor();
    failed |= Check_Symbol_Metadata();
    failed |= Check_Operator_Creation();
    failed |= Check_Mapped_Image_Finalizer();

    return failed;
}
