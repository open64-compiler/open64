/*
 * Native contract test for the minimal DSL builder boundary.
 *
 * This deliberately exercises the future Python-facing construction surface
 * without adding Python bindings or backend lowering dependencies.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "wn_util.h"
#include "stab.h"
#include "elf_stuff.h"
#include <sys/elf_whirl.h>
#include "pu_info.h"
#include "ir_reader.h"
#include "ir_bread.h"
#include "ir_bwrite.h"
#include "glob.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "srcpos.h"
#include "dsl_builder.h"
#include "dsl_contract.h"
#include "dsl_gatekeeper.h"
#include "dsl_memory_behavior.h"

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

    Preconfigure();
    Init_Controls_Tbl();
    ABI_Name = "n64";
    Configure();
    IR_reader_init();
    Initialize_Symbol_Tables(TRUE);
    DST_Init(NULL, 0);
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
Check_Upgraded_Ingestion_APIs(void)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    DSL_BUILDER_TENSOR_DESCRIPTOR different_descriptor;
    DSL_BUILDER_TENSOR_DESCRIPTOR observed;
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    DSL_BUILDER_COMPILER_METADATA metadata;
    DSL_BUILDER_SOURCE_POSITION source_position;
    DSL_BUILDER_VERIFY_RESULT verify_result;
    DSL_BUILDER_VALUE kids[2];
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID add_id;
    TY_IDX tensor_ty;
    TY_IDX duplicate_ty;
    TY_IDX contextual_duplicate_ty;
    TY_IDX different_ty;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_VALUE add;
    ST_IDX add_st;
    UINT32 file_id;
    char diagnostic[1024];
    USRCPOS observed_position;
    int failed = 0;

    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.type_core.kind = "tensor";
    descriptor.type_core.dtype = "int32";
    descriptor.type_core.rank = 2;
    descriptor.type_core.logical_shape = "[2,2]";
    descriptor.traits.traits = "activation";
    descriptor.representation.layout = "row_major";
    descriptor.representation.sharding = "replicated";
    descriptor.representation.placement = "host";
    descriptor.representation.memory = "contiguous";
    descriptor.representation.quantization = "none";
    descriptor.representation.runtime_state = "static";
    descriptor.lineage.lineage = "canonical_contract";

    tensor_ty = DSL_Builder_Intern_Tensor_Type
                    ("canonical_tensor_a", MTYPE_To_TY(MTYPE_I4),
                     &descriptor);
    duplicate_ty = DSL_Builder_Intern_Tensor_Type
                       ("canonical_tensor_b", MTYPE_To_TY(MTYPE_I4),
                        &descriptor);
    different_descriptor = descriptor;
    different_descriptor.representation.runtime_state = "dynamic";
    different_descriptor.lineage.lineage = "different_value_lineage";
    contextual_duplicate_ty = DSL_Builder_Intern_Tensor_Type
                                  ("canonical_tensor_contextual",
                                   MTYPE_To_TY(MTYPE_I4),
                                   &different_descriptor);
    different_descriptor = descriptor;
    different_descriptor.type_core.logical_shape = "[4,2]";
    different_ty = DSL_Builder_Intern_Tensor_Type
                       ("canonical_tensor_c", MTYPE_To_TY(MTYPE_I4),
                        &different_descriptor);

    if (tensor_ty == TY_IDX_ZERO || duplicate_ty != tensor_ty ||
        contextual_duplicate_ty != tensor_ty ||
        different_ty == TY_IDX_ZERO || different_ty == tensor_ty ||
        !DSL_Builder_Tensor_Type_Is_Canonical(tensor_ty) ||
        !TY_tensor_attributes_are_equivalent(tensor_ty, duplicate_ty) ||
        TY_tensor_attributes_are_equivalent(tensor_ty, different_ty)) {
        fprintf(stderr, "canonical tensor interning contract changed\n");
        failed = 1;
    }

    memset(&observed, 0, sizeof(observed));
    if (!DSL_Builder_Get_Tensor_Descriptor(tensor_ty, &observed) ||
        observed.type_core.rank != 2 ||
        observed.type_core.logical_shape == NULL ||
        strcmp(observed.type_core.logical_shape, "[2,2]") != 0 ||
        observed.representation.runtime_state != NULL ||
        observed.lineage.lineage != NULL ||
        DSL_Builder_Attach_Tensor_Descriptor(tensor_ty, &descriptor)) {
        fprintf(stderr, "canonical tensor descriptor query changed\n");
        failed = 1;
    }

    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");
    add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);
    pu = DSL_Builder_Create_Minimal_PU("upgraded_ingestion_contract");
    file_id = DSL_Builder_Register_Source_File(pu, "model.py");

    kids[0] = DSL_Builder_Create_Model_Input("input0", tensor_ty, 0);
    kids[1] = DSL_Builder_Create_Model_Input("input1", tensor_ty, 1);
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    add = DSL_Builder_Create_Operator_With_Result
              (add_id, 1, kids, 2, &attribute, 1, "explicit_add", tensor_ty);
    add_st = DSL_Builder_Get_Value_Result_Symbol(add);

    metadata.name = "source_layer_name";
    metadata.value = "residual_add";
    memset(&source_position, 0, sizeof(source_position));
    source_position.file_id = file_id;
    source_position.line = 27;
    source_position.column = 9;
    source_position.statement_begin = 1;

    if (pu == NULL || file_id == 0 || kids[0] == NULL || kids[1] == NULL ||
        add == NULL || DSL_Builder_Get_Value_Type(add) != tensor_ty ||
        ST_IDX_index(add_st) == 0 ||
        !DSL_Builder_Attach_Value_Metadata(add, &metadata, 1) ||
        !DSL_Builder_Attach_Value_Lineage(add, "residual_path") ||
        !DSL_Builder_Set_Value_Source_Position(add, &source_position)) {
        fprintf(stderr, "upgraded value-oriented builder API failed\n");
        failed = 1;
    }

    observed_position.srcpos = WN_Get_Linenum(add);
    if (USRCPOS_filenum(observed_position) != file_id ||
        USRCPOS_linenum(observed_position) != 27 ||
        USRCPOS_column(observed_position) != 9 ||
        !USRCPOS_stmt_begin(observed_position) ||
        !ST_tensor_metadata_is_bound(add_st, "source_layer_name") ||
        strcmp(ST_tensor_metadata(add_st, "source_layer_name"),
               "residual_add") != 0 ||
        !ST_tensor_attribute_is_bound
             (add_st,
              TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_LINEAGE)) ||
        strcmp(ST_tensor_attribute
                   (add_st,
                    TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_LINEAGE)),
               "residual_path") != 0) {
        fprintf(stderr, "value source position or metadata changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Append_PU_Value(pu, kids[0]) ||
        !DSL_Builder_Append_PU_Value(pu, kids[1]) ||
        !DSL_Builder_Append_PU_Value(pu, add)) {
        fprintf(stderr, "upgraded program construction failed\n");
        failed = 1;
    }

    memset(&verify_result, 0, sizeof(verify_result));
    verify_result.diagnostic = diagnostic;
    verify_result.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify_result) ||
        verify_result.native_node_count != 3 ||
        verify_result.result_symbol_count != 3 ||
        verify_result.error_count != 0 || diagnostic[0] != '\0') {
        fprintf(stderr, "structured builder verification failed: %s\n",
                diagnostic);
        failed = 1;
    }

    DSL_Builder_Abort_Program();
    memset(&verify_result, 0, sizeof(verify_result));
    verify_result.diagnostic = diagnostic;
    verify_result.diagnostic_capacity = sizeof(diagnostic);
    if (DSL_Builder_Get_Value_Type(add) != TY_IDX_ZERO ||
        DSL_Builder_Verify_Program(&verify_result) ||
        verify_result.error_count != 1 ||
        strstr(diagnostic, "no program unit") == NULL) {
        fprintf(stderr, "builder abort did not isolate program state\n");
        failed = 1;
    }

    DSL_Builder_Begin_Program();
    pu = DSL_Builder_Create_Minimal_PU("second_ingestion_contract");
    kids[0] = DSL_Builder_Create_Model_Input("second_input", tensor_ty, 0);
    memset(&verify_result, 0, sizeof(verify_result));
    verify_result.diagnostic = diagnostic;
    verify_result.diagnostic_capacity = sizeof(diagnostic);
    if (pu == NULL || kids[0] == NULL ||
        !DSL_Builder_Append_PU_Value(pu, kids[0]) ||
        !DSL_Builder_Verify_Program(&verify_result) ||
        verify_result.native_node_count != 1 ||
        verify_result.result_symbol_count != 1 ||
        verify_result.error_count != 0) {
        fprintf(stderr, "second builder program inherited stale state\n");
        failed = 1;
    }
    DSL_Builder_Abort_Program();

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
    DSL_OPCODE_ID matmul_id;
    DSL_BUILDER_VALUE kids[2];
    DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[1];
    DSL_BUILDER_OPERATOR op;
    DSL_BUILDER_OPERATOR matmul;
    DSL_BUILDER_OPERATOR legacy_eval;
    DSL_OPCODE_ANNOTATION annotation;
    DSL_WHIRL_NODE_RECORD op_record;
    DSL_WHIRL_NODE_RECORD marker_record;
    DSL_WHIRL_NODE_RECORD recreated_record;
    DSL_WHIRL_NODE_RECORD projected_record;
    DSL_WHIRL_NODE_RECORD xpragma_record;
    DSL_WHIRL_OPERAND_RECORD operand_record;
    DSL_BUILDER_VALUE_INFO operand_info;
    int failed = 0;

    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");
    add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);
    matmul_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_MATMUL, 1);

    kids[0] = DSL_WN_Create_Tensor_Const("kid_lhs", "int32", 2, "[2,2]",
                                         "splat", "0");
    kids[1] = DSL_WN_Create_Tensor_Const("kid_rhs", "int32", 2, "[2,2]",
                                         "splat", "1");
    attrs[0].name = "attr.broadcast_rule";
    attrs[0].value = "none";

    op = DSL_Builder_Create_Operator(add_id, 1, kids, 2, attrs, 1);

    if (op == NULL || !DSL_WN_Get_Opcode_Annotation(op, &annotation)) {
        fprintf(stderr, "builder failed to create annotated DSL value\n");
        return 1;
    }

    if (WN_operator(op) == OPR_COMMENT ||
        !DSL_WN_Is_Opcode_Node(op) ||
        DSL_WN_Is_Opcode_Marker(op) ||
        DSL_WN_Opcode_Carrier_Kind(op) != DSL_OPCODE_CARRIER_XPRAGMA ||
        strcmp(DSL_WN_Opcode_Carrier_Name
                   (DSL_WN_Opcode_Carrier_Kind(op)),
               "xpragma_lda_string") != 0) {
        fprintf(stderr, "builder did not create a non-eval DSL node\n");
        failed = 1;
    }

    if (WN_operator(op) != OPR_XPRAGMA ||
        WN_kid_count(op) != 1 ||
        WN_kid0(op) == NULL ||
        WN_operator(WN_kid0(op)) != OPR_COMMA ||
        DSL_WN_Opcode_Operand_Count(op) != 2) {
        fprintf(stderr, "builder DSL node carrier shape changed\n");
        failed = 1;
    }

    WN *xpragma_operand_block =
        WN_kid0(op) != NULL && WN_operator(WN_kid0(op)) == OPR_COMMA ?
        WN_kid0(WN_kid0(op)) : NULL;
    if (xpragma_operand_block == NULL ||
        WN_operator(xpragma_operand_block) != OPR_BLOCK ||
        WN_first(xpragma_operand_block) == NULL ||
        WN_operator(WN_first(xpragma_operand_block)) != OPR_XPRAGMA ||
        WN_next(WN_first(xpragma_operand_block)) == NULL ||
        WN_operator(WN_next(WN_first(xpragma_operand_block))) != OPR_XPRAGMA) {
        fprintf(stderr, "migrated operand evidence still uses EVAL\n");
        failed = 1;
    }

    if (WN_operator(kids[0]) != OPR_XPRAGMA ||
        WN_operator(kids[1]) != OPR_XPRAGMA ||
        !DSL_WN_Is_Opcode_Node(kids[0]) ||
        !DSL_WN_Is_Opcode_Node(kids[1]) ||
        DSL_WN_Opcode_Carrier_Kind(kids[0]) !=
            DSL_OPCODE_CARRIER_XPRAGMA ||
        DSL_WN_Opcode_Carrier_Kind(kids[1]) !=
            DSL_OPCODE_CARRIER_XPRAGMA ||
        DSL_WN_Opcode_Operand_Count(kids[0]) != 0 ||
        DSL_WN_Opcode_Operand_Count(kids[1]) != 0) {
        fprintf(stderr, "builder tensor value carrier is not non-eval\n");
        failed = 1;
    }

    matmul = DSL_Builder_Create_Operator(matmul_id, 1, kids, 2, NULL, 0);
    if (matmul == NULL || WN_operator(matmul) != OPR_XPRAGMA ||
        DSL_WN_Opcode_Carrier_Kind(matmul) !=
            DSL_OPCODE_CARRIER_XPRAGMA ||
        !DSL_WN_Verify_Opcode_Carrier(matmul, NULL)) {
        fprintf(stderr, "builder matmul carrier is not non-eval\n");
        failed = 1;
    }

    WN *matmul_operand_block =
        matmul != NULL && WN_kid0(matmul) != NULL &&
        WN_operator(WN_kid0(matmul)) == OPR_COMMA ?
        WN_kid0(WN_kid0(matmul)) : NULL;
    if (matmul_operand_block == NULL ||
        WN_operator(matmul_operand_block) != OPR_BLOCK ||
        WN_first(matmul_operand_block) == NULL ||
        WN_operator(WN_first(matmul_operand_block)) != OPR_XPRAGMA) {
        fprintf(stderr, "builder matmul operand evidence still uses EVAL\n");
        failed = 1;
    }

    legacy_eval = DSL_WN_Create_Opcode_With_Operands
                      (DSL_OPCODE_COMMON_MATMUL, 1,
                       "kid0=kid_lhs;kid1=kid_rhs", kids, 2);
    WN *eval_operand_block =
        legacy_eval != NULL && WN_kid0(legacy_eval) != NULL &&
        WN_operator(WN_kid0(legacy_eval)) == OPR_COMMA ?
        WN_kid0(WN_kid0(legacy_eval)) : NULL;
    if (legacy_eval == NULL || WN_operator(legacy_eval) != OPR_EVAL ||
        DSL_WN_Opcode_Carrier_Kind(legacy_eval) != DSL_OPCODE_CARRIER_NODE ||
        eval_operand_block == NULL ||
        WN_operator(eval_operand_block) != OPR_BLOCK ||
        WN_first(eval_operand_block) == NULL ||
        WN_operator(WN_first(eval_operand_block)) != OPR_EVAL ||
        !DSL_WN_Decode_Opcode_Operand_Record
             (legacy_eval, 0, &operand_record)) {
        fprintf(stderr, "legacy EVAL operand evidence compatibility changed\n");
        failed = 1;
    }

    if (!DSL_WN_Verify_Opcode_Carrier(op, NULL) ||
        !DSL_WN_Verify_Opcode_Carrier(kids[0], NULL) ||
        !DSL_WN_Verify_Opcode_Carrier(kids[1], NULL)) {
        fprintf(stderr, "builder DSL opcode carrier verifier rejected valid node\n");
        failed = 1;
    }

    WN *bad_carrier = WN_Create(OPC_EVAL, 1);
    WN_kid0(bad_carrier) =
        WN_CreateIntconst(OPR_INTCONST, MTYPE_I4, MTYPE_V, 0);
    if (DSL_WN_Verify_Opcode_Carrier(bad_carrier, NULL)) {
        fprintf(stderr, "builder DSL opcode carrier verifier accepted malformed node\n");
        failed = 1;
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

    if (!DSL_WN_Decode_Opcode_Operand_Record(op, 0, &operand_record) ||
        operand_record.ordinal != 0 ||
        operand_record.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_record.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_record.opcode_name_len) != 0 ||
        operand_record.version != 1 ||
        strstr(operand_record.payload, "name=kid_lhs") == NULL) {
        fprintf(stderr, "builder DSL operand kid0 record changed\n");
        failed = 1;
    }

    if (!DSL_WN_Decode_Opcode_Operand_Record(op, 1, &operand_record) ||
        operand_record.ordinal != 1 ||
        operand_record.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_record.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_record.opcode_name_len) != 0 ||
        operand_record.version != 1 ||
        strstr(operand_record.payload, "name=kid_rhs") == NULL) {
        fprintf(stderr, "builder DSL operand kid1 record changed\n");
        failed = 1;
    }

    if (DSL_WN_Decode_Opcode_Operand_Record(op, 2, &operand_record)) {
        fprintf(stderr, "builder accepted invalid DSL operand record lookup\n");
        failed = 1;
    }

    if (DSL_Builder_Count_Value_Operands(op) != 2 ||
        DSL_Builder_Count_Value_Operands(kids[0]) != 0) {
        fprintf(stderr, "builder DSL operand count API changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Get_Value_Operand(op, 0, &operand_info) ||
        operand_info.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_info.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_info.opcode_name_len) != 0 ||
        operand_info.version != 1 ||
        strstr(operand_info.payload, "name=kid_lhs") == NULL) {
        fprintf(stderr, "builder DSL operand info API changed\n");
        failed = 1;
    }

    if (DSL_Builder_Get_Value_Operand(op, 2, &operand_info) ||
        DSL_Builder_Get_Value_Operand(kids[0], 0, &operand_info) ||
        DSL_Builder_Get_Value_Operand(op, 0, NULL)) {
        fprintf(stderr, "builder accepted invalid DSL operand info lookup\n");
        failed = 1;
    }

    if (DSL_Builder_Create_Operator(DSL_OPCODE_INVALID_ID, 1,
                                    kids, 2, attrs, 1) != NULL) {
        fprintf(stderr, "builder accepted invalid opcode id\n");
        failed = 1;
    }

    WN *compat_marker = DSL_WN_Create_Opcode_Marker
                            (DSL_OPCODE_COMMON_ADD, 1,
                             "kid0=kid_lhs;kid1=kid_rhs;attr.broadcast_rule=none");
    if (!DSL_WN_Is_Opcode_Marker(compat_marker) ||
        WN_operator(compat_marker) != OPR_COMMENT ||
        DSL_WN_Opcode_Carrier_Kind(compat_marker) !=
            DSL_OPCODE_CARRIER_MARKER ||
        !DSL_WN_Get_Opcode_Annotation(compat_marker, &annotation) ||
        annotation.name_len != strlen(DSL_OPCODE_COMMON_ADD)) {
        fprintf(stderr, "compatibility DSL marker no longer decodes\n");
        failed = 1;
    }

    if (!DSL_WN_Verify_Opcode_Carrier(compat_marker, NULL)) {
        fprintf(stderr, "compatibility DSL marker verifier rejected valid marker\n");
        failed = 1;
    }

    if (DSL_WN_Decode_Opcode_Operand_Record(compat_marker, 0,
                                            &operand_record)) {
        fprintf(stderr, "compatibility marker unexpectedly had operand record\n");
        failed = 1;
    }

    if (!DSL_WN_Decode_Opcode_Record(op, &op_record) ||
        !DSL_WN_Decode_Opcode_Record(compat_marker, &marker_record) ||
        !DSL_WN_Opcode_Records_Equivalent(&op_record, &marker_record)) {
        fprintf(stderr, "DSL node and compatibility marker records differ\n");
        failed = 1;
    }

    WN *xpragma_op = DSL_WN_Create_Opcode_Xpragma
                         (DSL_OPCODE_COMMON_ADD, 1,
                          "kid0=kid_lhs;kid1=kid_rhs;attr.broadcast_rule=none",
                          kids, 2);
    if (xpragma_op == NULL ||
        WN_operator(xpragma_op) != OPR_XPRAGMA ||
        WN_operator(xpragma_op) == OPR_EVAL ||
        !DSL_WN_Is_Opcode_Node(xpragma_op) ||
        DSL_WN_Is_Opcode_Marker(xpragma_op) ||
        DSL_WN_Opcode_Carrier_Kind(xpragma_op) !=
            DSL_OPCODE_CARRIER_XPRAGMA ||
        strcmp(DSL_WN_Opcode_Carrier_Name
                   (DSL_WN_Opcode_Carrier_Kind(xpragma_op)),
               "xpragma_lda_string") != 0 ||
        DSL_WN_Opcode_Operand_Count(xpragma_op) != 2 ||
        !DSL_WN_Verify_Opcode_Carrier(xpragma_op, NULL) ||
        !DSL_WN_Decode_Opcode_Record(xpragma_op, &xpragma_record) ||
        !DSL_WN_Opcode_Records_Equivalent(&op_record, &xpragma_record)) {
        fprintf(stderr, "non-eval DSL node carrier changed semantics\n");
        failed = 1;
    }

    if (!DSL_WN_Decode_Opcode_Operand_Record
            (xpragma_op, 1, &operand_record) ||
        operand_record.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_record.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_record.opcode_name_len) != 0 ||
        strstr(operand_record.payload, "name=kid_rhs") == NULL) {
        fprintf(stderr, "non-eval DSL node operand record changed\n");
        failed = 1;
    }

    FILE *xpragma_dump = tmpfile();
    if (xpragma_dump == NULL) {
        fprintf(stderr, "failed to create temporary xpragma dump file\n");
        failed = 1;
    } else {
        char text[4096];
        size_t count;

        fdump_tree(xpragma_dump, xpragma_op);
        rewind(xpragma_dump);
        count = fread(text, 1, sizeof(text) - 1, xpragma_dump);
        text[count] = '\0';
        fclose(xpragma_dump);

        if (strstr(text, "XPRAGMA") == NULL ||
            strstr(text, "EVAL") != NULL ||
            strstr(text, "dsl_node=common.add.v1") == NULL ||
            strstr(text, "dsl_carrier=xpragma_lda_string") == NULL ||
            strstr(text, "dsl_operand_count=2") == NULL ||
            strstr(text, "dsl_comment_projection=OPR_COMMENT") == NULL) {
            fprintf(stderr, "fdump_tree missing non-eval DSL node evidence\n");
            failed = 1;
        }
    }

    WN *recreated = DSL_WN_Create_Opcode_From_Record(&op_record);
    if (recreated == NULL ||
        !DSL_WN_Is_Opcode_Node(recreated) ||
        !DSL_WN_Decode_Opcode_Record(recreated, &recreated_record) ||
        !DSL_WN_Opcode_Records_Equivalent(&op_record, &recreated_record)) {
        fprintf(stderr, "DSL record recreate path changed semantics\n");
        failed = 1;
    }

    WN *projection = DSL_WN_Create_Opcode_Comment_Projection(op);
    if (projection == NULL ||
        WN_operator(projection) != OPR_COMMENT ||
        !DSL_WN_Is_Opcode_Marker(projection) ||
        DSL_WN_Opcode_Carrier_Kind(projection) !=
            DSL_OPCODE_CARRIER_MARKER ||
        !DSL_WN_Decode_Opcode_Record(projection, &projected_record) ||
        !DSL_WN_Opcode_Records_Equivalent(&op_record, &projected_record)) {
        fprintf(stderr, "DSL comment projection changed logical record\n");
        failed = 1;
    }

    DSL_LOGICAL_OPCODE logical_opcode;
    if (!DSL_WN_Get_Logical_Opcode(op, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLADD ||
        logical_opcode.source_version != 1 ||
        logical_opcode.effective_version != 1 ||
        logical_opcode.version_disposition != DSL_OPCODE_VERSION_EXACT ||
        logical_opcode.carrier != DSL_OPCODE_CARRIER_XPRAGMA ||
        DSL_Operator_Find_Current(DSL_OPCODE_COMMON_ADD,
                                  strlen(DSL_OPCODE_COMMON_ADD)) !=
            OPR_DSLADD) {
        fprintf(stderr, "logical opcode accessor rejected exact XPRAGMA input\n");
        failed = 1;
    }

    if (!DSL_WN_Get_Logical_Opcode(legacy_eval, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLMATMUL ||
        logical_opcode.carrier != DSL_OPCODE_CARRIER_NODE ||
        !DSL_WN_Get_Logical_Opcode(compat_marker, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLADD ||
        logical_opcode.carrier != DSL_OPCODE_CARRIER_MARKER) {
        fprintf(stderr, "legacy carriers do not share logical opcode access\n");
        failed = 1;
    }

    WN *older = DSL_WN_Create_Opcode_Marker
                    (DSL_OPCODE_COMMON_ADD, 0, "attr.broadcast_rule=none");
    if (DSL_WN_Get_Logical_Opcode(older, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLADD ||
        logical_opcode.source_version != 0 ||
        logical_opcode.effective_version != 1 ||
        logical_opcode.version_disposition !=
            DSL_OPCODE_VERSION_REJECTED_OLDER) {
        fprintf(stderr, "older DSL opcode version was not deterministically rejected\n");
        failed = 1;
    }

    WN *newer = DSL_WN_Create_Opcode
                    (DSL_OPCODE_COMMON_ADD, 2, "attr.broadcast_rule=none");
    if (DSL_WN_Get_Logical_Opcode(newer, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLADD ||
        logical_opcode.source_version != 2 ||
        logical_opcode.effective_version != 1 ||
        logical_opcode.version_disposition !=
            DSL_OPCODE_VERSION_REJECTED_NEWER) {
        fprintf(stderr, "newer DSL opcode version was not deterministically rejected\n");
        failed = 1;
    }

    WN *unknown = DSL_WN_Create_Opcode_Marker
                      ("example.unknown", 1, "");
    if (DSL_WN_Get_Logical_Opcode(unknown, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLUNKNOWN ||
        logical_opcode.version_disposition !=
            DSL_OPCODE_VERSION_REJECTED_UNKNOWN_OPERATOR) {
        fprintf(stderr, "unknown DSL opcode did not receive stable rejection\n");
        failed = 1;
    }

    const char *add_payload =
        "kid0=kid_lhs;kid1=kid_rhs;attr.broadcast_rule=none";
    WN *native_output = DSL_WN_Create_Logical_Opcode
                            (OPR_DSLADD, 1, add_payload, kids, 2,
                             DSL_OPCODE_OUTPUT_NATIVE);
    WN *eval_output = DSL_WN_Create_Logical_Opcode
                          (OPR_DSLADD, 1, add_payload, kids, 2,
                           DSL_OPCODE_OUTPUT_LEGACY_EVAL);
    WN *xpragma_output = DSL_WN_Create_Logical_Opcode
                             (OPR_DSLADD, 1, add_payload, kids, 2,
                              DSL_OPCODE_OUTPUT_LEGACY_XPRAGMA);
    WN *comment_output = DSL_WN_Create_Logical_Opcode
                             (OPR_DSLADD, 1, add_payload, kids, 2,
                              DSL_OPCODE_OUTPUT_COMMENT_PROJECTION);
    if (native_output == NULL || !DSL_WN_Is_Native(native_output) ||
        eval_output == NULL || WN_operator(eval_output) != OPR_EVAL ||
        DSL_WN_Opcode_Operand_Count(eval_output) != 2 ||
        xpragma_output == NULL || WN_operator(xpragma_output) != OPR_XPRAGMA ||
        DSL_WN_Opcode_Operand_Count(xpragma_output) != 2 ||
        comment_output == NULL || WN_operator(comment_output) != OPR_COMMENT ||
        !DSL_WN_Get_Logical_Opcode(native_output, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLADD ||
        strcmp(DSL_Opcode_Output_Mode_Name(DSL_OPCODE_OUTPUT_LEGACY_EVAL),
               "legacy_eval") != 0 ||
        strcmp(DSL_Opcode_Version_Disposition_Name
                   (DSL_OPCODE_VERSION_REJECTED_NEWER),
               "rejected_newer") != 0) {
        fprintf(stderr, "explicit DSL compatibility output mode failed\n");
        failed = 1;
    }

    if (DSL_WN_Create_Logical_Opcode
            (OPR_DSLADD, 2, add_payload, kids, 2,
             DSL_OPCODE_OUTPUT_LEGACY_EVAL) != NULL ||
        DSL_WN_Create_Logical_Opcode
            (OPR_DSLADD, 1, add_payload, kids, 1,
             DSL_OPCODE_OUTPUT_LEGACY_EVAL) != NULL ||
        DSL_WN_Create_Logical_Opcode
            (OPR_DSLADD, 1, add_payload, kids, 2,
             (DSL_OPCODE_OUTPUT_MODE)99) != NULL) {
        fprintf(stderr, "compatibility output accepted invalid policy request\n");
        failed = 1;
    }

    return failed;
}

static int
Check_Program_Unit_Value_Attach(void)
{
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID add_id;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_VALUE kids[2];
    DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[1];
    DSL_BUILDER_OPERATOR op;
    DSL_BUILDER_VALUE_INFO info;
    DSL_BUILDER_VALUE_INFO operand_info;
    int failed = 0;

    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");
    add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);

    kids[0] = DSL_WN_Create_Tensor_Const("pu_lhs", "int32", 2, "[2,2]",
                                         "splat", "0");
    kids[1] = DSL_WN_Create_Tensor_Const("pu_rhs", "int32", 2, "[2,2]",
                                         "splat", "1");
    attrs[0].name = "attr.broadcast_rule";
    attrs[0].value = "none";

    op = DSL_Builder_Create_Operator(add_id, 1, kids, 2, attrs, 1);
    pu = DSL_Builder_Create_Minimal_PU("builder_value_contract");

    if (!DSL_Builder_Append_PU_Value(pu, kids[0]) ||
        !DSL_Builder_Append_PU_Value(pu, op)) {
        fprintf(stderr, "builder failed to append DSL PU values\n");
        return 1;
    }

    if (DSL_Builder_Count_PU_Values(pu) != 2 ||
        DSL_Builder_Count_PU_Markers(pu) != 2) {
        fprintf(stderr, "builder PU value count changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Get_PU_Value(pu, 1, &info) ||
        info.opcode_name_len != strlen(DSL_OPCODE_COMMON_ADD) ||
        strncmp(info.opcode_name, DSL_OPCODE_COMMON_ADD,
                info.opcode_name_len) != 0 ||
        strcmp(info.payload,
               "kid0=pu_lhs;kid1=pu_rhs;attr.broadcast_rule=none") != 0) {
        fprintf(stderr, "builder PU value inspection changed\n");
        failed = 1;
    }

    if (DSL_Builder_Count_Value_Operands(op) != 2 ||
        !DSL_Builder_Get_Value_Operand(op, 1, &operand_info) ||
        operand_info.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_info.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_info.opcode_name_len) != 0 ||
        strstr(operand_info.payload, "name=pu_rhs") == NULL) {
        fprintf(stderr, "builder PU operand inspection changed\n");
        failed = 1;
    }

    FILE *dump = tmpfile();
    if (dump == NULL) {
        fprintf(stderr, "failed to create temporary dump file\n");
        failed = 1;
    } else {
        char text[4096];
        size_t count;

        fdump_tree(dump, op);
        rewind(dump);
        count = fread(text, 1, sizeof(text) - 1, dump);
        text[count] = '\0';
        fclose(dump);

        if (strstr(text, "XPRAGMA") == NULL ||
            strstr(text, "EVAL") != NULL ||
            strstr(text, "LDA") == NULL ||
            strstr(text, "dsl_node=common.add.v1") == NULL ||
            strstr(text, "dsl_carrier=xpragma_lda_string") == NULL ||
            strstr(text, "dsl_operand_count=2") == NULL ||
            strstr(text, "__WHIRL_DSL_OPERAND__:kid0") == NULL ||
            strstr(text, "__WHIRL_DSL_OPERAND__:kid1") == NULL ||
            strstr(text, "dsl_comment_projection=OPR_COMMENT") == NULL ||
            strstr(text, "__WHIRL_DSL__:opcode:common.add:v1") == NULL) {
            fprintf(stderr, "fdump_tree missing non-comment DSL node evidence\n");
            failed = 1;
        }
    }

    return failed;
}

static int
Check_Mapped_Image_Finalizer(void)
{
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    void *input_handle;
    OFFSET_AND_SIZE revision_section(0, 0);
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

    input_handle = Open_Input_Info((char *)request.path);
    revision_section = input_handle == NULL ||
        input_handle == (void *)-1 ? OFFSET_AND_SIZE(0, 0) :
        get_section(input_handle, SHT_PROGBITS, 0);
    if (input_handle == NULL || input_handle == (void *)-1 ||
        revision_section.offset == 0 ||
        strcmp((const char *)input_handle + revision_section.offset,
               WHIRL_REVISION) != 0 ||
        get_section(input_handle, SHT_MIPS_WHIRL,
                    WT_DSL_IR_IMAGE).offset != 0 ||
        WN_get_dsl_ir_image(input_handle) != 0 ||
        DSL_IR_Image_Has_Records()) {
        fprintf(stderr, "legacy mapped image unexpectedly has DSL tables\n");
        failed = 1;
    }
    if (input_handle != NULL && input_handle != (void *)-1)
        Free_Input_Info();

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

static int
Check_Production_Native_Builder(void)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_OPERATOR_ATTRIBUTE add_attr;
    DSL_BUILDER_OPERATOR_ATTRIBUTE matmul_attrs[2];
    DSL_BUILDER_VALUE kids[2];
    DSL_BUILDER_VALUE add;
    DSL_BUILDER_VALUE matmul;
    DSL_BUILDER_VALUE_INFO info;
    DSL_BUILDER_VALUE_INFO operand_info;
    DSL_OPCODE_ANNOTATION annotation;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID add_id;
    DSL_OPCODE_ID matmul_id;
    DSL_IR_IMAGE_HEADER header;
    TY_IDX tensor_ty;
    WN *add_expression;
    WN *matmul_expression;
    DSL_GATEKEEPER_RESULT gatekeeper_result;
    int failed = 0;

    memset(&type_core, 0, sizeof(type_core));
    type_core.kind = "tensor";
    type_core.dtype = "int32";
    type_core.rank = 2;
    type_core.logical_shape = "[2,2]";
    tensor_ty = DSL_Builder_Create_Tensor_Type_Core
                    ("production_native_tensor", MTYPE_To_TY(MTYPE_I4),
                     &type_core);

    kids[0] = DSL_Builder_Create_Tensor_Constant
                  ("native_zero", tensor_ty, "int32", 2, "[2,2]",
                   "splat", "0");
    kids[1] = DSL_Builder_Create_Tensor_Constant
                  ("native_one", tensor_ty, "int32", 2, "[2,2]",
                   "splat", "1");

    common_id = DSL_Domain_Find("common");
    add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);
    matmul_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_MATMUL, 1);
    add_attr.name = "attr.broadcast_rule";
    add_attr.value = "none";
    matmul_attrs[0].name = "attr.transpose_kid0";
    matmul_attrs[0].value = "false";
    matmul_attrs[1].name = "attr.transpose_kid1";
    matmul_attrs[1].value = "false";
    add = DSL_Builder_Create_Operator(add_id, 1, kids, 2, &add_attr, 1);
    matmul = DSL_Builder_Create_Operator
                 (matmul_id, 1, kids, 2, matmul_attrs, 2);

    if (kids[0] == NULL || kids[1] == NULL || add == NULL || matmul == NULL) {
        fprintf(stderr, "production native builder rejected valid values\n");
        return 1;
    }

    add_expression = WN_kid0(add);
    matmul_expression = WN_kid0(matmul);
    if (WN_operator(kids[0]) != OPR_STID ||
        WN_operator(kids[1]) != OPR_STID ||
        WN_operator(add) != OPR_STID ||
        WN_operator(matmul) != OPR_STID ||
        !DSL_WN_Is_Native(WN_kid0(kids[0])) ||
        DSL_WN_operator(WN_kid0(kids[0])) != OPR_DSLTENSORCONST ||
        !DSL_WN_Is_Native(add_expression) ||
        DSL_WN_operator(add_expression) != OPR_DSLADD ||
        !DSL_WN_Is_Native(matmul_expression) ||
        DSL_WN_operator(matmul_expression) != OPR_DSLMATMUL ||
        WN_kid_count(add_expression) != 2 ||
        WN_operator(WN_kid0(add_expression)) != OPR_LDID ||
        WN_operator(WN_kid1(add_expression)) != OPR_LDID ||
        WN_st_idx(WN_kid0(add_expression)) != WN_st_idx(kids[0]) ||
        WN_st_idx(WN_kid1(add_expression)) != WN_st_idx(kids[1])) {
        fprintf(stderr, "production native builder tree shape changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Tensor_Has_Unique_Ownership(WN_st_idx(kids[0])) ||
        !DSL_Builder_Tensor_Has_Unique_Ownership(WN_st_idx(kids[1])) ||
        !DSL_Builder_Tensor_Has_Unique_Ownership(WN_st_idx(add)) ||
        !DSL_Builder_Tensor_Has_Unique_Ownership(WN_st_idx(matmul)) ||
        !ST_is_temp_var(St_Table[WN_st_idx(add)]) ||
        WN_ty(add) != tensor_ty) {
        fprintf(stderr, "production native result ownership changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Get_Value_Operand(add, 1, &operand_info) ||
        operand_info.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_info.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_info.opcode_name_len) != 0 ||
        strstr(operand_info.payload, "name=native_one") == NULL ||
        !DSL_WN_Get_Opcode_Annotation(add_expression, &annotation)) {
        fprintf(stderr, "production native value inspection changed\n");
        failed = 1;
    }

    pu = DSL_Builder_Create_Minimal_PU("builder_value_contract");
    BOOL append_kid0 = DSL_Builder_Append_PU_Value(pu, kids[0]);
    BOOL append_kid1 = DSL_Builder_Append_PU_Value(pu, kids[1]);
    BOOL append_add = DSL_Builder_Append_PU_Value(pu, add);
    BOOL append_matmul = DSL_Builder_Append_PU_Value(pu, matmul);
    UINT32 pu_value_count = DSL_Builder_Count_PU_Values(pu);
    BOOL got_add = DSL_Builder_Get_PU_Value(pu, 4, &info);
    if (!append_kid0 || !append_kid1 || !append_add || !append_matmul ||
        pu_value_count != 6 || !got_add ||
        info.opcode_name_len != strlen(DSL_OPCODE_COMMON_ADD) ||
        strncmp(info.opcode_name, DSL_OPCODE_COMMON_ADD,
                info.opcode_name_len) != 0) {
        fprintf(stderr,
                "production native PU value API changed: append=%d/%d/%d/%d "
                "count=%u get=%d\n",
                append_kid0, append_kid1, append_add, append_matmul,
                pu_value_count, got_add);
        failed = 1;
    }

    FILE *dump = tmpfile();
    if (dump == NULL) {
        fprintf(stderr, "failed to create production native dump file\n");
        failed = 1;
    } else {
        char text[4096];
        size_t count;

        fdump_tree(dump, add);
        rewind(dump);
        count = fread(text, 1, sizeof(text) - 1, dump);
        text[count] = '\0';
        fclose(dump);
        if (strstr(text, "OPR_DSLADD") == NULL ||
            strstr(text, "dsl_comment_projection=OPR_COMMENT") == NULL ||
            strstr(text, "OPR_DSL ") != NULL ||
            strstr(text, "MDSL ") != NULL) {
            fprintf(stderr, "production native dump exposed storage detail\n");
            failed = 1;
        }
    }

    DSL_IR_Image_Get_Header(&header);
    if (header.opcode_descriptor_count != 3 || header.node_count != 4 ||
        header.attribute_count != 7 || header.value_count != 4 ||
        header.value_reference_count != 4 ||
        !DSL_IR_Image_Validate(stderr)) {
        fprintf(stderr, "production builder DSL image rows changed\n");
        failed = 1;
    }
    FILE *gatekeeper_dump = tmpfile();
    if (!DSL_Gatekeeper_Verify_Program
             (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.native_node_count != 4 ||
        gatekeeper_result.result_symbol_count != 4 ||
        gatekeeper_result.error_count != 0) {
        fprintf(stderr, "gatekeeper rejected a valid native program\n");
        failed = 1;
    }
    ST_tensor_bind_attribute
        (WN_st_idx(add),
         TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS), "false");
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted missing no-alias ownership\n");
        failed = 1;
    }
    ST_tensor_bind_attribute
        (WN_st_idx(add),
         TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS), "true");

    DSL_BUILDER_TENSOR_TYPE_CORE incompatible_core = type_core;
    incompatible_core.dtype = "int64";
    TY_IDX incompatible_ty = DSL_Builder_Create_Tensor_Type_Core
                                 ("incompatible_native_tensor",
                                  MTYPE_To_TY(MTYPE_I8), &incompatible_core);
    TY_IDX saved_operand_ty = WN_ty(WN_kid1(add_expression));
    WN_set_ty(WN_kid1(add_expression), incompatible_ty);
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted incompatible tensor operands\n");
        failed = 1;
    }
    WN_set_ty(WN_kid1(add_expression), saved_operand_ty);

    WN *saved_operand = WN_kid1(add_expression);
    WN_kid1(add_expression) = WN_Intconst(MTYPE_I4, 1);
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted a non-LDID native operand\n");
        failed = 1;
    }
    WN_kid1(add_expression) = saved_operand;

    WN_OFFSET saved_record = WN_offset(add_expression);
    WN_offset(add_expression) = Save_Str
        ("__WHIRL_DSL__:opcode:common.add:v2:attr.broadcast_rule=none");
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted an unsupported operator version\n");
        failed = 1;
    }
    WN_offset(add_expression) = saved_record;

    saved_record = WN_offset(matmul_expression);
    WN_offset(matmul_expression) = Save_Str
        ("__WHIRL_DSL__:opcode:common.matmul:v1:");
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted missing typed attributes\n");
        failed = 1;
    }
    WN_offset(matmul_expression) = saved_record;

    WN *body = WN_func_body(PU_Info_tree_ptr(pu));
    WN *escape = WN_CreateEval
                     (WN_Lda(Pointer_Mtype, 0,
                             &St_Table[WN_st_idx(add)], 0));
    WN_INSERT_BlockLast(body, escape);
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted a tensor-result address escape\n");
        failed = 1;
    }
    WN_EXTRACT_FromBlock(body, escape);

    if (gatekeeper_dump != NULL)
        fclose(gatekeeper_dump);

    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    const char *retained_artifact = getenv("OPEN64_DSL_TEST_ARTIFACT");
    request.path = retained_artifact == NULL || retained_artifact[0] == '\0' ?
                   "gatekeeper_contract_test.B" : retained_artifact;
    request.flags = 0;
    (void) unlink(request.path);
    if (!DSL_Builder_Finalize_Mapped_Image(&request) ||
        access(request.path, F_OK) != 0) {
        fprintf(stderr, "gatekeeper blocked valid mapped-image output\n");
        failed = 1;
    }
    if (retained_artifact == NULL || retained_artifact[0] == '\0')
        (void) unlink(request.path);

    return failed;
}

static int
Check_DSL_IR_Image_Tables(void)
{
    DSL_OPERATOR_INFO logical_info;
    DSL_IR_IMAGE_HEADER header;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD saved_descriptor;
    DSL_IR_NODE_RECORD node;
    DSL_IR_NODE_RECORD saved_node;
    DSL_IR_ATTRIBUTE_RECORD attribute;
    DSL_IR_ATTRIBUTE_RECORD saved_attribute;
    DSL_IR_VALUE_RECORD value;
    DSL_IR_VALUE_RECORD saved_value;
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    DSL_IR_VALUE_REFERENCE_RECORD saved_reference;
    DSL_IR_OPCODE_DESCRIPTOR_ID descriptor_id;
    DSL_IR_NODE_ID node_id;
    DSL_IR_ATTRIBUTE_ID attribute_id;
    DSL_IR_VALUE_ID kid0_value_id;
    DSL_IR_VALUE_ID kid1_value_id;
    DSL_IR_VALUE_ID result_value_id;
    DSL_IR_VALUE_REFERENCE_ID kid0_reference_id;
    DSL_IR_VALUE_REFERENCE_ID kid1_reference_id;
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    void *input_handle;
    OFFSET_AND_SIZE revision_section(0, 0);
    int failed = 0;

    DSL_IR_Image_Reset();
    DSL_IR_Image_Get_Header(&header);
    if (header.version != DSL_IR_IMAGE_VERSION ||
        header.record_kind_count != DSL_IR_IMAGE_RECORD_VALUE_REFERENCE ||
        header.opcode_descriptor_count != 0 ||
        header.node_count != 0 ||
        header.attribute_count != 0 ||
        header.value_count != 0 ||
        header.value_reference_count != 0 ||
        (header.capabilities & DSL_IR_IMAGE_CAP_OPCODE_DESCRIPTOR) == 0 ||
        (header.capabilities & DSL_IR_IMAGE_CAP_NODE) == 0 ||
        (header.capabilities & DSL_IR_IMAGE_CAP_TYPED_ATTRIBUTE) == 0 ||
        (header.capabilities & DSL_IR_IMAGE_CAP_VALUE) == 0 ||
        (header.capabilities & DSL_IR_IMAGE_CAP_VALUE_REFERENCE) == 0) {
        fprintf(stderr, "DSL image header initialization changed\n");
        failed = 1;
    }

    if (!DSL_Operator_Get_Info(OPR_DSLADD, &logical_info)) {
        fprintf(stderr, "common.add logical descriptor is unavailable\n");
        return 1;
    }

    DSL_IR_Opcode_Descriptor_Record_Init(&descriptor);
    descriptor.logical_operator = logical_info.dsl_operator;
    descriptor.version = logical_info.version;
    descriptor.operand_count = logical_info.nkids;
    descriptor.category = logical_info.category;
    descriptor.level = logical_info.level;
    descriptor.shape_rule = logical_info.shape_rule;
    descriptor.effect_model = logical_info.effect_model;
    descriptor.lowering_model = logical_info.lowering_model;
    descriptor.logical_name = Save_Str(logical_info.logical_name);
    descriptor.stable_name = Save_Str(logical_info.stable_name);
    descriptor.attribute_schema = Save_Str(logical_info.attribute_schema);
    descriptor.diagnostic_prefix = Save_Str(logical_info.diagnostic_prefix);
    descriptor_id = DSL_IR_Image_Add_Opcode_Descriptor(&descriptor);

    DSL_IR_Node_Record_Init(&node);
    node.opcode_descriptor_id = descriptor_id;
    node.payload = Save_Str("attr.broadcast_rule=none");
    node_id = DSL_IR_Image_Add_Node(&node);

    DSL_IR_Value_Record_Init(&value);
    value.value_kind = DSL_IR_VALUE_CONSTANT;
    value.ty = MTYPE_To_TY(MTYPE_I4);
    value.name = Save_Str("kid0");
    kid0_value_id = DSL_IR_Image_Add_Value(&value);
    value.name = Save_Str("kid1");
    kid1_value_id = DSL_IR_Image_Add_Value(&value);
    value.value_kind = DSL_IR_VALUE_OPERATOR_RESULT;
    value.producer_node_id = node_id;
    value.name = Save_Str("result");
    result_value_id = DSL_IR_Image_Add_Value(&value);

    DSL_IR_Value_Reference_Record_Init(&reference);
    reference.owner_node_id = node_id;
    reference.ordinal = 0;
    reference.value_id = kid0_value_id;
    kid0_reference_id = DSL_IR_Image_Add_Value_Reference(&reference);
    reference.ordinal = 1;
    reference.value_id = kid1_value_id;
    kid1_reference_id = DSL_IR_Image_Add_Value_Reference(&reference);

    DSL_IR_Attribute_Record_Init(&attribute);
    attribute.owner_node_id = node_id;
    attribute.value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
    attribute.name = Save_Str("attr.broadcast_rule");
    attribute.value = Save_Str("none");
    attribute_id = DSL_IR_Image_Add_Attribute(&attribute);

    if (!DSL_IR_Image_Set_Node_Links
            (node_id, kid0_reference_id, 2, attribute_id, 1,
             result_value_id)) {
        fprintf(stderr, "DSL image node links were rejected\n");
        failed = 1;
    }

    DSL_IR_Image_Get_Header(&header);
    if (descriptor_id != 1 || node_id != 1 || attribute_id != 1 ||
        kid0_value_id != 1 || kid1_value_id != 2 || result_value_id != 3 ||
        kid0_reference_id != 1 || kid1_reference_id != 2 ||
        header.opcode_descriptor_count != 1 || header.node_count != 1 ||
        header.attribute_count != 1 || header.value_count != 3 ||
        header.value_reference_count != 2 ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (descriptor_id, &saved_descriptor) ||
        saved_descriptor.id != descriptor_id ||
        strcmp(Index_To_Str(saved_descriptor.stable_name), "common.add") != 0 ||
        !DSL_IR_Image_Get_Node(node_id, &saved_node) ||
        saved_node.first_operand_reference_id != kid0_reference_id ||
        saved_node.operand_count != 2 ||
        saved_node.first_attribute_id != attribute_id ||
        saved_node.attribute_count != 1 ||
        saved_node.result_value_id != result_value_id ||
        !DSL_IR_Image_Get_Attribute(attribute_id, &saved_attribute) ||
        saved_attribute.value_kind != DSL_IR_ATTRIBUTE_VALUE_STRING ||
        !DSL_IR_Image_Get_Value(result_value_id, &saved_value) ||
        saved_value.producer_node_id != node_id ||
        !DSL_IR_Image_Get_Value_Reference
             (kid1_reference_id, &saved_reference) ||
        saved_reference.ordinal != 1 ||
        saved_reference.value_id != kid1_value_id) {
        fprintf(stderr, "DSL fixed-row image table contract changed\n");
        failed = 1;
    }

    DSL_IR_Opcode_Descriptor_Record_Init(&descriptor);
    DSL_IR_Node_Record_Init(&node);
    node.opcode_descriptor_id = 99;
    DSL_IR_Attribute_Record_Init(&attribute);
    attribute.owner_node_id = node_id;
    attribute.name = Save_Str("invalid");
    DSL_IR_Value_Reference_Record_Init(&reference);
    reference.owner_node_id = node_id;
    reference.value_id = 99;
    if (DSL_IR_Image_Add_Opcode_Descriptor(&descriptor) !=
            DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID ||
        DSL_IR_Image_Add_Node(&node) != DSL_IR_NODE_INVALID_ID ||
        DSL_IR_Image_Add_Attribute(&attribute) !=
            DSL_IR_ATTRIBUTE_INVALID_ID ||
        DSL_IR_Image_Add_Value_Reference(&reference) !=
            DSL_IR_VALUE_REFERENCE_INVALID_ID ||
        DSL_IR_Image_Set_Node_Links
            (node_id, kid0_reference_id, 1, attribute_id, 1,
             result_value_id) ||
        DSL_IR_Image_Get_Node(99, NULL)) {
        fprintf(stderr, "DSL image tables accepted an invalid row or range\n");
        failed = 1;
    }

    request.path = "dsl_ir_image_contract_test.B";
    request.flags = 0;
    (void) unlink(request.path);
    Irb_File_Name = (char *)request.path;
    if (Open_Output_Info(Irb_File_Name) == NULL) {
        fprintf(stderr, "DSL image mapped output failed\n");
        failed = 1;
    } else {
        Write_Global_Info(NULL);
        Close_Output_Info();
    }

    DSL_IR_Image_Reset();
    input_handle = Open_Input_Info((char *)request.path);
    revision_section = input_handle == NULL || input_handle == (void *)-1 ?
        OFFSET_AND_SIZE(0, 0) : get_section(input_handle, SHT_PROGBITS, 0);
    if (input_handle == NULL || input_handle == (void *)-1 ||
        revision_section.offset == 0 ||
        strcmp((const char *)input_handle + revision_section.offset,
               WHIRL_DSL_REVISION) != 0 ||
        get_section(input_handle, SHT_MIPS_WHIRL,
                    WT_DSL_IR_IMAGE).offset == 0 ||
        WN_get_dsl_ir_image(input_handle) != 0 ||
        DSL_IR_Image_Opcode_Descriptor_Count() != 1 ||
        DSL_IR_Image_Node_Count() != 1 ||
        DSL_IR_Image_Attribute_Count() != 1 ||
        DSL_IR_Image_Value_Count() != 3 ||
        DSL_IR_Image_Value_Reference_Count() != 2 ||
        !DSL_IR_Image_Get_Node(node_id, &saved_node) ||
        saved_node.result_value_id != result_value_id) {
        fprintf(stderr, "DSL image mapped round trip changed\n");
        failed = 1;
    }
    if (input_handle != NULL && input_handle != (void *)-1)
        Free_Input_Info();
    (void) unlink(request.path);

    DSL_IR_IMAGE_HEADER malformed_header;
    memset(&malformed_header, 0, sizeof(malformed_header));
    malformed_header.version = DSL_IR_IMAGE_VERSION + 1;
    malformed_header.record_kind_count =
        DSL_IR_IMAGE_RECORD_VALUE_REFERENCE;
    malformed_header.capabilities =
        DSL_IR_IMAGE_CAP_OPCODE_DESCRIPTOR |
        DSL_IR_IMAGE_CAP_NODE |
        DSL_IR_IMAGE_CAP_TYPED_ATTRIBUTE |
        DSL_IR_IMAGE_CAP_VALUE |
        DSL_IR_IMAGE_CAP_VALUE_REFERENCE;
    if (DSL_IR_Image_Load_Mapped
            (&malformed_header, sizeof(malformed_header), NULL)) {
        fprintf(stderr, "DSL image accepted an unknown version\n");
        failed = 1;
    }

    DSL_IR_Image_Get_Header(&header);
    if (header.opcode_descriptor_count != 0 || header.node_count != 0 ||
        header.attribute_count != 0 || header.value_count != 0 ||
        header.value_reference_count != 0 ||
        DSL_IR_Image_Get_Node(node_id, NULL)) {
        fprintf(stderr, "DSL image table reset changed\n");
        failed = 1;
    }

    return failed;
}

static int
Check_Native_DSL_Node_Layout(void)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_OPERATOR_INFO operator_info;
    WN *kids[2];
    WN *node;
    WN *assignment;
    TY_IDX tensor_ty;
    ST_IDX temp_st;
    int failed = 0;

    if (!DSL_Operator_Get_Info(OPR_DSLADD, &operator_info) ||
        operator_info.dsl_operator != OPR_DSLADD ||
        strcmp(operator_info.logical_name, "OPR_DSLADD") != 0 ||
        strcmp(operator_info.stable_name, "common.add") != 0 ||
        operator_info.version != 1 ||
        operator_info.category != DSL_OPCODE_CATEGORY_EXECUTABLE ||
        operator_info.level != DSL_OPCODE_LEVEL_2_NUMERIC ||
        operator_info.nkids != 2 ||
        operator_info.shape_rule != DSL_SHAPE_RULE_BROADCAST ||
        operator_info.effect_model != DSL_EFFECT_MODEL_PURE ||
        operator_info.lowering_model != DSL_LOWERING_MODEL_RUNTIME_CALL ||
        strcmp(operator_info.diagnostic_prefix, "DOPC_COMMON_ADD") != 0 ||
        strcmp(operator_info.attribute_schema,
               "attr.broadcast_rule") != 0 ||
        DSL_Operator_Find("common.add", strlen("common.add"), 1) !=
            OPR_DSLADD ||
        DSL_Operator_Find("common.add", strlen("common.add"), 2) !=
            OPR_DSLUNKNOWN ||
        DSL_Operator_Get_Info(OPR_DSLUNKNOWN, NULL) ||
        strcmp(DSL_OPERATOR_name((DSL_OPERATOR)99),
               "OPR_DSLUNKNOWN") != 0) {
        fprintf(stderr, "logical DSL operator descriptor changed\n");
        failed = 1;
    }

    memset(&type_core, 0, sizeof(type_core));
    type_core.kind = "tensor";
    type_core.dtype = "int32";
    type_core.rank = 2;
    type_core.logical_shape = "[2,2]";

    tensor_ty = DSL_Builder_Create_Tensor_Type_Core
                    ("native_dsl_result_type", MTYPE_To_TY(MTYPE_I4),
                     &type_core);
    temp_st = DSL_Builder_Create_Tensor_Result_Symbol
                  ("native_dsl_temp", tensor_ty, SCLASS_AUTO, EXPORT_LOCAL);
    if (ST_IDX_index(temp_st) == 0) {
        fprintf(stderr, "native DSL result symbol was not created\n");
        failed = 1;
    }

    kids[0] = WN_Intconst(MTYPE_I4, 0);
    kids[1] = WN_Intconst(MTYPE_I4, 1);
    node = DSL_WN_Create_Native
               (OPR_DSLADD, 1, "attr.broadcast_rule=none", kids, 2);
    assignment = WN_CreateStid(OPR_STID, MTYPE_V, MTYPE_M, 0, temp_st,
                               tensor_ty, node);

#ifdef TARG_X8664
    if (OPR_ZDLBR != 147 || OPR_DSL != 148) {
        fprintf(stderr, "native DSL opcode renumbered an existing operator\n");
        failed = 1;
    }
#endif

    if (OPERATOR_nkids(OPR_DSL) != -1 ||
        !OPERATOR_is_expression(OPR_DSL) ||
        OPERATOR_is_stmt(OPR_DSL) ||
        OPERATOR_has_next_prev(OPR_DSL) ||
        OPERATOR_has_sym(OPR_DSL) ||
        !OPERATOR_has_offset(OPR_DSL) ||
        !OPERATOR_is_not_executable(OPR_DSL)) {
        fprintf(stderr, "native DSL operator properties changed\n");
        failed = 1;
    }

    if (node == NULL ||
        WN_operator(node) != OPR_DSL ||
        WN_operator(node) == OPR_COMMENT ||
        WN_operator(node) == OPR_EVAL ||
        !DSL_WN_Is_Native(node) ||
        DSL_WN_operator(node) != OPR_DSLADD ||
        strcmp(DSL_OPERATOR_name(DSL_WN_operator(node)),
               "OPR_DSLADD") != 0 ||
        DSL_WN_Opcode_Carrier_Kind(node) != DSL_OPCODE_CARRIER_NATIVE ||
        WN_rtype(node) != MTYPE_M ||
        WN_desc(node) != MTYPE_V ||
        WN_kid_count(node) != 2 ||
        WN_kid0(node) != kids[0] ||
        WN_kid1(node) != kids[1]) {
        fprintf(stderr, "native DSL WN layout changed\n");
        failed = 1;
    }

    FILE *dump = tmpfile();
    if (dump == NULL) {
        fprintf(stderr, "failed to create native DSL dump file\n");
        failed = 1;
    } else {
        char text[4096];
        size_t count;

        fdump_tree(dump, node);
        rewind(dump);
        count = fread(text, 1, sizeof(text) - 1, dump);
        text[count] = '\0';
        fclose(dump);

        if (strstr(text, "OPR_DSLADD") == NULL ||
            strstr(text, "version=1") == NULL ||
            strstr(text, "payload=attr.broadcast_rule=none") == NULL ||
            strstr(text, "OPR_DSL ") != NULL ||
            strstr(text, "MDSL ") != NULL ||
            strstr(text, "dsl_carrier=native") != NULL) {
            fprintf(stderr, "native DSL dump exposed escape representation\n");
            failed = 1;
        }
    }

    if (assignment == NULL ||
        WN_operator(assignment) != OPR_STID ||
        WN_st_idx(assignment) != temp_st ||
        WN_ty(assignment) != tensor_ty ||
        WN_kid0(assignment) != node) {
        fprintf(stderr, "native DSL expression assignment shape changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Tensor_Has_Unique_Ownership(temp_st) ||
        ST_class(St_Table[temp_st]) != CLASS_VAR ||
        !ST_is_temp_var(St_Table[temp_st]) ||
        !TY_is_tensor_extension(ST_type(St_Table[temp_st])) ||
        !ST_tensor_attribute_is_bound
             (temp_st,
              TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS)) ||
        strcmp(ST_tensor_attribute
                   (temp_st,
                    TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS)),
               "true") != 0 ||
        ST_pt_to_unique_mem(St_Table[temp_st])) {
        fprintf(stderr, "native DSL result no-alias contract changed\n");
        failed = 1;
    }

    if (DSL_WN_Create_Native
            (OPR_DSLUNKNOWN, 1, NULL, NULL, 0) != NULL ||
        DSL_WN_Create_Native(OPR_DSLADD, 0, NULL, NULL, 0) != NULL ||
        DSL_WN_Create_Native(OPR_DSLADD, 2, NULL, kids, 2) != NULL ||
        DSL_WN_Create_Native(OPR_DSLADD, 1, NULL, kids, 1) != NULL ||
        DSL_WN_Create_Native(OPR_DSLADD, 1, NULL, NULL, 1) != NULL) {
        fprintf(stderr, "native DSL WN accepted an invalid record\n");
        failed = 1;
    }

    return failed;
}

static int
Check_Llama2_Common_Substrate(void)
{
    DSL_OPERATOR_INFO current_info;
    DSL_OPERATOR_INFO exact_info;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID reshape_id;
    DSL_OPCODE_ID transpose_id;
    DSL_OPCODE_ID linear_v2_id;
    DSL_OPCODE_ID linear_v3_id;
    DSL_OPCODE_ID matmul_v2_id;
    DSL_OPCODE_ID output_v3_id;
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_VALUE values[10];
    UINT32 source_lines[10];
    DSL_BUILDER_VALUE kids[2];
    DSL_BUILDER_OPERATOR_ATTRIBUTE linear_attrs[4];
    DSL_BUILDER_OPERATOR_ATTRIBUTE reshape_attr;
    DSL_BUILDER_OPERATOR_ATTRIBUTE transpose_attr;
    DSL_BUILDER_OPERATOR_ATTRIBUTE matmul_attrs[4];
    DSL_BUILDER_OPERATOR_ATTRIBUTE output_attrs[3];
    DSL_BUILDER_VERIFY_RESULT verify;
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    TY_IDX activation_ty;
    TY_IDX weight_ty;
    TY_IDX attention_ty;
    TY_IDX logits_ty;
    UINT32 file_id;
    char diagnostic[2048];
    int failed = 0;

    if (!DSL_Builder_Begin_Program())
        return 1;
    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");
    reshape_id = DSL_Opcode_Find(common_id, "common.reshape", 1);
    transpose_id = DSL_Opcode_Find(common_id, "common.transpose", 1);
    linear_v2_id = DSL_Opcode_Find(common_id, "common.linear", 2);
    linear_v3_id = DSL_Opcode_Find(common_id, "common.linear", 3);
    matmul_v2_id = DSL_Opcode_Find(common_id, "common.matmul", 2);
    output_v3_id = DSL_Opcode_Find(common_id, "common.output_logits", 3);
    if (reshape_id == DSL_OPCODE_INVALID_ID ||
        transpose_id == DSL_OPCODE_INVALID_ID ||
        linear_v2_id == DSL_OPCODE_INVALID_ID ||
        linear_v3_id == DSL_OPCODE_INVALID_ID ||
        matmul_v2_id == DSL_OPCODE_INVALID_ID ||
        output_v3_id == DSL_OPCODE_INVALID_ID ||
        (UINT32)OPR_DSLRESHAPE != 14 ||
        (UINT32)OPR_DSLTRANSPOSE != 15 ||
        !DSL_Operator_Get_Info(OPR_DSLLINEAR, &current_info) ||
        current_info.version != 3 || current_info.nkids != 2 ||
        !DSL_Operator_Get_Info_Version(OPR_DSLLINEAR, 2, &exact_info) ||
        exact_info.version != 2 || exact_info.nkids != 3 ||
        !DSL_Operator_Get_Info_Version(OPR_DSLMATMUL, 1, &exact_info) ||
        exact_info.version != 1 ||
        !DSL_Operator_Get_Info_Version(OPR_DSLMATMUL, 2, &exact_info) ||
        exact_info.version != 2 ||
        !DSL_Operator_Get_Info_Version
             (OPR_DSLOUTPUTLOGITS, 2, &exact_info) ||
        !DSL_Operator_Get_Info_Version
             (OPR_DSLOUTPUTLOGITS, 3, &current_info) ||
        DSL_Operator_Get_Info_Version(OPR_DSLLINEAR, 1, NULL)) {
        fprintf(stderr, "item-23 exact operator schema lookup changed\n");
        return 1;
    }

    memset(&type_core, 0, sizeof(type_core));
    type_core.kind = "tensor";
    type_core.dtype = "float32";
    type_core.rank = 3;
    type_core.logical_shape = "[1,8,32]";
    activation_ty = DSL_Builder_Create_Tensor_Type_Core
                        ("llama_activation", MTYPE_To_TY(MTYPE_F4),
                         &type_core);
    type_core.rank = 2;
    type_core.logical_shape = "[64,32]";
    weight_ty = DSL_Builder_Create_Tensor_Type_Core
                    ("llama_weight", MTYPE_To_TY(MTYPE_F4), &type_core);
    type_core.rank = 4;
    type_core.logical_shape = "[1,4,8,8]";
    attention_ty = DSL_Builder_Create_Tensor_Type_Core
                       ("llama_attention", MTYPE_To_TY(MTYPE_F4),
                        &type_core);
    type_core.rank = 3;
    type_core.logical_shape = "[1,8,128]";
    logits_ty = DSL_Builder_Create_Tensor_Type_Core
                    ("llama_logits", MTYPE_To_TY(MTYPE_F4), &type_core);

    pu = DSL_Builder_Create_Minimal_PU("llama2_common_substrate");
    file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
    source_lines[0] = __LINE__ + 1;
    values[0] = DSL_Builder_Create_Model_Input
                    ("llama_activation_input", activation_ty, 0);
    source_lines[1] = __LINE__ + 1;
    values[1] = DSL_Builder_Create_Tensor_Constant
                    ("llama_projection_weight", weight_ty, "float32", 2,
                     "[64,32]", "splat", "1");
    source_lines[2] = __LINE__ + 1;
    values[2] = DSL_Builder_Create_Model_Input
                    ("llama_query", attention_ty, 1);
    source_lines[3] = __LINE__ + 1;
    values[3] = DSL_Builder_Create_Model_Input
                    ("llama_key", attention_ty, 2);
    source_lines[4] = __LINE__ + 1;
    values[4] = DSL_Builder_Create_Model_Input
                    ("llama_logits_input", logits_ty, 3);

    linear_attrs[0].name = "attr.has_bias";
    linear_attrs[0].value = "false";
    linear_attrs[1].name = "attr.transpose_input";
    linear_attrs[1].value = "false";
    linear_attrs[2].name = "attr.transpose_weight";
    linear_attrs[2].value = "true";
    linear_attrs[3].name = "attr.weight_layout";
    linear_attrs[3].value = "OI";
    kids[0] = values[0];
    kids[1] = values[1];
    source_lines[5] = __LINE__ + 1;
    values[5] = DSL_Builder_Create_Operator
                    (linear_v3_id, 3, kids, 2, linear_attrs, 4);

    reshape_attr.name = "attr.target_shape";
    reshape_attr.value = "1,8,4,16";
    kids[0] = values[5];
    source_lines[6] = __LINE__ + 1;
    values[6] = DSL_Builder_Create_Operator
                    (reshape_id, 1, kids, 1, &reshape_attr, 1);

    transpose_attr.name = "attr.permutation";
    transpose_attr.value = "0,2,1,3";
    kids[0] = values[6];
    source_lines[7] = __LINE__ + 1;
    values[7] = DSL_Builder_Create_Operator
                    (transpose_id, 1, kids, 1, &transpose_attr, 1);

    matmul_attrs[0].name = "attr.transpose_kid0";
    matmul_attrs[0].value = "false";
    matmul_attrs[1].name = "attr.transpose_kid1";
    matmul_attrs[1].value = "true";
    matmul_attrs[2].name = "attr.batch_rule";
    matmul_attrs[2].value = "exact";
    matmul_attrs[3].name = "attr.accum_dtype";
    matmul_attrs[3].value = "float32";
    kids[0] = values[2];
    kids[1] = values[3];
    source_lines[8] = __LINE__ + 1;
    values[8] = DSL_Builder_Create_Operator
                    (matmul_v2_id, 2, kids, 2, matmul_attrs, 4);

    output_attrs[0].name = "attr.semantic";
    output_attrs[0].value = "token_logits";
    output_attrs[1].name = "attr.sequence_axis";
    output_attrs[1].value = "-2";
    output_attrs[2].name = "attr.vocabulary_axis";
    output_attrs[2].value = "-1";
    kids[0] = values[4];
    source_lines[9] = __LINE__ + 1;
    values[9] = DSL_Builder_Create_Operator
                    (output_v3_id, 3, kids, 1, output_attrs, 3);

    for (UINT32 i = 0; i < sizeof(values) / sizeof(values[0]); ++i) {
        if (values[i] == NULL || !DSL_Builder_Append_PU_Value(pu, values[i])) {
            fprintf(stderr, "item-23 builder rejected value %u\n", i);
            return 1;
        }
        DSL_BUILDER_SOURCE_POSITION position;
        memset(&position, 0, sizeof(position));
        position.file_id = file_id;
        position.line = source_lines[i];
        position.column = 1;
        position.statement_begin = 1;
        if (!DSL_Builder_Set_Value_Source_Position(values[i], &position)) {
            fprintf(stderr, "item-23 source position failed for value %u\n",
                    i);
            failed = 1;
        }
    }

    if (strcmp(TY_tensor_attribute
                   (DSL_Builder_Get_Value_Type(values[5]),
                    TY_TENSOR_SCHEMA_SHAPE), "[1,8,64]") != 0 ||
        strcmp(TY_tensor_attribute
                   (DSL_Builder_Get_Value_Type(values[6]),
                    TY_TENSOR_SCHEMA_SHAPE), "[1,8,4,16]") != 0 ||
        strcmp(TY_tensor_attribute
                   (DSL_Builder_Get_Value_Type(values[7]),
                    TY_TENSOR_SCHEMA_SHAPE), "[1,4,8,16]") != 0 ||
        strcmp(TY_tensor_attribute
                   (DSL_Builder_Get_Value_Type(values[8]),
                    TY_TENSOR_SCHEMA_SHAPE), "[1,4,8,8]") != 0 ||
        DSL_Builder_Get_Value_Type(values[9]) != logits_ty) {
        fprintf(stderr, "item-23 result descriptor inference changed\n");
        failed = 1;
    }

    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify) || verify.error_count != 0) {
        fprintf(stderr, "item-23 gatekeeper rejected valid graph: %s\n",
                diagnostic);
        failed = 1;
    }

    TY_IDX transpose_result_ty = DSL_Builder_Get_Value_Type(values[7]);
    TY_tensor_bind_attribute
        (transpose_result_ty, TY_TENSOR_SCHEMA_SHAPE, "[1,8,4,16]");
    memset(&verify, 0, sizeof(verify));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (DSL_Builder_Verify_Program(&verify) || verify.error_count == 0) {
        fprintf(stderr, "item-23 gatekeeper accepted bad transpose result\n");
        failed = 1;
    }
    TY_tensor_bind_attribute
        (transpose_result_ty, TY_TENSOR_SCHEMA_SHAPE, "[1,4,8,16]");

    DSL_BUILDER_OPERATOR_ATTRIBUTE bad_reshape = reshape_attr;
    bad_reshape.value = "1,8,4,15";
    kids[0] = values[5];
    if (DSL_Builder_Create_Operator
            (reshape_id, 1, kids, 1, &bad_reshape, 1) != NULL) {
        fprintf(stderr, "item-23 builder accepted element-changing reshape\n");
        failed = 1;
    }

    DSL_BUILDER_OPERATOR_ATTRIBUTE bad_transpose = transpose_attr;
    bad_transpose.value = "0,2,2,3";
    kids[0] = values[6];
    if (DSL_Builder_Create_Operator
            (transpose_id, 1, kids, 1, &bad_transpose, 1) != NULL ||
        DSL_Builder_Create_Operator
            (linear_v2_id, 3, kids, 1, linear_attrs, 4) != NULL ||
        DSL_Builder_Create_Operator
            (output_v3_id, 3, kids, 1, output_attrs, 2) != NULL) {
        fprintf(stderr, "item-23 builder accepted a malformed schema\n");
        failed = 1;
    }

    FILE *image_dump = tmpfile();
    if (image_dump == NULL) {
        fprintf(stderr, "item-23 failed to create logical image dump\n");
        failed = 1;
    } else {
        char text[32768];
        size_t count;

        DSL_IR_Image_Print(image_dump);
        rewind(image_dump);
        count = fread(text, 1, sizeof(text) - 1, image_dump);
        text[count] = '\0';
        fclose(image_dump);
        if (strstr(text, "operator=OPR_DSLRESHAPE version=1") == NULL ||
            strstr(text, "operator=OPR_DSLTRANSPOSE version=1") == NULL ||
            strstr(text, "operator=OPR_DSLLINEAR version=3") == NULL ||
            strstr(text, "operator=OPR_DSLMATMUL version=2") == NULL ||
            strstr(text, "operator=OPR_DSLOUTPUTLOGITS version=3") == NULL ||
            strstr(text, "stable_name=common.reshape") == NULL ||
            strstr(text, "stable_name=common.transpose") == NULL ||
            strstr(text, "OPR_DSL ") != NULL ||
            strstr(text, "MDSL ") != NULL) {
            fprintf(stderr, "item-23 logical image dump changed\n");
            failed = 1;
        }
    }

    const char *artifact = getenv("OPEN64_DSL_LLAMA2_COMMON_ARTIFACT");
    request.path = artifact == NULL || artifact[0] == '\0' ?
                   "llama2_common_substrate.B" : artifact;
    request.flags = 0;
    (void) unlink(request.path);
    if (!DSL_Builder_Finalize_Mapped_Image(&request) ||
        access(request.path, F_OK) != 0) {
        fprintf(stderr, "item-23 mapped-image finalization failed\n");
        failed = 1;
    }
    if (artifact == NULL || artifact[0] == '\0')
        (void) unlink(request.path);

    return failed;
}

static int
Check_Llama2_Transformer_Expressions(void)
{
    DSL_BUILDER_TENSOR_TYPE_CORE core;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_DOMAIN_ID transformer_id;
    DSL_OPCODE_ID embedding_id;
    DSL_OPCODE_ID rms_id;
    DSL_OPCODE_ID rotary_id;
    DSL_OPCODE_ID attention_id;
    DSL_OPCODE_ID swiglu_id;
    DSL_BUILDER_VALUE values[16];
    UINT32 source_lines[16];
    DSL_BUILDER_VALUE kids[3];
    DSL_BUILDER_OPERATOR_ATTRIBUTE embedding_attrs[2];
    DSL_BUILDER_OPERATOR_ATTRIBUTE rms_attrs[3];
    DSL_BUILDER_OPERATOR_ATTRIBUTE rotary_attrs[6];
    DSL_BUILDER_OPERATOR_ATTRIBUTE attention_attrs[10];
    DSL_BUILDER_OPERATOR_ATTRIBUTE swiglu_attr;
    DSL_BUILDER_VERIFY_RESULT verify;
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    TY_IDX token_ty;
    TY_IDX embedding_weight_ty;
    TY_IDX scale_ty;
    TY_IDX qkv_ty;
    TY_IDX rope_ty;
    TY_IDX swiglu_ty;
    UINT32 file_id;
    char diagnostic[4096];
    int failed = 0;

    if (!DSL_Builder_Begin_Program() ||
        DSL_Opcode_Register_Transformer_Domain() != 5)
        return 1;
    transformer_id = DSL_Domain_Find("transformer");
    embedding_id = DSL_Opcode_Find
                       (transformer_id, "transformer.token_embedding", 1);
    rms_id = DSL_Opcode_Find
                 (transformer_id, "transformer.rms_norm", 1);
    rotary_id = DSL_Opcode_Find
                    (transformer_id, "transformer.rotary_embedding", 1);
    attention_id = DSL_Opcode_Find
                       (transformer_id, "transformer.attention", 1);
    swiglu_id = DSL_Opcode_Find
                    (transformer_id, "transformer.swiglu", 1);
    if (transformer_id == DSL_DOMAIN_INVALID_ID ||
        embedding_id == DSL_OPCODE_INVALID_ID ||
        rms_id == DSL_OPCODE_INVALID_ID ||
        rotary_id == DSL_OPCODE_INVALID_ID ||
        attention_id == DSL_OPCODE_INVALID_ID ||
        swiglu_id == DSL_OPCODE_INVALID_ID ||
        (UINT32)OPR_DSLTOKENEMBEDDING != 16 ||
        (UINT32)OPR_DSLRMSNORM != 17 ||
        (UINT32)OPR_DSLROTARYEMBEDDING != 18 ||
        (UINT32)OPR_DSLATTENTION != 19 ||
        (UINT32)OPR_DSLSWIGLU != 20) {
        fprintf(stderr, "item-24 transformer registry changed\n");
        return 1;
    }

    memset(&core, 0, sizeof(core));
    core.kind = "tensor";
    core.dtype = "int64";
    core.rank = 2;
    core.logical_shape = "[1,8]";
    token_ty = DSL_Builder_Create_Tensor_Type_Core
                   ("llama_token_ids", MTYPE_To_TY(MTYPE_I8), &core);
    core.dtype = "float32";
    core.rank = 2;
    core.logical_shape = "[128,32]";
    embedding_weight_ty = DSL_Builder_Create_Tensor_Type_Core
                              ("llama_embedding_weight",
                               MTYPE_To_TY(MTYPE_F4), &core);
    core.rank = 1;
    core.logical_shape = "[32]";
    scale_ty = DSL_Builder_Create_Tensor_Type_Core
                   ("llama_rms_scale", MTYPE_To_TY(MTYPE_F4), &core);
    core.rank = 4;
    core.logical_shape = "[1,4,8,8]";
    qkv_ty = DSL_Builder_Create_Tensor_Type_Core
                 ("llama_qkv", MTYPE_To_TY(MTYPE_F4), &core);
    core.logical_shape = "[1,1,8,8]";
    rope_ty = DSL_Builder_Create_Tensor_Type_Core
                  ("llama_rope_table", MTYPE_To_TY(MTYPE_F4), &core);
    core.rank = 3;
    core.logical_shape = "[1,8,88]";
    swiglu_ty = DSL_Builder_Create_Tensor_Type_Core
                    ("llama_swiglu", MTYPE_To_TY(MTYPE_F4), &core);

    pu = DSL_Builder_Create_Minimal_PU("llama2_transformer_expressions");
    file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
    source_lines[0] = __LINE__ + 1;
    values[0] = DSL_Builder_Create_Model_Input("token_ids", token_ty, 0);
    source_lines[1] = __LINE__ + 1;
    values[1] = DSL_Builder_Create_Tensor_Constant
                    ("embedding_weight", embedding_weight_ty, "float32", 2,
                     "[128,32]", "splat", "1");
    embedding_attrs[0].name = "attr.padding_idx";
    embedding_attrs[0].value = "none";
    embedding_attrs[1].name = "attr.bounds_policy";
    embedding_attrs[1].value = "runtime_check";
    kids[0] = values[0];
    kids[1] = values[1];
    source_lines[2] = __LINE__ + 1;
    values[2] = DSL_Builder_Create_Operator
                    (embedding_id, 1, kids, 2, embedding_attrs, 2);

    source_lines[3] = __LINE__ + 1;
    values[3] = DSL_Builder_Create_Tensor_Constant
                    ("rms_scale", scale_ty, "float32", 1, "[32]", "splat",
                     "1");
    rms_attrs[0].name = "attr.axis";
    rms_attrs[0].value = "-1";
    rms_attrs[1].name = "attr.epsilon";
    rms_attrs[1].value = "0.00001";
    rms_attrs[2].name = "attr.accum_dtype";
    rms_attrs[2].value = "float32";
    kids[0] = values[2];
    kids[1] = values[3];
    source_lines[4] = __LINE__ + 1;
    values[4] = DSL_Builder_Create_Operator
                    (rms_id, 1, kids, 2, rms_attrs, 3);

    source_lines[5] = __LINE__ + 1;
    values[5] = DSL_Builder_Create_Model_Input("query", qkv_ty, 1);
    source_lines[6] = __LINE__ + 1;
    values[6] = DSL_Builder_Create_Model_Input("key", qkv_ty, 2);
    source_lines[7] = __LINE__ + 1;
    values[7] = DSL_Builder_Create_Model_Input("value", qkv_ty, 3);
    source_lines[8] = __LINE__ + 1;
    values[8] = DSL_Builder_Create_Tensor_Constant
                    ("rope_cos", rope_ty, "float32", 4, "[1,1,8,8]",
                     "splat", "1");
    source_lines[9] = __LINE__ + 1;
    values[9] = DSL_Builder_Create_Tensor_Constant
                    ("rope_sin", rope_ty, "float32", 4, "[1,1,8,8]",
                     "splat", "0");
    rotary_attrs[0].name = "attr.head_layout";
    rotary_attrs[0].value = "BHSD";
    rotary_attrs[1].name = "attr.sequence_axis";
    rotary_attrs[1].value = "2";
    rotary_attrs[2].name = "attr.feature_axis";
    rotary_attrs[2].value = "3";
    rotary_attrs[3].name = "attr.pairing";
    rotary_attrs[3].value = "half_split";
    rotary_attrs[4].name = "attr.position_mode";
    rotary_attrs[4].value = "zero_based_static";
    rotary_attrs[5].name = "attr.position_offset";
    rotary_attrs[5].value = "0";
    kids[0] = values[5];
    kids[1] = values[8];
    kids[2] = values[9];
    source_lines[10] = __LINE__ + 1;
    values[10] = DSL_Builder_Create_Operator
                     (rotary_id, 1, kids, 3, rotary_attrs, 6);
    kids[0] = values[6];
    source_lines[11] = __LINE__ + 1;
    values[11] = DSL_Builder_Create_Operator
                     (rotary_id, 1, kids, 3, rotary_attrs, 6);

    attention_attrs[0].name = "attr.execution_mode";
    attention_attrs[0].value = "full_sequence";
    attention_attrs[1].name = "attr.mask_mode";
    attention_attrs[1].value = "causal";
    attention_attrs[2].name = "attr.head_layout";
    attention_attrs[2].value = "BHSD";
    attention_attrs[3].name = "attr.query_heads";
    attention_attrs[3].value = "4";
    attention_attrs[4].name = "attr.kv_heads";
    attention_attrs[4].value = "4";
    attention_attrs[5].name = "attr.head_dim";
    attention_attrs[5].value = "8";
    attention_attrs[6].name = "attr.scale_mode";
    attention_attrs[6].value = "inverse_sqrt_head_dim";
    attention_attrs[7].name = "attr.softmax_axis";
    attention_attrs[7].value = "-1";
    attention_attrs[8].name = "attr.softmax_accum_dtype";
    attention_attrs[8].value = "float32";
    attention_attrs[9].name = "attr.cache_mode";
    attention_attrs[9].value = "none";
    kids[0] = values[10];
    kids[1] = values[11];
    kids[2] = values[7];
    source_lines[12] = __LINE__ + 1;
    values[12] = DSL_Builder_Create_Operator
                     (attention_id, 1, kids, 3, attention_attrs, 10);

    source_lines[13] = __LINE__ + 1;
    values[13] = DSL_Builder_Create_Model_Input("gate_projection", swiglu_ty,
                                                4);
    source_lines[14] = __LINE__ + 1;
    values[14] = DSL_Builder_Create_Model_Input("up_projection", swiglu_ty,
                                                5);
    swiglu_attr.name = "attr.activation";
    swiglu_attr.value = "silu";
    kids[0] = values[13];
    kids[1] = values[14];
    source_lines[15] = __LINE__ + 1;
    values[15] = DSL_Builder_Create_Operator
                     (swiglu_id, 1, kids, 2, &swiglu_attr, 1);

    for (UINT32 i = 0; i < sizeof(values) / sizeof(values[0]); ++i) {
        if (values[i] == NULL || !DSL_Builder_Append_PU_Value(pu, values[i])) {
            fprintf(stderr, "item-24 builder rejected value %u\n", i);
            return 1;
        }
        DSL_BUILDER_SOURCE_POSITION position;
        memset(&position, 0, sizeof(position));
        position.file_id = file_id;
        position.line = source_lines[i];
        position.column = 1;
        position.statement_begin = 1;
        if (!DSL_Builder_Set_Value_Source_Position(values[i], &position))
            failed = 1;
    }

    if (strcmp(TY_tensor_attribute
                   (DSL_Builder_Get_Value_Type(values[2]),
                    TY_TENSOR_SCHEMA_SHAPE), "[1,8,32]") != 0 ||
        DSL_Builder_Get_Value_Type(values[4]) == TY_IDX_ZERO ||
        strcmp(TY_tensor_attribute
                   (DSL_Builder_Get_Value_Type(values[12]),
                    TY_TENSOR_SCHEMA_SHAPE), "[1,4,8,8]") != 0 ||
        strcmp(TY_tensor_attribute
                   (DSL_Builder_Get_Value_Type(values[15]),
                    TY_TENSOR_SCHEMA_SHAPE), "[1,8,88]") != 0) {
        fprintf(stderr, "item-24 transformer result inference changed\n");
        failed = 1;
    }

    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify) || verify.error_count != 0) {
        fprintf(stderr, "item-24 gatekeeper rejected valid graph: %s\n",
                diagnostic);
        failed = 1;
    }

    TY_IDX attention_result_ty = DSL_Builder_Get_Value_Type(values[12]);
    TY_tensor_bind_attribute
        (attention_result_ty, TY_TENSOR_SCHEMA_SHAPE, "[1,4,8,7]");
    memset(&verify, 0, sizeof(verify));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (DSL_Builder_Verify_Program(&verify) || verify.error_count == 0) {
        fprintf(stderr, "item-24 gatekeeper accepted bad attention result\n");
        failed = 1;
    }
    TY_tensor_bind_attribute
        (attention_result_ty, TY_TENSOR_SCHEMA_SHAPE, "[1,4,8,8]");

    DSL_BUILDER_OPERATOR_ATTRIBUTE bad_attention[10];
    memcpy(bad_attention, attention_attrs, sizeof(bad_attention));
    bad_attention[4].value = "2";
    kids[0] = values[10];
    kids[1] = values[11];
    kids[2] = values[7];
    if (DSL_Builder_Create_Operator
            (attention_id, 1, kids, 3, bad_attention, 10) != NULL) {
        fprintf(stderr, "item-24 builder accepted grouped-query attention\n");
        failed = 1;
    }
    bad_attention[4].value = "4";
    bad_attention[9].value = "static";
    if (DSL_Builder_Create_Operator
            (attention_id, 1, kids, 3, bad_attention, 10) != NULL) {
        fprintf(stderr, "item-24 builder accepted cache state\n");
        failed = 1;
    }
    DSL_BUILDER_OPERATOR_ATTRIBUTE bad_rms[3];
    memcpy(bad_rms, rms_attrs, sizeof(bad_rms));
    bad_rms[1].value = "0";
    kids[0] = values[2];
    kids[1] = values[3];
    if (DSL_Builder_Create_Operator
            (rms_id, 1, kids, 2, bad_rms, 3) != NULL) {
        fprintf(stderr, "item-24 builder accepted zero RMS epsilon\n");
        failed = 1;
    }
    DSL_BUILDER_OPERATOR_ATTRIBUTE bad_embedding[2];
    memcpy(bad_embedding, embedding_attrs, sizeof(bad_embedding));
    bad_embedding[1].value = "none";
    kids[0] = values[0];
    kids[1] = values[1];
    if (DSL_Builder_Create_Operator
            (embedding_id, 1, kids, 2, bad_embedding, 2) != NULL) {
        fprintf(stderr, "item-24 builder accepted missing bounds guard\n");
        failed = 1;
    }
    DSL_BUILDER_OPERATOR_ATTRIBUTE bad_rotary[6];
    memcpy(bad_rotary, rotary_attrs, sizeof(bad_rotary));
    bad_rotary[3].value = "interleaved";
    kids[0] = values[5];
    kids[1] = values[8];
    kids[2] = values[9];
    if (DSL_Builder_Create_Operator
            (rotary_id, 1, kids, 3, bad_rotary, 6) != NULL) {
        fprintf(stderr, "item-24 builder accepted unsupported RoPE pairing\n");
        failed = 1;
    }
    DSL_BUILDER_OPERATOR_ATTRIBUTE bad_swiglu = swiglu_attr;
    bad_swiglu.value = "relu";
    kids[0] = values[13];
    kids[1] = values[14];
    if (DSL_Builder_Create_Operator
            (swiglu_id, 1, kids, 2, &bad_swiglu, 1) != NULL) {
        fprintf(stderr, "item-24 builder accepted non-SiLU activation\n");
        failed = 1;
    }

    FILE *image_dump = tmpfile();
    if (image_dump == NULL) {
        failed = 1;
    } else {
        char text[65536];
        DSL_IR_Image_Print(image_dump);
        rewind(image_dump);
        size_t count = fread(text, 1, sizeof(text) - 1, image_dump);
        text[count] = '\0';
        fclose(image_dump);
        if (strstr(text, "operator=OPR_DSLTOKENEMBEDDING version=1") == NULL ||
            strstr(text, "operator=OPR_DSLRMSNORM version=1") == NULL ||
            strstr(text, "operator=OPR_DSLROTARYEMBEDDING version=1") == NULL ||
            strstr(text, "operator=OPR_DSLATTENTION version=1") == NULL ||
            strstr(text, "operator=OPR_DSLSWIGLU version=1") == NULL ||
            strstr(text, "OPR_DSL ") != NULL || strstr(text, "MDSL ") != NULL) {
            fprintf(stderr, "item-24 logical image dump changed\n");
            failed = 1;
        }
    }

    const char *artifact =
        getenv("OPEN64_DSL_LLAMA2_TRANSFORMER_ARTIFACT");
    request.path = artifact == NULL || artifact[0] == '\0' ?
                   "llama2_transformer_expressions.B" : artifact;
    request.flags = 0;
    (void) unlink(request.path);
    if (!DSL_Builder_Finalize_Mapped_Image(&request) ||
        access(request.path, F_OK) != 0) {
        fprintf(stderr, "item-24 mapped-image finalization failed\n");
        failed = 1;
    }
    if (artifact == NULL || artifact[0] == '\0')
        (void) unlink(request.path);
    return failed;
}

static int
Check_Llama2_Transformer_Regions(void)
{
    DSL_BUILDER_TENSOR_TYPE_CORE core;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_REGION prefill;
    DSL_BUILDER_REGION decoder;
    DSL_BUILDER_VALUE external[10];
    DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE references[9];
    DSL_BUILDER_VALUE decoder_values[19];
    DSL_BUILDER_VALUE prefill_values[4];
    DSL_BUILDER_VALUE kids[3];
    DSL_BUILDER_OPERATOR_ATTRIBUTE linear_attrs[4] = {
        { "attr.has_bias", "false" },
        { "attr.transpose_input", "false" },
        { "attr.transpose_weight", "true" },
        { "attr.weight_layout", "OI" }
    };
    DSL_BUILDER_OPERATOR_ATTRIBUTE rms_attrs[3] = {
        { "attr.axis", "-1" },
        { "attr.epsilon", "0.00001" },
        { "attr.accum_dtype", "float32" }
    };
    DSL_BUILDER_OPERATOR_ATTRIBUTE reshape_head =
        { "attr.target_shape", "1,8,4,8" };
    DSL_BUILDER_OPERATOR_ATTRIBUTE reshape_hidden =
        { "attr.target_shape", "1,8,32" };
    DSL_BUILDER_OPERATOR_ATTRIBUTE transpose_head =
        { "attr.permutation", "0,2,1,3" };
    DSL_BUILDER_OPERATOR_ATTRIBUTE rotary_attrs[6] = {
        { "attr.head_layout", "BHSD" },
        { "attr.sequence_axis", "2" },
        { "attr.feature_axis", "3" },
        { "attr.pairing", "half_split" },
        { "attr.position_mode", "zero_based_static" },
        { "attr.position_offset", "0" }
    };
    DSL_BUILDER_OPERATOR_ATTRIBUTE attention_attrs[10] = {
        { "attr.execution_mode", "full_sequence" },
        { "attr.mask_mode", "causal" },
        { "attr.head_layout", "BHSD" },
        { "attr.query_heads", "4" },
        { "attr.kv_heads", "4" },
        { "attr.head_dim", "8" },
        { "attr.scale_mode", "inverse_sqrt_head_dim" },
        { "attr.softmax_axis", "-1" },
        { "attr.softmax_accum_dtype", "float32" },
        { "attr.cache_mode", "none" }
    };
    DSL_BUILDER_OPERATOR_ATTRIBUTE residual_attrs[3] = {
        { "attr.broadcast_rule", "none" },
        { "attr.shape_check", "exact" },
        { "attr.residual_path", "true" }
    };
    DSL_BUILDER_OPERATOR_ATTRIBUTE swiglu_attr =
        { "attr.activation", "silu" };
    DSL_BUILDER_OPERATOR_ATTRIBUTE embedding_attrs[2] = {
        { "attr.padding_idx", "none" },
        { "attr.bounds_policy", "runtime_check" }
    };
    DSL_BUILDER_OPERATOR_ATTRIBUTE output_attrs[3] = {
        { "attr.semantic", "token_logits" },
        { "attr.sequence_axis", "-2" },
        { "attr.vocabulary_axis", "-1" }
    };
    DSL_BUILDER_VERIFY_RESULT verify;
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_SOURCE_POSITION position;
    DSL_DOMAIN_ID common_id;
    DSL_DOMAIN_ID transformer_id;
    DSL_OPCODE_ID linear;
    DSL_OPCODE_ID reshape;
    DSL_OPCODE_ID transpose;
    DSL_OPCODE_ID residual;
    DSL_OPCODE_ID output;
    DSL_OPCODE_ID embedding;
    DSL_OPCODE_ID rms;
    DSL_OPCODE_ID rotary;
    DSL_OPCODE_ID attention;
    DSL_OPCODE_ID swiglu;
    TY_IDX token_ty;
    TY_IDX hidden_ty;
    TY_IDX embedding_weight_ty;
    TY_IDX hidden_weight_ty;
    TY_IDX scale_ty;
    TY_IDX rope_ty;
    TY_IDX gate_weight_ty;
    TY_IDX down_weight_ty;
    TY_IDX logits_weight_ty;
    UINT32 file_id;
    char diagnostic[8192];
    int failed = 0;

    if (!DSL_Builder_Begin_Program())
        return 1;
    DSL_Opcode_Register_Common_Substrate();
    DSL_Opcode_Register_Transformer_Domain();
    common_id = DSL_Domain_Find("common");
    transformer_id = DSL_Domain_Find("transformer");
    linear = DSL_Opcode_Find(common_id, "common.linear", 3);
    reshape = DSL_Opcode_Find(common_id, "common.reshape", 1);
    transpose = DSL_Opcode_Find(common_id, "common.transpose", 1);
    residual = DSL_Opcode_Find(common_id, "common.residual_add", 2);
    output = DSL_Opcode_Find(common_id, "common.output_logits", 3);
    embedding = DSL_Opcode_Find
                    (transformer_id, "transformer.token_embedding", 1);
    rms = DSL_Opcode_Find(transformer_id, "transformer.rms_norm", 1);
    rotary = DSL_Opcode_Find
                 (transformer_id, "transformer.rotary_embedding", 1);
    attention = DSL_Opcode_Find
                   (transformer_id, "transformer.attention", 1);
    swiglu = DSL_Opcode_Find(transformer_id, "transformer.swiglu", 1);
    if (linear == DSL_OPCODE_INVALID_ID ||
        reshape == DSL_OPCODE_INVALID_ID ||
        transpose == DSL_OPCODE_INVALID_ID ||
        residual == DSL_OPCODE_INVALID_ID ||
        output == DSL_OPCODE_INVALID_ID ||
        embedding == DSL_OPCODE_INVALID_ID ||
        rms == DSL_OPCODE_INVALID_ID || rotary == DSL_OPCODE_INVALID_ID ||
        attention == DSL_OPCODE_INVALID_ID ||
        swiglu == DSL_OPCODE_INVALID_ID)
        return 1;

    memset(&core, 0, sizeof(core));
    core.kind = "tensor";
    core.dtype = "int64";
    core.rank = 2;
    core.logical_shape = "[1,8]";
    token_ty = DSL_Builder_Create_Tensor_Type_Core
                   ("region_token", MTYPE_To_TY(MTYPE_I8), &core);
    core.dtype = "float32";
    core.rank = 3;
    core.logical_shape = "[1,8,32]";
    hidden_ty = DSL_Builder_Create_Tensor_Type_Core
                    ("region_hidden", MTYPE_To_TY(MTYPE_F4), &core);
    core.rank = 2;
    core.logical_shape = "[128,32]";
    embedding_weight_ty = DSL_Builder_Create_Tensor_Type_Core
                              ("region_embedding_weight",
                               MTYPE_To_TY(MTYPE_F4), &core);
    core.logical_shape = "[32,32]";
    hidden_weight_ty = DSL_Builder_Create_Tensor_Type_Core
                           ("region_hidden_weight",
                            MTYPE_To_TY(MTYPE_F4), &core);
    core.rank = 1;
    core.logical_shape = "[32]";
    scale_ty = DSL_Builder_Create_Tensor_Type_Core
                   ("region_scale", MTYPE_To_TY(MTYPE_F4), &core);
    core.rank = 4;
    core.logical_shape = "[1,1,8,8]";
    rope_ty = DSL_Builder_Create_Tensor_Type_Core
                  ("region_rope", MTYPE_To_TY(MTYPE_F4), &core);
    core.rank = 2;
    core.logical_shape = "[88,32]";
    gate_weight_ty = DSL_Builder_Create_Tensor_Type_Core
                         ("region_gate_weight", MTYPE_To_TY(MTYPE_F4),
                          &core);
    core.logical_shape = "[32,88]";
    down_weight_ty = DSL_Builder_Create_Tensor_Type_Core
                         ("region_down_weight", MTYPE_To_TY(MTYPE_F4),
                          &core);
    core.logical_shape = "[128,32]";
    logits_weight_ty = DSL_Builder_Create_Tensor_Type_Core
                           ("region_logits_weight", MTYPE_To_TY(MTYPE_F4),
                            &core);
    if (token_ty == TY_IDX_ZERO || hidden_ty == TY_IDX_ZERO ||
        embedding_weight_ty == TY_IDX_ZERO ||
        hidden_weight_ty == TY_IDX_ZERO || scale_ty == TY_IDX_ZERO ||
        rope_ty == TY_IDX_ZERO || gate_weight_ty == TY_IDX_ZERO ||
        down_weight_ty == TY_IDX_ZERO || logits_weight_ty == TY_IDX_ZERO)
        return 1;

    pu = DSL_Builder_Create_Minimal_PU("llama2_transformer_regions");
    file_id = DSL_Builder_Register_Source_File
                  (pu, "/tmp/llama2_transformer_regions.py");
    external[0] = DSL_Builder_Create_Model_Input("tokens", token_ty, 0);
    const char *tensor_keys[9] = {
        "tok_embeddings.weight", "layers.0.attention_norm.weight",
        "layers.0.attention.wq.weight", "rope.cos", "rope.sin",
        "layers.0.feed_forward.w1.weight",
        "layers.0.feed_forward.w3.weight",
        "layers.0.feed_forward.w2.weight", "output.weight"
    };
    const UINT64 byte_offsets[9] = {
        0, 16384, 16512, 20608, 20864, 21120, 32384, 43648, 54912
    };
    const UINT64 byte_lengths[9] = {
        16384, 128, 4096, 256, 256, 11264, 11264, 11264, 16384
    };
    const char *external_names[9] = {
        "embedding_weight", "rms_scale", "hidden_weight", "rope_cos",
        "rope_sin", "gate_weight", "up_weight", "down_weight",
        "logits_weight"
    };
    TY_IDX external_types[9] = {
        embedding_weight_ty, scale_ty, hidden_weight_ty, rope_ty, rope_ty,
        gate_weight_ty, gate_weight_ty, down_weight_ty, logits_weight_ty
    };
    for (UINT32 i = 0; i < 9; ++i) {
        TY_tensor_bind_attribute
            (external_types[i], TY_TENSOR_SCHEMA_LAYOUT, "row_major");
        TY_tensor_bind_attribute
            (external_types[i], TY_TENSOR_SCHEMA_PLACEMENT, "side_file");
        TY_tensor_bind_attribute
            (external_types[i], TY_TENSOR_SCHEMA_MEMORY, "external_data");
        references[i].storage_format = "safetensors";
        references[i].side_file = "llama2.safetensors";
        references[i].tensor_key = tensor_keys[i];
        references[i].byte_offset = byte_offsets[i];
        references[i].byte_length = byte_lengths[i];
        references[i].checksum = "";
        external[i + 1] = DSL_Builder_Create_External_Tensor_Constant
                              (external_names[i], external_types[i],
                               &references[i]);
    }
    memset(&position, 0, sizeof(position));
    position.file_id = file_id;
    position.column = 1;
    position.statement_begin = 1;
    for (UINT32 i = 0; i < 10; ++i) {
        position.line = 10 + i;
        if (external[i] == NULL ||
            !DSL_Builder_Set_Value_Source_Position(external[i], &position) ||
            !DSL_Builder_Append_PU_Value(pu, external[i]))
            return 1;
    }

    prefill = DSL_Builder_Create_Region
                  (pu, NULL, "transformer.prefill", 1);
    decoder = DSL_Builder_Create_Region
                  (pu, prefill, "transformer.decoder_layer", 1);
    position.line = 30;
    position.basic_block_begin = 1;
    if (prefill == NULL || decoder == NULL ||
        !DSL_Builder_Set_Region_Source_Position(prefill, &position) ||
        !DSL_Builder_Set_Region_Metadata
             (prefill, "module_path", "TinyLlama.forward"))
        return 1;
    position.line = 40;
    if (!DSL_Builder_Set_Region_Source_Position(decoder, &position) ||
        !DSL_Builder_Set_Region_Metadata
             (decoder, "module_path", "layers.0") ||
        !DSL_Builder_Set_Region_Metadata(decoder, "layer_ordinal", "0"))
        return 1;

    kids[0] = external[0];
    kids[1] = external[1];
    prefill_values[0] = DSL_Builder_Create_Operator
                            (embedding, 1, kids, 2, embedding_attrs, 2);
    if (prefill_values[0] == NULL)
        return 1;
    TY_IDX embedding_result_ty =
        DSL_Builder_Get_Value_Type(prefill_values[0]);
    const char *embedding_placement = TY_tensor_attribute
        (embedding_result_ty, TY_TENSOR_SCHEMA_PLACEMENT);
    const char *embedding_memory = TY_tensor_attribute
        (embedding_result_ty, TY_TENSOR_SCHEMA_MEMORY);
    if ((embedding_placement != NULL &&
         strcmp(embedding_placement, "side_file") == 0) ||
        (embedding_memory != NULL &&
         strcmp(embedding_memory, "external_data") == 0)) {
        fprintf(stderr, "item-26 external storage escaped into activation\n");
        return 1;
    }
    kids[0] = prefill_values[0];
    kids[1] = external[2];
    decoder_values[0] = DSL_Builder_Create_Operator
                            (rms, 1, kids, 2, rms_attrs, 3);
    for (UINT32 i = 1; i <= 3; ++i) {
        kids[0] = decoder_values[0];
        kids[1] = external[3];
        decoder_values[i] = DSL_Builder_Create_Operator
                                (linear, 3, kids, 2, linear_attrs, 4);
    }
    kids[0] = decoder_values[1];
    decoder_values[4] = DSL_Builder_Create_Operator
                            (reshape, 1, kids, 1, &reshape_head, 1);
    kids[0] = decoder_values[4];
    decoder_values[5] = DSL_Builder_Create_Operator
                            (transpose, 1, kids, 1, &transpose_head, 1);
    kids[0] = decoder_values[5];
    kids[1] = external[4];
    kids[2] = external[5];
    decoder_values[6] = DSL_Builder_Create_Operator
                            (rotary, 1, kids, 3, rotary_attrs, 6);
    decoder_values[7] = DSL_Builder_Create_Operator
                            (rotary, 1, kids, 3, rotary_attrs, 6);
    kids[0] = decoder_values[6];
    kids[1] = decoder_values[7];
    kids[2] = decoder_values[5];
    decoder_values[8] = DSL_Builder_Create_Operator
                            (attention, 1, kids, 3, attention_attrs, 10);
    kids[0] = decoder_values[8];
    decoder_values[9] = DSL_Builder_Create_Operator
                            (transpose, 1, kids, 1, &transpose_head, 1);
    kids[0] = decoder_values[9];
    decoder_values[10] = DSL_Builder_Create_Operator
                             (reshape, 1, kids, 1, &reshape_hidden, 1);
    kids[0] = decoder_values[10];
    kids[1] = external[3];
    decoder_values[11] = DSL_Builder_Create_Operator
                             (linear, 3, kids, 2, linear_attrs, 4);
    kids[0] = prefill_values[0];
    kids[1] = decoder_values[11];
    decoder_values[12] = DSL_Builder_Create_Operator
                             (residual, 2, kids, 2, residual_attrs, 3);
    kids[0] = decoder_values[12];
    kids[1] = external[2];
    decoder_values[13] = DSL_Builder_Create_Operator
                             (rms, 1, kids, 2, rms_attrs, 3);
    kids[0] = decoder_values[13];
    kids[1] = external[6];
    decoder_values[14] = DSL_Builder_Create_Operator
                             (linear, 3, kids, 2, linear_attrs, 4);
    kids[1] = external[7];
    decoder_values[15] = DSL_Builder_Create_Operator
                             (linear, 3, kids, 2, linear_attrs, 4);
    kids[0] = decoder_values[14];
    kids[1] = decoder_values[15];
    decoder_values[16] = DSL_Builder_Create_Operator
                             (swiglu, 1, kids, 2, &swiglu_attr, 1);
    kids[0] = decoder_values[16];
    kids[1] = external[8];
    decoder_values[17] = DSL_Builder_Create_Operator
                             (linear, 3, kids, 2, linear_attrs, 4);
    kids[0] = decoder_values[12];
    kids[1] = decoder_values[17];
    decoder_values[18] = DSL_Builder_Create_Operator
                             (residual, 2, kids, 2, residual_attrs, 3);
    kids[0] = decoder_values[18];
    kids[1] = external[2];
    prefill_values[1] = DSL_Builder_Create_Operator
                            (rms, 1, kids, 2, rms_attrs, 3);
    kids[0] = prefill_values[1];
    kids[1] = external[9];
    prefill_values[2] = DSL_Builder_Create_Operator
                            (linear, 3, kids, 2, linear_attrs, 4);
    kids[0] = prefill_values[2];
    prefill_values[3] = DSL_Builder_Create_Operator
                            (output, 3, kids, 1, output_attrs, 3);

    position.basic_block_begin = 0;
    position.line = 31;
    if (prefill_values[0] == NULL ||
        !DSL_Builder_Set_Value_Source_Position(prefill_values[0], &position) ||
        !DSL_Builder_Append_Region_Value(prefill, prefill_values[0]))
        return 1;
    for (UINT32 i = 0; i < 19; ++i) {
        position.line = 41 + i;
        if (decoder_values[i] == NULL ||
            !DSL_Builder_Set_Value_Source_Position
                 (decoder_values[i], &position) ||
            !DSL_Builder_Append_Region_Value(decoder, decoder_values[i]))
            return 1;
    }
    if (!DSL_Builder_Append_Child_Region(prefill, decoder))
        return 1;
    for (UINT32 i = 1; i < 4; ++i) {
        position.line = 61 + i;
        if (prefill_values[i] == NULL ||
            !DSL_Builder_Set_Value_Source_Position
                 (prefill_values[i], &position) ||
            !DSL_Builder_Append_Region_Value(prefill, prefill_values[i]))
            return 1;
    }

    DSL_BUILDER_VALUE decoder_inputs[8] = {
        prefill_values[0], external[2], external[3], external[4],
        external[5], external[6], external[7], external[8]
    };
    for (UINT32 i = 0; i < 8; ++i) {
        if (!DSL_Builder_Declare_Region_Value
                 (decoder, decoder_inputs[i], DSL_REGION_VALUE_INPUT,
                  i, 0))
            return 1;
    }
    if (!DSL_Builder_Declare_Region_Value
             (decoder, decoder_values[18],
              DSL_REGION_VALUE_OUTPUT | DSL_REGION_VALUE_RESULT, 0, 0))
        return 1;
    for (UINT32 i = 0; i < 10; ++i) {
        if (!DSL_Builder_Declare_Region_Value
                 (prefill, external[i], DSL_REGION_VALUE_INPUT, i, 0))
            return 1;
    }
    if (!DSL_Builder_Declare_Region_Value
             (prefill, prefill_values[3],
              DSL_REGION_VALUE_OUTPUT | DSL_REGION_VALUE_RESULT, 0, 0) ||
        !DSL_Builder_Append_PU_Region(pu, prefill))
        return 1;

    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify) || verify.error_count != 0) {
        fprintf(stderr, "item-25 verifier rejected valid prefill: %s\n",
                diagnostic);
        failed = 1;
    }

    ST_IDX result_st =
        DSL_Builder_Get_Value_Result_Symbol(prefill_values[3]);
    ST_tensor_bind_attribute
        (result_st, TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS),
         "false");
    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (DSL_Builder_Verify_Program(&verify) || verify.error_count == 0 ||
        strstr(diagnostic, "DOPC_LLAMA_TOPOLOGY") == NULL) {
        fprintf(stderr, "item-25 verifier accepted aliased region result\n");
        failed = 1;
    }
    ST_tensor_bind_attribute
        (result_st, TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS),
         "true");
    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify) || verify.error_count != 0) {
        fprintf(stderr, "item-25 verifier did not accept restored result: "
                "%s\n", diagnostic);
        failed = 1;
    }

    WN *decoder_body = WN_region_body(DSL_Region_WN(decoder));
    WN *first_decoder_value = WN_first(decoder_body);
    WN *second_decoder_value = WN_next(first_decoder_value);
    WN_EXTRACT_FromBlock(decoder_body, first_decoder_value);
    WN_INSERT_BlockAfter
        (decoder_body, second_decoder_value, first_decoder_value);
    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (DSL_Builder_Verify_Program(&verify) || verify.error_count == 0 ||
        strstr(diagnostic, "DOPC_LLAMA_TOPOLOGY") == NULL) {
        fprintf(stderr, "item-25 verifier accepted reordered decoder body\n");
        failed = 1;
    }
    WN_EXTRACT_FromBlock(decoder_body, first_decoder_value);
    WN_INSERT_BlockFirst(decoder_body, first_decoder_value);
    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify) || verify.error_count != 0) {
        fprintf(stderr, "item-25 verifier did not accept restored topology: "
                "%s\n", diagnostic);
        failed = 1;
    }

    FILE *dump = tmpfile();
    if (dump == NULL) {
        failed = 1;
    } else {
        char text[16384];
        DSL_Region_Print_PU(dump, pu);
        rewind(dump);
        size_t count = fread(text, 1, sizeof(text) - 1, dump);
        text[count] = '\0';
        fclose(dump);
        if (strstr(text, "contract=transformer.prefill.v1") == NULL ||
            strstr(text, "contract=transformer.decoder_layer.v1") == NULL ||
            strstr(text, "METADATA module_path:TinyLlama.forward") == NULL ||
            strstr(text, "METADATA module_path:layers.0") == NULL ||
            strstr(text, "METADATA layer_ordinal:0") == NULL ||
            strstr(text, "roles=0xa") == NULL) {
            fprintf(stderr, "item-25 logical region inspection changed\n");
            failed = 1;
        }
    }

    const char *artifact = getenv("OPEN64_DSL_LLAMA2_REGION_ARTIFACT");
    request.path = artifact == NULL || artifact[0] == '\0' ?
                   "llama2_transformer_regions.B" : artifact;
    request.flags = 0;
    (void) unlink(request.path);
    if (!DSL_Builder_Finalize_Mapped_Image(&request) ||
        access(request.path, F_OK) != 0) {
        fprintf(stderr, "item-25 mapped-image finalization failed\n");
        failed = 1;
    }
    if (artifact == NULL || artifact[0] == '\0')
        (void) unlink(request.path);
    return failed;
}

static int
Check_Llama2_Decode_Contract_Registry(void)
{
    DSL_DOMAIN_ID common_id;
    DSL_DOMAIN_ID transformer_id;
    DSL_CONTRACT_ID rotary_v1;
    DSL_CONTRACT_ID rotary_v2;
    DSL_CONTRACT_ID attention_v2;
    DSL_CONTRACT_ID decoder_v2;
    DSL_CONTRACT_ID decode_v1;
    DSL_CONTRACT_ID cache_v1;
    DSL_CONTRACT_ID legacy_id;
    DSL_CONTRACT_INFO info;
    int failed = 0;

    DSL_Contract_Registry_Reset();
    common_id = DSL_Domain_Find("common");
    if (common_id == DSL_DOMAIN_INVALID_ID)
        common_id = DSL_Domain_Register
                        ("common", DSL_DOMAIN_INVALID_ID, 1, 0);
    transformer_id = DSL_Domain_Find("transformer");
    if (transformer_id == DSL_DOMAIN_INVALID_ID)
        transformer_id = DSL_Domain_Register
                             ("transformer", common_id, 1, 0);

    legacy_id = DSL_Contract_Register
                    ("legacy.name_only", transformer_id, transformer_id,
                     1, 0, NULL, 0, NULL, 0);
    if (legacy_id == DSL_CONTRACT_INVALID_ID ||
        DSL_Contract_Register
            ("legacy.name_only", transformer_id, transformer_id,
             99, 0, NULL, 0, NULL, 0) != legacy_id ||
        DSL_Contract_Count() != 1 ||
        DSL_Contract_Find
            ("legacy.name_only", transformer_id, transformer_id) !=
            legacy_id) {
        fprintf(stderr, "legacy contract registry compatibility changed\n");
        failed = 1;
    }

    DSL_Contract_Registry_Reset();

    if (common_id == DSL_DOMAIN_INVALID_ID ||
        transformer_id == DSL_DOMAIN_INVALID_ID ||
        DSL_Contract_Register_Transformer_Decode() != 8 ||
        DSL_Contract_Register_Transformer_Decode() != 8 ||
        DSL_Contract_Count() != 8) {
        fprintf(stderr, "decode contract seeding failed\n");
        return 1;
    }

    rotary_v1 = DSL_Contract_Find_Version
                    (DSL_CONTRACT_TRANSFORMER_ROTARY_EMBEDDING,
                     transformer_id, transformer_id, 1);
    rotary_v2 = DSL_Contract_Find_Version
                    (DSL_CONTRACT_TRANSFORMER_ROTARY_EMBEDDING,
                     transformer_id, transformer_id, 2);
    attention_v2 = DSL_Contract_Find_Version
                       (DSL_CONTRACT_TRANSFORMER_ATTENTION,
                        transformer_id, transformer_id, 2);
    decoder_v2 = DSL_Contract_Find_Version
                     (DSL_CONTRACT_TRANSFORMER_DECODER_LAYER,
                      transformer_id, transformer_id, 2);
    decode_v1 = DSL_Contract_Find_Version
                    (DSL_CONTRACT_TRANSFORMER_DECODE,
                     transformer_id, transformer_id, 1);
    cache_v1 = DSL_Contract_Find_Version
                   (DSL_CONTRACT_TRANSFORMER_KV_CACHE_STATE,
                    transformer_id, transformer_id, 1);

    if (rotary_v1 == DSL_CONTRACT_INVALID_ID ||
        rotary_v2 == DSL_CONTRACT_INVALID_ID ||
        rotary_v1 == rotary_v2 ||
        attention_v2 == DSL_CONTRACT_INVALID_ID ||
        decoder_v2 == DSL_CONTRACT_INVALID_ID ||
        decode_v1 == DSL_CONTRACT_INVALID_ID ||
        cache_v1 == DSL_CONTRACT_INVALID_ID ||
        DSL_Contract_Find
            (DSL_CONTRACT_TRANSFORMER_ROTARY_EMBEDDING,
             transformer_id, transformer_id) != rotary_v1 ||
        DSL_Contract_Find_Current
            (DSL_CONTRACT_TRANSFORMER_ROTARY_EMBEDDING,
             transformer_id, transformer_id) != rotary_v2) {
        fprintf(stderr, "decode contract version lookup failed\n");
        failed = 1;
    }

    if (!DSL_Contract_Get_Info(attention_v2, &info) || info.version != 2 ||
        info.flags !=
            (DSL_CONTRACT_FLAG_EXPRESSION | DSL_CONTRACT_FLAG_STATE) ||
        info.required_check_count != 5 || info.diagnostic_code_count != 4 ||
        strcmp(DSL_Contract_Required_Check_At(attention_v2, 1),
               "cached_attention_shape_valid") != 0 ||
        strcmp(DSL_Contract_Diagnostic_Code_At(attention_v2, 0),
               "DATTENTION201") != 0) {
        fprintf(stderr, "cached attention contract changed\n");
        failed = 1;
    }
    if (!DSL_Contract_Get_Info(decoder_v2, &info) || info.version != 2 ||
        info.flags != (DSL_CONTRACT_FLAG_REGION | DSL_CONTRACT_FLAG_STATE) ||
        strcmp(DSL_Contract_Required_Check_At(decoder_v2, 3),
               "layer_state_ownership_valid") != 0 ||
        !DSL_Contract_Get_Info(decode_v1, &info) || info.version != 1 ||
        strcmp(DSL_Contract_Diagnostic_Code_At(decode_v1, 2),
               "DDECODE003") != 0 ||
        !DSL_Contract_Get_Info(cache_v1, &info) ||
        info.flags != DSL_CONTRACT_FLAG_STATE ||
        strcmp(DSL_Contract_Required_Check_At(cache_v1, 4),
               "state_identity_declared") != 0) {
        fprintf(stderr, "decode region or cache-state contract changed\n");
        failed = 1;
    }

    if (DSL_Contract_Register_Versioned
            ("bad.version", transformer_id, transformer_id, 0, 0,
             NULL, 0, NULL, 0) != DSL_CONTRACT_INVALID_ID ||
        DSL_Contract_Find_Version
            (DSL_CONTRACT_TRANSFORMER_ATTENTION,
             transformer_id, transformer_id, 99) !=
            DSL_CONTRACT_INVALID_ID) {
        fprintf(stderr, "versioned contract registry accepted invalid input\n");
        failed = 1;
    }

    FILE *dump = tmpfile();
    if (dump == NULL) {
        perror("tmpfile");
        return 1;
    }
    char text[16384];
    DSL_Contract_fprint_registry(dump);
    rewind(dump);
    size_t count = fread(text, 1, sizeof(text) - 1, dump);
    text[count] = '\0';
    fclose(dump);
    if (strstr(text, "DSL Contract Registry: entries=8") == NULL ||
        strstr(text, "name=transformer.decode") == NULL ||
        strstr(text, "name=transformer.kv_cache_state") == NULL ||
        strstr(text, "name=transformer.rotary_embedding") == NULL ||
        strstr(text, "name=transformer.attention") == NULL ||
        strstr(text, "name=transformer.decoder_layer") == NULL ||
        strstr(text, "check[1]=cached_attention_shape_valid") == NULL ||
        strstr(text, "diagnostic[0]=DDECODE001") == NULL) {
        fprintf(stderr, "decode contract registry inspection changed\n");
        failed = 1;
    }

    return failed;
}

static int
Expect_Decode_Profile_Error
        (const DSL_TRANSFORMER_DECODE_PROFILE *profile,
         const char *expected_code)
{
    FILE *diagnostic = tmpfile();
    char text[512];

    if (diagnostic == NULL) {
        perror("tmpfile");
        return 1;
    }
    BOOL accepted = DSL_Gatekeeper_Verify_Transformer_Decode_Profile
                        (profile, diagnostic);
    rewind(diagnostic);
    size_t count = fread(text, 1, sizeof(text) - 1, diagnostic);
    text[count] = '\0';
    fclose(diagnostic);
    if (accepted || strstr(text, expected_code) == NULL) {
        fprintf(stderr, "decode profile expected %s, found: %s\n",
                expected_code, text);
        return 1;
    }
    return 0;
}

static int
Check_Llama2_Decode_Gatekeeper_Profile(void)
{
    DSL_TRANSFORMER_DECODE_PROFILE profile;
    DSL_TRANSFORMER_DECODE_PROFILE malformed;
    int failed = 0;

    memset(&profile, 0, sizeof(profile));
    profile.version = 1;
    profile.batch_size = 1;
    profile.decode_sequence_length = 1;
    profile.query_head_count = 4;
    profile.kv_head_count = 4;
    profile.head_dimension = 8;
    profile.input_cache_length = 3;
    profile.output_cache_length = 4;
    profile.cache_position = 3;
    profile.rope_capacity = 8;
    profile.cache_rank = 4;
    profile.cache_sequence_axis = 2;
    profile.cache_update = DSL_KV_CACHE_UPDATE_FUNCTIONAL_APPEND;

    if (!DSL_Gatekeeper_Verify_Transformer_Decode_Profile(&profile, NULL)) {
        fprintf(stderr, "valid tiny decode profile was rejected\n");
        failed = 1;
    }
    failed |= Expect_Decode_Profile_Error(NULL, "DDECODE001");

    malformed = profile;
    malformed.version = 2;
    failed |= Expect_Decode_Profile_Error(&malformed, "DDECODE001");
    malformed = profile;
    malformed.kv_head_count = 2;
    failed |= Expect_Decode_Profile_Error(&malformed, "DATTENTION201");
    malformed = profile;
    malformed.cache_sequence_axis = 1;
    failed |= Expect_Decode_Profile_Error(&malformed, "DKVCACHE002");
    malformed = profile;
    malformed.cache_position = 2;
    failed |= Expect_Decode_Profile_Error(&malformed, "DDECODE002");
    malformed = profile;
    malformed.output_cache_length = 5;
    failed |= Expect_Decode_Profile_Error(&malformed, "DATTENTION202");
    malformed = profile;
    malformed.rope_capacity = 3;
    failed |= Expect_Decode_Profile_Error(&malformed, "DROTARY202");

    return failed;
}

static int
Check_Structured_Region_Builder(void)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    DSL_BUILDER_SOURCE_POSITION position;
    DSL_BUILDER_VERIFY_RESULT verify;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_REGION region;
    DSL_BUILDER_VALUE input;
    DSL_BUILDER_VALUE add;
    DSL_BUILDER_VALUE kids[2];
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    DSL_DOMAIN_ID common_id;
    TY_IDX tensor_ty;
    int failed = 0;

    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");
    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.type_core.kind = "tensor";
    descriptor.type_core.dtype = "float32";
    descriptor.type_core.rank = 2;
    descriptor.type_core.logical_shape = "[2,2]";
    tensor_ty = DSL_Builder_Intern_Tensor_Type
                    ("region_tensor", MTYPE_To_TY(MTYPE_F4), &descriptor);
    pu = DSL_Builder_Create_Minimal_PU("region_contract_test");
    input = DSL_Builder_Create_Model_Input("region_input", tensor_ty, 0);
    kids[0] = input;
    kids[1] = input;
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    add = DSL_Builder_Create_Operator_With_Result
              (DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1), 1,
               kids, 2, &attribute, 1, "region_result", tensor_ty);
    region = DSL_Builder_Create_Region(pu, NULL, "cnn.basic_block", 1);

    if (tensor_ty == TY_IDX_ZERO || pu == NULL || input == NULL ||
        add == NULL || region == NULL ||
        !DSL_Builder_Append_PU_Value(pu, input) ||
        !DSL_Builder_Append_Region_Value(region, add) ||
        !DSL_Builder_Declare_Region_Value
             (region, input, DSL_REGION_VALUE_INPUT, 0, 0) ||
        !DSL_Builder_Declare_Region_Value
             (region, add,
              DSL_REGION_VALUE_OUTPUT | DSL_REGION_VALUE_RESULT, 1, 0) ||
        !DSL_Builder_Append_PU_Region(pu, region)) {
        fprintf(stderr, "structured region construction failed\n");
        return 1;
    }

    UINT32 file_id = DSL_Builder_Register_Source_File
                         (pu, "/tmp/region_contract_test.py");
    memset(&position, 0, sizeof(position));
    position.file_id = file_id;
    position.line = 17;
    position.statement_begin = 1;
    position.basic_block_begin = 1;
    if (file_id == 0 ||
        !DSL_Builder_Set_Region_Source_Position(region, &position)) {
        fprintf(stderr, "structured region source position failed\n");
        failed = 1;
    }

    WN *region_wn = DSL_Region_WN(region);
    WN *body = WN_func_body(PU_Info_tree_ptr(pu));
    if (region_wn == NULL || WN_operator(region_wn) != OPR_REGION ||
        WN_region_kind(region_wn) != REGION_KIND_PRAGMA ||
        WN_first(WN_region_body(region_wn)) != add ||
        WN_first(WN_region_pragmas(region_wn)) == NULL ||
        WN_operator(WN_first(WN_region_pragmas(region_wn))) != OPR_PRAGMA ||
        WN_pragma(WN_first(WN_region_pragmas(region_wn))) !=
            WN_PRAGMA_OPAQUE ||
        WN_first(body) != input || WN_last(body) != region_wn ||
        WN_Get_Linenum(region_wn) == 0) {
        fprintf(stderr, "structured region WN layout changed\n");
        failed = 1;
    }

    char diagnostic[1024];
    memset(&verify, 0, sizeof(verify));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify)) {
        fprintf(stderr, "structured region verification failed: %s\n",
                diagnostic);
        failed = 1;
    }

    FILE *dump = tmpfile();
    if (dump == NULL) {
        failed = 1;
    } else {
        char text[2048];
        DSL_Region_Print_PU(dump, pu);
        rewind(dump);
        size_t count = fread(text, 1, sizeof(text) - 1, dump);
        text[count] = '\0';
        fclose(dump);
        if (strstr(text, "contract=cnn.basic_block.v1") == NULL ||
            strstr(text, "roles=0xa") == NULL) {
            fprintf(stderr, "structured region inspection changed\n");
            failed = 1;
        }
    }
    return failed;
}

static int
Check_Abstract_State_Effects(void)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    DSL_BUILDER_VALUE values[3];
    DSL_BUILDER_VALUE add_kids[2];
    DSL_BUILDER_VALUE add;
    DSL_BUILDER_VALUE scatter;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_STATE runtime_status;
    DSL_BUILDER_STATE random_state;
    DSL_BUILDER_STATE mutable_buffer;
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID add_id;
    DSL_OPCODE_ID scatter_id;
    TY_IDX tensor_ty;
    int failed = 0;

    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.type_core.kind = "tensor";
    descriptor.type_core.dtype = "int32";
    descriptor.type_core.rank = 1;
    descriptor.type_core.logical_shape = "[1]";
    descriptor.representation.layout = "contiguous";
    descriptor.representation.sharding = "replicated";
    descriptor.representation.placement = "host";
    descriptor.representation.memory = "contiguous";
    descriptor.representation.quantization = "none";

    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");
    add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);
    scatter_id = DSL_Opcode_Find(common_id, "common.scatter", 1);
    tensor_ty = DSL_Builder_Intern_Tensor_Type
                    ("abstract_state_i32", MTYPE_To_TY(MTYPE_I4),
                     &descriptor);
    pu = DSL_Builder_Create_Minimal_PU("abstract_state_effect_contract");
    values[0] = DSL_Builder_Create_Model_Input
                    ("status_query", tensor_ty, 0);
    values[1] = DSL_Builder_Create_Model_Input
                    ("random_step", tensor_ty, 1);
    values[2] = DSL_Builder_Create_Model_Input
                    ("buffer_update", tensor_ty, 2);
    runtime_status = DSL_Builder_Declare_State_Object
                         (pu, "runtime_status", DSL_STATE_KIND_RUNTIME_STATUS);
    random_state = DSL_Builder_Declare_State_Object
                       (pu, "random_state", DSL_STATE_KIND_RANDOM);
    mutable_buffer = DSL_Builder_Declare_State_Object
                         (pu, "mutable_buffer",
                          DSL_STATE_KIND_MUTABLE_BUFFER);

    if (pu == NULL || tensor_ty == TY_IDX_ZERO ||
        add_id == DSL_OPCODE_INVALID_ID ||
        scatter_id == DSL_OPCODE_INVALID_ID ||
        values[0] == NULL || values[1] == NULL || values[2] == NULL ||
        runtime_status == NULL || random_state == NULL ||
        mutable_buffer == NULL) {
        fprintf(stderr, "abstract-state builder construction failed: "
                "pu=%d ty=%u add=%u values=%d/%d/%d states=%d/%d/%d "
                "state_rows=%u effect_rows=%u\n", pu != NULL,
                (UINT32)tensor_ty, add_id, values[0] != NULL,
                values[1] != NULL, values[2] != NULL,
                runtime_status != NULL, random_state != NULL,
                mutable_buffer != NULL,
                DSL_Effect_Image_State_Object_Count(),
                DSL_Effect_Image_State_Effect_Count());
        return 1;
    }

    for (UINT32 i = 0; i < 3; ++i) {
        if (!DSL_Builder_Append_PU_Value(pu, values[i]))
            failed = 1;
    }
    add_kids[0] = values[0];
    add_kids[1] = values[1];
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    add = DSL_Builder_Create_Operator_With_Result
              (add_id, 1, add_kids, 2, &attribute, 1,
               "pure_add", tensor_ty);
    if (add == NULL || !DSL_Builder_Append_PU_Value(pu, add) ||
        DSL_Builder_Add_State_Effect
            (values[0], runtime_status, DSL_STATE_EFFECT_READ) ||
        DSL_Builder_Add_State_Effect
            (values[1], random_state, DSL_STATE_EFFECT_MODIFY) ||
        DSL_Builder_Add_State_Effect
            (values[2], mutable_buffer, DSL_STATE_EFFECT_MODIFY) ||
        DSL_Builder_Add_State_Effect
            (add, runtime_status, DSL_STATE_EFFECT_READ)) {
        fprintf(stderr, "pure-operator state-effect rejection changed\n");
        failed = 1;
    }

    DSL_MEMORY_BEHAVIOR_CONTRACT memory_contract;
    attribute.name = "attr.axis";
    attribute.value = "0";
    scatter = DSL_Builder_Create_Operator_With_Result
                  (scatter_id, 1, values, 3, &attribute, 1,
                   "stateful_scatter", tensor_ty);
    if (scatter == NULL ||
        !DSL_Memory_Behavior_Get_Contract
             (OPR_DSLSCATTER, 1, &memory_contract) ||
        memory_contract.operand_count != 3 ||
        (memory_contract.operand_flags[0] &
         DSL_MEMORY_BEHAVIOR_MODIFY) == 0 ||
        (memory_contract.result_flags &
         DSL_MEMORY_BEHAVIOR_INPLACE_UPDATE) == 0 ||
        memory_contract.related_operand != 0 ||
        !DSL_Builder_Add_State_Effect
             (scatter, runtime_status, DSL_STATE_EFFECT_READ) ||
        !DSL_Builder_Add_State_Effect
             (scatter, random_state, DSL_STATE_EFFECT_MODIFY) ||
        !DSL_Builder_Add_State_Effect
             (scatter, mutable_buffer, DSL_STATE_EFFECT_MODIFY) ||
        !DSL_Builder_Append_PU_Value(pu, scatter)) {
        fprintf(stderr, "stateful scatter effect construction failed\n");
        failed = 1;
    }

    DSL_GATEKEEPER_RESULT gatekeeper;
    if (!DSL_Effect_Image_Validate(stderr) ||
        DSL_Effect_Image_State_Object_Count() != 3 ||
        DSL_Effect_Image_State_Effect_Count() != 3 ||
        !DSL_Gatekeeper_Verify_Program(pu, stderr, &gatekeeper)) {
        fprintf(stderr, "abstract-state verification failed\n");
        failed = 1;
    }

    const char *artifact = getenv("OPEN64_DSL_STATE_EFFECT_ARTIFACT");
    if (artifact != NULL && artifact[0] != '\0') {
        DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
        request.path = artifact;
        request.flags = 0;
        if (!DSL_Builder_Finalize_Mapped_Image(&request)) {
            fprintf(stderr, "abstract-state mapped image failed\n");
            failed = 1;
        }
    }
    return failed;
}

int
main(void)
{
    int failed = 0;

    Initialize_Test_Context();
    if (getenv("OPEN64_DSL_INGESTION_API_ONLY") != NULL)
        return Check_Upgraded_Ingestion_APIs();
    if (getenv("OPEN64_DSL_LLAMA2_COMMON_ONLY") != NULL)
        return Check_Llama2_Common_Substrate();
    if (getenv("OPEN64_DSL_LLAMA2_TRANSFORMER_ONLY") != NULL)
        return Check_Llama2_Transformer_Expressions();
    if (getenv("OPEN64_DSL_LLAMA2_REGIONS_ONLY") != NULL)
        return Check_Llama2_Transformer_Regions();
    if (getenv("OPEN64_DSL_LLAMA2_DECODE_CONTRACT_ONLY") != NULL)
        return Check_Llama2_Decode_Contract_Registry() |
               Check_Llama2_Decode_Gatekeeper_Profile();
    if (getenv("OPEN64_DSL_STRUCTURED_REGION_ONLY") != NULL)
        return Check_Structured_Region_Builder();
    if (getenv("OPEN64_DSL_STATE_EFFECT_ONLY") != NULL)
        return Check_Abstract_State_Effects();

    failed |= Check_Tensor_Type_And_Descriptor();
    failed |= Check_Symbol_Metadata();
    failed |= Check_Operator_Creation();
    failed |= Check_Mapped_Image_Finalizer();
    failed |= Check_Program_Unit_Value_Attach();
    failed |= Check_Production_Native_Builder();
    failed |= Check_Llama2_Common_Substrate();
    failed |= Check_Llama2_Transformer_Expressions();
    failed |= Check_Llama2_Transformer_Regions();
    failed |= Check_Llama2_Decode_Contract_Registry();
    failed |= Check_Llama2_Decode_Gatekeeper_Profile();
    failed |= Check_Structured_Region_Builder();
    failed |= Check_Abstract_State_Effects();
    failed |= Check_Native_DSL_Node_Layout();
    failed |= Check_DSL_IR_Image_Tables();

    return failed;
}
