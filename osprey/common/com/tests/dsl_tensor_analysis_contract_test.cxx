/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Certifies AIO-3 semantic tensor facts, operand roles, ownership, descriptor
 * completeness, and metadata independence within one PU. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO3-SEMANTIC-TENSOR.md.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "stab.h"
#include "ir_reader.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_opcode.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_evolution.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void
Signal_Cleanup(INT sig) {}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
    return "";
}

typedef struct {
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_VALUE operands[5];
    UINT32 operand_count;
    DSL_BUILDER_VALUE result;
    DSL_OPERATOR expected_operator;
    UINT16 expected_version;
} AIO3_FIXTURE;

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

static TY_IDX
Create_Tensor_Type
        (const char *name, INT32 rank, const char *shape)
{
    TY_TENSOR_CANONICAL_DESCRIPTOR descriptor;
    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.kind = "tensor";
    descriptor.dtype = "float32";
    descriptor.rank = rank;
    descriptor.logical_shape = shape;
    descriptor.traits = "dense";
    descriptor.layout = "row_major";
    descriptor.sharding = "replicated";
    descriptor.placement = "host";
    descriptor.memory = "contiguous";
    descriptor.quantization = "none";
    return TY_Intern_Tensor_Type(name, MTYPE_To_TY(MTYPE_F4), &descriptor);
}

static BOOL
Initialize_PU
        (const char *pu_name, DSL_BUILDER_PROGRAM_UNIT *pu,
         UINT32 *file_id)
{
    DSL_BUILDER_PU_SOURCE_IDENTITY identity;
    *pu = DSL_Builder_Create_Minimal_PU(pu_name);
    if (*pu == NULL)
        return FALSE;
    memset(&identity, 0, sizeof(identity));
    identity.canonical_definition_name = pu_name;
    identity.defining_module = "aio3.semantic_tensor_contract";
    identity.defining_file = __FILE__;
    identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(*pu, &identity))
        return FALSE;
    *file_id = DSL_Builder_Register_Source_File(*pu, __FILE__);
    return *file_id != 0;
}

static DSL_BUILDER_VALUE
Create_Constant
        (const char *name, TY_IDX ty, INT32 rank, const char *shape,
         UINT32 file_id, UINT32 line)
{
    DSL_BUILDER_SOURCE_POSITION source;
    DSL_BUILDER_VALUE value = DSL_Builder_Create_Tensor_Constant
                                  (name, ty, "float32", rank, shape,
                                   "splat", "1.0");
    memset(&source, 0, sizeof(source));
    source.file_id = file_id;
    source.statement_begin = 1;
    source.line = line;
    return value != NULL && DSL_Builder_Set_Value_Source_Position
                                (value, &source) ? value : NULL;
}

static DSL_BUILDER_VALUE
Create_Model_Input
        (const char *name, TY_IDX ty, UINT32 input_ordinal,
         UINT32 file_id, UINT32 line)
{
    DSL_BUILDER_SOURCE_POSITION source;
    DSL_BUILDER_VALUE value = DSL_Builder_Create_Model_Input
                                  (name, ty, input_ordinal);
    memset(&source, 0, sizeof(source));
    source.file_id = file_id;
    source.statement_begin = 1;
    source.line = line;
    return value != NULL && DSL_Builder_Set_Value_Source_Position
                                (value, &source) ? value : NULL;
}

static BOOL
Finish_Fixture
        (AIO3_FIXTURE *fixture, DSL_OPCODE_ID opcode,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attributes,
         UINT32 attribute_count, const char *result_name, TY_IDX result_ty,
         UINT32 file_id, UINT32 line)
{
    DSL_BUILDER_SOURCE_POSITION source;
    fixture->result = DSL_Builder_Create_Operator_With_Result
                          (opcode, fixture->expected_version,
                           fixture->operands, fixture->operand_count,
                           attributes, attribute_count,
                           result_name, result_ty);
    memset(&source, 0, sizeof(source));
    source.file_id = file_id;
    source.statement_begin = 1;
    source.line = line;
    if (fixture->result == NULL ||
        !DSL_Builder_Set_Value_Source_Position(fixture->result, &source))
        return FALSE;
    for (UINT32 i = 0; i < fixture->operand_count; ++i) {
        if (!DSL_Builder_Append_PU_Value(fixture->pu,
                                         fixture->operands[i]))
            return FALSE;
    }
    return DSL_Builder_Append_PU_Value(fixture->pu, fixture->result);
}

static BOOL
Create_Matmul_Fixture (AIO3_FIXTURE *fixture)
{
    DSL_BUILDER_OPERATOR_ATTRIBUTE attributes[2];
    DSL_DOMAIN_ID domain;
    TY_IDX kid0_ty;
    TY_IDX kid1_ty;
    TY_IDX result_ty;
    UINT32 file_id;
    memset(fixture, 0, sizeof(*fixture));
    fixture->expected_operator = OPR_DSLMATMUL;
    fixture->expected_version = 1;
    fixture->operand_count = 2;
    if (!Initialize_PU("aio3_common_matmul", &fixture->pu, &file_id))
        return FALSE;
    kid0_ty = Create_Tensor_Type("aio3_matmul_kid0", 2, "[2,3]");
    kid1_ty = Create_Tensor_Type("aio3_matmul_kid1", 2, "[3,4]");
    result_ty = Create_Tensor_Type("aio3_matmul_result", 2, "[2,4]");
    fixture->operands[0] = Create_Constant
                               ("matmul_kid0", kid0_ty, 2, "[2,3]",
                                file_id, __LINE__);
    fixture->operands[1] = Create_Constant
                               ("matmul_kid1", kid1_ty, 2, "[3,4]",
                                file_id, __LINE__);
    attributes[0].name = "attr.transpose_kid0";
    attributes[0].value = "false";
    attributes[1].name = "attr.transpose_kid1";
    attributes[1].value = "false";
    domain = DSL_Domain_Find("common");
    return fixture->operands[0] != NULL && fixture->operands[1] != NULL &&
           Finish_Fixture
               (fixture,
                DSL_Opcode_Find(domain, DSL_OPCODE_COMMON_MATMUL, 1),
                attributes, 2, "matmul_result", result_ty,
                file_id, __LINE__);
}

static BOOL
Create_CNN_Fixture (AIO3_FIXTURE *fixture)
{
    DSL_BUILDER_OPERATOR_ATTRIBUTE attributes[8];
    DSL_DOMAIN_ID domain;
    TY_IDX input_ty;
    TY_IDX weight_ty;
    TY_IDX bias_ty;
    TY_IDX result_ty;
    UINT32 file_id;
    memset(fixture, 0, sizeof(*fixture));
    fixture->expected_operator = OPR_DSLCONV2D;
    fixture->expected_version = 2;
    fixture->operand_count = 3;
    if (!Initialize_PU("aio3_resnet_conv2d", &fixture->pu, &file_id))
        return FALSE;
    input_ty = Create_Tensor_Type("aio3_conv_input", 4, "[1,3,8,8]");
    weight_ty = Create_Tensor_Type("aio3_conv_weight", 4, "[4,3,3,3]");
    bias_ty = Create_Tensor_Type("aio3_conv_bias", 1, "[4]");
    result_ty = Create_Tensor_Type("aio3_conv_result", 4, "[1,4,6,6]");
    fixture->operands[0] = Create_Model_Input
                               ("conv_input", input_ty, 0,
                                file_id, __LINE__);
    fixture->operands[1] = Create_Constant
                               ("conv_weight", weight_ty, 4, "[4,3,3,3]",
                                file_id, __LINE__);
    fixture->operands[2] = Create_Constant
                               ("conv_bias", bias_ty, 1, "[4]",
                                file_id, __LINE__);
    attributes[0].name = "attr.kernel_shape";
    attributes[0].value = "3,3";
    attributes[1].name = "attr.stride";
    attributes[1].value = "1,1";
    attributes[2].name = "attr.padding";
    attributes[2].value = "0,0";
    attributes[3].name = "attr.dilation";
    attributes[3].value = "1,1";
    attributes[4].name = "attr.groups";
    attributes[4].value = "1";
    attributes[5].name = "attr.input_layout";
    attributes[5].value = "NCHW";
    attributes[6].name = "attr.weight_layout";
    attributes[6].value = "OIHW";
    attributes[7].name = "attr.output_layout";
    attributes[7].value = "NCHW";
    domain = DSL_Domain_Find("cnn");
    return fixture->operands[0] != NULL && fixture->operands[1] != NULL &&
           fixture->operands[2] != NULL &&
           Finish_Fixture
               (fixture, DSL_Opcode_Find(domain, "cnn.conv2d", 2),
                attributes, 8, "conv_result", result_ty,
                file_id, __LINE__);
}

static BOOL
Create_Transformer_Fixture (AIO3_FIXTURE *fixture)
{
    DSL_BUILDER_OPERATOR_ATTRIBUTE attributes[3];
    DSL_DOMAIN_ID domain;
    TY_IDX input_ty;
    TY_IDX weight_ty;
    TY_IDX result_ty;
    UINT32 file_id;
    memset(fixture, 0, sizeof(*fixture));
    fixture->expected_operator = OPR_DSLRMSNORM;
    fixture->expected_version = 1;
    fixture->operand_count = 2;
    if (!Initialize_PU("aio3_llama_rms_norm", &fixture->pu, &file_id))
        return FALSE;
    input_ty = Create_Tensor_Type("aio3_rms_input", 3, "[1,4,8]");
    weight_ty = Create_Tensor_Type("aio3_rms_weight", 1, "[8]");
    result_ty = Create_Tensor_Type("aio3_rms_result", 3, "[1,4,8]");
    fixture->operands[0] = Create_Model_Input
                               ("rms_input", input_ty, 0,
                                file_id, __LINE__);
    fixture->operands[1] = Create_Constant
                               ("rms_weight", weight_ty, 1, "[8]",
                                file_id, __LINE__);
    attributes[0].name = "attr.axis";
    attributes[0].value = "-1";
    attributes[1].name = "attr.epsilon";
    attributes[1].value = "1e-5";
    attributes[2].name = "attr.accum_dtype";
    attributes[2].value = "float32";
    domain = DSL_Domain_Find("transformer");
    return fixture->operands[0] != NULL && fixture->operands[1] != NULL &&
           Finish_Fixture
               (fixture,
                DSL_Opcode_Find(domain, "transformer.rms_norm", 1),
                attributes, 3, "rms_result", result_ty,
                file_id, __LINE__);
}

static BOOL
Check_Operand_Role
        (const DSL_TENSOR_ANALYSIS *analysis,
         DSL_BUILDER_VALUE value, UINT32 expected_ordinal,
         UINT32 expected_role,
         DSL_OPERATOR expected_consumer)
{
    DSL_TENSOR_FACT_RECORD fact;
    DSL_TENSOR_USE_FACT_RECORD use;
    return DSL_Tensor_Analysis_Find_Fact
               (analysis, DSL_Builder_Get_Value_Image_Id(value), &fact) &&
           fact.use_count == 1 &&
           DSL_Tensor_Analysis_Get_Use
               (analysis, fact.first_use_id, &use) &&
           use.role == expected_role &&
           use.consumer_operator == expected_consumer &&
           use.operand_ordinal == expected_ordinal;
}

static BOOL
Analyze_Fixture
        (const AIO3_FIXTURE *fixture, FILE *trace)
{
    static const UINT32 common_roles[] = {
        DSL_TENSOR_USE_ROLE_CONTRACTION_KID0,
        DSL_TENSOR_USE_ROLE_CONTRACTION_KID1
    };
    static const UINT32 cnn_roles[] = {
        DSL_TENSOR_USE_ROLE_ACTIVATION,
        DSL_TENSOR_USE_ROLE_WEIGHT,
        DSL_TENSOR_USE_ROLE_BIAS
    };
    static const UINT32 transformer_roles[] = {
        DSL_TENSOR_USE_ROLE_ACTIVATION,
        DSL_TENSOR_USE_ROLE_WEIGHT
    };
    const UINT32 *roles = fixture->expected_operator == OPR_DSLMATMUL ?
                          common_roles :
                          fixture->expected_operator == OPR_DSLCONV2D ?
                          cnn_roles : transformer_roles;
    DSL_TENSOR_EVOLUTION_GRAPH *graph =
        DSL_Tensor_Evolution_Create(fixture->pu, stderr);
    DSL_TENSOR_ANALYSIS *analysis;
    DSL_TENSOR_FACT_RECORD result;
    if (graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots(graph, stderr))
        return FALSE;
    analysis = DSL_Tensor_Analysis_Create(fixture->pu, graph, stderr);
    if (analysis == NULL || !DSL_Tensor_Analysis_Build(analysis, stderr) ||
        !DSL_Tensor_Analysis_Build(analysis, stderr) ||
        !DSL_Tensor_Analysis_Verify(analysis, stderr) ||
        !DSL_Tensor_Analysis_Is_Complete(analysis) ||
        DSL_Tensor_Analysis_Fact_Count(analysis) !=
            fixture->operand_count + 1 ||
        DSL_Tensor_Analysis_Use_Count(analysis) != fixture->operand_count ||
        !DSL_Tensor_Analysis_Find_Fact
             (analysis, DSL_Builder_Get_Value_Image_Id(fixture->result),
              &result) ||
        result.producer_operator != fixture->expected_operator ||
        result.producer_version != fixture->expected_version ||
        result.value_role != DSL_TENSOR_VALUE_ROLE_INTERMEDIATE ||
        result.dimension_state != DSL_TENSOR_DIMENSION_STATIC ||
        result.ownership != DSL_TENSOR_OWNERSHIP_UNIQUE ||
        result.reuse_role != DSL_TENSOR_REUSE_UNUSED)
        return FALSE;
    if (fixture->expected_operator != OPR_DSLMATMUL) {
        DSL_TENSOR_FACT_RECORD input;
        if (!DSL_Tensor_Analysis_Find_Fact
                 (analysis,
                  DSL_Builder_Get_Value_Image_Id(fixture->operands[0]),
                  &input) ||
            input.value_role != DSL_TENSOR_VALUE_ROLE_MODEL_INPUT)
            return FALSE;
    }
    for (UINT32 i = 0; i < fixture->operand_count; ++i) {
        if (!Check_Operand_Role
                 (analysis, fixture->operands[i], i, roles[i],
                  fixture->expected_operator))
            return FALSE;
    }
    if (trace != NULL)
        DSL_Tensor_Analysis_Print(trace, analysis);
    DSL_Tensor_Analysis_Destroy(analysis);
    DSL_Tensor_Evolution_Destroy(graph);
    return TRUE;
}

static BOOL
Create_All_Fixtures (AIO3_FIXTURE *fixtures)
{
    if (!Create_Matmul_Fixture(&fixtures[0])) {
        fprintf(stderr, "AIO-3 matmul fixture creation failed\n");
        return FALSE;
    }
    if (!Create_CNN_Fixture(&fixtures[1])) {
        fprintf(stderr, "AIO-3 CNN fixture creation failed\n");
        return FALSE;
    }
    if (!Create_Transformer_Fixture(&fixtures[2])) {
        fprintf(stderr, "AIO-3 Transformer fixture creation failed\n");
        return FALSE;
    }
    return TRUE;
}

static int
Run_Image_Mode (BOOL run_analysis)
{
    const char *artifact = getenv("OPEN64_AIO3_ARTIFACT");
    const char *trace_path = getenv("OPEN64_AIO3_ANALYSIS");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_VERIFY_RESULT verify;
    AIO3_FIXTURE fixtures[3];
    char diagnostic[4096];
    FILE *trace = NULL;
    UINT32 image_nodes;
    UINT32 image_values;
    UINT32 type_count;
    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    DSL_Opcode_Register_Domain_Wrapper_Examples();
    DSL_Opcode_Register_Transformer_Domain();
    if (!Create_All_Fixtures(fixtures))
        return 1;
    image_nodes = DSL_IR_Image_Node_Count();
    image_values = DSL_IR_Image_Value_Count();
    type_count = TY_Table_Size();
    if (run_analysis) {
        if (trace_path == NULL || trace_path[0] == '\0')
            return 1;
        trace = fopen(trace_path, "w");
        if (trace == NULL) {
            fprintf(stderr, "AIO-3 analysis trace open failed\n");
            return 1;
        }
        for (UINT32 i = 0; i < 3; ++i) {
            if (!DSL_Builder_Select_PU(fixtures[i].pu) ||
                !Analyze_Fixture(&fixtures[i], trace)) {
                fprintf(stderr, "AIO-3 fixture analysis failed index=%u\n",
                        i);
                fclose(trace);
                return 1;
            }
        }
        fclose(trace);
        if (DSL_IR_Image_Node_Count() != image_nodes ||
            DSL_IR_Image_Value_Count() != image_values ||
            TY_Table_Size() != type_count) {
            fprintf(stderr, "AIO-3 check-only image census changed\n");
            return 1;
        }
    }
    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify)) {
        fprintf(stderr, "AIO-3 program verification failed: %s\n",
                diagnostic);
        return 1;
    }
    request.path = artifact;
    request.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&request)) {
        fprintf(stderr, "AIO-3 mapped-image finalization failed\n");
        return 1;
    }
    printf("AIO-3 %s image passed\n", run_analysis ? "after" : "before");
    return 0;
}

static BOOL
Records_Equal
        (const DSL_TENSOR_ANALYSIS *first,
         const DSL_TENSOR_ANALYSIS *second)
{
    if (DSL_Tensor_Analysis_Fact_Count(first) !=
            DSL_Tensor_Analysis_Fact_Count(second) ||
        DSL_Tensor_Analysis_Use_Count(first) !=
            DSL_Tensor_Analysis_Use_Count(second))
        return FALSE;
    for (UINT32 id = 1; id <= DSL_Tensor_Analysis_Fact_Count(first); ++id) {
        DSL_TENSOR_FACT_RECORD left;
        DSL_TENSOR_FACT_RECORD right;
        if (!DSL_Tensor_Analysis_Get_Fact(first, id, &left) ||
            !DSL_Tensor_Analysis_Get_Fact(second, id, &right) ||
            memcmp(&left, &right, sizeof(left)) != 0)
            return FALSE;
    }
    for (UINT32 id = 1; id <= DSL_Tensor_Analysis_Use_Count(first); ++id) {
        DSL_TENSOR_USE_FACT_RECORD left;
        DSL_TENSOR_USE_FACT_RECORD right;
        if (!DSL_Tensor_Analysis_Get_Use(first, id, &left) ||
            !DSL_Tensor_Analysis_Get_Use(second, id, &right) ||
            memcmp(&left, &right, sizeof(left)) != 0)
            return FALSE;
    }
    return TRUE;
}

static int
Run_Metadata_Independence(void)
{
    AIO3_FIXTURE fixture;
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_TENSOR_ANALYSIS *before;
    DSL_TENSOR_ANALYSIS *after;
    DSL_BUILDER_COMPILER_METADATA metadata;
    DSL_TENSOR_FACT_RECORD unused;
    DSL_TENSOR_USE_FACT_RECORD use;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Matmul_Fixture(&fixture))
        return 1;
    graph = DSL_Tensor_Evolution_Create(fixture.pu, stderr);
    if (graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots(graph, stderr))
        return 1;
    before = DSL_Tensor_Analysis_Create(fixture.pu, graph, stderr);
    if (before == NULL || !DSL_Tensor_Analysis_Build(before, stderr))
        return 1;
    metadata.name = "source_layer_name";
    metadata.value = "metadata_must_not_change_semantics";
    if (!DSL_Builder_Attach_Value_Metadata
             (fixture.operands[0], &metadata, 1))
        return 1;
    after = DSL_Tensor_Analysis_Create(fixture.pu, graph, stderr);
    if (after == NULL || !DSL_Tensor_Analysis_Build(after, stderr) ||
        !Records_Equal(before, after) ||
        DSL_Tensor_Analysis_Get_Fact(after, 0, &unused) ||
        DSL_Tensor_Analysis_Get_Fact(after, 4, &unused) ||
        DSL_Tensor_Analysis_Get_Use(after, 0, &use) ||
        DSL_Tensor_Analysis_Get_Use(after, 3, &use) ||
        strcmp(DSL_Tensor_Dimension_State_Name(99), "unknown") != 0 ||
        strcmp(DSL_Tensor_Ownership_Name(99), "unknown") != 0 ||
        strcmp(DSL_Tensor_Reuse_Role_Name(99), "unknown") != 0 ||
        strcmp(DSL_Tensor_Value_Role_Name(99), "unknown") != 0 ||
        strcmp(DSL_Tensor_Use_Role_Name(99), "unknown") != 0)
        return 1;
    DSL_Tensor_Analysis_Destroy(after);
    DSL_Tensor_Analysis_Destroy(before);
    DSL_Tensor_Evolution_Destroy(graph);
    printf("AIO-3 metadata independence contract passed\n");
    return 0;
}

static int
Run_Incomplete_And_Ownership(void)
{
    AIO3_FIXTURE first;
    AIO3_FIXTURE second;
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_TENSOR_ANALYSIS *analysis;
    DSL_TENSOR_FACT_RECORD fact;
    FILE *quiet = tmpfile();
    TY_IDX pending_ty;
    UINT32 file_id;
    DSL_BUILDER_VALUE pending;
    DSL_BUILDER_PROGRAM_UNIT pending_pu;

    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    DSL_Opcode_Register_Domain_Wrapper_Examples();
    if (quiet == NULL ||
        !Initialize_PU("aio3_pending_shape", &pending_pu, &file_id))
        return 1;
    pending_ty = Create_Tensor_Type
                     ("aio3_pending_type", 2, "[2,<pending>]");
    pending = Create_Constant
                  ("pending", pending_ty, 2, "[2,<pending>]",
                   file_id, __LINE__);
    if (pending == NULL || !DSL_Builder_Append_PU_Value(pending_pu, pending))
        return 1;
    graph = DSL_Tensor_Evolution_Create(pending_pu, stderr);
    if (graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots(graph, stderr))
        return 1;
    analysis = DSL_Tensor_Analysis_Create(pending_pu, graph, stderr);
    if (analysis == NULL || !DSL_Tensor_Analysis_Build(analysis, quiet) ||
        DSL_Tensor_Analysis_Is_Complete(analysis) ||
        !DSL_Tensor_Analysis_Find_Fact
             (analysis, DSL_Builder_Get_Value_Image_Id(pending), &fact) ||
        fact.dimension_state != DSL_TENSOR_DIMENSION_UNRESOLVED ||
        fact.dynamic_dimension_count != DSL_TENSOR_DIMENSION_COUNT_UNKNOWN ||
        (fact.completeness & DSL_TENSOR_FACT_COMPLETE_SHAPE) != 0)
        return 1;
    DSL_Tensor_Analysis_Destroy(analysis);
    DSL_Tensor_Evolution_Destroy(graph);

    if (!Create_Matmul_Fixture(&first))
        return 1;
    graph = DSL_Tensor_Evolution_Create(first.pu, stderr);
    if (graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots(graph, stderr))
        return 1;
    analysis = DSL_Tensor_Analysis_Create(first.pu, graph, stderr);
    if (analysis == NULL || !DSL_Tensor_Analysis_Build(analysis, stderr) ||
        !Create_CNN_Fixture(&second) ||
        DSL_Tensor_Analysis_Verify(analysis, quiet) ||
        !DSL_Builder_Select_PU(first.pu) ||
        !DSL_Tensor_Analysis_Verify(analysis, stderr))
        return 1;
    DSL_Tensor_Analysis_Destroy(analysis);
    DSL_Tensor_Evolution_Destroy(graph);
    fclose(quiet);
    printf("AIO-3 incomplete and per-PU ownership contracts passed\n");
    return 0;
}

int
main(void)
{
    const char *mode;
    Initialize_Test_Context();
    mode = getenv("OPEN64_AIO3_MODE");
    if (mode == NULL)
        return 1;
    if (strcmp(mode, "before") == 0)
        return Run_Image_Mode(FALSE);
    if (strcmp(mode, "after") == 0)
        return Run_Image_Mode(TRUE);
    if (strcmp(mode, "metadata") == 0)
        return Run_Metadata_Independence();
    if (strcmp(mode, "contract") == 0)
        return Run_Incomplete_And_Ownership();
    return 1;
}
