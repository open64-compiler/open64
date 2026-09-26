/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Certifies the AIO-1 PU-local TensorEvolutionGraph ownership, immutable-root,
 * and check-only representation contracts. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO1-TENSOR-EVOLUTION.md.
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
    DSL_BUILDER_VALUE kid0;
    DSL_BUILDER_VALUE kid1;
    DSL_BUILDER_VALUE result;
} AIO1_FIXTURE;

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
Create_Tensor_Type(const char *name, const char *shape)
{
    TY_TENSOR_CANONICAL_DESCRIPTOR descriptor;
    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.kind = "tensor";
    descriptor.dtype = "float32";
    descriptor.rank = 2;
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
Create_Fixture(const char *pu_name, AIO1_FIXTURE *fixture)
{
    DSL_BUILDER_PU_SOURCE_IDENTITY identity;
    DSL_BUILDER_SOURCE_POSITION source_position;
    DSL_BUILDER_OPERATOR_ATTRIBUTE attributes[2];
    DSL_BUILDER_VALUE kids[2];
    DSL_DOMAIN_ID common_domain;
    DSL_OPCODE_ID matmul_opcode;
    TY_IDX kid0_ty;
    TY_IDX kid1_ty;
    TY_IDX result_ty;
    UINT32 file_id;

    if (fixture == NULL)
        return FALSE;
    memset(fixture, 0, sizeof(*fixture));
    fixture->pu = DSL_Builder_Create_Minimal_PU(pu_name);
    if (fixture->pu == NULL)
        return FALSE;

    memset(&identity, 0, sizeof(identity));
    identity.canonical_definition_name = pu_name;
    identity.defining_module = "aio1.tensor_evolution_contract";
    identity.defining_file = __FILE__;
    identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(fixture->pu, &identity))
        return FALSE;
    file_id = DSL_Builder_Register_Source_File(fixture->pu, __FILE__);
    if (file_id == 0)
        return FALSE;

    kid0_ty = Create_Tensor_Type("aio1_kid0_type", "[2,3]");
    kid1_ty = Create_Tensor_Type("aio1_kid1_type", "[3,4]");
    result_ty = Create_Tensor_Type("aio1_result_type", "[2,4]");
    if (kid0_ty == TY_IDX_ZERO || kid1_ty == TY_IDX_ZERO ||
        result_ty == TY_IDX_ZERO)
        return FALSE;

    memset(&source_position, 0, sizeof(source_position));
    source_position.file_id = file_id;
    source_position.statement_begin = 1;
    source_position.line = __LINE__ + 1;
    fixture->kid0 = DSL_Builder_Create_Tensor_Constant
                        ("kid0", kid0_ty, "float32", 2, "[2,3]",
                         "splat", "1.0");
    if (fixture->kid0 == NULL ||
        !DSL_Builder_Set_Value_Source_Position
             (fixture->kid0, &source_position))
        return FALSE;

    source_position.line = __LINE__ + 1;
    fixture->kid1 = DSL_Builder_Create_Tensor_Constant
                        ("kid1", kid1_ty, "float32", 2, "[3,4]",
                         "splat", "1.0");
    if (fixture->kid1 == NULL ||
        !DSL_Builder_Set_Value_Source_Position
             (fixture->kid1, &source_position))
        return FALSE;

    common_domain = DSL_Domain_Find("common");
    matmul_opcode = DSL_Opcode_Find
                        (common_domain, DSL_OPCODE_COMMON_MATMUL, 1);
    attributes[0].name = "attr.transpose_kid0";
    attributes[0].value = "false";
    attributes[1].name = "attr.transpose_kid1";
    attributes[1].value = "false";
    kids[0] = fixture->kid0;
    kids[1] = fixture->kid1;
    source_position.line = __LINE__ + 1;
    fixture->result = DSL_Builder_Create_Operator_With_Result
                          (matmul_opcode, 1, kids, 2, attributes, 2,
                           "matmul_result", result_ty);
    if (fixture->result == NULL ||
        !DSL_Builder_Set_Value_Source_Position
             (fixture->result, &source_position) ||
        !DSL_Builder_Append_PU_Value(fixture->pu, fixture->kid0) ||
        !DSL_Builder_Append_PU_Value(fixture->pu, fixture->kid1) ||
        !DSL_Builder_Append_PU_Value(fixture->pu, fixture->result))
        return FALSE;
    return TRUE;
}

static BOOL
Check_Root
        (DSL_TENSOR_EVOLUTION_GRAPH *graph,
         DSL_BUILDER_VALUE value,
         DSL_TENSOR_EVOLUTION_NODE_ID expected_id,
         TY_IDX expected_ty)
{
    DSL_TENSOR_EVOLUTION_NODE_RECORD root;
    return DSL_Tensor_Evolution_Find_Semantic_Root
               (graph, DSL_Builder_Get_Value_Image_Id(value), &root) &&
           root.id == expected_id &&
           root.kind == DSL_TENSOR_EVOLUTION_NODE_SEMANTIC &&
           root.semantic_value_id == DSL_Builder_Get_Value_Image_Id(value) &&
           root.descriptor_ty == expected_ty &&
           root.semantic_root_id == root.id && root.flags == 0;
}

static int
Run_Image_Mode(BOOL build_graph)
{
    const char *artifact = getenv("OPEN64_AIO1_ARTIFACT");
    const char *graph_path = getenv("OPEN64_AIO1_GRAPH");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST image_request;
    DSL_TENSOR_EVOLUTION_GRAPH *graph = NULL;
    AIO1_FIXTURE fixture;
    UINT32 image_nodes;
    UINT32 image_values;
    UINT32 type_count;
    DSL_TENSOR_EVOLUTION_NODE_RECORD node;
    DSL_TENSOR_EVOLUTION_EDGE_RECORD edge;

    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio1_common_matmul", &fixture))
        return 1;

    image_nodes = DSL_IR_Image_Node_Count();
    image_values = DSL_IR_Image_Value_Count();
    type_count = TY_Table_Size();
    if (build_graph) {
        graph = DSL_Tensor_Evolution_Create(fixture.pu, stderr);
        if (graph == NULL ||
            !DSL_Tensor_Evolution_Build_Semantic_Roots(graph, stderr) ||
            !DSL_Tensor_Evolution_Build_Semantic_Roots(graph, stderr) ||
            !DSL_Tensor_Evolution_Verify(graph, stderr) ||
            DSL_Tensor_Evolution_Node_Count(graph) != 3 ||
            DSL_Tensor_Evolution_Edge_Count(graph) != 0 ||
            !Check_Root(graph, fixture.kid0, 1,
                        DSL_Builder_Get_Value_Type(fixture.kid0)) ||
            !Check_Root(graph, fixture.kid1, 2,
                        DSL_Builder_Get_Value_Type(fixture.kid1)) ||
            !Check_Root(graph, fixture.result, 3,
                        DSL_Builder_Get_Value_Type(fixture.result)) ||
            DSL_Tensor_Evolution_Get_Node
                (graph, DSL_TENSOR_EVOLUTION_NODE_INVALID_ID, &node) ||
            DSL_Tensor_Evolution_Get_Node(graph, 4, &node) ||
            DSL_Tensor_Evolution_Get_Edge(graph, 1, &edge) ||
            DSL_Tensor_Evolution_Find_Semantic_Root
                (graph, DSL_IR_VALUE_INVALID_ID, &node) ||
            strcmp(DSL_Tensor_Evolution_Node_Kind_Name(99), "unknown") != 0 ||
            strcmp(DSL_Tensor_Evolution_Transform_Kind_Name(99),
                   "unknown") != 0 ||
            DSL_IR_Image_Node_Count() != image_nodes ||
            DSL_IR_Image_Value_Count() != image_values ||
            TY_Table_Size() != type_count) {
            fprintf(stderr, "AIO-1 semantic root contract changed\n");
            DSL_Tensor_Evolution_Destroy(graph);
            return 1;
        }
        if (graph_path != NULL && graph_path[0] != '\0') {
            FILE *file = fopen(graph_path, "w");
            if (file == NULL) {
                DSL_Tensor_Evolution_Destroy(graph);
                return 1;
            }
            DSL_Tensor_Evolution_Print(file, graph);
            fclose(file);
        }
        DSL_Tensor_Evolution_Destroy(graph);
        graph = DSL_Tensor_Evolution_Create(fixture.pu, stderr);
        if (graph == NULL ||
            !DSL_Tensor_Evolution_Build_Semantic_Roots(graph, stderr) ||
            DSL_Tensor_Evolution_Node_Count(graph) != 3 ||
            DSL_Tensor_Evolution_Edge_Count(graph) != 0) {
            fprintf(stderr, "AIO-1 graph reset contract changed\n");
            DSL_Tensor_Evolution_Destroy(graph);
            return 1;
        }
        DSL_Tensor_Evolution_Destroy(graph);
    }

    image_request.path = artifact;
    image_request.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&image_request))
        return 1;
    printf("AIO-1 %s image passed\n", build_graph ? "after" : "before");
    return 0;
}

static int
Run_Ownership_Contract(void)
{
    AIO1_FIXTURE first;
    AIO1_FIXTURE second;
    DSL_TENSOR_EVOLUTION_GRAPH *first_graph;
    DSL_TENSOR_EVOLUTION_GRAPH *second_graph;
    FILE *quiet = tmpfile();

    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio1_first_pu", &first))
        return 1;
    first_graph = DSL_Tensor_Evolution_Create(first.pu, stderr);
    if (first_graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots(first_graph, stderr) ||
        DSL_Tensor_Evolution_Node_Count(first_graph) != 3)
        return 1;

    if (!Create_Fixture("aio1_second_pu", &second) || quiet == NULL ||
        DSL_Tensor_Evolution_Create(first.pu, quiet) != NULL ||
        DSL_Tensor_Evolution_Build_Semantic_Roots(first_graph, quiet) ||
        DSL_Tensor_Evolution_Node_Count(first_graph) != 3)
        return 1;
    second_graph = DSL_Tensor_Evolution_Create(second.pu, stderr);
    if (second_graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots(second_graph, stderr) ||
        DSL_Tensor_Evolution_Node_Count(second_graph) != 3 ||
        DSL_Tensor_Evolution_Verify(first_graph, quiet))
        return 1;

    if (!DSL_Builder_Select_PU(first.pu) ||
        !DSL_Tensor_Evolution_Verify(first_graph, stderr) ||
        DSL_Tensor_Evolution_Verify(second_graph, quiet) ||
        !DSL_Builder_Select_PU(second.pu) ||
        !DSL_Tensor_Evolution_Verify(second_graph, stderr))
        return 1;

    DSL_Tensor_Evolution_Destroy(first_graph);
    DSL_Tensor_Evolution_Destroy(second_graph);
    fclose(quiet);
    printf("AIO-1 per-PU ownership contract passed\n");
    return 0;
}

int
main(void)
{
    const char *mode;
    Initialize_Test_Context();
    mode = getenv("OPEN64_AIO1_MODE");
    if (mode == NULL)
        return 1;
    if (strcmp(mode, "before") == 0)
        return Run_Image_Mode(FALSE);
    if (strcmp(mode, "after") == 0)
        return Run_Image_Mode(TRUE);
    if (strcmp(mode, "contract") == 0)
        return Run_Ownership_Contract();
    return 1;
}
