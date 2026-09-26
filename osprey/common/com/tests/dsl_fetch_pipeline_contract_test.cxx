/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Certifies AIO-10 fetch/pipeline planning, target capability fallback,
 * buffering, overlap cost, safety rejection, and unchanged binary WHIRL.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO10-FETCH-PIPELINE.md.
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
#include "dsl_fetch_pipeline.h"
#include "dsl_opcode.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void Signal_Cleanup(INT sig) {}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
    return "";
}

typedef struct {
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_VALUE values[4];
    UINT32 file_id;
} AIO10_FIXTURE;

typedef struct {
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_TENSOR_ANALYSIS *tensor;
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    DSL_TILE_ANALYSIS *tile;
    DSL_FETCH_PIPELINE_ANALYSIS *pipeline;
} AIO10_ANALYSIS;

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
Create_Tensor_Type (const char *name)
{
    TY_TENSOR_CANONICAL_DESCRIPTOR descriptor;
    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.kind = "tensor";
    descriptor.dtype = "float32";
    descriptor.rank = 2;
    descriptor.logical_shape = "[64,64]";
    descriptor.traits = "dense";
    descriptor.layout = "row_major";
    descriptor.sharding = "replicated";
    descriptor.placement = "host";
    descriptor.memory = "contiguous";
    descriptor.quantization = "none";
    return TY_Intern_Tensor_Type
               (name, MTYPE_To_TY(MTYPE_F4), &descriptor);
}

static BOOL
Set_Source (DSL_BUILDER_VALUE value, UINT32 file_id, UINT32 line)
{
    DSL_BUILDER_SOURCE_POSITION source;
    memset(&source, 0, sizeof(source));
    source.file_id = file_id;
    source.statement_begin = 1;
    source.line = line;
    return value != NULL &&
           DSL_Builder_Set_Value_Source_Position(value, &source);
}

static BOOL
Create_Fixture (const char *name, AIO10_FIXTURE *fixture)
{
    DSL_BUILDER_PU_SOURCE_IDENTITY identity;
    DSL_BUILDER_OPERATOR_ATTRIBUTE attributes[2];
    DSL_BUILDER_VALUE kids[2];
    DSL_DOMAIN_ID common;
    DSL_OPCODE_ID matmul;
    DSL_OPCODE_ID relu;
    TY_IDX tensor_ty;

    memset(fixture, 0, sizeof(*fixture));
    fixture->pu = DSL_Builder_Create_Minimal_PU(name);
    if (fixture->pu == NULL)
        return FALSE;
    memset(&identity, 0, sizeof(identity));
    identity.canonical_definition_name = name;
    identity.defining_module = "aio10.fetch_pipeline_contract";
    identity.defining_file = __FILE__;
    identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(fixture->pu, &identity))
        return FALSE;
    fixture->file_id = DSL_Builder_Register_Source_File
                           (fixture->pu, __FILE__);
    tensor_ty = Create_Tensor_Type(name);
    common = DSL_Domain_Find("common");
    matmul = DSL_Opcode_Find(common, DSL_OPCODE_COMMON_MATMUL, 1);
    relu = DSL_Opcode_Find(common, "common.relu", 2);
    if (fixture->file_id == 0 || tensor_ty == TY_IDX_ZERO ||
        matmul == DSL_OPCODE_INVALID_ID || relu == DSL_OPCODE_INVALID_ID)
        return FALSE;

    fixture->values[0] = DSL_Builder_Create_Tensor_Constant
                             ("aio10_kid0", tensor_ty, "float32", 2,
                              "[64,64]", "splat", "1.0");
    fixture->values[1] = DSL_Builder_Create_Tensor_Constant
                             ("aio10_kid1", tensor_ty, "float32", 2,
                              "[64,64]", "splat", "1.0");
    attributes[0].name = "attr.transpose_kid0";
    attributes[0].value = "false";
    attributes[1].name = "attr.transpose_kid1";
    attributes[1].value = "false";
    kids[0] = fixture->values[0];
    kids[1] = fixture->values[1];
    fixture->values[2] = DSL_Builder_Create_Operator_With_Result
                             (matmul, 1, kids, 2, attributes, 2,
                              "aio10_matmul", tensor_ty);
    kids[0] = fixture->values[2];
    fixture->values[3] = DSL_Builder_Create_Operator_With_Result
                             (relu, 2, kids, 1, NULL, 0,
                              "aio10_relu", tensor_ty);
    const UINT32 source_lines[4] = { 132, 135, 144, 148 };
    for (UINT32 i = 0; i < 4; ++i) {
        if (fixture->values[i] == NULL ||
            !Set_Source(fixture->values[i], fixture->file_id,
                        source_lines[i]) ||
            !DSL_Builder_Append_PU_Value(fixture->pu, fixture->values[i]))
            return FALSE;
    }
    return TRUE;
}

static DSL_IR_NODE_ID
Value_Node (DSL_BUILDER_VALUE value)
{
    DSL_IR_VALUE_RECORD record;
    return DSL_IR_Image_Get_Value
               (DSL_Builder_Get_Value_Image_Id(value), &record) ?
           record.producer_node_id : DSL_IR_NODE_INVALID_ID;
}

static DSL_TENSOR_CONTROL_SNAPSHOT *
Create_Snapshot (const AIO10_FIXTURE *fixture)
{
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot =
        DSL_Tensor_Control_Snapshot_Create(fixture->pu, stderr);
    DSL_TENSOR_CONTROL_BLOCK block;
    memset(&block, 0, sizeof(block));
    block.block_id = 1;
    block.reverse_postorder = 1;
    if (snapshot == NULL ||
        !DSL_Tensor_Control_Snapshot_Add_Block(snapshot, &block, stderr))
        return NULL;
    for (UINT32 i = 0; i < 4; ++i) {
        DSL_TENSOR_CONTROL_POSITION position;
        memset(&position, 0, sizeof(position));
        position.node_id = Value_Node(fixture->values[i]);
        position.block_id = 1;
        position.reverse_postorder = 1;
        position.statement_order = i + 1;
        if (!DSL_Tensor_Control_Snapshot_Add_Position
                 (snapshot, &position, stderr))
            return NULL;
    }
    return DSL_Tensor_Control_Snapshot_Seal(snapshot, stderr) ?
           snapshot : NULL;
}

static void
Destroy_Analysis (AIO10_ANALYSIS *analysis)
{
    DSL_Fetch_Pipeline_Destroy(analysis->pipeline);
    DSL_Tile_Destroy(analysis->tile);
    DSL_Tensor_Locality_Destroy(analysis->locality);
    DSL_Tensor_Control_Snapshot_Destroy(analysis->snapshot);
    DSL_Tensor_Analysis_Destroy(analysis->tensor);
    DSL_Tensor_Evolution_Destroy(analysis->graph);
    memset(analysis, 0, sizeof(*analysis));
}

static BOOL
Build_Analysis (const AIO10_FIXTURE *fixture, UINT32 profile,
                UINT32 prefetch_distance, BOOL select_plans,
                FILE *trace, AIO10_ANALYSIS *analysis)
{
    DSL_TILE_CONTROL tile_control;
    DSL_FETCH_PIPELINE_CONTROL fetch_control;
    memset(analysis, 0, sizeof(*analysis));
    analysis->graph = DSL_Tensor_Evolution_Create(fixture->pu, stderr);
    if (analysis->graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots
             (analysis->graph, stderr))
        return FALSE;
    analysis->tensor = DSL_Tensor_Analysis_Create
                           (fixture->pu, analysis->graph, stderr);
    if (analysis->tensor == NULL ||
        !DSL_Tensor_Analysis_Build(analysis->tensor, stderr))
        return FALSE;
    analysis->snapshot = Create_Snapshot(fixture);
    analysis->locality = DSL_Tensor_Locality_Create
                             (fixture->pu, analysis->tensor,
                              analysis->snapshot, stderr);
    if (analysis->snapshot == NULL || analysis->locality == NULL ||
        !DSL_Tensor_Locality_Build(analysis->locality, stderr))
        return FALSE;

    DSL_Tile_Control_Init(&tile_control);
    tile_control.target_profile_id = profile;
    tile_control.focus_value_id =
        DSL_Builder_Get_Value_Image_Id(fixture->values[2]);
    analysis->tile = DSL_Tile_Create
                         (fixture->pu, analysis->graph, analysis->tensor,
                          analysis->locality, NULL, &tile_control, stderr);
    if (analysis->tile == NULL ||
        !DSL_Tile_Build(analysis->tile, stderr) ||
        !DSL_Tile_Verify(analysis->tile, stderr))
        return FALSE;

    DSL_Fetch_Pipeline_Control_Init(&fetch_control);
    fetch_control.target_profile_id = profile;
    fetch_control.focus_value_id = tile_control.focus_value_id;
    fetch_control.prefetch_distance_hint = prefetch_distance;
    fetch_control.select_plans = select_plans;
    analysis->pipeline = DSL_Fetch_Pipeline_Create
                             (fixture->pu, analysis->graph,
                              analysis->locality, NULL, analysis->tile,
                              &fetch_control, stderr);
    if (analysis->pipeline == NULL ||
        !DSL_Fetch_Pipeline_Build(analysis->pipeline, stderr) ||
        !DSL_Fetch_Pipeline_Verify(analysis->pipeline, stderr))
        return FALSE;
    if (trace != NULL) {
        DSL_Tile_Print(trace, analysis->tile);
        DSL_Fetch_Pipeline_Print(trace, analysis->pipeline);
    }
    return TRUE;
}

static BOOL
Check_Main_Contract (const AIO10_ANALYSIS *analysis, UINT32 profile,
                     BOOL unsafe_distance)
{
    DSL_FETCH_SITE_RECORD site;
    UINT32 expected_plans = profile == DSL_TARGET_PROFILE_CPU_BASELINE ?
                            1 : 4;
    UINT32 expected_fetches = expected_plans * 2;
    UINT32 expected_stages = profile == DSL_TARGET_PROFILE_CPU_BASELINE ?
                             1 : 7;
    if (DSL_Fetch_Pipeline_Site_Count(analysis->pipeline) != 1 ||
        DSL_Fetch_Pipeline_Plan_Count(analysis->pipeline) !=
            expected_plans ||
        DSL_Fetch_Pipeline_Fetch_Count(analysis->pipeline) !=
            expected_fetches ||
        DSL_Fetch_Pipeline_Stage_Count(analysis->pipeline) !=
            expected_stages ||
        !DSL_Fetch_Pipeline_Get_Site
             (analysis->pipeline, 1, &site) ||
        site.pipeline_plan_count != expected_plans)
        return FALSE;

    UINT32 proven = 0;
    UINT32 rejected = 0;
    UINT32 async_stages = 0;
    UINT32 tma_stages = 0;
    UINT32 selected_engine = DSL_MEMORY_MOVEMENT_UNKNOWN;
    for (UINT32 id = 1; id <= expected_plans; ++id) {
        DSL_FETCH_PLAN_RECORD plan;
        if (!DSL_Fetch_Pipeline_Get_Plan
                 (analysis->pipeline, id, &plan) ||
            plan.raw_movement_cost != plan.hidden_movement_cost +
                                      plan.unhidden_movement_cost)
            return FALSE;
        if (plan.optimization_plan_id == site.selected_plan_id)
            selected_engine = plan.engine;
        if (plan.engine == DSL_MEMORY_MOVEMENT_ASYNC_COPY)
            async_stages = plan.stage_count;
        if (plan.engine ==
                DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC)
            tma_stages = plan.stage_count;
        if (id != 1 && plan.legality == DSL_OPT_LEGALITY_PROVEN)
            ++proven;
        if (id != 1 && plan.legality == DSL_OPT_LEGALITY_REJECTED &&
            plan.rejection_reason == DSL_OPT_REJECT_RESOURCE)
            ++rejected;
    }
    if (profile == DSL_TARGET_PROFILE_CPU_BASELINE)
        return selected_engine == DSL_MEMORY_MOVEMENT_DEMAND;
    if (async_stages != 2 || tma_stages != 3)
        return FALSE;
    if (unsafe_distance)
        return rejected == 2 && proven == 1 &&
               selected_engine == DSL_MEMORY_MOVEMENT_VECTOR;
    return proven == 3 && rejected == 0 &&
           selected_engine ==
               DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC;
}

static int
Run_Image_Mode (BOOL run_analysis)
{
    const char *artifact = getenv("OPEN64_AIO10_ARTIFACT");
    const char *trace_path = getenv("OPEN64_AIO10_ANALYSIS");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_VERIFY_RESULT verify;
    AIO10_FIXTURE fixture;
    AIO10_ANALYSIS analysis;
    UINT32 nodes;
    UINT32 values;
    UINT32 types;
    char diagnostic[4096];
    FILE *trace = NULL;
    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio10_pipeline", &fixture))
        return 1;
    nodes = DSL_IR_Image_Node_Count();
    values = DSL_IR_Image_Value_Count();
    types = TY_Table_Size();
    if (run_analysis) {
        if (trace_path == NULL || trace_path[0] == '\0' ||
            (trace = fopen(trace_path, "w")) == NULL ||
            !Build_Analysis
                 (&fixture, DSL_TARGET_PROFILE_NVIDIA_HOPPER, 1,
                  TRUE, trace, &analysis) ||
            !Check_Main_Contract
                 (&analysis, DSL_TARGET_PROFILE_NVIDIA_HOPPER, FALSE) ||
            DSL_IR_Image_Node_Count() != nodes ||
            DSL_IR_Image_Value_Count() != values ||
            TY_Table_Size() != types)
            return 1;
        fclose(trace);
        Destroy_Analysis(&analysis);
    }
    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify))
        return 1;
    request.path = artifact;
    request.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&request))
        return 1;
    printf("AIO-10 %s G12 image passed\n",
           run_analysis ? "analyzed" : "baseline");
    return 0;
}

static int
Run_Target (UINT32 profile, BOOL unsafe_distance)
{
    const char *trace_path = getenv("OPEN64_AIO10_ANALYSIS");
    AIO10_FIXTURE fixture;
    AIO10_ANALYSIS analysis;
    FILE *trace = trace_path == NULL ? NULL : fopen(trace_path, "w");
    if (trace_path != NULL && trace == NULL)
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio10_target", &fixture) ||
        !Build_Analysis
             (&fixture, profile, unsafe_distance ? 4 : 1,
              TRUE, trace, &analysis) ||
        !Check_Main_Contract(&analysis, profile, unsafe_distance))
        return 1;
    if (trace != NULL)
        fclose(trace);
    Destroy_Analysis(&analysis);
    printf("AIO-10 target profile %u%s passed\n", profile,
           unsafe_distance ? " unsafe-distance rejection" : "");
    return 0;
}

static int
Run_Control(void)
{
    AIO10_FIXTURE fixture;
    AIO10_ANALYSIS analysis;
    DSL_FETCH_PIPELINE_CONTROL control;
    DSL_FETCH_PIPELINE_ANALYSIS *disabled;
    FILE *quiet = tmpfile();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (quiet == NULL || !Create_Fixture("aio10_control", &fixture) ||
        !Build_Analysis
             (&fixture, DSL_TARGET_PROFILE_NVIDIA_HOPPER, 1,
              FALSE, NULL, &analysis))
        return 1;
    DSL_Fetch_Pipeline_Destroy(analysis.pipeline);
    analysis.pipeline = NULL;
    DSL_Fetch_Pipeline_Control_Init(&control);
    control.apply_transformation = 1;
    if (DSL_Fetch_Pipeline_Create
            (fixture.pu, analysis.graph, analysis.locality, NULL,
             analysis.tile, &control, quiet) != NULL)
        return 1;
    control.apply_transformation = 0;
    control.generate_candidates = 0;
    control.select_plans = 0;
    disabled = DSL_Fetch_Pipeline_Create
                   (fixture.pu, analysis.graph, analysis.locality, NULL,
                    analysis.tile, &control, stderr);
    if (disabled == NULL ||
        !DSL_Fetch_Pipeline_Build(disabled, stderr) ||
        !DSL_Fetch_Pipeline_Verify(disabled, stderr) ||
        DSL_Fetch_Pipeline_Site_Count(disabled) != 0)
        return 1;
    DSL_Fetch_Pipeline_Destroy(disabled);
    Destroy_Analysis(&analysis);
    fclose(quiet);
    printf("AIO-10 control contract passed\n");
    return 0;
}

int
main(void)
{
    const char *mode;
    Initialize_Test_Context();
    mode = getenv("OPEN64_AIO10_MODE");
    if (mode == NULL)
        return 1;
    if (strcmp(mode, "before") == 0)
        return Run_Image_Mode(FALSE);
    if (strcmp(mode, "pipeline") == 0)
        return Run_Image_Mode(TRUE);
    if (strcmp(mode, "hopper") == 0)
        return Run_Target(DSL_TARGET_PROFILE_NVIDIA_HOPPER, FALSE);
    if (strcmp(mode, "blackwell") == 0)
        return Run_Target(DSL_TARGET_PROFILE_NVIDIA_BLACKWELL, FALSE);
    if (strcmp(mode, "cpu") == 0)
        return Run_Target(DSL_TARGET_PROFILE_CPU_BASELINE, FALSE);
    if (strcmp(mode, "unsafe") == 0)
        return Run_Target(DSL_TARGET_PROFILE_NVIDIA_HOPPER, TRUE);
    if (strcmp(mode, "control") == 0)
        return Run_Control();
    return 1;
}
