/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Certifies AIO-10 fetch/pipeline planning, target capability fallback,
 * buffering, overlap cost, safety rejection, and unchanged binary WHIRL.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO10-FETCH-PIPELINE.md.
 * Direct negatives: doc/AI-COMPILER-OPTIMIZATION-D-S1-T2-COMPARISON.md.
 * Resource gates: doc/AI-COMPILER-OPTIMIZATION-MOVEMENT-RESOURCES.md.
 * Hardware lookup: doc/AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "stab.h"
#include "ir_reader.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "config_dsl.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_tensor_analysis_opt.h"
#include "dsl_tensor_evolution_opt.h"
#include "dsl_tensor_locality_opt.h"
#include "dsl_fetch_pipeline.h"
#include "dsl_fetch_pipeline_opt.h"
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

/* Build one test-owned semantic PU and preserve exact builder source lines
 * for independent -src inspection; failure stops certification before output. */
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
    UINT32 source_lines[4];

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

    source_lines[0] = __LINE__ + 1;
    fixture->values[0] = DSL_Builder_Create_Tensor_Constant
                             ("aio10_kid0", tensor_ty, "float32", 2,
                              "[64,64]", "splat", "1.0");
    source_lines[1] = __LINE__ + 1;
    fixture->values[1] = DSL_Builder_Create_Tensor_Constant
                             ("aio10_kid1", tensor_ty, "float32", 2,
                              "[64,64]", "splat", "1.0");
    attributes[0].name = "attr.transpose_kid0";
    attributes[0].value = "false";
    attributes[1].name = "attr.transpose_kid1";
    attributes[1].value = "false";
    kids[0] = fixture->values[0];
    kids[1] = fixture->values[1];
    source_lines[2] = __LINE__ + 1;
    fixture->values[2] = DSL_Builder_Create_Operator_With_Result
                             (matmul, 1, kids, 2, attributes, 2,
                              "aio10_matmul", tensor_ty);
    kids[0] = fixture->values[2];
    source_lines[3] = __LINE__ + 1;
    fixture->values[3] = DSL_Builder_Create_Operator_With_Result
                             (relu, 2, kids, 1, NULL, 0,
                              "aio10_relu", tensor_ty);
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
        DSL_tensor_control_snapshot_create(fixture->pu, stderr);
    DSL_TENSOR_CONTROL_BLOCK block;
    memset(&block, 0, sizeof(block));
    block.block_id = 1;
    block.reverse_postorder = 1;
    if (snapshot == NULL ||
        !DSL_tensor_control_snapshot_add_block(snapshot, &block, stderr))
        return NULL;
    for (UINT32 i = 0; i < 4; ++i) {
        DSL_TENSOR_CONTROL_POSITION position;
        memset(&position, 0, sizeof(position));
        position.node_id = Value_Node(fixture->values[i]);
        position.block_id = 1;
        position.reverse_postorder = 1;
        position.statement_order = i + 1;
        if (!DSL_tensor_control_snapshot_add_position
                 (snapshot, &position, stderr))
            return NULL;
    }
    return DSL_tensor_control_snapshot_seal(snapshot, stderr) ?
           snapshot : NULL;
}

static void
Destroy_Analysis (AIO10_ANALYSIS *analysis)
{
    VHO_DSL_Fetch_Pipeline_Destroy(analysis->pipeline);
    VHO_DSL_Tile_Destroy(analysis->tile);
    DSL_tensor_locality_destroy(analysis->locality);
    DSL_tensor_control_snapshot_destroy(analysis->snapshot);
    DSL_tensor_analysis_destroy(analysis->tensor);
    DSL_tensor_evolution_destroy(analysis->graph);
    memset(analysis, 0, sizeof(*analysis));
}

/* Build PU-local movement alternatives using a compact TMA-capable P7
 * fixture; joint mode defers P7 resources and uses an explicit topology.
 * Restore the caller option after construction; no executable tree changes. */
static BOOL
Build_Analysis (const AIO10_FIXTURE *fixture, UINT32 profile,
                UINT32 prefetch_distance, BOOL select_plans,
                FILE *trace, AIO10_ANALYSIS *analysis,
                BOOL movement_aware = FALSE)
{
    VHO_DSL_TILE_CONTROL tile_control;
    VHO_DSL_FETCH_PIPELINE_CONTROL fetch_control;
    memset(analysis, 0, sizeof(*analysis));
    analysis->graph = DSL_tensor_evolution_create(fixture->pu, stderr);
    if (analysis->graph == NULL ||
        !VHO_DSL_Tensor_Evolution_Build_Semantic_Roots
             (analysis->graph, stderr))
        return FALSE;
    analysis->tensor = DSL_tensor_analysis_create
                           (fixture->pu, analysis->graph, stderr);
    if (analysis->tensor == NULL ||
        !VHO_DSL_Tensor_Analysis_Build(analysis->tensor, stderr))
        return FALSE;
    analysis->snapshot = Create_Snapshot(fixture);
    analysis->locality = DSL_tensor_locality_create
                             (fixture->pu, analysis->tensor,
                              analysis->snapshot, stderr);
    if (analysis->snapshot == NULL || analysis->locality == NULL ||
        !VHO_DSL_Tensor_Locality_Build(analysis->locality, stderr))
        return FALSE;

    VHO_DSL_Tile_Control_Init(&tile_control);
    const BOOL saved_tma = VHO_DSL_CUDA_TMA_Prefetch;
    VHO_DSL_CUDA_TMA_Prefetch = TRUE;
    tile_control.target_profile_id = profile;
    tile_control.focus_value_id =
        DSL_Builder_Get_Value_Image_Id(fixture->values[2]);
    if (movement_aware) {
        tile_control.select_plans = 0;
        tile_control.defer_movement_resources = 1;
    }
    analysis->tile = VHO_DSL_Tile_Create
                         (fixture->pu, analysis->graph, analysis->tensor,
                          analysis->locality, NULL, &tile_control, stderr);
    const BOOL tile_valid = analysis->tile != NULL &&
        VHO_DSL_Tile_Build(analysis->tile, stderr) &&
        VHO_DSL_Tile_Verify(analysis->tile, stderr);
    VHO_DSL_CUDA_TMA_Prefetch = saved_tma;
    if (!tile_valid)
        return FALSE;

    VHO_DSL_Fetch_Pipeline_Control_Init(&fetch_control);
    fetch_control.target_profile_id = profile;
    fetch_control.focus_value_id = tile_control.focus_value_id;
    fetch_control.prefetch_distance_hint = prefetch_distance;
    fetch_control.select_plans = select_plans;
    if (movement_aware) {
        fetch_control.joint_cost = fetch_control.enable_direct = 1;
        fetch_control.tile_plan_id = 2;
        fetch_control.enable_vector = fetch_control.enable_async_copy = 0;
        fetch_control.maximum_buffer_stages = 2;
    }
    analysis->pipeline = VHO_DSL_Fetch_Pipeline_Create
                             (fixture->pu, analysis->graph,
                              analysis->locality, NULL, analysis->tile,
                              &fetch_control, stderr);
    if (analysis->pipeline == NULL ||
        !VHO_DSL_Fetch_Pipeline_Build(analysis->pipeline, stderr) ||
        !VHO_DSL_Fetch_Pipeline_Verify(analysis->pipeline, stderr))
        return FALSE;
    if (trace != NULL) {
        VHO_DSL_Tile_Print(trace, analysis->tile);
        VHO_DSL_Fetch_Pipeline_Print(trace, analysis->pipeline);
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
    const DSL_FETCH_PIPELINE_IR *pipeline_ir =
        VHO_DSL_Fetch_Pipeline_Get_IR(analysis->pipeline);
    if (DSL_fetch_pipeline_ir_site_count(pipeline_ir) != 1 ||
        DSL_fetch_pipeline_ir_plan_count(pipeline_ir) !=
            expected_plans ||
        DSL_fetch_pipeline_ir_fetch_count(pipeline_ir) !=
            expected_fetches ||
        DSL_fetch_pipeline_ir_stage_count(pipeline_ir) !=
            expected_stages ||
        !DSL_fetch_pipeline_ir_get_site(pipeline_ir, 1, &site) ||
        site.pipeline_plan_count != expected_plans)
        return FALSE;

    UINT32 proven = 0;
    UINT32 rejected = 0;
    UINT32 async_stages = 0;
    UINT32 tma_stages = 0;
    UINT32 selected_engine = DSL_MEMORY_MOVEMENT_UNKNOWN;
    for (UINT32 id = 1; id <= expected_plans; ++id) {
        DSL_FETCH_PLAN_RECORD plan;
        if (!DSL_fetch_pipeline_ir_get_plan(pipeline_ir, id, &plan) ||
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
    VHO_DSL_FETCH_PIPELINE_CONTROL control;
    DSL_FETCH_PIPELINE_ANALYSIS *disabled;
    FILE *quiet = tmpfile();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (quiet == NULL || !Create_Fixture("aio10_control", &fixture) ||
        !Build_Analysis
             (&fixture, DSL_TARGET_PROFILE_NVIDIA_HOPPER, 1,
              FALSE, NULL, &analysis))
        return 1;
    VHO_DSL_Fetch_Pipeline_Destroy(analysis.pipeline);
    analysis.pipeline = NULL;
    VHO_DSL_Fetch_Pipeline_Control_Init(&control);
    control.apply_transformation = 1;
    if (VHO_DSL_Fetch_Pipeline_Create
            (fixture.pu, analysis.graph, analysis.locality, NULL,
             analysis.tile, &control, quiet) != NULL)
        return 1;
    control.apply_transformation = 0;
    control.generate_candidates = 0;
    control.select_plans = 0;
    disabled = VHO_DSL_Fetch_Pipeline_Create
                   (fixture.pu, analysis.graph, analysis.locality, NULL,
                    analysis.tile, &control, stderr);
    if (disabled == NULL ||
        !VHO_DSL_Fetch_Pipeline_Build(disabled, stderr) ||
        !VHO_DSL_Fetch_Pipeline_Verify(disabled, stderr) ||
        DSL_fetch_pipeline_ir_site_count
            (VHO_DSL_Fetch_Pipeline_Get_IR(disabled)) != 0)
        return 1;
    VHO_DSL_Fetch_Pipeline_Destroy(disabled);
    Destroy_Analysis(&analysis);
    fclose(quiet);
    printf("AIO-10 control contract passed\n");
    return 0;
}

/* Compose the production P7 classifier and P8 resource/lowering preflight on
 * constrained copies of a real tile. No fixture-only target or option enters
 * production policy; failed assertions leave executable WHIRL unchanged.
 * Include logical occupancy rejection without losing the direct sibling. */
static BOOL
DSL_test_movement_resource_gates
        (const DSL_TILE_PLAN_RECORD &original,
         const std::vector<DSL_FETCH_PLAN_RECORD> &plans)
{
    DSL_TENSOR_LOCALITY_FACT_RECORD locality;
    memset(&locality, 0, sizeof(locality));
    locality.lifetime_state = DSL_TENSOR_LIFETIME_EXACT_BLOCK;
    locality.alias_state = DSL_TENSOR_ALIAS_PROVEN_UNIQUE;
    DSL_TILE_PLAN_RECORD tile = original;
    tile.flags |= DSL_TILE_PLAN_FLAG_DEFERRED_RESOURCES;
    tile.shared_capacity_bytes = tile.shared_bytes_per_stage - 1;
    tile.maximum_buffer_stages = 0;
    VHO_DSL_tile_classify(locality, FALSE, &tile);
    if (tile.legality != DSL_OPT_LEGALITY_REJECTED || tile.rejection_reason != DSL_OPT_REJECT_RESOURCE)
        return FALSE;
    VHO_DSL_tile_classify(locality, TRUE, &tile);
    if (tile.legality != DSL_OPT_LEGALITY_PROVEN) return FALSE;
    DSL_FETCH_PLAN_RECORD staged = plans[0], direct = plans[1], tma = plans[2];
    if (!VHO_DSL_fetch_resource_requirements(tile, staged, &staged.resources) ||
        !VHO_DSL_fetch_resource_requirements(tile, direct, &direct.resources) ||
        staged.resources.shared_bytes <= staged.resources.shared_limit_bytes ||
        direct.resources.shared_bytes != 0 ||
        VHO_DSL_fetch_pipeline_can_lower(tile, staged) ||
        !VHO_DSL_fetch_pipeline_can_lower(tile, direct)) return FALSE;
    tile.shared_capacity_bytes = 0;
    if (!VHO_DSL_fetch_resource_requirements(tile, direct, &direct.resources) ||
        !VHO_DSL_fetch_pipeline_can_lower(tile, direct)) return FALSE;
    tile.shared_capacity_bytes = original.shared_capacity_bytes;
    tile.register_capacity_per_thread = direct.resources.register_bytes_per_thread;
    if (!VHO_DSL_fetch_resource_requirements(tile, staged, &staged.resources) ||
        !VHO_DSL_fetch_resource_requirements(tile, direct, &direct.resources) ||
        staged.resources.register_bytes_per_thread <= tile.register_capacity_per_thread ||
        VHO_DSL_fetch_pipeline_can_lower(tile, staged) ||
        !VHO_DSL_fetch_pipeline_can_lower(tile, direct)) return FALSE;
    --tile.register_capacity_per_thread;
    if (!VHO_DSL_fetch_resource_requirements(tile, direct, &direct.resources) ||
        VHO_DSL_fetch_pipeline_can_lower(tile, direct)) return FALSE;
    tile = original;
    tile.flags |= DSL_TILE_PLAN_FLAG_DEFERRED_RESOURCES;
    tile.shared_capacity_bytes = tile.shared_bytes_per_stage * 2;
    if (!VHO_DSL_fetch_resource_requirements(tile, tma, &tma.resources) ||
        tma.resources.shared_bytes != tile.shared_bytes_per_stage * 2 + 16 ||
        VHO_DSL_fetch_pipeline_can_lower(tile, tma)) return FALSE;
    tile.shared_capacity_bytes += 16;
    if (!VHO_DSL_fetch_resource_requirements(tile, tma, &tma.resources) ||
        !VHO_DSL_fetch_pipeline_can_lower(tile, tma)) return FALSE;
    --tma.resources.shared_bytes;
    if (VHO_DSL_fetch_pipeline_can_lower(tile, tma)) return FALSE;
    DSL_HARDWARE_DESCRIPTION_RECORD hw;
    if (!DSL_hardware_description_find_arch("sm_90", &hw)) return FALSE;
    hw.resources.shared_budget = hw.resources.shared_reserved;
    tile = original;
    DSL_FETCH_RESOURCE_RECORD sr, dr;
    DSL_GEMM_OCCUPANCY_RESULT so, d;
    if (!VHO_DSL_fetch_resource_requirements(tile, plans[0], &sr) ||
        !VHO_DSL_fetch_resource_requirements(tile, plans[1], &dr) ||
        !DSL_gemm_occupancy(hw.resources, hw.performance.warp_lanes, hw.performance.sms,
            sr.threads_per_cta, sr.register_bytes_per_thread, sr.shared_bytes, 100, &so) ||
        !DSL_gemm_occupancy(hw.resources, hw.performance.warp_lanes, hw.performance.sms,
            dr.threads_per_cta, dr.register_bytes_per_thread, dr.shared_bytes, 100, &d) ||
        so.resident != 0 || d.resident == 0) return FALSE;
    staged = plans[0]; direct = plans[1];
    if (!VHO_DSL_fetch_classify_occupancy(tile, hw, &staged) ||
        !VHO_DSL_fetch_classify_occupancy(tile, hw, &direct) ||
        staged.legality != DSL_OPT_LEGALITY_REJECTED ||
        staged.rejection_reason != DSL_OPT_REJECT_RESOURCE ||
        direct.legality != DSL_OPT_LEGALITY_PROVEN) return FALSE;
    DSL_FETCH_PLAN_RECORD saved = direct;
    hw.resources.register_quantum = 0;
    if (VHO_DSL_fetch_classify_occupancy(tile, hw, &direct) ||
        memcmp(&saved, &direct, sizeof(saved))) return FALSE;
    memset(&tma.resources, 0, sizeof(tma.resources));
    if (VHO_DSL_fetch_pipeline_can_lower(tile, tma)) return FALSE;
    for (UINT32 fault = 0; fault < 4; ++fault) {
        DSL_TENSOR_LOCALITY_FACT_RECORD unsafe = locality;
        tile = original;
        if (fault == 0) unsafe.alias_state = DSL_TENSOR_ALIAS_UNKNOWN;
        if (fault == 1) unsafe.lifetime_state = DSL_TENSOR_LIFETIME_EFFECT;
        if (fault == 2) tile.threads_per_cta = 1025;
        if (fault == 3) tile.thread_m = 0;
        VHO_DSL_tile_classify(unsafe, TRUE, &tile);
        if (tile.legality == DSL_OPT_LEGALITY_PROVEN) return FALSE;
    }
    printf("Movement resource gates passed: staged-only SMEM/register rejection preserves D; D register limit, TMA barrier storage, altered ledger and four safety negatives rejected\n");
    return TRUE;
}

/* Exercise direct-plan runtime records and malformed-copy rejection without
 * publishing an image. The active PU supplies all ownership/alias facts;
 * copied arrays isolate each negative and analysis destruction owns cleanup.
 * Also prove the production adapter consumes per-path hardware timing. */
static int
DSL_test_direct_record_contracts ()
{
    AIO10_FIXTURE fixture;
    AIO10_ANALYSIS analysis;
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio10_direct", &fixture) ||
        !Build_Analysis(&fixture, DSL_TARGET_PROFILE_NVIDIA_HOPPER, 1, FALSE, NULL, &analysis, TRUE))
        return 1;
    const DSL_FETCH_PIPELINE_IR *ir = VHO_DSL_Fetch_Pipeline_Get_IR(analysis.pipeline);
    DSL_FETCH_SITE_RECORD site;
    if (!DSL_fetch_pipeline_ir_get_site(ir, 1, &site) || site.pipeline_plan_count != 3) return 1;
    std::vector<DSL_FETCH_PLAN_RECORD> plans(DSL_fetch_pipeline_ir_plan_count(ir));
    std::vector<DSL_FETCH_RECORD> fetches(DSL_fetch_pipeline_ir_fetch_count(ir));
    std::vector<DSL_PIPELINE_STAGE_RECORD> stages(DSL_fetch_pipeline_ir_stage_count(ir));
    UINT32 direct = 0;
    for (UINT32 i = 0; i < plans.size(); ++i) {
        if (!DSL_fetch_pipeline_ir_get_plan(ir, i + 1, &plans[i])) return 1;
        if ((plans[i].flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0) direct = i;
    }
    if (direct != 1 || plans[direct].buffering_bytes != 0 ||
        plans[direct].barrier_count != 0 || plans[direct].legality != DSL_OPT_LEGALITY_PROVEN) return 1;
    DSL_TILE_PLAN_RECORD geometry;
    UINT64 sector_bytes[2];
    if (!DSL_tile_plan_ir_get_plan(VHO_DSL_Tile_Get_IR(analysis.tile), 2, &geometry)) return 1;
    if ((geometry.flags & DSL_TILE_PLAN_FLAG_DEFERRED_RESOURCES) == 0) return 1;
    if (!DSL_test_movement_resource_gates(geometry, plans)) return 1;
    // The real adapter must select the path's timing, not a private default.
    DSL_HARDWARE_DESCRIPTION_RECORD hardware;
    if (!DSL_hardware_description_find_arch("sm_90", &hardware)) return 1;
    hardware.performance.demand_latency = 17;
    hardware.performance.staged_latency = 31;
    hardware.performance.tma_latency = 43;
    hardware.performance.store_latency = 71;
    hardware.performance.local_issues_per_cycle = 2;
    for (UINT32 i = 0; i < plans.size(); ++i) {
        DSL_GEMM_READINESS_INPUT input;
        DSL_GEMM_READINESS_RESULT result;
        if (!VHO_DSL_fetch_readiness_cost(geometry, plans[i], hardware, &input, &result) ||
            input.latency != ((plans[i].flags & DSL_FETCH_PLAN_FLAG_DIRECT) ? 17 :
                plans[i].engine == DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC ? 43 : 31) ||
            memcmp(&input.hardware, &hardware.performance, sizeof(input.hardware))) return 1;
    }
    geometry.problem_m = geometry.cta_m = 8;
    geometry.problem_n = geometry.cta_n = 32;
    geometry.problem_k = geometry.cta_k = 8;
    geometry.thread_m = geometry.thread_n = 1;
    geometry.threads_per_cta = 256;
    if (!VHO_DSL_direct_gmem_sectors(geometry, sector_bytes) ||
        sector_bytes[0] != 2048 || sector_bytes[1] != 10240) return 1;
    geometry.problem_m = geometry.problem_n = geometry.problem_k = 1;
    if (!VHO_DSL_direct_gmem_sectors(geometry, sector_bytes) ||
        sector_bytes[0] != 32 || sector_bytes[1] != 32) return 1;
    for (UINT32 i = 0; i < fetches.size(); ++i)
        if (!DSL_fetch_pipeline_ir_get_fetch(ir, i + 1, &fetches[i])) return 1;
    for (UINT32 i = 0; i < stages.size(); ++i)
        if (!DSL_fetch_pipeline_ir_get_stage(ir, i + 1, &stages[i])) return 1;
    DSL_FETCH_PIPELINE_IR_CREATE_INFO info;
    memset(&info, 0, sizeof(info));
    info.owner_pu_st = site.owner_pu_st;
    info.sites = &site; info.site_count = 1;
    info.plan_count = plans.size(); info.fetch_count = fetches.size(); info.stage_count = stages.size();
    FILE *quiet = tmpfile();
    if (quiet == NULL) return 1;
    VHO_DSL_TILE_CONTROL tile_control;
    VHO_DSL_Tile_Control_Init(&tile_control);
    tile_control.defer_movement_resources = 1;
    if (VHO_DSL_Tile_Create(fixture.pu, analysis.graph, analysis.tensor,
            analysis.locality, NULL, &tile_control, quiet) != NULL) return 1;
    tile_control.select_plans = 0;
    tile_control.defer_movement_resources = 2;
    if (VHO_DSL_Tile_Create(fixture.pu, analysis.graph, analysis.tensor,
            analysis.locality, NULL, &tile_control, quiet) != NULL) return 1;
    for (UINT32 fault = 0; fault < 11; ++fault) {
        std::vector<DSL_FETCH_PLAN_RECORD> bad_plans = plans;
        std::vector<DSL_FETCH_RECORD> bad_fetches = fetches;
        UINT32 f = plans[direct].first_fetch_id - 1;
        if (fault == 0) bad_plans[direct].buffering_bytes = 4;
        if (fault == 1) bad_plans[direct].barrier_count = 1;
        if (fault == 2) bad_plans[direct].flags |= DSL_FETCH_PLAN_FLAG_ASYNC;
        if (fault == 3) bad_fetches[f].destination_tier_kind = DSL_MEMORY_TIER_SHARED;
        if (fault == 4) bad_fetches[f].flags = DSL_FETCH_RECORD_FLAG_STAGED;
        if (fault == 5) bad_fetches[f].evolution_edge_id = 1;
        if (fault == 6) bad_plans[direct].resources.register_capacity_per_thread = 1;
        if (fault == 7) bad_plans[direct].resources.shared_bytes = 4;
        if (fault == 8) bad_plans[direct].resources.model = 2;
        if (fault == 9) bad_plans[direct].resources.thread_limit = 1;
        if (fault == 10) bad_plans[0].resources.shared_bytes = 0;
        info.plans = &bad_plans[0]; info.fetches = &bad_fetches[0]; info.stages = &stages[0];
        DSL_FETCH_PIPELINE_IR *bad = DSL_fetch_pipeline_ir_create(&info, quiet);
        if (bad != NULL) { DSL_fetch_pipeline_ir_destroy(bad); return 1; }
    }
    fclose(quiet);
    Destroy_Analysis(&analysis);
    printf("D/S1/T2 direct record contract passed: eleven malformed contracts rejected\n");
    return 0;
}

/* Verify target lookup and explicit override isolation through the production
 * common/VHO APIs; failure must not mutate source tables or caller outputs. */
static int
DSL_test_hardware_description()
{
    const char *names[] = { "sm_90", "sm_90a", "sm_100", "sm_100a" };
    for (UINT32 i = 0; i < 4; ++i) {
        DSL_HARDWARE_DESCRIPTION_RECORD original, copy;
        if (!DSL_hardware_description_find_arch(names[i], &original) ||
            strcmp(original.architecture, names[i]) ||
            !DSL_hardware_performance_valid(original.performance) ||
            !DSL_hardware_resources_valid(original.resources) || original.revision != 2) return 1;
        VHO_DSL_FETCH_PIPELINE_CONTROL control;
        VHO_DSL_Fetch_Pipeline_Control_Init(&control);
        control.target_profile_id = original.target_profile_id;
        control.hardware_arch_id = original.arch_id;
        if (!VHO_DSL_fetch_hardware_description(control, &copy) ||
            memcmp(&copy.performance, &original.performance, sizeof(copy.performance))) return 1;
        control.memory_latency_set = 1;
        control.memory_latency = 17;
        if (!VHO_DSL_fetch_hardware_description(control, &copy) ||
            copy.performance.tma_latency != 17 || copy.performance.store_latency != 17 ||
            !copy.latency_override) return 1;
        control.memory_latency = 0;
        if (!VHO_DSL_fetch_hardware_description(control, &copy) ||
            copy.performance.tma_latency || copy.performance.store_latency ||
            !copy.latency_override) return 1;
        if (!DSL_hardware_description_find_arch(names[i], &copy) ||
            memcmp(&copy.performance, &original.performance, sizeof(copy.performance)) ||
            memcmp(&copy.resources, &original.resources, sizeof(copy.resources)) ||
            copy.latency_override) return 1;
        control.target_profile_id = DSL_TARGET_PROFILE_CPU_BASELINE;
        if (VHO_DSL_fetch_hardware_description(control, &copy)) return 1;
        if (DSL_hardware_description_find_arch("sm_90typo", &copy) ||
            DSL_hardware_description_find_arch("sm_120", &copy) ||
            DSL_hardware_description_get_arch(0, &copy) ||
            memcmp(&copy.performance, &original.performance, sizeof(copy.performance))) return 1;
    }
    printf("Hardware Description IR exact lookup, suffixes, overrides and family isolation passed\n");
    return 0;
}

/* Dispatch one isolated contract mode; individual producers own all artifacts
 * and return failure before the shell harness may certify a receipt. */
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
    if (strcmp(mode, "direct") == 0)
        return DSL_test_direct_record_contracts();
    if (strcmp(mode, "hardware") == 0)
        return DSL_test_hardware_description();
    return 1;
}
