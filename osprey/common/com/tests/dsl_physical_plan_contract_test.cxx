/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Certifies the AIO-11 selector-first physical-plan contract, reviewed
 * provider capabilities, deterministic fallback, and unchanged binary WHIRL.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO11-PHYSICAL-PLAN.md.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif

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
#include "glob.h"
#include "ir_bwrite.h"
#include "dsl_builder.h"
#include "dsl_fetch_pipeline.h"
#include "dsl_gatekeeper.h"
#include "dsl_lower.h"
#include "dsl_opcode.h"
#include "dsl_physical_plan.h"
#include "dsl_runtime_variant.h"
#include "open64_dsl_runtime_abi.h"

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
} AIO11_FIXTURE;

typedef struct {
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_TENSOR_ANALYSIS *tensor;
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    DSL_TILE_ANALYSIS *tile;
    DSL_FETCH_PIPELINE_ANALYSIS *pipeline;
    DSL_PHYSICAL_PLAN_ANALYSIS *physical;
    DSL_RUNTIME_VARIANT_ANALYSIS *runtime_variant;
} AIO11_ANALYSIS;

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
    descriptor.memory = "host";
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
Create_Fixture (const char *name, AIO11_FIXTURE *fixture)
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
    identity.defining_module = "aio11.physical_plan_contract";
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
                             ("aio11_kid0", tensor_ty, "float32", 2,
                              "[64,64]", "splat", "1.0");
    fixture->values[1] = DSL_Builder_Create_Tensor_Constant
                             ("aio11_kid1", tensor_ty, "float32", 2,
                              "[64,64]", "splat", "1.0");
    attributes[0].name = "attr.transpose_kid0";
    attributes[0].value = "false";
    attributes[1].name = "attr.transpose_kid1";
    attributes[1].value = "false";
    kids[0] = fixture->values[0];
    kids[1] = fixture->values[1];
    fixture->values[2] = DSL_Builder_Create_Operator_With_Result
                             (matmul, 1, kids, 2, attributes, 2,
                              "aio11_matmul", tensor_ty);
    kids[0] = fixture->values[2];
    fixture->values[3] = DSL_Builder_Create_Operator_With_Result
                             (relu, 2, kids, 1, NULL, 0,
                              "aio11_relu", tensor_ty);
    const UINT32 source_lines[4] = { 139, 142, 151, 155 };
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
Create_Snapshot (const AIO11_FIXTURE *fixture)
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
Destroy_Analysis (AIO11_ANALYSIS *analysis)
{
    DSL_runtime_variant_destroy(analysis->runtime_variant);
    DSL_physical_plan_destroy(analysis->physical);
    DSL_fetch_pipeline_destroy(analysis->pipeline);
    DSL_tile_destroy(analysis->tile);
    DSL_tensor_locality_destroy(analysis->locality);
    DSL_tensor_control_snapshot_destroy(analysis->snapshot);
    DSL_tensor_analysis_destroy(analysis->tensor);
    DSL_tensor_evolution_destroy(analysis->graph);
    memset(analysis, 0, sizeof(*analysis));
}

static BOOL
Build_Analysis
        (const AIO11_FIXTURE *fixture, UINT32 profile,
         UINT32 optimization_level, UINT32 enabled_provider_mask,
         UINT32 available_provider_mask, FILE *trace,
         AIO11_ANALYSIS *analysis)
{
    DSL_TILE_CONTROL tile_control;
    DSL_FETCH_PIPELINE_CONTROL fetch_control;
    DSL_PHYSICAL_PLAN_CONTROL physical_control;
    memset(analysis, 0, sizeof(*analysis));
    analysis->graph = DSL_tensor_evolution_create(fixture->pu, stderr);
    if (analysis->graph == NULL ||
        !DSL_tensor_evolution_build_semantic_roots
             (analysis->graph, stderr))
        return FALSE;
    analysis->tensor = DSL_tensor_analysis_create
                           (fixture->pu, analysis->graph, stderr);
    if (analysis->tensor == NULL ||
        !DSL_tensor_analysis_build(analysis->tensor, stderr))
        return FALSE;
    analysis->snapshot = Create_Snapshot(fixture);
    analysis->locality = DSL_tensor_locality_create
                             (fixture->pu, analysis->tensor,
                              analysis->snapshot, stderr);
    if (analysis->snapshot == NULL || analysis->locality == NULL ||
        !DSL_tensor_locality_build(analysis->locality, stderr))
        return FALSE;

    DSL_tile_control_init(&tile_control);
    tile_control.target_profile_id = profile;
    tile_control.focus_value_id =
        DSL_Builder_Get_Value_Image_Id(fixture->values[2]);
    analysis->tile = DSL_tile_create
                         (fixture->pu, analysis->graph, analysis->tensor,
                          analysis->locality, NULL, &tile_control, stderr);
    if (analysis->tile == NULL ||
        !DSL_tile_build(analysis->tile, stderr) ||
        !DSL_tile_verify(analysis->tile, stderr))
        return FALSE;

    DSL_fetch_pipeline_control_init(&fetch_control);
    fetch_control.target_profile_id = profile;
    fetch_control.focus_value_id = tile_control.focus_value_id;
    fetch_control.prefetch_distance_hint = 1;
    analysis->pipeline = DSL_fetch_pipeline_create
                             (fixture->pu, analysis->graph,
                              analysis->locality, NULL, analysis->tile,
                              &fetch_control, stderr);
    if (analysis->pipeline == NULL ||
        !DSL_fetch_pipeline_build(analysis->pipeline, stderr) ||
        !DSL_fetch_pipeline_verify(analysis->pipeline, stderr))
        return FALSE;

    DSL_physical_plan_control_init(&physical_control);
    physical_control.target_profile_id = profile;
    physical_control.focus_value_id = tile_control.focus_value_id;
    physical_control.optimization_level = optimization_level;
    physical_control.enabled_provider_mask = enabled_provider_mask;
    physical_control.available_provider_mask = available_provider_mask;
    analysis->physical = DSL_physical_plan_create
                             (fixture->pu, analysis->graph, analysis->tile,
                              analysis->pipeline, &physical_control, stderr);
    if (analysis->physical == NULL ||
        !DSL_physical_plan_build(analysis->physical, stderr) ||
        !DSL_physical_plan_verify(analysis->physical, stderr))
        return FALSE;
    if (trace != NULL) {
        DSL_tile_print(trace, analysis->tile);
        DSL_fetch_pipeline_print(trace, analysis->pipeline);
        DSL_physical_plan_print(trace, analysis->physical);
    }
    return TRUE;
}

static BOOL
Build_Runtime_Variants
        (const AIO11_FIXTURE *fixture, FILE *trace,
         AIO11_ANALYSIS *analysis)
{
    DSL_RUNTIME_VARIANT_CONTROL control;
    DSL_runtime_variant_control_init(&control);
    control.focus_value_id =
        DSL_Builder_Get_Value_Image_Id(fixture->values[2]);
    analysis->runtime_variant = DSL_runtime_variant_create
        (fixture->pu, analysis->graph, analysis->physical,
         &control, stderr);
    if (analysis->runtime_variant == NULL ||
        !DSL_runtime_variant_build(analysis->runtime_variant, stderr) ||
        !DSL_runtime_variant_verify(analysis->runtime_variant, stderr))
        return FALSE;
    if (trace != NULL)
        DSL_runtime_variant_print(trace, analysis->runtime_variant);
    return TRUE;
}

static BOOL
Check_Runtime_Variants (AIO11_ANALYSIS *analysis, FILE *trace)
{
    DSL_RUNTIME_VARIANT_SITE_RECORD site;
    DSL_RUNTIME_VARIANT_RECORD baseline;
    DSL_RUNTIME_VARIANT_RECORD fast;
    DSL_RUNTIME_GUARD_RECORD guard0;
    DSL_RUNTIME_GUARD_RECORD guard1;
    DSL_PHYSICAL_IMPLEMENTATION_RECORD baseline_implementation;
    DSL_PHYSICAL_IMPLEMENTATION_RECORD fast_implementation;
    const DSL_OPT_PLAN_CONTEXT *context;
    DSL_OPT_PLAN_RECORD fast_plan;
    DSL_OPT_COST_RECORD fast_cost;
    DSL_RUNTIME_VARIANT_EVALUATION_RESULT guard_true;
    DSL_RUNTIME_VARIANT_EVALUATION_RESULT guard_false;
    DSL_RUNTIME_VARIANT_EVALUATION_RESULT malformed;
    DSL_RUNTIME_GUARD_OBSERVATION true_observations[2];
    DSL_RUNTIME_GUARD_OBSERVATION false_observations[2];
    DSL_RUNTIME_GUARD_OBSERVATION duplicate_observations[2];
    FILE *quiet = tmpfile();
    memset(true_observations, 0, sizeof(true_observations));
    memset(false_observations, 0, sizeof(false_observations));
    memset(duplicate_observations, 0, sizeof(duplicate_observations));
    true_observations[0].operand_ordinal = 0;
    true_observations[0].observed_alignment = 16;
    true_observations[1].operand_ordinal = 1;
    true_observations[1].observed_alignment = 32;
    false_observations[0].operand_ordinal = 0;
    false_observations[0].observed_alignment = 8;
    false_observations[1].operand_ordinal = 1;
    false_observations[1].observed_alignment = 16;
    duplicate_observations[0].operand_ordinal = 0;
    duplicate_observations[0].observed_alignment = 16;
    duplicate_observations[1].operand_ordinal = 0;
    duplicate_observations[1].observed_alignment = 16;

    if (quiet == NULL ||
        DSL_runtime_variant_site_count(analysis->runtime_variant) != 1 ||
        DSL_runtime_variant_count(analysis->runtime_variant) != 2 ||
        DSL_runtime_guard_count(analysis->runtime_variant) != 2 ||
        !DSL_runtime_variant_get_site
             (analysis->runtime_variant, 1, &site) ||
        !DSL_runtime_variant_get_variant
             (analysis->runtime_variant, site.baseline_variant_id,
              &baseline) ||
        !DSL_runtime_variant_get_variant
             (analysis->runtime_variant, site.selected_variant_id, &fast) ||
        baseline.id == fast.id || baseline.guard_count != 0 ||
        fast.guard_count != 2 ||
        fast.fallback_variant_id != baseline.id ||
        !DSL_runtime_variant_get_guard
             (analysis->runtime_variant, fast.first_guard_id, &guard0) ||
        !DSL_runtime_variant_get_guard
             (analysis->runtime_variant, fast.first_guard_id + 1,
              &guard1) ||
        guard0.operand_ordinal != 0 || guard1.operand_ordinal != 1 ||
        guard0.required_value != 16 || guard1.required_value != 16 ||
        guard0.evaluation_cost != 2 || guard1.evaluation_cost != 2 ||
        !DSL_physical_plan_get_implementation
             (analysis->physical, baseline.physical_implementation_id,
              &baseline_implementation) ||
        !DSL_physical_plan_get_implementation
             (analysis->physical, fast.physical_implementation_id,
              &fast_implementation) ||
        baseline_implementation.provider !=
            DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT ||
        fast_implementation.provider !=
            DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT ||
        (context = DSL_runtime_variant_get_plan_context
                       (analysis->runtime_variant, site.id)) == NULL ||
        !DSL_opt_plan_get_plan
             (context, fast.optimization_plan_id, &fast_plan) ||
        !DSL_opt_plan_get_cost(context, fast_plan.cost_id, &fast_cost) ||
        fast_cost.terms[DSL_OPT_COST_RUNTIME_SELECTION].amount != 4 ||
        fast.guard_cost != 4 || fast.total_cost != fast_cost.total ||
        !DSL_runtime_variant_evaluate
             (analysis->runtime_variant, site.id,
              true_observations, 2, &guard_true, stderr) ||
        guard_true.selected_variant_id != fast.id ||
        guard_true.selected_implementation_id != fast_implementation.id ||
        !guard_true.guard_passed || guard_true.fallback_taken ||
        guard_true.evaluated_guard_count != 2 ||
        guard_true.evaluation_cost != 4 ||
        !DSL_runtime_variant_evaluate
             (analysis->runtime_variant, site.id,
              false_observations, 2, &guard_false, stderr) ||
        guard_false.selected_variant_id != baseline.id ||
        guard_false.selected_implementation_id !=
            baseline_implementation.id ||
        guard_false.guard_passed || !guard_false.fallback_taken ||
        guard_false.evaluated_guard_count != 1 ||
        guard_false.evaluation_cost != 2 ||
        DSL_runtime_variant_evaluate
             (analysis->runtime_variant, site.id,
              duplicate_observations, 2, &malformed, quiet))
        return FALSE;
    if (trace != NULL) {
        DSL_runtime_variant_print_evaluation
            (trace, "alignment_guard_true", &guard_true);
        DSL_runtime_variant_print_evaluation
            (trace, "alignment_guard_false", &guard_false);
    }
    fclose(quiet);
    return TRUE;
}

static BOOL
Check_Selection
        (const AIO11_ANALYSIS *analysis, UINT32 expected_count,
         UINT32 expected_provider, UINT32 rejected_reason)
{
    DSL_PHYSICAL_SITE_RECORD site;
    if (DSL_physical_plan_site_count(analysis->physical) != 1 ||
        DSL_physical_plan_implementation_count(analysis->physical) !=
            expected_count ||
        !DSL_physical_plan_get_site(analysis->physical, 1, &site) ||
        site.implementation_count != expected_count ||
        site.selected_implementation_id == 0)
        return FALSE;
    UINT32 rejected = 0;
    for (UINT32 i = 0; i < site.implementation_count; ++i) {
        DSL_PHYSICAL_IMPLEMENTATION_RECORD implementation;
        if (!DSL_physical_plan_get_implementation
                 (analysis->physical,
                  site.first_implementation_id + i,
                  &implementation))
            return FALSE;
        if (implementation.id == site.selected_implementation_id) {
            if (implementation.provider != expected_provider ||
                implementation.legality != DSL_OPT_LEGALITY_PROVEN ||
                (implementation.flags &
                 DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED) == 0)
                return FALSE;
        }
        if (rejected_reason != DSL_OPT_REJECT_NONE &&
            implementation.rejection_reason == rejected_reason)
            ++rejected;
    }
    return rejected_reason == DSL_OPT_REJECT_NONE || rejected == 1;
}

static BOOL
Check_Lowered_Runtime
        (WN *tree, const char *matmul_name, UINT32 matmul_parameters)
{
    WN *body = WN_func_body(tree);
    UINT32 call_count = 0;
    UINT32 matmul_count = 0;
    for (WN *statement = WN_first(body); statement != NULL;
         statement = WN_next(statement)) {
        if (WN_operator(statement) != OPR_CALL)
            continue;
        ++call_count;
        if (strcmp(ST_name(WN_st(statement)), matmul_name) != 0)
            continue;
        ++matmul_count;
        if ((UINT32)WN_kid_count(statement) != matmul_parameters)
            return FALSE;
        if (matmul_parameters == 5) {
            WN *parameter = WN_kid0(WN_kid(statement, 4));
            if (WN_operator(parameter) != OPR_LDA ||
                strstr(ST_name(WN_st(parameter)), ".dsl_physical_plan") ==
                    NULL ||
                TY_size(ST_type(WN_st(parameter))) !=
                    OPEN64_DSL_PHYSICAL_PLAN_V1_SIZE ||
                TY_align(ST_type(WN_st(parameter))) != 8)
                return FALSE;
        }
    }
    UINT32 remaining = 0;
    return call_count == 4 && matmul_count == 1 &&
           VHO_DSL_Lowered_Tree_Is_Canonical(tree, stderr, &remaining) &&
           remaining == 0;
}

static BOOL
Write_Lowered_Image (DSL_BUILDER_PROGRAM_UNIT pu, const char *path)
{
    char temporary[PATH_MAX];
    if (pu == NULL || path == NULL || path[0] == '\0')
        return FALSE;
    INT written = snprintf(temporary, sizeof(temporary), "%s.tmp", path);
    if (written < 0 || (size_t)written >= sizeof(temporary))
        return FALSE;
    (void)unlink(temporary);
    Irb_File_Name = temporary;
    if (Open_Output_Info(Irb_File_Name) == NULL)
        return FALSE;
    if (!DSL_Builder_Select_PU(pu)) {
        Close_Output_Info();
        (void)unlink(temporary);
        return FALSE;
    }
    Write_PU_Info(pu);
    Write_Global_Info(pu);
    Close_Output_Info();
    if (access(temporary, F_OK) != 0 || rename(temporary, path) != 0) {
        (void)unlink(temporary);
        return FALSE;
    }
    return TRUE;
}

static int
Run_Lowering_Mode
        (UINT32 selected_provider, BOOL induce_late_failure,
         const char *artifact)
{
    AIO11_FIXTURE fixture;
    AIO11_ANALYSIS analysis;
    DSL_BUILDER_VERIFY_RESULT verify;
    VHO_DSL_LOWER_RESULT lower;
    UINT32 direct =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT);
    UINT32 generated =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED);
    UINT32 cublas =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT);
    UINT32 enabled = direct;
    UINT32 available = direct;
    if (selected_provider == DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT) {
        enabled |= cublas;
        available |= cublas;
    } else if (selected_provider == DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED) {
        enabled |= generated;
        available |= generated;
    } else {
        enabled |= cublas;
    }

    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio11_runtime_lowering", &fixture)) {
        fprintf(stderr, "AIO-11 runtime fixture construction failed\n");
        return 1;
    }
    if (!Build_Analysis
             (&fixture, DSL_TARGET_PROFILE_NVIDIA_HOPPER, 3,
              enabled, available, NULL, &analysis)) {
        fprintf(stderr, "AIO-11 runtime analysis failed\n");
        return 1;
    }
    if (!Check_Selection
             (&analysis, 2, selected_provider, DSL_OPT_REJECT_NONE)) {
        fprintf(stderr, "AIO-11 runtime selection mismatch\n");
        return 1;
    }
    memset(&verify, 0, sizeof(verify));
    if (!DSL_Builder_Verify_Program(&verify)) {
        fprintf(stderr, "AIO-11 runtime source verification failed\n");
        return 1;
    }

    WN *tree = PU_Info_tree_ptr(fixture.pu);
    WN *original_tree = tree;
    WN *malformed_native = NULL;
    if (induce_late_failure) {
        WN *definition = WN_first(WN_func_body(tree));
        definition = WN_next(definition);
        definition = WN_next(definition);
        malformed_native = WN_kid0(definition);
        WN_kid(malformed_native, 0) = WN_Intconst(MTYPE_I4, 0);
    }
    BOOL lowered = VHO_DSL_Lower_Verified_Program_Unit_With_Physical_Plan
                       (fixture.pu, &tree, analysis.physical, stderr, &lower);
    if (induce_late_failure) {
        BOOL unchanged = !lowered && tree == original_tree &&
                         PU_Info_tree_ptr(fixture.pu) == original_tree &&
                         malformed_native != NULL &&
                         WN_operator(WN_kid(malformed_native, 0)) ==
                             OPR_INTCONST;
        Destroy_Analysis(&analysis);
        if (!unchanged || artifact != NULL)
            return 1;
        printf("AIO-11 late lowering failure rolled back WN mutation\n");
        return 0;
    }
    if (selected_provider == DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED) {
        BOOL unchanged = !lowered && tree == original_tree &&
                         PU_Info_tree_ptr(fixture.pu) == original_tree &&
                         DSL_WN_Is_Native
                             (WN_kid0(WN_first(WN_func_body(tree))));
        Destroy_Analysis(&analysis);
        if (!unchanged || artifact != NULL)
            return 1;
        printf("AIO-11 unsupported provider rejected before WN mutation\n");
        return 0;
    }

    const BOOL physical = selected_provider ==
                          DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT;
    if (!lowered || tree == original_tree ||
        PU_Info_tree_ptr(fixture.pu) != tree ||
        lower.physical_plan_site_count != 1 ||
        lower.physical_provider_site_count != (physical ? 1U : 0U) ||
        lower.physical_provider_lowered_count != (physical ? 1U : 0U) ||
        !Check_Lowered_Runtime
             (tree,
              physical ? "__open64_dsl_matmul_physical_v1" :
                         "__open64_dsl_matmul_v1",
              physical ? 5 : 4)) {
        fprintf(stderr,
                "AIO-11 runtime lowering mismatch: valid=%u sites=%u "
                "provider_sites=%u lowered=%u\n",
                lowered, lower.physical_plan_site_count,
                lower.physical_provider_site_count,
                lower.physical_provider_lowered_count);
        fdump_tree(stderr, tree);
        return 1;
    }
    Destroy_Analysis(&analysis);

    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    if (!Write_Lowered_Image(fixture.pu, artifact)) {
        fprintf(stderr, "AIO-11 lowered image finalization failed\n");
        return 1;
    }
    printf("AIO-11 %s runtime lowering passed\n",
           physical ? "cuBLASLt" : "direct fallback");
    return 0;
}

static int
Run_Image_Mode (BOOL run_analysis)
{
    const char *artifact = getenv("OPEN64_AIO11_ARTIFACT");
    const char *trace_path = getenv("OPEN64_AIO11_ANALYSIS");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_VERIFY_RESULT verify;
    AIO11_FIXTURE fixture;
    AIO11_ANALYSIS analysis;
    UINT32 nodes;
    UINT32 values;
    UINT32 types;
    char diagnostic[4096];
    FILE *trace = NULL;
    UINT32 direct =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT);
    UINT32 generated =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED);
    UINT32 cublas =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT);
    UINT32 triton =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_TRITON);
    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio11_physical_plan", &fixture))
        return 1;
    nodes = DSL_IR_Image_Node_Count();
    values = DSL_IR_Image_Value_Count();
    types = TY_Table_Size();
    if (run_analysis) {
        if (trace_path == NULL || trace_path[0] == '\0' ||
            (trace = fopen(trace_path, "w")) == NULL ||
            !Build_Analysis
                 (&fixture, DSL_TARGET_PROFILE_NVIDIA_HOPPER, 3,
                  direct | generated | cublas | triton,
                  direct | generated | cublas | triton,
                  trace, &analysis) ||
            !Check_Selection
                 (&analysis, 4,
                  DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT,
                  DSL_OPT_REJECT_NONE) ||
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
    printf("AIO-11 %s G13 image passed\n",
           run_analysis ? "selected" : "baseline");
    return 0;
}

static int
Run_Selection_Mode
        (UINT32 optimization_level, UINT32 enabled_provider_mask,
         UINT32 available_provider_mask, UINT32 expected_count,
         UINT32 expected_provider, UINT32 rejected_reason,
         const char *label)
{
    const char *trace_path = getenv("OPEN64_AIO11_ANALYSIS");
    AIO11_FIXTURE fixture;
    AIO11_ANALYSIS analysis;
    FILE *trace = trace_path == NULL ? NULL : fopen(trace_path, "w");
    if (trace_path != NULL && trace == NULL)
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio11_selection", &fixture) ||
        !Build_Analysis
             (&fixture, DSL_TARGET_PROFILE_NVIDIA_HOPPER,
              optimization_level, enabled_provider_mask,
              available_provider_mask, trace, &analysis) ||
        !Check_Selection
             (&analysis, expected_count, expected_provider,
              rejected_reason))
        return 1;
    if (trace != NULL)
        fclose(trace);
    Destroy_Analysis(&analysis);
    printf("AIO-11 %s selection passed\n", label);
    return 0;
}

static int
Run_Control(void)
{
    AIO11_FIXTURE fixture;
    AIO11_ANALYSIS analysis;
    DSL_PHYSICAL_PLAN_CONTROL control;
    DSL_PHYSICAL_PLAN_ANALYSIS *disabled;
    FILE *quiet = tmpfile();
    UINT32 direct =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT);
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (quiet == NULL || !Create_Fixture("aio11_control", &fixture) ||
        !Build_Analysis
             (&fixture, DSL_TARGET_PROFILE_NVIDIA_HOPPER, 0,
              direct, direct, NULL, &analysis))
        return 1;
    DSL_physical_plan_destroy(analysis.physical);
    analysis.physical = NULL;
    DSL_physical_plan_control_init(&control);
    control.apply_selected_plan = 1;
    if (DSL_physical_plan_create
            (fixture.pu, analysis.graph, analysis.tile, analysis.pipeline,
             &control, quiet) != NULL)
        return 1;
    control.apply_selected_plan = 0;
    control.generate_candidates = 0;
    control.select_plan = 0;
    disabled = DSL_physical_plan_create
                   (fixture.pu, analysis.graph, analysis.tile,
                    analysis.pipeline, &control, stderr);
    if (disabled == NULL ||
        !DSL_physical_plan_build(disabled, stderr) ||
        !DSL_physical_plan_verify(disabled, stderr) ||
        DSL_physical_plan_site_count(disabled) != 0)
        return 1;
    DSL_physical_plan_destroy(disabled);
    Destroy_Analysis(&analysis);
    fclose(quiet);
    printf("AIO-11 control contract passed\n");
    return 0;
}

static int
Run_Runtime_Variant_Mode(void)
{
    const char *artifact = getenv("OPEN64_AIO12_ARTIFACT");
    const char *trace_path = getenv("OPEN64_AIO12_ANALYSIS");
    AIO11_FIXTURE fixture;
    AIO11_ANALYSIS analysis;
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_VERIFY_RESULT verify;
    UINT32 direct =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT);
    UINT32 cublas =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT);
    UINT32 nodes;
    UINT32 values;
    UINT32 types;
    FILE *trace;
    if (artifact == NULL || artifact[0] == '\0' || trace_path == NULL ||
        trace_path[0] == '\0' || (trace = fopen(trace_path, "w")) == NULL)
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio11_physical_plan", &fixture))
        return 1;
    nodes = DSL_IR_Image_Node_Count();
    values = DSL_IR_Image_Value_Count();
    types = TY_Table_Size();
    if (!Build_Analysis
             (&fixture, DSL_TARGET_PROFILE_NVIDIA_HOPPER, 3,
              direct | cublas, direct | cublas, trace, &analysis) ||
        !Build_Runtime_Variants(&fixture, trace, &analysis) ||
        !Check_Runtime_Variants(&analysis, trace) ||
        DSL_IR_Image_Node_Count() != nodes ||
        DSL_IR_Image_Value_Count() != values || TY_Table_Size() != types) {
        fclose(trace);
        return 1;
    }
    fclose(trace);
    Destroy_Analysis(&analysis);
    memset(&verify, 0, sizeof(verify));
    if (!DSL_Builder_Verify_Program(&verify))
        return 1;
    request.path = artifact;
    request.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&request))
        return 1;
    printf("AIO-12 alignment-guard runtime variants passed\n");
    return 0;
}

static int
Run_Runtime_Variant_Control(void)
{
    AIO11_FIXTURE fixture;
    AIO11_ANALYSIS analysis;
    DSL_RUNTIME_VARIANT_CONTROL control;
    DSL_RUNTIME_VARIANT_ANALYSIS *disabled;
    UINT32 direct =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT);
    UINT32 cublas =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT);
    FILE *quiet = tmpfile();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (quiet == NULL ||
        !Create_Fixture("aio12_runtime_variant_control", &fixture) ||
        !Build_Analysis
             (&fixture, DSL_TARGET_PROFILE_NVIDIA_HOPPER, 3,
              direct | cublas, direct | cublas, NULL, &analysis))
        return 1;
    DSL_runtime_variant_control_init(&control);
    control.focus_value_id =
        DSL_Builder_Get_Value_Image_Id(fixture.values[2]);
    control.apply_selected_variant = 1;
    if (DSL_runtime_variant_create
            (fixture.pu, analysis.graph, analysis.physical,
             &control, quiet) != NULL)
        return 1;
    control.apply_selected_variant = 0;
    control.generate_variants = 0;
    control.select_policy = 0;
    disabled = DSL_runtime_variant_create
                   (fixture.pu, analysis.graph, analysis.physical,
                    &control, stderr);
    if (disabled == NULL ||
        !DSL_runtime_variant_build(disabled, stderr) ||
        !DSL_runtime_variant_verify(disabled, stderr) ||
        DSL_runtime_variant_site_count(disabled) != 0 ||
        DSL_runtime_variant_count(disabled) != 0 ||
        DSL_runtime_guard_count(disabled) != 0)
        return 1;
    DSL_runtime_variant_destroy(disabled);
    Destroy_Analysis(&analysis);
    fclose(quiet);
    printf("AIO-12 runtime variant control contract passed\n");
    return 0;
}

int
main(void)
{
    const char *mode;
    UINT32 direct =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT);
    UINT32 generated =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED);
    UINT32 cublas =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT);
    UINT32 cudnn =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_NVIDIA_CUDNN);
    Initialize_Test_Context();
    mode = getenv("OPEN64_AIO11_MODE");
    if (mode == NULL)
        return 1;
    if (strcmp(mode, "before") == 0)
        return Run_Image_Mode(FALSE);
    if (strcmp(mode, "select") == 0)
        return Run_Image_Mode(TRUE);
    if (strcmp(mode, "o0") == 0)
        return Run_Selection_Mode
                   (0, direct | generated | cublas,
                    direct | generated | cublas, 1,
                    DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT,
                    DSL_OPT_REJECT_NONE, "O0 baseline");
    if (strcmp(mode, "generated") == 0)
        return Run_Selection_Mode
                   (3, direct | generated, direct | generated, 2,
                    DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED,
                    DSL_OPT_REJECT_NONE, "generated kernel");
    if (strcmp(mode, "unavailable") == 0)
        return Run_Selection_Mode
                   (3, direct | cublas, direct, 2,
                    DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT,
                    DSL_OPT_REJECT_PROVIDER_UNAVAILABLE,
                    "unavailable provider fallback");
    if (strcmp(mode, "mismatch") == 0)
        return Run_Selection_Mode
                   (3, direct | cudnn, direct | cudnn, 2,
                    DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT,
                    DSL_OPT_REJECT_PROVIDER_MISMATCH,
                    "provider mismatch fallback");
    if (strcmp(mode, "control") == 0)
        return Run_Control();
    if (strcmp(mode, "lower-provider") == 0)
        return Run_Lowering_Mode
                   (DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT,
                    FALSE,
                    getenv("OPEN64_AIO11_ARTIFACT"));
    if (strcmp(mode, "lower-fallback") == 0)
        return Run_Lowering_Mode
                   (DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT,
                    FALSE,
                    getenv("OPEN64_AIO11_ARTIFACT"));
    if (strcmp(mode, "lower-reject") == 0)
        return Run_Lowering_Mode
                   (DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED, FALSE, NULL);
    if (strcmp(mode, "lower-rollback") == 0)
        return Run_Lowering_Mode
                   (DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT, TRUE, NULL);
    if (strcmp(mode, "runtime-variant") == 0)
        return Run_Runtime_Variant_Mode();
    if (strcmp(mode, "runtime-variant-control") == 0)
        return Run_Runtime_Variant_Control();
    return 1;
}
