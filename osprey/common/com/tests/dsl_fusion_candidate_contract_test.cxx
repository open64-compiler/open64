/*
 * Check-only contract tests for AIO-5 high-level fusion candidates.
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
#include "dsl_fusion_candidate.h"
#include "dsl_opcode.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_evolution.h"
#include "dsl_tensor_locality.h"

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
    DSL_BUILDER_VALUE values[9];
    UINT32 file_id;
} AIO5_FIXTURE;

typedef struct {
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_TENSOR_ANALYSIS *tensor_analysis;
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    DSL_FUSION_CANDIDATE_ANALYSIS *fusion;
} AIO5_ANALYSIS;

typedef enum {
    AIO5_FIXTURE_LEGAL = 0,
    AIO5_FIXTURE_DESCRIPTOR = 1,
    AIO5_FIXTURE_EFFECT = 2,
    AIO5_FIXTURE_SEMANTIC = 3
} AIO5_FIXTURE_KIND;

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
Create_Tensor_Type (const char *name, const char *shape)
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
    return TY_Intern_Tensor_Type
               (name, MTYPE_To_TY(MTYPE_F4), &descriptor);
}

static BOOL
Set_Source
        (DSL_BUILDER_VALUE value, UINT32 file_id, UINT32 line)
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
Create_Fixture
        (const char *name, const char *shape, AIO5_FIXTURE_KIND kind,
         AIO5_FIXTURE *fixture)
{
    DSL_BUILDER_PU_SOURCE_IDENTITY identity;
    DSL_DOMAIN_ID common = DSL_Domain_Find("common");
    DSL_BUILDER_OPERATOR_ATTRIBUTE broadcast;
    DSL_BUILDER_OPERATOR_ATTRIBUTE matmul_attributes[2];
    DSL_BUILDER_OPERATOR_ATTRIBUTE residual_attributes[3];
    DSL_BUILDER_VALUE kids[2];
    TY_IDX ty = Create_Tensor_Type(name, shape);
    TY_IDX result_ty = ty;
    memset(fixture, 0, sizeof(*fixture));
    if (kind == AIO5_FIXTURE_DESCRIPTOR)
        result_ty = Create_Tensor_Type("aio5_mismatch", "[2,3]");
    fixture->pu = DSL_Builder_Create_Minimal_PU(name);
    if (fixture->pu == NULL || ty == TY_IDX_ZERO ||
        result_ty == TY_IDX_ZERO)
        return FALSE;
    memset(&identity, 0, sizeof(identity));
    identity.canonical_definition_name = name;
    identity.defining_module = "aio5.fusion_candidate_contract";
    identity.defining_file = __FILE__;
    identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(fixture->pu, &identity))
        return FALSE;
    fixture->file_id = DSL_Builder_Register_Source_File
                           (fixture->pu, __FILE__);
    fixture->values[0] = DSL_Builder_Create_Tensor_Constant
                             ("aio5_lhs", ty, "float32", 2, shape,
                              "splat", "1.0");
    fixture->values[1] = DSL_Builder_Create_Tensor_Constant
                             ("aio5_rhs", ty, "float32", 2, shape,
                              "splat", "1.0");
    if (kind == AIO5_FIXTURE_SEMANTIC)
        fixture->values[2] = DSL_Builder_Create_Model_Input
                                 ("aio5_not_bias", ty, 0);
    else
        fixture->values[2] = DSL_Builder_Create_Tensor_Constant
                                 ("aio5_bias", ty, "float32", 2, shape,
                                  "splat", "0.0");
    fixture->values[3] = DSL_Builder_Create_Tensor_Constant
                             ("aio5_skip", result_ty, "float32", 2,
                              kind == AIO5_FIXTURE_DESCRIPTOR ?
                                  "[2,3]" : shape,
                              "splat", "1.0");
    kids[0] = fixture->values[0];
    kids[1] = fixture->values[1];
    matmul_attributes[0].name = "attr.transpose_kid0";
    matmul_attributes[0].value = "false";
    matmul_attributes[1].name = "attr.transpose_kid1";
    matmul_attributes[1].value = "false";
    fixture->values[4] = DSL_Builder_Create_Operator_With_Result
                             (DSL_Opcode_Find
                                  (common, DSL_OPCODE_COMMON_MATMUL, 1),
                              1, kids, 2, matmul_attributes, 2,
                              "aio5_matmul", ty);
    broadcast.name = "attr.broadcast_rule";
    broadcast.value = "numpy";
    kids[0] = fixture->values[4];
    kids[1] = fixture->values[2];
    fixture->values[5] = DSL_Builder_Create_Operator_With_Result
                             (DSL_Opcode_Find
                                  (common, DSL_OPCODE_COMMON_ADD, 1),
                              1, kids, 2, &broadcast, 1,
                              "aio5_bias_add", ty);
    kids[0] = fixture->values[5];
    fixture->values[6] = DSL_Builder_Create_Operator_With_Result
                             (DSL_Opcode_Find(common, "common.relu", 2),
                              2, kids, 1, NULL, 0,
                              "aio5_activation", result_ty);
    kids[0] = fixture->values[6];
    kids[1] = fixture->values[3];
    residual_attributes[0].name = "attr.broadcast_rule";
    residual_attributes[0].value = "none";
    residual_attributes[1].name = "attr.shape_check";
    residual_attributes[1].value = "exact";
    residual_attributes[2].name = "attr.residual_path";
    residual_attributes[2].value = "true";
    fixture->values[7] = DSL_Builder_Create_Operator_With_Result
                             (DSL_Opcode_Find
                                  (common, "common.residual_add", 2),
                              2, kids, 2, residual_attributes, 3,
                              "aio5_residual", result_ty);
    kids[0] = fixture->values[7];
    fixture->values[8] = DSL_Builder_Create_Operator_With_Result
                             (DSL_Opcode_Find(common, "common.relu", 2),
                              2, kids, 1, NULL, 0,
                              "aio5_residual_activation", result_ty);
    for (UINT32 i = 0; i < 9; ++i) {
        if (!Set_Source(fixture->values[i], fixture->file_id, 10 + i) ||
            !DSL_Builder_Append_PU_Value(fixture->pu,
                                         fixture->values[i]))
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
Create_Control_Snapshot
        (const AIO5_FIXTURE *fixture, UINT32 control_flags)
{
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot =
        DSL_Tensor_Control_Snapshot_Create(fixture->pu, stderr);
    DSL_TENSOR_CONTROL_BLOCK block;
    BOOL split_region =
        (control_flags & DSL_TENSOR_CONTROL_REGION) != 0;
    if (snapshot == NULL)
        return NULL;
    memset(&block, 0, sizeof(block));
    block.block_id = 1;
    block.reverse_postorder = 1;
    block.flags = control_flags;
    block.region_id = split_region ? 10 : 0;
    block.immediate_postdominator = split_region ? 2 : 0;
    if (!DSL_Tensor_Control_Snapshot_Add_Block
             (snapshot, &block, stderr))
        return NULL;
    if (split_region) {
        memset(&block, 0, sizeof(block));
        block.block_id = 2;
        block.reverse_postorder = 2;
        block.immediate_dominator = 1;
        block.region_id = 20;
        block.flags = control_flags;
        if (!DSL_Tensor_Control_Snapshot_Add_Block
                 (snapshot, &block, stderr))
            return NULL;
    }
    for (UINT32 i = 0; i < 9; ++i) {
        DSL_TENSOR_CONTROL_POSITION position;
        memset(&position, 0, sizeof(position));
        position.node_id = Value_Node(fixture->values[i]);
        position.block_id = split_region && i >= 6 ? 2 : 1;
        position.statement_order = split_region && i >= 6 ?
                                   i - 5 : i + 1;
        position.reverse_postorder = position.block_id;
        if (!DSL_Tensor_Control_Snapshot_Add_Position
                 (snapshot, &position, stderr))
            return NULL;
    }
    if (!DSL_Tensor_Control_Snapshot_Seal(snapshot, stderr))
        return NULL;
    return snapshot;
}

static void
Destroy_Analysis (AIO5_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    DSL_Fusion_Candidates_Destroy(analysis->fusion);
    DSL_Tensor_Locality_Destroy(analysis->locality);
    DSL_Tensor_Control_Snapshot_Destroy(analysis->snapshot);
    DSL_Tensor_Analysis_Destroy(analysis->tensor_analysis);
    DSL_Tensor_Evolution_Destroy(analysis->graph);
    memset(analysis, 0, sizeof(*analysis));
}

static BOOL
Build_Analysis
        (const AIO5_FIXTURE *fixture, UINT32 control_flags,
         const DSL_FUSION_CONTROL *control, FILE *trace,
         AIO5_ANALYSIS *analysis)
{
    memset(analysis, 0, sizeof(*analysis));
    analysis->graph = DSL_Tensor_Evolution_Create(fixture->pu, stderr);
    if (analysis->graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots
             (analysis->graph, stderr))
        return FALSE;
    analysis->tensor_analysis = DSL_Tensor_Analysis_Create
                                    (fixture->pu, analysis->graph,
                                     stderr);
    if (analysis->tensor_analysis == NULL ||
        !DSL_Tensor_Analysis_Build(analysis->tensor_analysis, stderr))
        return FALSE;
    analysis->snapshot = Create_Control_Snapshot
                             (fixture, control_flags);
    analysis->locality = DSL_Tensor_Locality_Create
                             (fixture->pu, analysis->tensor_analysis,
                              analysis->snapshot, stderr);
    if (analysis->snapshot == NULL || analysis->locality == NULL ||
        !DSL_Tensor_Locality_Build(analysis->locality, stderr))
        return FALSE;
    analysis->fusion = DSL_Fusion_Candidates_Create
                           (fixture->pu, analysis->graph,
                            analysis->tensor_analysis,
                            analysis->locality, control, stderr);
    if (analysis->fusion == NULL ||
        !DSL_Fusion_Candidates_Build(analysis->fusion, stderr) ||
        !DSL_Fusion_Candidates_Verify(analysis->fusion, stderr))
        return FALSE;
    if (trace != NULL) {
        DSL_Tensor_Locality_Print(trace, analysis->locality);
        DSL_Fusion_Candidates_Print(trace, analysis->fusion);
    }
    return TRUE;
}

static BOOL
Check_Legal_Selected (const AIO5_ANALYSIS *analysis, BOOL selected)
{
    DSL_FUSION_SITE_RECORD matmul;
    DSL_FUSION_SITE_RECORD residual;
    DSL_OPT_COST_RECORD cost;
    if (DSL_Fusion_Candidates_Site_Count(analysis->fusion) != 2 ||
        DSL_Fusion_Candidates_Member_Count(analysis->fusion) != 5 ||
        DSL_Fusion_Candidates_Boundary_Count(analysis->fusion) != 10 ||
        !DSL_Fusion_Candidates_Get_Site
             (analysis->fusion, 1, &matmul) ||
        !DSL_Fusion_Candidates_Get_Site
             (analysis->fusion, 2, &residual) ||
        matmul.pattern != DSL_FUSION_PATTERN_MATMUL_BIAS_ACTIVATION ||
        residual.pattern != DSL_FUSION_PATTERN_RESIDUAL_ACTIVATION ||
        matmul.legality != DSL_OPT_LEGALITY_PROVEN ||
        residual.legality != DSL_OPT_LEGALITY_PROVEN ||
        matmul.eliminated_materialization_count != 2 ||
        matmul.eliminated_materialization_bytes != 32 ||
        matmul.live_range_growth_bytes != 48 ||
        residual.eliminated_materialization_count != 1 ||
        residual.eliminated_materialization_bytes != 16 ||
        residual.live_range_growth_bytes != 32 ||
        matmul.selected_plan_id != (selected ? 2 : 0) ||
        residual.selected_plan_id != (selected ? 2 : 0) ||
        !DSL_Opt_Plan_Get_Cost
             (DSL_Fusion_Candidates_Get_Plan_Context
                  (analysis->fusion, 1),
              2, &cost) || !cost.complete)
        return FALSE;
    return TRUE;
}

static int
Run_Image_Mode (BOOL run_analysis)
{
    const char *artifact = getenv("OPEN64_AIO5_ARTIFACT");
    const char *trace_path = getenv("OPEN64_AIO5_ANALYSIS");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_VERIFY_RESULT verify;
    DSL_FUSION_CONTROL control;
    AIO5_FIXTURE fixture;
    AIO5_ANALYSIS analysis;
    FILE *trace = NULL;
    UINT32 nodes;
    UINT32 values;
    UINT32 types;
    char diagnostic[4096];
    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture
             ("aio5_fusion_candidates", "[2,2]", AIO5_FIXTURE_LEGAL,
              &fixture))
        return 1;
    nodes = DSL_IR_Image_Node_Count();
    values = DSL_IR_Image_Value_Count();
    types = TY_Table_Size();
    if (run_analysis) {
        if (trace_path == NULL || trace_path[0] == '\0')
            return 1;
        trace = fopen(trace_path, "w");
        if (trace == NULL)
            return 1;
        DSL_Fusion_Control_Init(&control);
        control.select_plans = 1;
        control.resource_limit_bytes = 256;
        if (!Build_Analysis(&fixture, FALSE, &control, trace, &analysis) ||
            !Check_Legal_Selected(&analysis, TRUE) ||
            DSL_IR_Image_Node_Count() != nodes ||
            DSL_IR_Image_Value_Count() != values ||
            TY_Table_Size() != types)
            return 1;
        Destroy_Analysis(&analysis);
        fclose(trace);
    }
    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify)) {
        fprintf(stderr, "AIO-5 verification failed: %s\n", diagnostic);
        return 1;
    }
    request.path = artifact;
    request.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&request))
        return 1;
    printf("AIO-5 %s image passed\n", run_analysis ? "after" : "before");
    return 0;
}

static int
Run_Candidate_Only(void)
{
    DSL_FUSION_CONTROL control;
    AIO5_FIXTURE fixture;
    AIO5_ANALYSIS analysis;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    DSL_Fusion_Control_Init(&control);
    control.resource_limit_bytes = 256;
    if (!Create_Fixture
             ("aio5_candidate_only", "[2,2]", AIO5_FIXTURE_LEGAL,
              &fixture) ||
        !Build_Analysis(&fixture, FALSE, &control, NULL, &analysis) ||
        !Check_Legal_Selected(&analysis, FALSE))
        return 1;
    Destroy_Analysis(&analysis);
    printf("AIO-5 candidate-only control passed\n");
    return 0;
}

static int
Run_Rejection
        (AIO5_FIXTURE_KIND kind, UINT32 control_flags,
         UINT64 resource_limit,
         UINT32 expected_legality, UINT32 expected_reason,
         UINT32 expected_selected)
{
    DSL_FUSION_CONTROL control;
    AIO5_FIXTURE fixture;
    AIO5_ANALYSIS analysis;
    DSL_FUSION_SITE_RECORD site;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    DSL_Fusion_Control_Init(&control);
    control.select_plans = 1;
    control.resource_limit_bytes = resource_limit;
    if (!Create_Fixture
             ("aio5_negative", "[2,2]", kind, &fixture) ||
        !Build_Analysis
             (&fixture, control_flags, &control, NULL, &analysis) ||
        DSL_Fusion_Candidates_Site_Count(analysis.fusion) != 2 ||
        !DSL_Fusion_Candidates_Get_Site(analysis.fusion, 1, &site) ||
        site.legality != expected_legality ||
        site.rejection_reason != expected_reason ||
        site.selected_plan_id != expected_selected)
        return 1;
    Destroy_Analysis(&analysis);
    printf("AIO-5 rejection passed legality=%s reason=%s\n",
           DSL_Opt_Legality_Name(expected_legality),
           DSL_Opt_Rejection_Reason_Name(expected_reason));
    return 0;
}

static int
Run_Control_Contract(void)
{
    DSL_FUSION_CONTROL control;
    AIO5_FIXTURE fixture;
    AIO5_ANALYSIS analysis;
    DSL_FUSION_CANDIDATE_ANALYSIS *fusion;
    FILE *quiet = tmpfile();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (quiet == NULL ||
        !Create_Fixture
             ("aio5_control", "[2,2]", AIO5_FIXTURE_LEGAL,
              &fixture))
        return 1;
    DSL_Fusion_Control_Init(&control);
    control.generate_candidates = 0;
    if (!Build_Analysis(&fixture, FALSE, &control, NULL, &analysis) ||
        DSL_Fusion_Candidates_Site_Count(analysis.fusion) != 0)
        return 1;
    Destroy_Analysis(&analysis);

    DSL_Fusion_Control_Init(&control);
    control.apply_transformation = 1;
    analysis.graph = DSL_Tensor_Evolution_Create(fixture.pu, quiet);
    if (analysis.graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots
             (analysis.graph, quiet))
        return 1;
    analysis.tensor_analysis = DSL_Tensor_Analysis_Create
                                   (fixture.pu, analysis.graph, quiet);
    if (analysis.tensor_analysis == NULL ||
        !DSL_Tensor_Analysis_Build(analysis.tensor_analysis, quiet))
        return 1;
    analysis.snapshot = Create_Control_Snapshot(&fixture, FALSE);
    analysis.locality = DSL_Tensor_Locality_Create
                            (fixture.pu, analysis.tensor_analysis,
                             analysis.snapshot, quiet);
    if (analysis.locality == NULL ||
        !DSL_Tensor_Locality_Build(analysis.locality, quiet))
        return 1;
    fusion = DSL_Fusion_Candidates_Create
                 (fixture.pu, analysis.graph, analysis.tensor_analysis,
                  analysis.locality, &control, quiet);
    if (fusion != NULL)
        return 1;
    analysis.fusion = NULL;
    Destroy_Analysis(&analysis);
    fclose(quiet);
    printf("AIO-5 generation/selection/transformation controls passed\n");
    return 0;
}

static int
Run_PU_Scope_Contract(void)
{
    DSL_FUSION_CONTROL control;
    AIO5_FIXTURE fixture;
    AIO5_FIXTURE other;
    AIO5_ANALYSIS analysis;
    FILE *quiet = tmpfile();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    DSL_Fusion_Control_Init(&control);
    control.resource_limit_bytes = 256;
    if (quiet == NULL ||
        !Create_Fixture
             ("aio5_scope", "[2,2]", AIO5_FIXTURE_LEGAL, &fixture) ||
        !Build_Analysis(&fixture, FALSE, &control, NULL, &analysis) ||
        !Create_Fixture
             ("aio5_scope_other", "[2,2]", AIO5_FIXTURE_LEGAL,
              &other) ||
        DSL_Fusion_Candidates_Verify(analysis.fusion, quiet) ||
        !DSL_Builder_Select_PU(fixture.pu) ||
        !DSL_Fusion_Candidates_Verify(analysis.fusion, quiet))
        return 1;
    Destroy_Analysis(&analysis);
    fclose(quiet);
    printf("AIO-5 per-PU scope contract passed\n");
    return 0;
}

int
main(void)
{
    const char *mode;
    Initialize_Test_Context();
    mode = getenv("OPEN64_AIO5_MODE");
    if (mode == NULL)
        return 1;
    if (strcmp(mode, "before") == 0)
        return Run_Image_Mode(FALSE);
    if (strcmp(mode, "after") == 0)
        return Run_Image_Mode(TRUE);
    if (strcmp(mode, "candidate") == 0)
        return Run_Candidate_Only();
    if (strcmp(mode, "descriptor") == 0)
        return Run_Rejection
                   (AIO5_FIXTURE_DESCRIPTOR, 0, 256,
                    DSL_OPT_LEGALITY_REJECTED,
                    DSL_OPT_REJECT_DESCRIPTOR, 1);
    if (strcmp(mode, "effect") == 0)
        return Run_Rejection
                   (AIO5_FIXTURE_EFFECT,
                    DSL_TENSOR_CONTROL_EFFECT_BARRIER, 256,
                    DSL_OPT_LEGALITY_REJECTED,
                    DSL_OPT_REJECT_EFFECT, 1);
    if (strcmp(mode, "semantic") == 0)
        return Run_Rejection
                   (AIO5_FIXTURE_SEMANTIC, 0, 256,
                    DSL_OPT_LEGALITY_REJECTED,
                    DSL_OPT_REJECT_MALFORMED, 1);
    if (strcmp(mode, "resource") == 0)
        return Run_Rejection
                   (AIO5_FIXTURE_LEGAL, 0, 1,
                    DSL_OPT_LEGALITY_REJECTED,
                    DSL_OPT_REJECT_RESOURCE, 1);
    if (strcmp(mode, "unknown") == 0)
        return Run_Rejection
                   (AIO5_FIXTURE_LEGAL, 0, 0,
                    DSL_OPT_LEGALITY_UNKNOWN,
                    DSL_OPT_REJECT_INCOMPLETE_ANALYSIS, 1);
    if (strcmp(mode, "region") == 0)
        return Run_Rejection
                   (AIO5_FIXTURE_LEGAL,
                    DSL_TENSOR_CONTROL_REGION, 256,
                    DSL_OPT_LEGALITY_UNKNOWN,
                    DSL_OPT_REJECT_INCOMPLETE_ANALYSIS, 1);
    if (strcmp(mode, "control") == 0)
        return Run_Control_Contract();
    if (strcmp(mode, "scope") == 0)
        return Run_PU_Scope_Contract();
    return 1;
}
