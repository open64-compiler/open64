/*
 * Check-only contract tests for AIO-2 candidate, plan, legality, and cost IR.
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
#include "dsl_opt_plan.h"
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
} AIO2_FIXTURE;

typedef struct {
    DSL_OPT_PLAN_CONTEXT *context;
    DSL_OPT_CANDIDATE_ID baseline_candidate;
    DSL_OPT_CANDIDATE_ID tile_candidate;
    DSL_OPT_COST_ID baseline_cost;
    DSL_OPT_COST_ID tile_cost;
    DSL_OPT_PLAN_ID baseline_plan;
    DSL_OPT_PLAN_ID tile_plan;
} AIO2_PLAN_FIXTURE;

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
Create_Fixture(const char *pu_name, AIO2_FIXTURE *fixture)
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
    identity.defining_module = "aio2.optimization_plan_contract";
    identity.defining_file = __FILE__;
    identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(fixture->pu, &identity))
        return FALSE;
    file_id = DSL_Builder_Register_Source_File(fixture->pu, __FILE__);
    if (file_id == 0)
        return FALSE;

    kid0_ty = Create_Tensor_Type("aio2_kid0_type", "[2,3]");
    kid1_ty = Create_Tensor_Type("aio2_kid1_type", "[3,4]");
    result_ty = Create_Tensor_Type("aio2_result_type", "[2,4]");
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
    return fixture->result != NULL &&
           DSL_Builder_Set_Value_Source_Position
               (fixture->result, &source_position) &&
           DSL_Builder_Append_PU_Value(fixture->pu, fixture->kid0) &&
           DSL_Builder_Append_PU_Value(fixture->pu, fixture->kid1) &&
           DSL_Builder_Append_PU_Value(fixture->pu, fixture->result);
}

static void
Set_Known_Term
        (DSL_OPT_COST_TERM *term, UINT64 amount, UINT32 evidence)
{
    memset(term, 0, sizeof(*term));
    term->amount = amount;
    term->unit = DSL_OPT_COST_UNIT_RELATIVE;
    term->confidence = DSL_OPT_COST_CONFIDENCE_EXACT;
    term->evidence = evidence;
}

static BOOL
Build_Plan_Fixture
        (const AIO2_FIXTURE *fixture,
         const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_OPT_PLAN_BUDGET *budget,
         AIO2_PLAN_FIXTURE *plan_fixture,
         FILE *diagnostic)
{
    DSL_TENSOR_EVOLUTION_NODE_RECORD result_root;
    DSL_IR_VALUE_RECORD result_value;
    DSL_OPT_CANDIDATE_INPUT candidate;
    DSL_OPT_COST_INPUT cost;
    DSL_OPT_PLAN_INPUT plan;
    DSL_OPT_CANDIDATE_ID member;

    memset(plan_fixture, 0, sizeof(*plan_fixture));
    if (!DSL_Tensor_Evolution_Find_Semantic_Root
             (graph, DSL_Builder_Get_Value_Image_Id(fixture->result),
              &result_root) ||
        !DSL_IR_Image_Get_Value(result_root.semantic_value_id, &result_value))
        return FALSE;
    plan_fixture->context = DSL_Opt_Plan_Create
                                (fixture->pu, graph, budget, diagnostic);
    if (plan_fixture->context == NULL)
        return FALSE;

    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = DSL_OPT_CANDIDATE_BASELINE;
    candidate.semantic_node_id = result_value.producer_node_id;
    candidate.source_evolution_node_id = result_root.id;
    candidate.result_evolution_node_id = result_root.id;
    candidate.legality = DSL_OPT_LEGALITY_PROVEN;
    candidate.rejection_reason = DSL_OPT_REJECT_NONE;
    candidate.ordering_key = 100;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_BASELINE;
    if (!DSL_Opt_Plan_Add_Candidate
             (plan_fixture->context, &candidate,
              &plan_fixture->baseline_candidate, diagnostic))
        return FALSE;

    candidate.kind = DSL_OPT_CANDIDATE_TILE;
    candidate.parent_candidate_id = plan_fixture->baseline_candidate;
    candidate.ordering_key = 200;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
    if (!DSL_Opt_Plan_Add_Candidate
             (plan_fixture->context, &candidate,
              &plan_fixture->tile_candidate, diagnostic))
        return FALSE;

    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = 1;
    cost.ordering_key = 100;
    Set_Known_Term(&cost.terms[DSL_OPT_COST_COMPUTE], 100,
                   DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    Set_Known_Term(&cost.terms[DSL_OPT_COST_MEMORY_UNHIDDEN], 40,
                   DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    Set_Known_Term(&cost.terms[DSL_OPT_COST_COMMUNICATION_UNHIDDEN], 0,
                   DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    Set_Known_Term(&cost.terms[DSL_OPT_COST_SYNC], 5,
                   DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    Set_Known_Term(&cost.terms[DSL_OPT_COST_LAUNCH], 10,
                   DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    Set_Known_Term(&cost.terms[DSL_OPT_COST_RUNTIME_SELECTION], 0,
                   DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    if (!DSL_Opt_Plan_Add_Cost
             (plan_fixture->context, &cost,
              &plan_fixture->baseline_cost, diagnostic))
        return FALSE;

    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = 1;
    cost.ordering_key = 200;
    Set_Known_Term(&cost.terms[DSL_OPT_COST_COMPUTE], 70,
                   DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    Set_Known_Term(&cost.terms[DSL_OPT_COST_COMMUNICATION_UNHIDDEN], 0,
                   DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    Set_Known_Term(&cost.terms[DSL_OPT_COST_SYNC], 8,
                   DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    Set_Known_Term(&cost.terms[DSL_OPT_COST_LAUNCH], 10,
                   DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    Set_Known_Term(&cost.terms[DSL_OPT_COST_RUNTIME_SELECTION], 0,
                   DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    if (!DSL_Opt_Plan_Add_Cost
             (plan_fixture->context, &cost,
              &plan_fixture->tile_cost, diagnostic))
        return FALSE;

    memset(&plan, 0, sizeof(plan));
    member = plan_fixture->baseline_candidate;
    plan.candidate_ids = &member;
    plan.candidate_count = 1;
    plan.cost_id = plan_fixture->baseline_cost;
    plan.legality = DSL_OPT_LEGALITY_PROVEN;
    plan.rejection_reason = DSL_OPT_REJECT_NONE;
    plan.ordering_key = 100;
    plan.flags = DSL_OPT_PLAN_FLAG_BASELINE;
    if (!DSL_Opt_Plan_Add_Plan
             (plan_fixture->context, &plan,
              &plan_fixture->baseline_plan, diagnostic))
        return FALSE;

    member = plan_fixture->tile_candidate;
    plan.candidate_ids = &member;
    plan.cost_id = plan_fixture->tile_cost;
    plan.fallback_plan_id = plan_fixture->baseline_plan;
    plan.ordering_key = 200;
    plan.flags = DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    return DSL_Opt_Plan_Add_Plan
               (plan_fixture->context, &plan,
                &plan_fixture->tile_plan, diagnostic);
}

static BOOL
Check_Main_Plan(const AIO2_PLAN_FIXTURE *fixture)
{
    DSL_OPT_CANDIDATE_RECORD candidate;
    DSL_OPT_COST_RECORD cost;
    DSL_OPT_PLAN_RECORD plan;
    DSL_OPT_PLAN_MEMBER_RECORD member;
    DSL_OPT_SELECTION_RESULT selection;
    if (!DSL_Opt_Plan_Verify(fixture->context, stderr) ||
        !DSL_Opt_Plan_Select(fixture->context, 1, &selection, stderr) ||
        !DSL_Opt_Plan_Select(fixture->context, 1, &selection, stderr) ||
        selection.selected_plan_id != fixture->baseline_plan ||
        selection.legal_plan_count != 2 ||
        selection.complete_cost_count != 1 ||
        selection.incomplete_cost_count != 1 ||
        selection.target_mismatch_count != 0 ||
        selection.rejected_plan_count != 0 ||
        DSL_Opt_Plan_Candidate_Count(fixture->context) != 2 ||
        DSL_Opt_Plan_Cost_Count(fixture->context) != 2 ||
        DSL_Opt_Plan_Plan_Count(fixture->context) != 2 ||
        !DSL_Opt_Plan_Get_Candidate
             (fixture->context, fixture->tile_candidate, &candidate) ||
        candidate.parent_candidate_id != fixture->baseline_candidate ||
        !DSL_Opt_Plan_Get_Cost
             (fixture->context, fixture->baseline_cost, &cost) ||
        !cost.complete || cost.total != 155 ||
        !DSL_Opt_Plan_Get_Cost
             (fixture->context, fixture->tile_cost, &cost) ||
        cost.complete || cost.total != 0 ||
        !DSL_Opt_Plan_Get_Plan
             (fixture->context, fixture->tile_plan, &plan) ||
        plan.fallback_plan_id != fixture->baseline_plan ||
        !DSL_Opt_Plan_Get_Member
             (fixture->context, fixture->tile_plan, 0, &member) ||
        member.candidate_id != fixture->tile_candidate ||
        DSL_Opt_Plan_Get_Member
             (fixture->context, fixture->tile_plan, 1, &member))
        return FALSE;
    return TRUE;
}

static int
Run_Image_Mode(BOOL build_plan)
{
    const char *artifact = getenv("OPEN64_AIO2_ARTIFACT");
    const char *graph_path = getenv("OPEN64_AIO2_GRAPH");
    const char *plan_path = getenv("OPEN64_AIO2_PLAN");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST image_request;
    DSL_TENSOR_EVOLUTION_GRAPH *graph = NULL;
    AIO2_PLAN_FIXTURE plan_fixture;
    AIO2_FIXTURE fixture;
    DSL_OPT_PLAN_BUDGET budget;
    UINT32 image_nodes;
    UINT32 image_values;
    UINT32 type_count;

    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio2_common_matmul", &fixture))
        return 1;
    image_nodes = DSL_IR_Image_Node_Count();
    image_values = DSL_IR_Image_Value_Count();
    type_count = TY_Table_Size();

    if (build_plan) {
        graph = DSL_Tensor_Evolution_Create(fixture.pu, stderr);
        budget.max_candidates = 4;
        budget.max_plans = 4;
        if (graph == NULL ||
            !DSL_Tensor_Evolution_Build_Semantic_Roots(graph, stderr) ||
            !Build_Plan_Fixture
                 (&fixture, graph, &budget, &plan_fixture, stderr) ||
            !Check_Main_Plan(&plan_fixture) ||
            DSL_IR_Image_Node_Count() != image_nodes ||
            DSL_IR_Image_Value_Count() != image_values ||
            TY_Table_Size() != type_count)
            return 1;
        if (graph_path != NULL && graph_path[0] != '\0') {
            FILE *file = fopen(graph_path, "w");
            if (file == NULL)
                return 1;
            DSL_Tensor_Evolution_Print(file, graph);
            fclose(file);
        }
        if (plan_path != NULL && plan_path[0] != '\0') {
            FILE *file = fopen(plan_path, "w");
            if (file == NULL)
                return 1;
            DSL_Opt_Plan_Print(file, plan_fixture.context);
            fclose(file);
        }
        DSL_Opt_Plan_Destroy(plan_fixture.context);
        DSL_Tensor_Evolution_Destroy(graph);
    }

    image_request.path = artifact;
    image_request.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&image_request))
        return 1;
    printf("AIO-2 %s image passed\n", build_plan ? "after" : "before");
    return 0;
}

static int
Run_Negative_Contract(void)
{
    AIO2_FIXTURE fixture;
    AIO2_PLAN_FIXTURE plan_fixture;
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_OPT_PLAN_BUDGET budget;
    DSL_OPT_CANDIDATE_INPUT candidate;
    DSL_OPT_COST_INPUT cost;
    DSL_OPT_PLAN_INPUT plan;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_CANDIDATE_ID baseline_candidate_id;
    DSL_OPT_COST_ID cost_id;
    DSL_OPT_COST_ID baseline_cost_id;
    DSL_OPT_PLAN_ID plan_id;
    DSL_OPT_CANDIDATE_ID duplicate_members[2];
    DSL_OPT_SELECTION_RESULT selection;
    FILE *quiet = tmpfile();

    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (quiet == NULL ||
        !Create_Fixture("aio2_negative_contract", &fixture))
        return 1;
    graph = DSL_Tensor_Evolution_Create(fixture.pu, stderr);
    budget.max_candidates = 4;
    budget.max_plans = 4;
    if (graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots(graph, stderr) ||
        !Build_Plan_Fixture
             (&fixture, graph, &budget, &plan_fixture, stderr))
        return 1;

    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = 1;
    cost.ordering_key = 300;
    cost.terms[DSL_OPT_COST_COMPUTE].amount = 1;
    if (DSL_Opt_Plan_Add_Cost
            (plan_fixture.context, &cost, &cost_id, quiet) ||
        DSL_Opt_Plan_Cost_Count(plan_fixture.context) != 2)
        return 1;

    duplicate_members[0] = plan_fixture.baseline_candidate;
    duplicate_members[1] = plan_fixture.baseline_candidate;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = duplicate_members;
    plan.candidate_count = 2;
    plan.cost_id = plan_fixture.baseline_cost;
    plan.fallback_plan_id = plan_fixture.baseline_plan;
    plan.legality = DSL_OPT_LEGALITY_PROVEN;
    plan.rejection_reason = DSL_OPT_REJECT_NONE;
    plan.ordering_key = 300;
    if (DSL_Opt_Plan_Add_Plan
            (plan_fixture.context, &plan, &plan_id, quiet) ||
        DSL_Opt_Plan_Plan_Count(plan_fixture.context) != 2 ||
        DSL_Opt_Plan_Select
            (plan_fixture.context, 2, &selection, quiet) ||
        selection.selected_plan_id != 0 ||
        selection.target_mismatch_count != 2 ||
        !DSL_Opt_Plan_Select
            (plan_fixture.context, 1, &selection, stderr) ||
        selection.selected_plan_id != plan_fixture.baseline_plan)
        return 1;

    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = DSL_OPT_CANDIDATE_TILE;
    candidate.semantic_node_id = 1;
    candidate.source_evolution_node_id = 1;
    candidate.result_evolution_node_id = 1;
    candidate.parent_candidate_id = plan_fixture.baseline_candidate;
    candidate.legality = DSL_OPT_LEGALITY_PROVEN;
    candidate.rejection_reason = DSL_OPT_REJECT_NONE;
    candidate.ordering_key = 300;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
    if (DSL_Opt_Plan_Add_Candidate
            (plan_fixture.context, &candidate, &candidate_id, quiet))
        return 1;
    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = 1;
    cost.ordering_key = 300;
    for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i)
        Set_Known_Term(&cost.terms[i], i + 1,
                       DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    if (DSL_Opt_Plan_Add_Cost
            (plan_fixture.context, &cost, &cost_id, quiet) ||
        DSL_Opt_Plan_Add_Plan
            (plan_fixture.context, &plan, &plan_id, quiet) ||
        DSL_Opt_Plan_Candidate_Count(plan_fixture.context) != 2 ||
        DSL_Opt_Plan_Cost_Count(plan_fixture.context) != 2 ||
        DSL_Opt_Plan_Plan_Count(plan_fixture.context) != 2)
        return 1;
    DSL_Opt_Plan_Destroy(plan_fixture.context);

    budget.max_candidates = 1;
    budget.max_plans = 1;
    if (!Build_Plan_Fixture
             (&fixture, graph, &budget, &plan_fixture, quiet)) {
        if (plan_fixture.context != NULL)
            DSL_Opt_Plan_Destroy(plan_fixture.context);
    } else {
        return 1;
    }

    DSL_OPT_PLAN_CONTEXT *limited = DSL_Opt_Plan_Create
                                        (fixture.pu, graph, &budget, stderr);
    DSL_TENSOR_EVOLUTION_NODE_RECORD result_root;
    DSL_IR_VALUE_RECORD result_value;
    if (limited == NULL ||
        !DSL_Tensor_Evolution_Find_Semantic_Root
             (graph, DSL_Builder_Get_Value_Image_Id(fixture.result),
              &result_root) ||
        !DSL_IR_Image_Get_Value(result_root.semantic_value_id, &result_value))
        return 1;
    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = DSL_OPT_CANDIDATE_BASELINE;
    candidate.semantic_node_id = result_value.producer_node_id;
    candidate.source_evolution_node_id = result_root.id;
    candidate.result_evolution_node_id = result_root.id;
    candidate.legality = DSL_OPT_LEGALITY_PROVEN;
    candidate.rejection_reason = DSL_OPT_REJECT_NONE;
    candidate.ordering_key = 100;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_BASELINE;
    if (!DSL_Opt_Plan_Add_Candidate
             (limited, &candidate, &candidate_id, stderr))
        return 1;
    baseline_candidate_id = candidate_id;
    candidate.kind = DSL_OPT_CANDIDATE_TILE;
    candidate.parent_candidate_id = candidate_id;
    candidate.ordering_key = 200;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
    if (DSL_Opt_Plan_Add_Candidate
            (limited, &candidate, &candidate_id, quiet) ||
        !DSL_Opt_Plan_Candidate_Budget_Exhausted(limited) ||
        DSL_Opt_Plan_Candidate_Count(limited) != 1)
        return 1;

    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = 1;
    cost.ordering_key = 100;
    for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i)
        Set_Known_Term(&cost.terms[i], i + 1,
                       DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    if (!DSL_Opt_Plan_Add_Cost(limited, &cost, &cost_id, stderr))
        return 1;
    baseline_cost_id = cost_id;
    cost.ordering_key = 200;
    if (DSL_Opt_Plan_Add_Cost(limited, &cost, &cost_id, quiet) ||
        DSL_Opt_Plan_Cost_Count(limited) != 1)
        return 1;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &baseline_candidate_id;
    plan.candidate_count = 1;
    plan.cost_id = baseline_cost_id;
    plan.legality = DSL_OPT_LEGALITY_PROVEN;
    plan.rejection_reason = DSL_OPT_REJECT_NONE;
    plan.ordering_key = 100;
    plan.flags = DSL_OPT_PLAN_FLAG_BASELINE;
    if (!DSL_Opt_Plan_Add_Plan(limited, &plan, &plan_id, stderr) ||
        DSL_Opt_Plan_Add_Plan(limited, &plan, &plan_id, quiet) ||
        !DSL_Opt_Plan_Plan_Budget_Exhausted(limited) ||
        DSL_Opt_Plan_Plan_Count(limited) != 1 ||
        !DSL_Opt_Plan_Verify(limited, stderr))
        return 1;

    DSL_Opt_Plan_Destroy(limited);
    DSL_Tensor_Evolution_Destroy(graph);
    fclose(quiet);
    printf("AIO-2 negative and budget contracts passed\n");
    return 0;
}

int
main(void)
{
    const char *mode;
    Initialize_Test_Context();
    mode = getenv("OPEN64_AIO2_MODE");
    if (mode == NULL)
        return 1;
    if (strcmp(mode, "before") == 0)
        return Run_Image_Mode(FALSE);
    if (strcmp(mode, "after") == 0)
        return Run_Image_Mode(TRUE);
    if (strcmp(mode, "contract") == 0)
        return Run_Negative_Contract();
    return 1;
}
