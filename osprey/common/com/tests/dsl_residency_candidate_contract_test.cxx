/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Certifies AIO-8 memory hierarchy and PU-local residency alternatives without
 * allocating target storage or transforming WHIRL. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO8-RESIDENCY.md.
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
#include "dsl_residency_candidate.h"
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
} AIO8_FIXTURE;

typedef enum {
    AIO8_CONTROL_STRAIGHT = 0,
    AIO8_CONTROL_REGION = 1,
    AIO8_CONTROL_EFFECT = 2
} AIO8_CONTROL_KIND;

typedef struct {
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_TENSOR_ANALYSIS *tensor;
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    DSL_RESIDENCY_ANALYSIS *residency;
} AIO8_ANALYSIS;

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
Create_Fixture (const char *name, const char *shape, AIO8_FIXTURE *fixture)
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
    identity.defining_module = "aio8.residency_candidate_contract";
    identity.defining_file = __FILE__;
    identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(fixture->pu, &identity))
        return FALSE;
    fixture->file_id = DSL_Builder_Register_Source_File
                           (fixture->pu, __FILE__);
    tensor_ty = Create_Tensor_Type(name, shape);
    if (fixture->file_id == 0 || tensor_ty == TY_IDX_ZERO)
        return FALSE;
    common = DSL_Domain_Find("common");
    matmul = DSL_Opcode_Find(common, DSL_OPCODE_COMMON_MATMUL, 1);
    relu = DSL_Opcode_Find(common, "common.relu", 2);
    if (matmul == DSL_OPCODE_INVALID_ID || relu == DSL_OPCODE_INVALID_ID)
        return FALSE;
    fixture->values[0] = DSL_Builder_Create_Tensor_Constant
                             ("aio8_kid0", tensor_ty, "float32", 2, shape,
                              "splat", "1.0");
    fixture->values[1] = DSL_Builder_Create_Tensor_Constant
                             ("aio8_kid1", tensor_ty, "float32", 2, shape,
                              "splat", "1.0");
    attributes[0].name = "attr.transpose_kid0";
    attributes[0].value = "false";
    attributes[1].name = "attr.transpose_kid1";
    attributes[1].value = "false";
    kids[0] = fixture->values[0];
    kids[1] = fixture->values[1];
    fixture->values[2] = DSL_Builder_Create_Operator_With_Result
                             (matmul, 1, kids, 2, attributes, 2,
                              "aio8_matmul", tensor_ty);
    kids[0] = fixture->values[2];
    fixture->values[3] = DSL_Builder_Create_Operator_With_Result
                             (relu, 2, kids, 1, NULL, 0,
                              "aio8_relu", tensor_ty);
    for (UINT32 i = 0; i < 4; ++i) {
        if (fixture->values[i] == NULL ||
            !Set_Source(fixture->values[i], fixture->file_id, 10 + i) ||
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

static BOOL
Add_Block (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, UINT32 id, UINT32 rpo,
           UINT32 region, UINT32 flags)
{
    DSL_TENSOR_CONTROL_BLOCK block;
    memset(&block, 0, sizeof(block));
    block.block_id = id;
    block.reverse_postorder = rpo;
    block.region_id = region;
    block.flags = flags;
    return DSL_Tensor_Control_Snapshot_Add_Block(snapshot, &block, stderr);
}

static BOOL
Add_Position (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
              DSL_BUILDER_VALUE value, UINT32 block, UINT32 rpo,
              UINT32 order)
{
    DSL_TENSOR_CONTROL_POSITION position;
    memset(&position, 0, sizeof(position));
    position.node_id = Value_Node(value);
    position.block_id = block;
    position.reverse_postorder = rpo;
    position.statement_order = order;
    return DSL_Tensor_Control_Snapshot_Add_Position
               (snapshot, &position, stderr);
}

static DSL_TENSOR_CONTROL_SNAPSHOT *
Create_Snapshot (const AIO8_FIXTURE *fixture, AIO8_CONTROL_KIND kind)
{
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot =
        DSL_Tensor_Control_Snapshot_Create(fixture->pu, stderr);
    if (snapshot == NULL)
        return NULL;
    if (kind == AIO8_CONTROL_REGION) {
        if (!Add_Block(snapshot, 1, 1, 10, DSL_TENSOR_CONTROL_REGION) ||
            !Add_Block(snapshot, 2, 2, 20, DSL_TENSOR_CONTROL_REGION) ||
            !Add_Position(snapshot, fixture->values[0], 1, 1, 1) ||
            !Add_Position(snapshot, fixture->values[1], 1, 1, 2) ||
            !Add_Position(snapshot, fixture->values[2], 1, 1, 3) ||
            !Add_Position(snapshot, fixture->values[3], 2, 2, 1))
            return NULL;
    } else {
        UINT32 flags = kind == AIO8_CONTROL_EFFECT ?
                       DSL_TENSOR_CONTROL_EFFECT_BARRIER : 0;
        if (!Add_Block(snapshot, 1, 1, 0, flags))
            return NULL;
        for (UINT32 i = 0; i < 4; ++i) {
            if (!Add_Position(snapshot, fixture->values[i], 1, 1, i + 1))
                return NULL;
        }
    }
    return DSL_Tensor_Control_Snapshot_Seal(snapshot, stderr) ?
           snapshot : NULL;
}

static void
Destroy_Analysis (AIO8_ANALYSIS *analysis)
{
    DSL_Residency_Destroy(analysis->residency);
    DSL_Tensor_Locality_Destroy(analysis->locality);
    DSL_Tensor_Control_Snapshot_Destroy(analysis->snapshot);
    DSL_Tensor_Analysis_Destroy(analysis->tensor);
    DSL_Tensor_Evolution_Destroy(analysis->graph);
    memset(analysis, 0, sizeof(*analysis));
}

static BOOL
Build_Analysis (const AIO8_FIXTURE *fixture, AIO8_CONTROL_KIND kind,
                UINT32 profile_id, BOOL select_plans, FILE *trace,
                AIO8_ANALYSIS *analysis)
{
    DSL_RESIDENCY_CONTROL control;
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
    analysis->snapshot = Create_Snapshot(fixture, kind);
    analysis->locality = DSL_Tensor_Locality_Create
                             (fixture->pu, analysis->tensor,
                              analysis->snapshot, stderr);
    if (analysis->snapshot == NULL || analysis->locality == NULL ||
        !DSL_Tensor_Locality_Build(analysis->locality, stderr))
        return FALSE;
    DSL_Residency_Control_Init(&control);
    control.target_profile_id = profile_id;
    control.select_plans = select_plans;
    control.focus_value_id =
        DSL_Builder_Get_Value_Image_Id(fixture->values[2]);
    analysis->residency = DSL_Residency_Create
                              (fixture->pu, analysis->graph,
                               analysis->tensor, analysis->locality,
                               &control, stderr);
    if (analysis->residency == NULL ||
        !DSL_Residency_Build(analysis->residency, stderr) ||
        !DSL_Residency_Verify(analysis->residency, stderr))
        return FALSE;
    if (trace != NULL) {
        DSL_Tensor_Evolution_Print(trace, analysis->graph);
        DSL_Residency_Print(trace, analysis->residency);
    }
    return TRUE;
}

static BOOL
Find_Alternative
        (const AIO8_ANALYSIS *analysis, UINT32 tier_kind,
         DSL_RESIDENCY_ALTERNATIVE_RECORD *alternative)
{
    for (UINT32 id = 1;
         id <= DSL_Residency_Alternative_Count(analysis->residency); ++id) {
        DSL_RESIDENCY_ALTERNATIVE_RECORD candidate;
        DSL_RESIDENCY_DESCRIPTOR_RECORD descriptor;
        if (!DSL_Residency_Get_Alternative
                 (analysis->residency, id, &candidate) ||
            !DSL_Residency_Get_Descriptor
                 (analysis->residency, candidate.descriptor_id,
                  &descriptor))
            return FALSE;
        if (descriptor.tier_kind == tier_kind) {
            if (alternative != NULL)
                *alternative = candidate;
            return TRUE;
        }
    }
    return FALSE;
}

static BOOL
Check_Main_Contract (const AIO8_ANALYSIS *analysis)
{
    DSL_RESIDENCY_SITE_RECORD site;
    DSL_RESIDENCY_ALTERNATIVE_RECORD alternative;
    if (DSL_Residency_Site_Count(analysis->residency) != 1 ||
        DSL_Residency_Descriptor_Count(analysis->residency) != 6 ||
        DSL_Residency_Alternative_Count(analysis->residency) != 6 ||
        DSL_Tensor_Evolution_Node_Count(analysis->graph) != 10 ||
        DSL_Tensor_Evolution_Edge_Count(analysis->graph) != 6 ||
        !DSL_Residency_Get_Site(analysis->residency, 1, &site) ||
        site.alternative_count != 6 || site.selected_plan_id != 7 ||
        !Find_Alternative
             (analysis, DSL_MEMORY_TIER_HBM, &alternative) ||
        alternative.legality != DSL_OPT_LEGALITY_PROVEN ||
        !Find_Alternative
             (analysis, DSL_MEMORY_TIER_SHARED, &alternative) ||
        alternative.legality != DSL_OPT_LEGALITY_PROVEN ||
        !Find_Alternative
             (analysis, DSL_MEMORY_TIER_REGISTER, &alternative) ||
        alternative.legality != DSL_OPT_LEGALITY_PROVEN ||
        !Find_Alternative
             (analysis, DSL_MEMORY_TIER_SYSTEM, &alternative) ||
        alternative.legality != DSL_OPT_LEGALITY_UNKNOWN)
        return FALSE;
    return TRUE;
}

static int
Run_Image_Mode (BOOL run_analysis)
{
    const char *artifact = getenv("OPEN64_AIO8_ARTIFACT");
    const char *trace_path = getenv("OPEN64_AIO8_ANALYSIS");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_VERIFY_RESULT verify;
    AIO8_FIXTURE fixture;
    AIO8_ANALYSIS analysis;
    UINT32 nodes;
    UINT32 values;
    UINT32 types;
    char diagnostic[4096];
    FILE *trace = NULL;
    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio8_residency", "[8,8]", &fixture))
        return 1;
    nodes = DSL_IR_Image_Node_Count();
    values = DSL_IR_Image_Value_Count();
    types = TY_Table_Size();
    if (run_analysis) {
        if (trace_path == NULL || trace_path[0] == '\0')
            return 1;
        trace = fopen(trace_path, "w");
        if (trace == NULL ||
            !Build_Analysis
                 (&fixture, AIO8_CONTROL_STRAIGHT,
                  DSL_TARGET_PROFILE_NVIDIA_HOPPER, TRUE, trace,
                  &analysis) ||
            !Check_Main_Contract(&analysis) ||
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
    printf("AIO-8 %s image passed\n", run_analysis ? "after" : "before");
    return 0;
}

static int
Run_Profile_Case (UINT32 profile_id, UINT32 expected_l2_legality)
{
    const char *trace_path = getenv("OPEN64_AIO8_ANALYSIS");
    AIO8_FIXTURE fixture;
    AIO8_ANALYSIS analysis;
    DSL_RESIDENCY_ALTERNATIVE_RECORD alternative;
    FILE *trace = trace_path == NULL ? NULL : fopen(trace_path, "w");
    if (trace_path != NULL && trace == NULL)
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio8_profile", "[4096,4096]", &fixture) ||
        !Build_Analysis
             (&fixture, AIO8_CONTROL_STRAIGHT, profile_id, FALSE,
              trace, &analysis) ||
        !Find_Alternative
             (&analysis, DSL_MEMORY_TIER_L2, &alternative) ||
        alternative.legality != expected_l2_legality)
        return 1;
    if (trace != NULL)
        fclose(trace);
    Destroy_Analysis(&analysis);
    return 0;
}

static int
Run_Lifetime_Case (AIO8_CONTROL_KIND kind, UINT32 expected_legality,
                   UINT32 expected_reason)
{
    const char *trace_path = getenv("OPEN64_AIO8_ANALYSIS");
    AIO8_FIXTURE fixture;
    AIO8_ANALYSIS analysis;
    DSL_RESIDENCY_ALTERNATIVE_RECORD alternative;
    FILE *trace = trace_path == NULL ? NULL : fopen(trace_path, "w");
    if (trace_path != NULL && trace == NULL)
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio8_lifetime", "[8,8]", &fixture) ||
        !Build_Analysis
             (&fixture, kind, DSL_TARGET_PROFILE_NVIDIA_HOPPER,
              FALSE, trace, &analysis) ||
        !Find_Alternative
             (&analysis, DSL_MEMORY_TIER_REGISTER, &alternative) ||
        alternative.legality != expected_legality ||
        alternative.rejection_reason != expected_reason)
        return 1;
    if (trace != NULL)
        fclose(trace);
    Destroy_Analysis(&analysis);
    return 0;
}

static int
Run_Symbolic_Case(void)
{
    AIO8_FIXTURE fixture;
    AIO8_ANALYSIS analysis;
    DSL_RESIDENCY_ALTERNATIVE_RECORD alternative;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio8_symbolic", "[8,<pending>]", &fixture) ||
        !Build_Analysis
             (&fixture, AIO8_CONTROL_STRAIGHT,
              DSL_TARGET_PROFILE_NVIDIA_HOPPER, FALSE, NULL,
              &analysis) ||
        !Find_Alternative
             (&analysis, DSL_MEMORY_TIER_HBM, &alternative) ||
        alternative.legality != DSL_OPT_LEGALITY_UNKNOWN ||
        alternative.rejection_reason !=
            DSL_OPT_REJECT_INCOMPLETE_ANALYSIS)
        return 1;
    Destroy_Analysis(&analysis);
    return 0;
}

static int
Run_Control(void)
{
    AIO8_FIXTURE fixture;
    AIO8_ANALYSIS analysis;
    DSL_RESIDENCY_CONTROL control;
    DSL_RESIDENCY_ANALYSIS *disabled;
    FILE *quiet = tmpfile();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (quiet == NULL ||
        !Create_Fixture("aio8_control", "[8,8]", &fixture))
        return 1;
    memset(&analysis, 0, sizeof(analysis));
    analysis.graph = DSL_Tensor_Evolution_Create(fixture.pu, stderr);
    if (analysis.graph == NULL ||
        !DSL_Tensor_Evolution_Build_Semantic_Roots(analysis.graph, stderr))
        return 1;
    analysis.tensor = DSL_Tensor_Analysis_Create
                          (fixture.pu, analysis.graph, stderr);
    if (analysis.tensor == NULL ||
        !DSL_Tensor_Analysis_Build(analysis.tensor, stderr))
        return 1;
    analysis.snapshot = Create_Snapshot(&fixture, AIO8_CONTROL_STRAIGHT);
    analysis.locality = DSL_Tensor_Locality_Create
                            (fixture.pu, analysis.tensor,
                             analysis.snapshot, stderr);
    if (analysis.locality == NULL ||
        !DSL_Tensor_Locality_Build(analysis.locality, stderr))
        return 1;
    DSL_Residency_Control_Init(&control);
    control.apply_transformation = 1;
    if (DSL_Residency_Create
            (fixture.pu, analysis.graph, analysis.tensor,
             analysis.locality, &control, quiet) != NULL)
        return 1;
    control.apply_transformation = 0;
    control.generate_candidates = 0;
    disabled = DSL_Residency_Create
                   (fixture.pu, analysis.graph, analysis.tensor,
                    analysis.locality, &control, stderr);
    if (disabled == NULL || !DSL_Residency_Build(disabled, stderr) ||
        !DSL_Residency_Verify(disabled, stderr) ||
        DSL_Residency_Site_Count(disabled) != 0)
        return 1;
    DSL_Residency_Destroy(disabled);
    DSL_Tensor_Locality_Destroy(analysis.locality);
    DSL_Tensor_Control_Snapshot_Destroy(analysis.snapshot);
    DSL_Tensor_Analysis_Destroy(analysis.tensor);
    DSL_Tensor_Evolution_Destroy(analysis.graph);
    fclose(quiet);
    return 0;
}

int
main(void)
{
    const char *mode;
    Initialize_Test_Context();
    mode = getenv("OPEN64_AIO8_MODE");
    if (mode == NULL)
        return 1;
    if (strcmp(mode, "before") == 0)
        return Run_Image_Mode(FALSE);
    if (strcmp(mode, "after") == 0)
        return Run_Image_Mode(TRUE);
    if (strcmp(mode, "hopper_capacity") == 0)
        return Run_Profile_Case
                   (DSL_TARGET_PROFILE_NVIDIA_HOPPER,
                    DSL_OPT_LEGALITY_REJECTED);
    if (strcmp(mode, "blackwell_capacity") == 0)
        return Run_Profile_Case
                   (DSL_TARGET_PROFILE_NVIDIA_BLACKWELL,
                    DSL_OPT_LEGALITY_PROVEN);
    if (strcmp(mode, "cpu") == 0)
        return Run_Profile_Case
                   (DSL_TARGET_PROFILE_CPU_BASELINE,
                    DSL_OPT_LEGALITY_REJECTED);
    if (strcmp(mode, "effect") == 0)
        return Run_Lifetime_Case
                   (AIO8_CONTROL_EFFECT, DSL_OPT_LEGALITY_REJECTED,
                    DSL_OPT_REJECT_EFFECT);
    if (strcmp(mode, "region") == 0)
        return Run_Lifetime_Case
                   (AIO8_CONTROL_REGION, DSL_OPT_LEGALITY_REJECTED,
                    DSL_OPT_REJECT_DESCRIPTOR);
    if (strcmp(mode, "symbolic") == 0)
        return Run_Symbolic_Case();
    if (strcmp(mode, "control") == 0)
        return Run_Control();
    return 1;
}
