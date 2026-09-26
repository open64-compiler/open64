/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Certifies AIO-6 logical-layout compatibility, conversion cost, fallback, and
 * check-only alternative construction. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO6-LOGICAL-LAYOUT.md.
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
#include "dsl_layout_candidate.h"
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
    DSL_BUILDER_VALUE values[4];
    UINT32 file_id;
} AIO6_FIXTURE;

typedef enum {
    AIO6_CONTROL_STRAIGHT = 0,
    AIO6_CONTROL_REGION = 1,
    AIO6_CONTROL_EFFECT = 2
} AIO6_CONTROL_KIND;

typedef struct {
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_TENSOR_ANALYSIS *tensor_analysis;
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    DSL_LOGICAL_LAYOUT_ANALYSIS *layout;
} AIO6_ANALYSIS;

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
        (const char *name, const char *shape, const char *layout)
{
    TY_TENSOR_CANONICAL_DESCRIPTOR descriptor;
    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.kind = "tensor";
    descriptor.dtype = "float32";
    descriptor.rank = 2;
    descriptor.logical_shape = shape;
    descriptor.traits = "dense";
    descriptor.layout = layout;
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
Create_Fixture
        (const char *name, const char *shape, AIO6_FIXTURE *fixture)
{
    DSL_BUILDER_PU_SOURCE_IDENTITY identity;
    DSL_BUILDER_OPERATOR_ATTRIBUTE attributes[2];
    DSL_BUILDER_VALUE kids[2];
    DSL_DOMAIN_ID common;
    DSL_OPCODE_ID matmul;
    DSL_OPCODE_ID relu;
    TY_IDX source_ty;

    memset(fixture, 0, sizeof(*fixture));
    fixture->pu = DSL_Builder_Create_Minimal_PU(name);
    if (fixture->pu == NULL) {
        fprintf(stderr, "AIO-6 failed to create PU\n");
        return FALSE;
    }
    memset(&identity, 0, sizeof(identity));
    identity.canonical_definition_name = name;
    identity.defining_module = "aio6.layout_candidate_contract";
    identity.defining_file = __FILE__;
    identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(fixture->pu, &identity)) {
        fprintf(stderr, "AIO-6 failed to set PU identity\n");
        return FALSE;
    }
    fixture->file_id = DSL_Builder_Register_Source_File
                           (fixture->pu, __FILE__);
    source_ty = Create_Tensor_Type(name, shape, "row_major");
    if (fixture->file_id == 0 || source_ty == TY_IDX_ZERO) {
        fprintf(stderr, "AIO-6 failed to create source evidence/types\n");
        return FALSE;
    }

    common = DSL_Domain_Find("common");
    matmul = DSL_Opcode_Find(common, DSL_OPCODE_COMMON_MATMUL, 1);
    relu = DSL_Opcode_Find(common, "common.relu", 2);
    if (matmul == DSL_OPCODE_INVALID_ID ||
        relu == DSL_OPCODE_INVALID_ID) {
        fprintf(stderr, "AIO-6 failed to resolve opcodes\n");
        return FALSE;
    }

    fixture->values[0] = DSL_Builder_Create_Tensor_Constant
                             ("aio6_kid0", source_ty, "float32", 2, shape,
                              "splat", "1.0");
    fixture->values[1] = DSL_Builder_Create_Tensor_Constant
                             ("aio6_kid1", source_ty, "float32", 2, shape,
                              "splat", "1.0");
    attributes[0].name = "attr.transpose_kid0";
    attributes[0].value = "false";
    attributes[1].name = "attr.transpose_kid1";
    attributes[1].value = "false";
    kids[0] = fixture->values[0];
    kids[1] = fixture->values[1];
    fixture->values[2] = DSL_Builder_Create_Operator_With_Result
                             (matmul, 1, kids, 2, attributes, 2,
                              "aio6_matmul", source_ty);
    kids[0] = fixture->values[2];
    fixture->values[3] = DSL_Builder_Create_Operator_With_Result
                             (relu, 2, kids, 1, NULL, 0,
                              "aio6_relu", source_ty);
    for (UINT32 i = 0; i < 4; ++i) {
        if (fixture->values[i] == NULL) {
            fprintf(stderr, "AIO-6 failed to create value %u\n", i);
            return FALSE;
        }
    }
    for (UINT32 i = 0; i < 4; ++i) {
        if (!Set_Source(fixture->values[i], fixture->file_id, 10 + i) ||
            !DSL_Builder_Append_PU_Value(fixture->pu, fixture->values[i])) {
            fprintf(stderr, "AIO-6 failed to append value %u\n", i);
            return FALSE;
        }
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
Add_Block
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, UINT32 id, UINT32 rpo,
         UINT32 region, UINT32 flags)
{
    DSL_TENSOR_CONTROL_BLOCK block;
    memset(&block, 0, sizeof(block));
    block.block_id = id;
    block.reverse_postorder = rpo;
    block.region_id = region;
    block.flags = flags;
    return DSL_tensor_control_snapshot_add_block(snapshot, &block, stderr);
}

static BOOL
Add_Position
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, DSL_BUILDER_VALUE value,
         UINT32 block, UINT32 rpo, UINT32 order)
{
    DSL_TENSOR_CONTROL_POSITION position;
    memset(&position, 0, sizeof(position));
    position.node_id = Value_Node(value);
    position.block_id = block;
    position.reverse_postorder = rpo;
    position.statement_order = order;
    return DSL_tensor_control_snapshot_add_position
               (snapshot, &position, stderr);
}

static DSL_TENSOR_CONTROL_SNAPSHOT *
Create_Snapshot
        (const AIO6_FIXTURE *fixture, AIO6_CONTROL_KIND kind)
{
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot =
        DSL_tensor_control_snapshot_create(fixture->pu, stderr);
    if (snapshot == NULL)
        return NULL;
    if (kind == AIO6_CONTROL_REGION) {
        if (!Add_Block(snapshot, 1, 1, 10, DSL_TENSOR_CONTROL_REGION) ||
            !Add_Block(snapshot, 2, 2, 20, DSL_TENSOR_CONTROL_REGION) ||
            !Add_Position(snapshot, fixture->values[0], 1, 1, 1) ||
            !Add_Position(snapshot, fixture->values[1], 1, 1, 2) ||
            !Add_Position(snapshot, fixture->values[2], 1, 1, 3) ||
            !Add_Position(snapshot, fixture->values[3], 2, 2, 1))
            return NULL;
    } else {
        UINT32 flags = kind == AIO6_CONTROL_EFFECT ?
                       DSL_TENSOR_CONTROL_EFFECT_BARRIER : 0;
        if (!Add_Block(snapshot, 1, 1, 0, flags))
            return NULL;
        for (UINT32 i = 0; i < 4; ++i) {
            if (!Add_Position(snapshot, fixture->values[i], 1, 1, i + 1))
                return NULL;
        }
    }
    return DSL_tensor_control_snapshot_seal(snapshot, stderr) ?
           snapshot : NULL;
}

static void
Destroy_Analysis (AIO6_ANALYSIS *analysis)
{
    DSL_logical_layout_destroy(analysis->layout);
    DSL_tensor_locality_destroy(analysis->locality);
    DSL_tensor_control_snapshot_destroy(analysis->snapshot);
    DSL_tensor_analysis_destroy(analysis->tensor_analysis);
    DSL_tensor_evolution_destroy(analysis->graph);
    memset(analysis, 0, sizeof(*analysis));
}

static BOOL
Build_Analysis
        (const AIO6_FIXTURE *fixture, AIO6_CONTROL_KIND kind,
         BOOL select_plans, FILE *trace, AIO6_ANALYSIS *analysis)
{
    DSL_LOGICAL_LAYOUT_CONTROL control;
    memset(analysis, 0, sizeof(*analysis));
    analysis->graph = DSL_tensor_evolution_create(fixture->pu, stderr);
    if (analysis->graph == NULL ||
        !DSL_tensor_evolution_build_semantic_roots
             (analysis->graph, stderr))
        return FALSE;
    analysis->tensor_analysis = DSL_tensor_analysis_create
                                    (fixture->pu, analysis->graph, stderr);
    if (analysis->tensor_analysis == NULL ||
        !DSL_tensor_analysis_build(analysis->tensor_analysis, stderr))
        return FALSE;
    analysis->snapshot = Create_Snapshot(fixture, kind);
    analysis->locality = DSL_tensor_locality_create
                             (fixture->pu, analysis->tensor_analysis,
                              analysis->snapshot, stderr);
    if (analysis->snapshot == NULL || analysis->locality == NULL ||
        !DSL_tensor_locality_build(analysis->locality, stderr))
        return FALSE;
    DSL_logical_layout_control_init(&control);
    control.select_plans = select_plans;
    control.focus_value_id =
        DSL_Builder_Get_Value_Image_Id(fixture->values[2]);
    analysis->layout = DSL_logical_layout_create
                           (fixture->pu, analysis->graph,
                            analysis->tensor_analysis, analysis->locality,
                            &control, stderr);
    if (analysis->layout == NULL ||
        !DSL_logical_layout_build(analysis->layout, stderr) ||
        !DSL_logical_layout_verify(analysis->layout, stderr))
        return FALSE;
    if (trace != NULL) {
        DSL_tensor_evolution_print(trace, analysis->graph);
        DSL_logical_layout_print(trace, analysis->layout);
    }
    return TRUE;
}

static BOOL
Check_Main_Contract (const AIO6_ANALYSIS *analysis, BOOL selected)
{
    DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD descriptor;
    DSL_LOGICAL_LAYOUT_AXIS_RECORD axis;
    DSL_LOGICAL_LAYOUT_BLOCK_RECORD block;
    DSL_LOGICAL_LAYOUT_SITE_RECORD site;
    DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD alternative;
    if (DSL_logical_layout_descriptor_count(analysis->layout) != 2 ||
        DSL_logical_layout_axis_count(analysis->layout) != 4 ||
        DSL_logical_layout_block_count(analysis->layout) != 2 ||
        DSL_logical_layout_site_count(analysis->layout) != 1 ||
        DSL_logical_layout_alternative_count(analysis->layout) != 2 ||
        DSL_tensor_evolution_node_count(analysis->graph) != 6 ||
        DSL_tensor_evolution_edge_count(analysis->graph) != 2 ||
        !DSL_logical_layout_get_descriptor
             (analysis->layout, 1, &descriptor) ||
        descriptor.kind != DSL_LOGICAL_LAYOUT_PERMUTED ||
        !DSL_logical_layout_get_axis(analysis->layout, 1, &axis) ||
        axis.source_axis != 1 ||
        !DSL_logical_layout_get_axis(analysis->layout, 2, &axis) ||
        axis.source_axis != 0 ||
        !DSL_logical_layout_get_descriptor
             (analysis->layout, 2, &descriptor) ||
        descriptor.kind != DSL_LOGICAL_LAYOUT_BLOCKED ||
        !DSL_logical_layout_get_block(analysis->layout, 1, &block) ||
        block.axis != 0 || block.factor != 8 ||
        !DSL_logical_layout_get_block(analysis->layout, 2, &block) ||
        block.axis != 1 || block.factor != 8 ||
        !DSL_logical_layout_get_site(analysis->layout, 1, &site) ||
        site.alternative_count != 2 ||
        (selected && site.selected_plan_id != site.baseline_plan_id) ||
        (!selected && site.selected_plan_id != 0))
        return FALSE;
    for (UINT32 id = 1; id <= 2; ++id) {
        if (!DSL_logical_layout_get_alternative
                 (analysis->layout, id, &alternative) ||
            alternative.compatibility_state !=
                DSL_LAYOUT_COMPATIBILITY_PROVEN ||
            alternative.conversion_state != DSL_LAYOUT_CONVERSION_KNOWN ||
            alternative.conversion_bytes != 2048 ||
            alternative.legality != DSL_OPT_LEGALITY_PROVEN ||
            alternative.rejection_reason != DSL_OPT_REJECT_NONE)
            return FALSE;
    }
    return TRUE;
}

static int
Run_Image_Mode (BOOL run_analysis)
{
    const char *artifact = getenv("OPEN64_AIO6_ARTIFACT");
    const char *trace_path = getenv("OPEN64_AIO6_ANALYSIS");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_VERIFY_RESULT verify;
    AIO6_FIXTURE fixture;
    AIO6_ANALYSIS analysis;
    UINT32 nodes;
    UINT32 values;
    UINT32 types;
    char diagnostic[4096];
    FILE *trace = NULL;
    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio6_logical_layout", "[16,16]", &fixture))
        return 1;
    nodes = DSL_IR_Image_Node_Count();
    values = DSL_IR_Image_Value_Count();
    types = TY_Table_Size();
    if (run_analysis) {
        if (trace_path == NULL || trace_path[0] == '\0')
            return 1;
        trace = fopen(trace_path, "w");
        if (trace == NULL ||
            !Build_Analysis(&fixture, AIO6_CONTROL_STRAIGHT, TRUE,
                            trace, &analysis) ||
            !Check_Main_Contract(&analysis, TRUE) ||
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
    if (!DSL_Builder_Verify_Program(&verify)) {
        fprintf(stderr, "AIO-6 verification failed: %s\n", diagnostic);
        return 1;
    }
    request.path = artifact;
    request.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&request))
        return 1;
    printf("AIO-6 %s image passed\n", run_analysis ? "after" : "before");
    return 0;
}

static int
Run_Classification (AIO6_CONTROL_KIND kind, UINT32 expected_state)
{
    AIO6_FIXTURE fixture;
    AIO6_ANALYSIS analysis;
    DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD alternative;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio6_classification", "[16,16]", &fixture) ||
        !Build_Analysis(&fixture, kind, FALSE, NULL, &analysis) ||
        !DSL_logical_layout_get_alternative
             (analysis.layout, 1, &alternative) ||
        alternative.compatibility_state != expected_state)
        return 1;
    Destroy_Analysis(&analysis);
    printf("AIO-6 compatibility contract passed state=%s\n",
           DSL_layout_compatibility_name(expected_state));
    return 0;
}

static int
Run_Unknown_Shape(void)
{
    AIO6_FIXTURE fixture;
    AIO6_ANALYSIS analysis;
    DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD alternative;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture
             ("aio6_unknown_shape", "[16,<pending>]", &fixture) ||
        !Build_Analysis
             (&fixture, AIO6_CONTROL_STRAIGHT, FALSE, NULL, &analysis) ||
        DSL_logical_layout_alternative_count(analysis.layout) != 1 ||
        !DSL_logical_layout_get_alternative
             (analysis.layout, 1, &alternative) ||
        alternative.conversion_state != DSL_LAYOUT_CONVERSION_UNKNOWN ||
        alternative.legality != DSL_OPT_LEGALITY_UNKNOWN)
        return 1;
    Destroy_Analysis(&analysis);
    printf("AIO-6 unknown-shape contract passed\n");
    return 0;
}

static int
Run_Control_And_Scope(void)
{
    AIO6_FIXTURE first;
    AIO6_FIXTURE second;
    AIO6_ANALYSIS analysis;
    DSL_LOGICAL_LAYOUT_CONTROL control;
    DSL_LOGICAL_LAYOUT_ANALYSIS *disabled;
    FILE *quiet = tmpfile();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (quiet == NULL ||
        !Create_Fixture("aio6_scope_first", "[16,16]", &first))
        return 1;
    DSL_logical_layout_control_init(&control);
    control.apply_transformation = 1;
    analysis.graph = DSL_tensor_evolution_create(first.pu, stderr);
    if (analysis.graph == NULL ||
        !DSL_tensor_evolution_build_semantic_roots
             (analysis.graph, stderr))
        return 1;
    analysis.tensor_analysis = DSL_tensor_analysis_create
                                   (first.pu, analysis.graph, stderr);
    if (analysis.tensor_analysis == NULL ||
        !DSL_tensor_analysis_build(analysis.tensor_analysis, stderr))
        return 1;
    analysis.snapshot = Create_Snapshot(&first, AIO6_CONTROL_STRAIGHT);
    analysis.locality = DSL_tensor_locality_create
                            (first.pu, analysis.tensor_analysis,
                             analysis.snapshot, stderr);
    if (analysis.locality == NULL ||
        !DSL_tensor_locality_build(analysis.locality, stderr) ||
        DSL_logical_layout_create
            (first.pu, analysis.graph, analysis.tensor_analysis,
             analysis.locality, &control, quiet) != NULL)
        return 1;
    control.apply_transformation = 0;
    control.generate_candidates = 0;
    disabled = DSL_logical_layout_create
                   (first.pu, analysis.graph, analysis.tensor_analysis,
                    analysis.locality, &control, stderr);
    if (disabled == NULL ||
        !DSL_logical_layout_build(disabled, stderr) ||
        !DSL_logical_layout_verify(disabled, stderr) ||
        DSL_logical_layout_site_count(disabled) != 0 ||
        DSL_logical_layout_alternative_count(disabled) != 0)
        return 1;
    DSL_logical_layout_destroy(disabled);
    control.generate_candidates = 1;
    control.focus_value_id =
        DSL_Builder_Get_Value_Image_Id(first.values[2]);
    analysis.layout = DSL_logical_layout_create
                          (first.pu, analysis.graph,
                           analysis.tensor_analysis, analysis.locality,
                           &control, stderr);
    if (analysis.layout == NULL ||
        !DSL_logical_layout_build(analysis.layout, stderr) ||
        !Create_Fixture("aio6_scope_second", "[16,16]", &second) ||
        DSL_logical_layout_verify(analysis.layout, quiet) ||
        !DSL_Builder_Select_PU(first.pu) ||
        !DSL_logical_layout_verify(analysis.layout, stderr))
        return 1;
    Destroy_Analysis(&analysis);
    fclose(quiet);
    printf("AIO-6 control and per-PU scope contracts passed\n");
    return 0;
}

int
main(void)
{
    const char *mode;
    Initialize_Test_Context();
    mode = getenv("OPEN64_AIO6_MODE");
    if (mode == NULL)
        return 1;
    if (strcmp(mode, "before") == 0)
        return Run_Image_Mode(FALSE);
    if (strcmp(mode, "after") == 0)
        return Run_Image_Mode(TRUE);
    if (strcmp(mode, "candidate") == 0) {
        AIO6_FIXTURE fixture;
        AIO6_ANALYSIS analysis;
        DSL_Builder_Begin_Program();
        DSL_Opcode_Register_Common_Substrate();
        if (!Create_Fixture("aio6_candidate", "[16,16]", &fixture) ||
            !Build_Analysis
                 (&fixture, AIO6_CONTROL_STRAIGHT, FALSE, NULL, &analysis) ||
            !Check_Main_Contract(&analysis, FALSE))
            return 1;
        Destroy_Analysis(&analysis);
        return 0;
    }
    if (strcmp(mode, "effect") == 0)
        return Run_Classification
                   (AIO6_CONTROL_EFFECT, DSL_LAYOUT_COMPATIBILITY_REJECTED);
    if (strcmp(mode, "region") == 0)
        return Run_Classification
                   (AIO6_CONTROL_REGION, DSL_LAYOUT_COMPATIBILITY_UNKNOWN);
    if (strcmp(mode, "unknown") == 0)
        return Run_Unknown_Shape();
    if (strcmp(mode, "control") == 0)
        return Run_Control_And_Scope();
    return 1;
}
