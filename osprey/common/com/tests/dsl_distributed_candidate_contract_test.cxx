/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Certifies AIO-7 placement, sharding, ownership, communication derivation,
 * and conservative PU-local rejection behavior. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO7-DISTRIBUTED.md.
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
#include "dsl_distributed_candidate.h"
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
} AIO7_FIXTURE;

typedef enum {
    AIO7_CONTROL_STRAIGHT = 0,
    AIO7_CONTROL_REGION = 1,
    AIO7_CONTROL_EFFECT = 2
} AIO7_CONTROL_KIND;

typedef struct {
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_TENSOR_ANALYSIS *tensor;
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    DSL_DISTRIBUTED_ANALYSIS *distributed;
} AIO7_ANALYSIS;

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
Create_Fixture (const char *name, const char *shape, AIO7_FIXTURE *fixture)
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
    identity.defining_module = "aio7.distributed_candidate_contract";
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
                             ("aio7_kid0", tensor_ty, "float32", 2, shape,
                              "splat", "1.0");
    fixture->values[1] = DSL_Builder_Create_Tensor_Constant
                             ("aio7_kid1", tensor_ty, "float32", 2, shape,
                              "splat", "1.0");
    attributes[0].name = "attr.transpose_kid0";
    attributes[0].value = "false";
    attributes[1].name = "attr.transpose_kid1";
    attributes[1].value = "false";
    kids[0] = fixture->values[0];
    kids[1] = fixture->values[1];
    fixture->values[2] = DSL_Builder_Create_Operator_With_Result
                             (matmul, 1, kids, 2, attributes, 2,
                              "aio7_matmul", tensor_ty);
    kids[0] = fixture->values[2];
    fixture->values[3] = DSL_Builder_Create_Operator_With_Result
                             (relu, 2, kids, 1, NULL, 0,
                              "aio7_relu", tensor_ty);
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
    return DSL_tensor_control_snapshot_add_block(snapshot, &block, stderr);
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
    return DSL_tensor_control_snapshot_add_position
               (snapshot, &position, stderr);
}

static DSL_TENSOR_CONTROL_SNAPSHOT *
Create_Snapshot (const AIO7_FIXTURE *fixture, AIO7_CONTROL_KIND kind)
{
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot =
        DSL_tensor_control_snapshot_create(fixture->pu, stderr);
    if (snapshot == NULL)
        return NULL;
    if (kind == AIO7_CONTROL_REGION) {
        if (!Add_Block(snapshot, 1, 1, 10, DSL_TENSOR_CONTROL_REGION) ||
            !Add_Block(snapshot, 2, 2, 20, DSL_TENSOR_CONTROL_REGION) ||
            !Add_Position(snapshot, fixture->values[0], 1, 1, 1) ||
            !Add_Position(snapshot, fixture->values[1], 1, 1, 2) ||
            !Add_Position(snapshot, fixture->values[2], 1, 1, 3) ||
            !Add_Position(snapshot, fixture->values[3], 2, 2, 1))
            return NULL;
    } else {
        UINT32 flags = kind == AIO7_CONTROL_EFFECT ?
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
Destroy_Analysis (AIO7_ANALYSIS *analysis)
{
    DSL_distributed_destroy(analysis->distributed);
    DSL_tensor_locality_destroy(analysis->locality);
    DSL_tensor_control_snapshot_destroy(analysis->snapshot);
    DSL_tensor_analysis_destroy(analysis->tensor);
    DSL_tensor_evolution_destroy(analysis->graph);
    memset(analysis, 0, sizeof(*analysis));
}

static BOOL
Build_Analysis (const AIO7_FIXTURE *fixture, AIO7_CONTROL_KIND kind,
                BOOL derive_communication, BOOL select_plans,
                FILE *trace, AIO7_ANALYSIS *analysis)
{
    DSL_DISTRIBUTED_CONTROL control;
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
    analysis->snapshot = Create_Snapshot(fixture, kind);
    analysis->locality = DSL_tensor_locality_create
                             (fixture->pu, analysis->tensor,
                              analysis->snapshot, stderr);
    if (analysis->snapshot == NULL || analysis->locality == NULL ||
        !DSL_tensor_locality_build(analysis->locality, stderr))
        return FALSE;
    DSL_distributed_control_init(&control);
    control.derive_communication = derive_communication;
    control.select_plans = select_plans;
    control.focus_value_id =
        DSL_Builder_Get_Value_Image_Id(fixture->values[2]);
    analysis->distributed = DSL_distributed_create
                                (fixture->pu, analysis->graph,
                                 analysis->tensor, analysis->locality,
                                 &control, stderr);
    if (analysis->distributed == NULL ||
        !DSL_distributed_build(analysis->distributed, stderr) ||
        !DSL_distributed_verify(analysis->distributed, stderr))
        return FALSE;
    if (trace != NULL) {
        DSL_tensor_evolution_print(trace, analysis->graph);
        DSL_distributed_print(trace, analysis->distributed);
    }
    return TRUE;
}

static BOOL
Check_Main_Contract (const AIO7_ANALYSIS *analysis, BOOL selected)
{
    static const UINT32 placement[] = {
        DSL_PLACEMENT_REPLICATED, DSL_PLACEMENT_PARTITIONED,
        DSL_PLACEMENT_PARTITIONED, DSL_PLACEMENT_REMOTE_SINGLE
    };
    static const UINT32 sharding[] = {
        DSL_SHARDING_REPLICATED, DSL_SHARDING_AXIS,
        DSL_SHARDING_PARTIAL_REDUCTION, DSL_SHARDING_MIGRATED
    };
    static const UINT32 ownership[] = {
        DSL_DISTRIBUTED_OWNERSHIP_REPLICATED,
        DSL_DISTRIBUTED_OWNERSHIP_DISJOINT,
        DSL_DISTRIBUTED_OWNERSHIP_REDUCED,
        DSL_DISTRIBUTED_OWNERSHIP_MIGRATED
    };
    static const UINT32 communication[] = {
        DSL_COMMUNICATION_ALL_GATHER, DSL_COMMUNICATION_SCATTER,
        DSL_COMMUNICATION_ALL_REDUCE, DSL_COMMUNICATION_PEER_COPY
    };
    static const UINT64 bytes[] = { 256, 256, 512, 256 };
    DSL_DISTRIBUTED_SITE_RECORD site;
    if (DSL_distributed_site_count(analysis->distributed) != 1 ||
        DSL_distributed_alternative_count(analysis->distributed) != 4 ||
        DSL_distributed_descriptor_count(analysis->distributed) != 4 ||
        DSL_distributed_alias_count(analysis->distributed) != 4 ||
        DSL_distributed_range_count(analysis->distributed) != 7 ||
        DSL_communication_epoch_count(analysis->distributed) != 1 ||
        DSL_communication_intent_count(analysis->distributed) != 4 ||
        DSL_tensor_evolution_node_count(analysis->graph) != 8 ||
        DSL_tensor_evolution_edge_count(analysis->graph) != 4 ||
        !DSL_distributed_get_site(analysis->distributed, 1, &site) ||
        site.alternative_count != 4 ||
        (selected && site.selected_plan_id != 3) ||
        (!selected && site.selected_plan_id != 0))
        return FALSE;
    for (UINT32 id = 1; id <= 4; ++id) {
        DSL_DISTRIBUTED_ALTERNATIVE_RECORD alternative;
        DSL_DISTRIBUTED_DESCRIPTOR_RECORD descriptor;
        DSL_DISTRIBUTED_ALIAS_RECORD alias;
        DSL_COMMUNICATION_INTENT_RECORD intent;
        if (!DSL_distributed_get_alternative
                 (analysis->distributed, id, &alternative) ||
            !DSL_distributed_get_descriptor
                 (analysis->distributed, alternative.descriptor_id,
                  &descriptor) ||
            !DSL_distributed_get_alias
                 (analysis->distributed, descriptor.alias_id, &alias) ||
            !DSL_communication_get_intent
                 (analysis->distributed,
                  alternative.communication_intent_id, &intent) ||
            descriptor.placement_kind != placement[id - 1] ||
            descriptor.sharding_kind != sharding[id - 1] ||
            descriptor.ownership != ownership[id - 1] ||
            alternative.legality != DSL_OPT_LEGALITY_PROVEN ||
            alternative.communication_bytes != bytes[id - 1] ||
            intent.kind != communication[id - 1] ||
            intent.bytes != bytes[id - 1])
            return FALSE;
    }
    return TRUE;
}

static int
Run_Image_Mode (BOOL run_analysis)
{
    const char *artifact = getenv("OPEN64_AIO7_ARTIFACT");
    const char *trace_path = getenv("OPEN64_AIO7_ANALYSIS");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_VERIFY_RESULT verify;
    AIO7_FIXTURE fixture;
    AIO7_ANALYSIS analysis;
    UINT32 nodes;
    UINT32 values;
    UINT32 types;
    char diagnostic[4096];
    FILE *trace = NULL;
    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio7_distributed", "[8,8]", &fixture))
        return 1;
    nodes = DSL_IR_Image_Node_Count();
    values = DSL_IR_Image_Value_Count();
    types = TY_Table_Size();
    if (run_analysis) {
        if (trace_path == NULL || trace_path[0] == '\0')
            return 1;
        trace = fopen(trace_path, "w");
        if (trace == NULL ||
            !Build_Analysis(&fixture, AIO7_CONTROL_STRAIGHT, TRUE, TRUE,
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
    if (!DSL_Builder_Verify_Program(&verify))
        return 1;
    request.path = artifact;
    request.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&request))
        return 1;
    printf("AIO-7 %s image passed\n", run_analysis ? "after" : "before");
    return 0;
}

static int
Run_Classification (AIO7_CONTROL_KIND kind, UINT32 expected_legality)
{
    AIO7_FIXTURE fixture;
    AIO7_ANALYSIS analysis;
    DSL_DISTRIBUTED_ALTERNATIVE_RECORD alternative;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio7_classification", "[8,8]", &fixture) ||
        !Build_Analysis(&fixture, kind, TRUE, FALSE, NULL, &analysis) ||
        !DSL_distributed_get_alternative
             (analysis.distributed, 2, &alternative) ||
        alternative.legality != expected_legality)
        return 1;
    Destroy_Analysis(&analysis);
    return 0;
}

static int
Run_Shape_Case (const char *shape, UINT32 expected_legality)
{
    AIO7_FIXTURE fixture;
    AIO7_ANALYSIS analysis;
    DSL_DISTRIBUTED_ALTERNATIVE_RECORD alternative;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio7_shape", shape, &fixture) ||
        !Build_Analysis
             (&fixture, AIO7_CONTROL_STRAIGHT, TRUE, FALSE, NULL,
              &analysis) ||
        !DSL_distributed_get_alternative
             (analysis.distributed, 2, &alternative) ||
        alternative.legality != expected_legality)
        return 1;
    Destroy_Analysis(&analysis);
    return 0;
}

static int
Run_Communication_Disabled(void)
{
    AIO7_FIXTURE fixture;
    AIO7_ANALYSIS analysis;
    DSL_DISTRIBUTED_ALTERNATIVE_RECORD alternative;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio7_no_communication", "[8,8]", &fixture) ||
        !Build_Analysis
             (&fixture, AIO7_CONTROL_STRAIGHT, FALSE, FALSE, NULL,
              &analysis) ||
        DSL_communication_intent_count(analysis.distributed) != 0 ||
        !DSL_distributed_get_alternative
             (analysis.distributed, 2, &alternative) ||
        alternative.communication_intent_id != 0 ||
        alternative.legality != DSL_OPT_LEGALITY_UNKNOWN)
        return 1;
    Destroy_Analysis(&analysis);
    return 0;
}

static int
Run_Control_And_Scope(void)
{
    AIO7_FIXTURE first;
    AIO7_FIXTURE second;
    AIO7_ANALYSIS analysis;
    DSL_DISTRIBUTED_CONTROL control;
    DSL_DISTRIBUTED_ANALYSIS *disabled;
    FILE *quiet = tmpfile();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (quiet == NULL ||
        !Create_Fixture("aio7_scope_first", "[8,8]", &first))
        return 1;
    memset(&analysis, 0, sizeof(analysis));
    analysis.graph = DSL_tensor_evolution_create(first.pu, stderr);
    if (analysis.graph == NULL ||
        !DSL_tensor_evolution_build_semantic_roots(analysis.graph, stderr))
        return 1;
    analysis.tensor = DSL_tensor_analysis_create
                          (first.pu, analysis.graph, stderr);
    if (analysis.tensor == NULL ||
        !DSL_tensor_analysis_build(analysis.tensor, stderr))
        return 1;
    analysis.snapshot = Create_Snapshot(&first, AIO7_CONTROL_STRAIGHT);
    analysis.locality = DSL_tensor_locality_create
                            (first.pu, analysis.tensor,
                             analysis.snapshot, stderr);
    if (analysis.locality == NULL ||
        !DSL_tensor_locality_build(analysis.locality, stderr))
        return 1;
    DSL_distributed_control_init(&control);
    control.apply_transformation = 1;
    if (DSL_distributed_create
            (first.pu, analysis.graph, analysis.tensor, analysis.locality,
             &control, quiet) != NULL)
        return 1;
    control.apply_transformation = 0;
    control.generate_candidates = 0;
    disabled = DSL_distributed_create
                   (first.pu, analysis.graph, analysis.tensor,
                    analysis.locality, &control, stderr);
    if (disabled == NULL || !DSL_distributed_build(disabled, stderr) ||
        !DSL_distributed_verify(disabled, stderr) ||
        DSL_distributed_site_count(disabled) != 0)
        return 1;
    DSL_distributed_destroy(disabled);
    control.generate_candidates = 1;
    control.focus_value_id = DSL_Builder_Get_Value_Image_Id(first.values[2]);
    analysis.distributed = DSL_distributed_create
                               (first.pu, analysis.graph, analysis.tensor,
                                analysis.locality, &control, stderr);
    if (analysis.distributed == NULL ||
        !DSL_distributed_build(analysis.distributed, stderr) ||
        !Create_Fixture("aio7_scope_second", "[8,8]", &second) ||
        DSL_distributed_verify(analysis.distributed, quiet) ||
        !DSL_Builder_Select_PU(first.pu) ||
        !DSL_distributed_verify(analysis.distributed, stderr))
        return 1;
    Destroy_Analysis(&analysis);
    fclose(quiet);
    return 0;
}

int
main(void)
{
    const char *mode;
    Initialize_Test_Context();
    mode = getenv("OPEN64_AIO7_MODE");
    if (mode == NULL)
        return 1;
    if (strcmp(mode, "before") == 0)
        return Run_Image_Mode(FALSE);
    if (strcmp(mode, "after") == 0)
        return Run_Image_Mode(TRUE);
    if (strcmp(mode, "candidate") == 0) {
        AIO7_FIXTURE fixture;
        AIO7_ANALYSIS analysis;
        DSL_Builder_Begin_Program();
        DSL_Opcode_Register_Common_Substrate();
        if (!Create_Fixture("aio7_candidate", "[8,8]", &fixture) ||
            !Build_Analysis
                 (&fixture, AIO7_CONTROL_STRAIGHT, TRUE, FALSE, NULL,
                  &analysis) ||
            !Check_Main_Contract(&analysis, FALSE))
            return 1;
        Destroy_Analysis(&analysis);
        return 0;
    }
    if (strcmp(mode, "effect") == 0)
        return Run_Classification
                   (AIO7_CONTROL_EFFECT, DSL_OPT_LEGALITY_REJECTED);
    if (strcmp(mode, "region") == 0)
        return Run_Classification
                   (AIO7_CONTROL_REGION, DSL_OPT_LEGALITY_UNKNOWN);
    if (strcmp(mode, "unknown") == 0)
        return Run_Shape_Case
                   ("[8,<pending>]", DSL_OPT_LEGALITY_UNKNOWN);
    if (strcmp(mode, "nondivisible") == 0)
        return Run_Shape_Case("[7,8]", DSL_OPT_LEGALITY_REJECTED);
    if (strcmp(mode, "no_communication") == 0)
        return Run_Communication_Disabled();
    if (strcmp(mode, "control") == 0)
        return Run_Control_And_Scope();
    return 1;
}
