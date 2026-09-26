/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Certifies AIO-4 PU-local lifetime, reuse, alias, control, and locality
 * classification without retaining optimizer-owned CFG objects. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md.
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
    DSL_BUILDER_VALUE values[5];
    UINT32 file_id;
} AIO4_FIXTURE;

typedef enum {
    AIO4_CONTROL_STRAIGHT = 0,
    AIO4_CONTROL_BRANCH = 1,
    AIO4_CONTROL_LOOP = 2,
    AIO4_CONTROL_REGION = 3,
    AIO4_CONTROL_EFFECT = 4
} AIO4_CONTROL_KIND;

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
        (const char *name, const char *shape, AIO4_FIXTURE *fixture)
{
    DSL_BUILDER_PU_SOURCE_IDENTITY identity;
    DSL_DOMAIN_ID common = DSL_Domain_Find("common");
    DSL_OPCODE_ID tensor_const = DSL_Opcode_Find
                                     (common,
                                      DSL_OPCODE_COMMON_TENSOR_CONST, 1);
    DSL_OPCODE_ID add = DSL_Opcode_Find
                            (common, DSL_OPCODE_COMMON_ADD, 1);
    DSL_OPCODE_ID mul = DSL_Opcode_Find
                            (common, DSL_OPCODE_COMMON_MUL, 1);
    DSL_BUILDER_OPERATOR_ATTRIBUTE broadcast;
    TY_IDX ty;
    DSL_BUILDER_VALUE kids[2];

    broadcast.name = "attr.broadcast_rule";
    broadcast.value = "numpy";

    memset(fixture, 0, sizeof(*fixture));
    fixture->pu = DSL_Builder_Create_Minimal_PU(name);
    if (fixture->pu == NULL)
        return FALSE;
    memset(&identity, 0, sizeof(identity));
    identity.canonical_definition_name = name;
    identity.defining_module = "aio4.tensor_locality_contract";
    identity.defining_file = __FILE__;
    identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(fixture->pu, &identity))
        return FALSE;
    fixture->file_id = DSL_Builder_Register_Source_File
                           (fixture->pu, __FILE__);
    ty = Create_Tensor_Type(name, shape);
    fixture->values[0] = DSL_Builder_Create_Tensor_Constant
                             ("aio4_zero", ty, "float32", 2, shape,
                              "splat", "0.0");
    fixture->values[1] = DSL_Builder_Create_Tensor_Constant
                             ("aio4_one", ty, "float32", 2, shape,
                              "splat", "1.0");
    kids[0] = fixture->values[0];
    kids[1] = fixture->values[1];
    fixture->values[2] = DSL_Builder_Create_Operator_With_Result
                             (add, 1, kids, 2, &broadcast, 1,
                              "aio4_reused", ty);
    kids[0] = fixture->values[2];
    kids[1] = fixture->values[1];
    fixture->values[3] = DSL_Builder_Create_Operator_With_Result
                             (add, 1, kids, 2, &broadcast, 1,
                              "aio4_add", ty);
    kids[0] = fixture->values[2];
    kids[1] = fixture->values[3];
    fixture->values[4] = DSL_Builder_Create_Operator_With_Result
                             (mul, 1, kids, 2, &broadcast, 1,
                              "aio4_mul", ty);
    if (tensor_const == DSL_OPCODE_INVALID_ID ||
        add == DSL_OPCODE_INVALID_ID || mul == DSL_OPCODE_INVALID_ID)
        return FALSE;
    for (UINT32 i = 0; i < 5; ++i) {
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

static BOOL
Add_Block
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, UINT32 id, UINT32 rpo,
         UINT32 idom, UINT32 ipdom, UINT32 loop_depth,
         UINT32 region_id, UINT32 flags)
{
    DSL_TENSOR_CONTROL_BLOCK block;
    memset(&block, 0, sizeof(block));
    block.block_id = id;
    block.reverse_postorder = rpo;
    block.immediate_dominator = idom;
    block.immediate_postdominator = ipdom;
    block.loop_depth = loop_depth;
    block.region_id = region_id;
    block.flags = flags;
    return DSL_tensor_control_snapshot_add_block
               (snapshot, &block, stderr);
}

static BOOL
Add_Position
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, DSL_BUILDER_VALUE value,
         UINT32 block_id, UINT32 rpo, UINT32 statement_order)
{
    DSL_TENSOR_CONTROL_POSITION position;
    memset(&position, 0, sizeof(position));
    position.node_id = Value_Node(value);
    position.block_id = block_id;
    position.statement_order = statement_order;
    position.reverse_postorder = rpo;
    return DSL_tensor_control_snapshot_add_position
               (snapshot, &position, stderr);
}

static DSL_TENSOR_CONTROL_SNAPSHOT *
Create_Control_Snapshot
        (const AIO4_FIXTURE *fixture, AIO4_CONTROL_KIND kind)
{
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot =
        DSL_tensor_control_snapshot_create(fixture->pu, stderr);
    if (snapshot == NULL)
        return NULL;
    if (kind == AIO4_CONTROL_STRAIGHT || kind == AIO4_CONTROL_EFFECT) {
        UINT32 flags = kind == AIO4_CONTROL_EFFECT ?
                       DSL_TENSOR_CONTROL_EFFECT_BARRIER : 0;
        if (!Add_Block(snapshot, 1, 1, 0, 0, 0, 0, flags))
            return NULL;
        for (UINT32 i = 0; i < 5; ++i) {
            if (!Add_Position(snapshot, fixture->values[i], 1, 1, i + 1))
                return NULL;
        }
    } else if (kind == AIO4_CONTROL_BRANCH) {
        if (!Add_Block(snapshot, 1, 1, 0, 0, 0, 0,
                       DSL_TENSOR_CONTROL_BRANCH) ||
            !Add_Block(snapshot, 2, 2, 1, 0, 0, 0, 0) ||
            !Add_Block(snapshot, 3, 3, 1, 0, 0, 0, 0))
            return NULL;
        for (UINT32 i = 0; i < 3; ++i) {
            if (!Add_Position(snapshot, fixture->values[i], 1, 1, i + 1))
                return NULL;
        }
        if (!Add_Position(snapshot, fixture->values[3], 2, 2, 1) ||
            !Add_Position(snapshot, fixture->values[4], 3, 3, 1))
            return NULL;
    } else if (kind == AIO4_CONTROL_LOOP) {
        if (!Add_Block(snapshot, 1, 1, 0, 0, 0, 0, 0) ||
            !Add_Block(snapshot, 2, 2, 1, 0, 1, 0,
                       DSL_TENSOR_CONTROL_LOOP))
            return NULL;
        for (UINT32 i = 0; i < 3; ++i) {
            if (!Add_Position(snapshot, fixture->values[i], 1, 1, i + 1))
                return NULL;
        }
        if (!Add_Position(snapshot, fixture->values[3], 2, 2, 1) ||
            !Add_Position(snapshot, fixture->values[4], 2, 2, 2))
            return NULL;
    } else {
        if (!Add_Block(snapshot, 1, 1, 0, 0, 0, 10,
                       DSL_TENSOR_CONTROL_REGION) ||
            !Add_Block(snapshot, 2, 2, 1, 0, 0, 20,
                       DSL_TENSOR_CONTROL_REGION))
            return NULL;
        for (UINT32 i = 0; i < 3; ++i) {
            if (!Add_Position(snapshot, fixture->values[i], 1, 1, i + 1))
                return NULL;
        }
        if (!Add_Position(snapshot, fixture->values[3], 2, 2, 1) ||
            !Add_Position(snapshot, fixture->values[4], 2, 2, 2))
            return NULL;
    }
    if (!DSL_tensor_control_snapshot_seal(snapshot, stderr))
        return NULL;
    return snapshot;
}

static BOOL
Analyze
        (const AIO4_FIXTURE *fixture, AIO4_CONTROL_KIND kind,
         FILE *trace, DSL_TENSOR_LOCALITY_FACT_RECORD *reused_fact)
{
    DSL_TENSOR_EVOLUTION_GRAPH *graph =
        DSL_tensor_evolution_create(fixture->pu, stderr);
    DSL_TENSOR_ANALYSIS *tensor_analysis;
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    if (graph == NULL ||
        !DSL_tensor_evolution_build_semantic_roots(graph, stderr))
        return FALSE;
    tensor_analysis = DSL_tensor_analysis_create
                          (fixture->pu, graph, stderr);
    if (tensor_analysis == NULL ||
        !DSL_tensor_analysis_build(tensor_analysis, stderr))
        return FALSE;
    snapshot = Create_Control_Snapshot(fixture, kind);
    locality = DSL_tensor_locality_create
                   (fixture->pu, tensor_analysis, snapshot, stderr);
    if (snapshot == NULL || locality == NULL ||
        !DSL_tensor_locality_build(locality, stderr) ||
        !DSL_tensor_locality_build(locality, stderr) ||
        !DSL_tensor_locality_verify(locality, stderr) ||
        DSL_tensor_locality_fact_count(locality) != 5 ||
        DSL_tensor_locality_use_count(locality) != 6 ||
        !DSL_tensor_locality_find_fact
             (locality,
              DSL_Builder_Get_Value_Image_Id(fixture->values[2]),
              reused_fact))
        return FALSE;
    if (trace != NULL) {
        DSL_tensor_control_snapshot_print(trace, snapshot);
        DSL_tensor_locality_print(trace, locality);
    }
    DSL_tensor_locality_destroy(locality);
    DSL_tensor_control_snapshot_destroy(snapshot);
    DSL_tensor_analysis_destroy(tensor_analysis);
    DSL_tensor_evolution_destroy(graph);
    return TRUE;
}

static int
Run_Image_Mode (BOOL run_analysis)
{
    const char *artifact = getenv("OPEN64_AIO4_ARTIFACT");
    const char *trace_path = getenv("OPEN64_AIO4_ANALYSIS");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_VERIFY_RESULT verify;
    AIO4_FIXTURE fixture;
    DSL_TENSOR_LOCALITY_FACT_RECORD reused;
    FILE *trace = NULL;
    UINT32 nodes;
    UINT32 values;
    UINT32 types;
    char diagnostic[4096];
    if (artifact == NULL || artifact[0] == '\0')
        return 1;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio4_lifetime_locality", "[2,4]", &fixture))
        return 1;
    nodes = DSL_IR_Image_Node_Count();
    values = DSL_IR_Image_Value_Count();
    types = TY_Table_Size();
    if (run_analysis) {
        if (trace_path == NULL || trace_path[0] == '\0')
            return 1;
        trace = fopen(trace_path, "w");
        if (trace == NULL ||
            !Analyze(&fixture, AIO4_CONTROL_STRAIGHT, trace, &reused))
            return 1;
        fclose(trace);
        if (reused.size_state != DSL_TENSOR_SIZE_STATIC ||
            reused.object_bytes != 32 ||
            reused.lifetime_state != DSL_TENSOR_LIFETIME_EXACT_BLOCK ||
            reused.reuse_distance_state != DSL_TENSOR_DISTANCE_EXACT ||
            reused.reuse_distance_statements != 1 ||
            reused.access_pattern != DSL_TENSOR_ACCESS_ELEMENTWISE ||
            reused.residency_benefit != DSL_TENSOR_RESIDENCY_HIGH ||
            reused.critical_path_state != DSL_TENSOR_CRITICAL_PATH_ON ||
            reused.alias_state != DSL_TENSOR_ALIAS_PROVEN_UNIQUE ||
            reused.estimated_read_bytes != 64 ||
            DSL_IR_Image_Node_Count() != nodes ||
            DSL_IR_Image_Value_Count() != values ||
            TY_Table_Size() != types)
            return 1;
    }
    memset(&verify, 0, sizeof(verify));
    memset(diagnostic, 0, sizeof(diagnostic));
    verify.diagnostic = diagnostic;
    verify.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify)) {
        fprintf(stderr, "AIO-4 verification failed: %s\n", diagnostic);
        return 1;
    }
    request.path = artifact;
    request.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&request))
        return 1;
    printf("AIO-4 %s image passed\n", run_analysis ? "after" : "before");
    return 0;
}

static int
Run_Control_Contract (AIO4_CONTROL_KIND kind)
{
    static const UINT32 expected[] = {
        DSL_TENSOR_LIFETIME_BRANCH,
        DSL_TENSOR_LIFETIME_LOOP,
        DSL_TENSOR_LIFETIME_REGION,
        DSL_TENSOR_LIFETIME_EFFECT
    };
    AIO4_FIXTURE fixture;
    DSL_TENSOR_LOCALITY_FACT_RECORD reused;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (kind < AIO4_CONTROL_BRANCH || kind > AIO4_CONTROL_EFFECT ||
        !Create_Fixture("aio4_control_contract", "[2,4]", &fixture) ||
        !Analyze(&fixture, kind, NULL, &reused))
        return 1;
    if (reused.lifetime_state != expected[kind - 1] ||
        reused.reuse_distance_state !=
            DSL_TENSOR_DISTANCE_CONSERVATIVE ||
        reused.critical_path_state !=
            DSL_TENSOR_CRITICAL_PATH_UNKNOWN) {
        fprintf(stderr,
                "AIO-4 control mismatch kind=%u lifetime=%s "
                "distance=%s critical=%s\n",
                kind,
                DSL_tensor_lifetime_state_name(reused.lifetime_state),
                DSL_tensor_distance_state_name
                    (reused.reuse_distance_state),
                DSL_tensor_critical_path_state_name
                    (reused.critical_path_state));
        return 1;
    }
    printf("AIO-4 control contract passed kind=%u lifetime=%s\n",
           kind, DSL_tensor_lifetime_state_name(reused.lifetime_state));
    return 0;
}

static int
Run_Unknown_And_PU_Scope(void)
{
    AIO4_FIXTURE pending;
    AIO4_FIXTURE other;
    DSL_TENSOR_LOCALITY_FACT_RECORD reused;
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_TENSOR_ANALYSIS *tensor_analysis;
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    FILE *quiet = tmpfile();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (quiet == NULL ||
        !Create_Fixture("aio4_pending", "[2,<pending>]", &pending) ||
        !Analyze(&pending, AIO4_CONTROL_STRAIGHT, NULL, &reused) ||
        reused.size_state != DSL_TENSOR_SIZE_UNKNOWN ||
        reused.object_bytes != DSL_TENSOR_LOCALITY_UNKNOWN_U64)
        return 1;

    graph = DSL_tensor_evolution_create(pending.pu, stderr);
    if (graph == NULL ||
        !DSL_tensor_evolution_build_semantic_roots(graph, stderr))
        return 1;
    tensor_analysis = DSL_tensor_analysis_create(pending.pu, graph, stderr);
    if (tensor_analysis == NULL ||
        !DSL_tensor_analysis_build(tensor_analysis, quiet))
        return 1;
    snapshot = Create_Control_Snapshot(&pending, AIO4_CONTROL_STRAIGHT);
    locality = DSL_tensor_locality_create
                   (pending.pu, tensor_analysis, snapshot, stderr);
    if (locality == NULL ||
        !DSL_tensor_locality_build(locality, quiet) ||
        !Create_Fixture("aio4_other_pu", "[2,4]", &other) ||
        DSL_tensor_locality_verify(locality, quiet) ||
        !DSL_Builder_Select_PU(pending.pu) ||
        !DSL_tensor_locality_verify(locality, quiet))
        return 1;
    DSL_tensor_locality_destroy(locality);
    DSL_tensor_control_snapshot_destroy(snapshot);
    DSL_tensor_analysis_destroy(tensor_analysis);
    DSL_tensor_evolution_destroy(graph);
    fclose(quiet);
    printf("AIO-4 unknown-shape and per-PU scope contracts passed\n");
    return 0;
}

static int
Run_Alias_Contract(void)
{
    AIO4_FIXTURE fixture;
    DSL_TENSOR_LOCALITY_FACT_RECORD reused;
    ST_IDX st;
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    if (!Create_Fixture("aio4_alias_contract", "[2,4]", &fixture))
        return 1;
    st = DSL_Builder_Get_Value_Result_Symbol(fixture.values[2]);
    ST_tensor_bind_attribute
        (st, TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS), "false");
    if (!Analyze(&fixture, AIO4_CONTROL_STRAIGHT, NULL, &reused) ||
        reused.alias_state != DSL_TENSOR_ALIAS_UNKNOWN ||
        reused.lifetime_state != DSL_TENSOR_LIFETIME_ALIAS ||
        reused.critical_path_state != DSL_TENSOR_CRITICAL_PATH_UNKNOWN ||
        reused.residency_benefit != DSL_TENSOR_RESIDENCY_UNKNOWN)
        return 1;
    printf("AIO-4 unresolved-alias contract passed\n");
    return 0;
}

int
main(void)
{
    const char *mode;
    Initialize_Test_Context();
    mode = getenv("OPEN64_AIO4_MODE");
    if (mode == NULL)
        return 1;
    if (strcmp(mode, "before") == 0)
        return Run_Image_Mode(FALSE);
    if (strcmp(mode, "after") == 0)
        return Run_Image_Mode(TRUE);
    if (strcmp(mode, "branch") == 0)
        return Run_Control_Contract(AIO4_CONTROL_BRANCH);
    if (strcmp(mode, "loop") == 0)
        return Run_Control_Contract(AIO4_CONTROL_LOOP);
    if (strcmp(mode, "region") == 0)
        return Run_Control_Contract(AIO4_CONTROL_REGION);
    if (strcmp(mode, "effect") == 0)
        return Run_Control_Contract(AIO4_CONTROL_EFFECT);
    if (strcmp(mode, "scope") == 0)
        return Run_Unknown_And_PU_Scope();
    if (strcmp(mode, "alias") == 0)
        return Run_Alias_Contract();
    return 1;
}
