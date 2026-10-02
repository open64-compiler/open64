/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: certify and publish complete SYNC-5 ResNet-20 runtime lowering.
 * The FHE policy freezes the approved provider and static operation schedule;
 * the generic DSL transaction owns physical WHIRL and image mutation.
 *
 * Design references:
 *   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-F and S5-G
 *   doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 */

#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <set>
#include <string>
#include <vector>

#include "dsl_ir_image.h"
#include "dsl_region.h"
#include "fhe_plan.h"
#include "fhe_runtime_lower.h"
#include "fhe_runtime_operation_plan.h"
#include "fhe_runtime_production.h"
#include "fhe_semantic_materialize.h"
#include "fhe_semantic_runtime_lower.h"
#include "fhe_standard_whirl.h"
#include "fhe_unlowered_gate.h"
#include "pu_info.h"
#include "symtab.h"
#include "wn.h"

typedef struct {
    BOOL initialized;
    std::string provider_path;
    std::string provider_sha256;
    std::string checkpoint_path;
    std::vector<VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD> schedule;
    std::set<ST_IDX> processed_owners;
    UINT32 computed_count;
    UINT32 promoted_count;
    UINT32 dead_count;
    UINT32 selector_count;
    UINT32 evaluation_count;
} VHO_FHE_RUNTIME_PRODUCTION_STATE;

typedef struct {
    WN *call;
    ST_IDX result_st;
    SRCPOS source_position;
} VHO_FHE_RUNTIME_CALL_RESULT_GUARD;

static VHO_FHE_RUNTIME_PRODUCTION_STATE VHO_FHE_runtime_production;

/* Emit one stable production diagnostic without changing the active PU. */
static BOOL
VHO_FHE_Runtime_Production_Report
        (FILE *diagnostic, const char *code, const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "%s: %s\n", code, message);
    return FALSE;
}

/* Compare every semantic schedule field; structure padding is not identity. */
static BOOL
VHO_FHE_Runtime_Production_Same_Schedule
        (const VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD &left,
         const VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD &right)
{
    return left.source_node_id == right.source_node_id &&
           left.result_value_id == right.result_value_id &&
           left.owner_pu_st == right.owner_pu_st &&
           left.logical_operator == right.logical_operator &&
           left.first_static_ordinal == right.first_static_ordinal &&
           left.static_evaluation_count == right.static_evaluation_count &&
           left.execution_multiplicity == right.execution_multiplicity &&
           left.dynamic_evaluation_count == right.dynamic_evaluation_count;
}

/* Reject malformed function prototypes before the backend symbol verifier. */
static BOOL
VHO_FHE_Runtime_Production_Prototypes_Valid (FILE *diagnostic)
{
    for (UINT32 i = 1; i < TY_Table_Size(); ++i) {
        if (TY_kind(Ty_tab[i]) != KIND_FUNCTION)
            continue;
        TYLIST_IDX first = TY_tylist(Ty_tab[i]);
        TYLIST_IDX position = first;
        if (first == 0 || first >= TYLIST_Table_Size()) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "CFHELOWER-PROTOTYPE-001: ty=%u invalid_tylist=%u\n",
                        i, first);
            return FALSE;
        }
        while (position > 0 && position < TYLIST_Table_Size()) {
            TY_IDX entry = TYLIST_type(Tylist_Table[position]);
            if (entry == TY_IDX_ZERO && position != first)
                break;
            if (TY_IDX_index(entry) == 0 ||
                TY_IDX_index(entry) >= TY_Table_Size()) {
                if (diagnostic != NULL)
                    fprintf(diagnostic,
                            "CFHELOWER-PROTOTYPE-001: ty=%u tylist=%u "
                            "entry=%u invalid_ty=%u\n", i, first, position,
                            (UINT32)entry);
                return FALSE;
            }
            ++position;
        }
    }
    return TRUE;
}

/* Authenticate and freeze the input policy before the first PU mutates. */
static BOOL
VHO_FHE_Runtime_Production_Prepare
        (const VHO_FHE_RUNTIME_LOWER_OPTIONS *options, FILE *diagnostic)
{
    if (options == NULL || options->provider_manifest_path == NULL ||
        options->provider_manifest_sha256 == NULL ||
        options->checkpoint_output_path == NULL ||
        options->checkpoint_output_path[0] == '\0')
        return VHO_FHE_Runtime_Production_Report
                   (diagnostic, "CFHELOWER-POLICY-001",
                    "provider or checkpoint options are missing");
    if (VHO_FHE_runtime_production.initialized) {
        if (VHO_FHE_runtime_production.provider_path !=
                options->provider_manifest_path ||
            VHO_FHE_runtime_production.provider_sha256 !=
                options->provider_manifest_sha256 ||
            VHO_FHE_runtime_production.checkpoint_path !=
                options->checkpoint_output_path ||
            VHO_FHE_runtime_production.schedule.size() !=
                VHO_FHE_Runtime_Static_Schedule_Record_Count())
            return VHO_FHE_Runtime_Production_Report
                       (diagnostic, "CFHELOWER-POLICY-002",
                        "runtime policy changed across PUs");
        for (UINT32 i = 0; i < VHO_FHE_runtime_production.schedule.size();
             ++i) {
            VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD current;
            if (!VHO_FHE_Runtime_Static_Schedule_Get(i, &current) ||
                !VHO_FHE_Runtime_Production_Same_Schedule
                    (VHO_FHE_runtime_production.schedule[i], current))
                return VHO_FHE_Runtime_Production_Report
                           (diagnostic, "CFHELOWER-POLICY-002",
                            "runtime schedule changed across PUs");
        }
        return TRUE;
    }
    if (!VHO_FHE_Authenticate_Approved_Provider_Manifest
             (options->provider_manifest_path,
              options->provider_manifest_sha256, diagnostic) ||
        !VHO_FHE_Runtime_Static_Schedule_Prepare(diagnostic) ||
        VHO_FHE_Runtime_Static_Schedule_Record_Count() != 32 ||
        VHO_FHE_Runtime_Static_Evaluation_Count() != 87 ||
        VHO_FHE_Runtime_Dynamic_Evaluation_Count() != 147)
        return VHO_FHE_Runtime_Production_Report
                   (diagnostic, "CFHELOWER-POLICY-003",
                    "approved provider or 32/87/147 schedule is invalid");
    VHO_FHE_RUNTIME_INTERFACE_CENSUS census;
    if (!VHO_FHE_Runtime_Interface_Census_Prepare
             (diagnostic, &census) || census.pu_count != 6 ||
        census.callsite_count != 9 ||
        census.retired_formal_count != 48 ||
        census.retired_call_argument_count != 80 ||
        census.source_external_input_count != 44 ||
        census.runtime_resource_input_count != 4 ||
        census.root_source_binding_count != 44 ||
        census.threaded_source_binding_count != 24 ||
        census.resource_binding_count != 24 ||
        census.runtime_input_call_count != 76)
        return VHO_FHE_Runtime_Production_Report
                   (diagnostic, "CFHELOWER-POLICY-004",
                    "six-PU runtime interface census is invalid");
    VHO_FHE_runtime_production.provider_path =
        options->provider_manifest_path;
    VHO_FHE_runtime_production.provider_sha256 =
        options->provider_manifest_sha256;
    VHO_FHE_runtime_production.checkpoint_path =
        options->checkpoint_output_path;
    for (UINT32 i = 0;
         i < VHO_FHE_Runtime_Static_Schedule_Record_Count(); ++i) {
        VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD record;
        if (!VHO_FHE_Runtime_Static_Schedule_Get(i, &record))
            return FALSE;
        VHO_FHE_runtime_production.schedule.push_back(record);
    }
    VHO_FHE_runtime_production.initialized = TRUE;
    return TRUE;
}

/* Admit one PU before lowering and verify it again after its transaction. */
static BOOL
VHO_FHE_Runtime_Production_Gatekeeper
        (PU_Info *pu_info, WN *tree,
         const VHO_FHE_RUNTIME_LOWER_OPTIONS *options,
         FILE *diagnostic)
{
    if (pu_info == NULL || pu_info != Current_PU_Info ||
        tree == NULL || tree != PU_Info_tree_ptr(pu_info) ||
        !VHO_FHE_Runtime_Production_Prepare(options, diagnostic))
        return FALSE;
    if (VHO_FHE_runtime_production.processed_owners.find
            (PU_Info_proc_sym(pu_info)) !=
        VHO_FHE_runtime_production.processed_owners.end())
        return DSL_IR_Image_Validate_Lowered_Relations(diagnostic) &&
               DSL_Region_Verify_PU(pu_info, diagnostic);
    return DSL_Program_Interface_Validate_PU(pu_info, diagnostic) &&
           DSL_Region_Verify_PU(pu_info, diagnostic);
}

/* Return from the current PU on a nonzero runtime status. */
static WN *
VHO_FHE_Runtime_Production_Failure
        (ST_IDX status_st, ST_IDX output_st,
         void *context, FILE *diagnostic)
{
    (void)context;
    (void)diagnostic;
    WN *block = WN_CreateBlock();
    TYPE_ID status_mtype = TY_mtype(ST_type(status_st));
    WN_INSERT_BlockLast
        (block, WN_CreateEval
                    (WN_CreateLdid(OPR_LDID, status_mtype, status_mtype, 0,
                                   status_st, ST_type(status_st))));
    if (output_st != ST_IDX_ZERO) {
        TYPE_ID mtype = TY_mtype(ST_type(output_st));
        WN_INSERT_BlockLast
            (block, WN_CreateEval
                        (WN_CreateLdid(OPR_LDID, mtype, mtype, 0,
                                       output_st, ST_type(output_st))));
    }
    WN_INSERT_BlockLast(block, WN_CreateReturn());
    return block;
}

/* Preflight the v1 void/hidden-result ABI before adding any caller guards. */
static BOOL
VHO_FHE_Runtime_Production_Call_Guards
        (PU_Info *pu_info, std::vector<VHO_FHE_RUNTIME_CALL_RESULT_GUARD> *guards,
         FILE *diagnostic)
{
    WN *entry = pu_info == NULL ? NULL : PU_Info_tree_ptr(pu_info);
    if (guards == NULL || pu_info != Current_PU_Info)
        return VHO_FHE_Runtime_Production_Report
                   (diagnostic, "CFHELOWER-CALL-000",
                    "runtime PU has no valid function entry");
    if (!VHO_FHE_Standard_Function_Body_Valid(entry, diagnostic))
        return FALSE;
    WN *body = WN_func_body(entry);
    for (WN *call = WN_first(body); call != NULL; call = WN_next(call)) {
        if (WN_opcode(call) != OPC_VCALL)
            continue;
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Find_Callsite(call, &callsite) ||
            callsite.owner_pu_st != PU_Info_proc_sym(pu_info) ||
            callsite.callee_pu_st != WN_st_idx(call))
            return VHO_FHE_Runtime_Production_Report
                       (diagnostic, "CFHELOWER-CALL-001",
                        "runtime callsite identity is invalid");
        ST_IDX result_st = ST_IDX_ZERO;
        for (INT32 i = 0; i < WN_kid_count(call); ++i) {
            WN *parm = WN_kid(call, i);
            if (parm == NULL || WN_operator(parm) != OPR_PARM ||
                !WN_Parm_Out(parm))
                continue;
            WN *address = WN_kid_count(parm) == 1 ? WN_kid0(parm) : NULL;
            if (result_st != ST_IDX_ZERO ||
                !WN_Parm_By_Reference(parm) ||
                !WN_Parm_Passed_Not_Saved(parm) ||
                address == NULL || WN_operator(address) != OPR_LDA)
                return VHO_FHE_Runtime_Production_Report
                           (diagnostic, "CFHELOWER-CALL-002",
                            "runtime call has no unique result slot");
            result_st = WN_st_idx(address);
        }
        WN *initialization = WN_prev(call);
        if (!VHO_FHE_Standard_Null_Initializer_Valid
                 (initialization, result_st, diagnostic))
            return FALSE;
        if (TY_mtype(ST_type(result_st)) != Pointer_Mtype ||
            WN_Get_Linenum(call) == 0)
            return VHO_FHE_Runtime_Production_Report
                       (diagnostic, "CFHELOWER-CALL-003",
                        "runtime result slot is not freshly null");
        VHO_FHE_RUNTIME_CALL_RESULT_GUARD guard;
        guard.call = call;
        guard.result_st = result_st;
        guard.source_position = WN_Get_Linenum(call);
        guards->push_back(guard);
    }
    return TRUE;
}

/* A failed void callee leaves its fresh hidden result null; stop its caller. */
static BOOL
VHO_FHE_Runtime_Production_Install_Call_Guards
        (PU_Info *pu_info, FILE *diagnostic)
{
    std::vector<VHO_FHE_RUNTIME_CALL_RESULT_GUARD> guards;
    if (!VHO_FHE_Runtime_Production_Call_Guards
             (pu_info, &guards, diagnostic))
        return FALSE;
    if (guards.size() !=
        (DSL_Call_Image_PU_Has_Calls(PU_Info_proc_sym(pu_info)) ? 9U : 0U))
        return VHO_FHE_Runtime_Production_Report
                   (diagnostic, "CFHELOWER-CALL-004",
                    "runtime PU call count is invalid");
    WN *body = WN_func_body(PU_Info_tree_ptr(pu_info));
    for (UINT32 i = 0; i < guards.size(); ++i) {
        const VHO_FHE_RUNTIME_CALL_RESULT_GUARD &guard = guards[i];
        TY_IDX result_ty = ST_type(guard.result_st);
        WN *result = WN_CreateLdid
                         (OPR_LDID, Pointer_Mtype, Pointer_Mtype, 0,
                          guard.result_st, result_ty);
        WN *test = WN_EQ(Pointer_Mtype, result,
                         WN_Zerocon(Pointer_Mtype));
        WN *failure = WN_CreateBlock();
        WN *leave = WN_CreateReturn();
        WN_Set_Linenum(leave, guard.source_position);
        WN_INSERT_BlockLast(failure, leave);
        WN *check = WN_CreateIf(test, failure, WN_CreateBlock());
        WN_Set_Linenum(check, guard.source_position);
        WN_INSERT_BlockAfter(body, guard.call, check);
    }
    return TRUE;
}

/* Reopened middle WHIRL must retain the exact fail-closed call boundary. */
static BOOL
VHO_FHE_Runtime_Production_Verify_Call_Guards
        (PU_Info *pu_info, FILE *diagnostic)
{
    std::vector<VHO_FHE_RUNTIME_CALL_RESULT_GUARD> guards;
    if (!VHO_FHE_Runtime_Production_Call_Guards
             (pu_info, &guards, diagnostic) ||
        guards.size() !=
            (DSL_Call_Image_PU_Has_Calls(PU_Info_proc_sym(pu_info))
                 ? 9U : 0U))
        return FALSE;
    for (UINT32 i = 0; i < guards.size(); ++i) {
        const VHO_FHE_RUNTIME_CALL_RESULT_GUARD &guard = guards[i];
        WN *check = WN_next(guard.call);
        if (!VHO_FHE_Standard_Null_Guard_Valid
                 (check, guard.result_st, guard.source_position,
                  diagnostic))
            return FALSE;
    }
    return TRUE;
}

/* Replace one full PU after the plan and all source identities are checked. */
static BOOL
VHO_FHE_Runtime_Production_Pass
        (PU_Info *pu_info, WN **tree,
         const VHO_FHE_RUNTIME_LOWER_OPTIONS *options,
         FILE *diagnostic, VHO_FHE_RUNTIME_LOWER_RESULT *result)
{
    if (pu_info == NULL || tree == NULL || result == NULL ||
        !VHO_FHE_Runtime_Production_Prepare(options, diagnostic) ||
        VHO_FHE_runtime_production.processed_owners.find
            (PU_Info_proc_sym(pu_info)) !=
        VHO_FHE_runtime_production.processed_owners.end())
        return FALSE;
    VHO_FHE_RUNTIME_OPERATION_PLAN_RESULT summary;
    if (!VHO_FHE_Runtime_Operation_Plan_Prepare_PU
             (pu_info, VHO_FHE_Runtime_Production_Failure, NULL,
              diagnostic, &summary))
        return FALSE;
    const DSL_IR_NATIVE_VALUE_LOWER_REQUEST *requests = NULL;
    UINT32 count = 0;
    UINT32 expected_definitions = 0;
    UINT32 expected_evaluations = 0;
    for (UINT32 i = 0; i < VHO_FHE_runtime_production.schedule.size();
         ++i) {
        const VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD &scheduled =
            VHO_FHE_runtime_production.schedule[i];
        if (scheduled.owner_pu_st != PU_Info_proc_sym(pu_info))
            continue;
        ++expected_definitions;
        expected_evaluations += scheduled.static_evaluation_count;
    }
    if (!VHO_FHE_Runtime_Operation_Plan_Get(&requests, &count) ||
        count != summary.computed_count + summary.promoted_source_count +
                 summary.unpromoted_source_count ||
        summary.computed_count != expected_definitions +
            (summary.promoted_source_count != 0 ? 1 : 0) ||
        summary.evaluation_count != expected_evaluations ||
        summary.live_unpromoted_source_count != 0 ||
        summary.selector_count != summary.evaluation_count ||
        summary.standard_call_count != 2 * summary.evaluation_count ||
        summary.output_handle_count != summary.standard_call_count ||
        summary.status_check_count != summary.standard_call_count) {
        VHO_FHE_Runtime_Operation_Plan_Reset();
        return VHO_FHE_Runtime_Production_Report
                   (diagnostic, "CFHELOWER-PASS-001",
                    "per-PU operation request census is invalid");
    }
    if (count == 0 || requests == NULL) {
        VHO_FHE_Runtime_Operation_Plan_Reset();
        return VHO_FHE_Runtime_Production_Report
                   (diagnostic, "CFHELOWER-PASS-003",
                    "runtime PU has no lowering requests");
    }
    std::vector<DSL_IR_NATIVE_VALUE_LOWER_RESULT> lowered(count);
    BOOL valid = VHO_FHE_Runtime_Operation_Plan_Apply_PU
                     (pu_info, diagnostic, &lowered[0]);
    if (!valid) {
        VHO_FHE_Runtime_Operation_Plan_Reset();
        return FALSE;
    }
    for (UINT32 i = 0; i < count; ++i) {
        if (lowered[i].source_value_id != requests[i].source_value_id ||
            lowered[i].mode != requests[i].mode) {
            VHO_FHE_Runtime_Operation_Plan_Reset();
            return VHO_FHE_Runtime_Production_Report
                       (diagnostic, "CFHELOWER-PASS-002",
                        "committed source identity changed");
        }
    }
    VHO_FHE_Runtime_Operation_Plan_Reset();
    if (!VHO_FHE_Runtime_Production_Install_Call_Guards
             (pu_info, diagnostic))
        return FALSE;
    VHO_FHE_runtime_production.computed_count += summary.computed_count;
    VHO_FHE_runtime_production.promoted_count +=
        summary.promoted_source_count;
    VHO_FHE_runtime_production.dead_count +=
        summary.unpromoted_source_count;
    VHO_FHE_runtime_production.selector_count += summary.selector_count;
    VHO_FHE_runtime_production.evaluation_count +=
        summary.evaluation_count;
    VHO_FHE_runtime_production.processed_owners.insert
        (PU_Info_proc_sym(pu_info));
    result->standard_call_count += summary.standard_call_count;
    result->output_handle_count += summary.output_handle_count;
    result->status_check_count += summary.status_check_count;
    *tree = PU_Info_tree_ptr(pu_info);
    return TRUE;
}

/* Reopen lowered images without requiring the producer's process-local state. */
static BOOL
VHO_FHE_Runtime_Production_Final_Verify
        (PU_Info *pu_info, WN *tree, FILE *diagnostic)
{
    return pu_info != NULL && tree == PU_Info_tree_ptr(pu_info) &&
           (!VHO_FHE_runtime_production.initialized ||
            VHO_FHE_runtime_production.processed_owners.find
                (PU_Info_proc_sym(pu_info)) !=
                VHO_FHE_runtime_production.processed_owners.end()) &&
           VHO_FHE_Runtime_Production_Prototypes_Valid(diagnostic) &&
           VHO_FHE_Runtime_Production_Verify_Call_Guards
               (pu_info, diagnostic) &&
           DSL_IR_Image_Validate_Lowered_Relations(diagnostic) &&
           DSL_Program_Interface_Validate_Lowered_PU
               (pu_info, diagnostic) &&
           DSL_Region_Verify_PU(pu_info, diagnostic);
}

/* Open an exclusive auxiliary temporary file; the checkpoint owns cleanup. */
static FILE *
VHO_FHE_Runtime_Production_Open_Auxiliary
        (const std::string &path)
{
    INT fd = open(path.c_str(), O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0)
        return NULL;
    FILE *file = fdopen(fd, "w");
    if (file == NULL) {
        close(fd);
        unlink(path.c_str());
    }
    return file;
}

/* Persist the exact static schedule as a deterministic auxiliary manifest. */
static BOOL
VHO_FHE_Runtime_Production_Write_Schedule
        (const std::string &temporary_path)
{
    FILE *file = VHO_FHE_Runtime_Production_Open_Auxiliary
                     (temporary_path);
    if (file == NULL)
        return FALSE;
    fprintf(file,
            "{\"schema\":\"open64.fhe.sync5.runtime-schedule.v1\","
            "\"provider_sha256\":\"%s\",\"static_evaluations\":87,"
            "\"dynamic_evaluations\":147,\"records\":[",
            VHO_FHE_runtime_production.provider_sha256.c_str());
    for (UINT32 i = 0; i < VHO_FHE_runtime_production.schedule.size(); ++i) {
        const VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD &record =
            VHO_FHE_runtime_production.schedule[i];
        fprintf(file,
                "%s{\"node\":%u,\"value\":%u,\"owner\":%u,"
                "\"operator\":%u,\"first_ordinal\":%u,"
                "\"static_count\":%u,\"multiplicity\":%u,"
                "\"dynamic_count\":%u}",
                i == 0 ? "" : ",", record.source_node_id,
                record.result_value_id, (UINT32)record.owner_pu_st,
                record.logical_operator, record.first_static_ordinal,
                record.static_evaluation_count,
                record.execution_multiplicity,
                record.dynamic_evaluation_count);
    }
    fprintf(file, "]}\n");
    BOOL valid = !ferror(file) && fflush(file) == 0 &&
                 fsync(fileno(file)) == 0;
    valid = fclose(file) == 0 && valid;
    if (!valid)
        unlink(temporary_path.c_str());
    return valid;
}

/* Persist the independently checked all-PU lowering census. */
static BOOL
VHO_FHE_Runtime_Production_Write_Report
        (const std::string &temporary_path,
         const VHO_FHE_RUNTIME_LOWER_RESULT *aggregate)
{
    FILE *file = VHO_FHE_Runtime_Production_Open_Auxiliary
                     (temporary_path);
    if (file == NULL)
        return FALSE;
    fprintf(file,
            "{\"schema\":\"open64.fhe.sync5.runtime-lowering-report.v1\","
            "\"pu_count\":%u,\"scheduled_definitions\":32,"
            "\"computed_definitions\":%u,\"promoted_sources\":%u,"
            "\"dead_sources\":%u,\"static_evaluations\":%u,"
            "\"dynamic_evaluations\":147,\"selectors\":%u,"
            "\"standard_calls\":%u,\"output_handles\":%u,"
            "\"status_checks\":%u}\n",
            (UINT32)VHO_FHE_runtime_production.processed_owners.size(),
            VHO_FHE_runtime_production.computed_count,
            VHO_FHE_runtime_production.promoted_count,
            VHO_FHE_runtime_production.dead_count,
            VHO_FHE_runtime_production.evaluation_count,
            VHO_FHE_runtime_production.selector_count,
            aggregate->standard_call_count, aggregate->output_handle_count,
            aggregate->status_check_count);
    BOOL valid = !ferror(file) && fflush(file) == 0 &&
                 fsync(fileno(file)) == 0;
    valid = fclose(file) == 0 && valid;
    if (!valid)
        unlink(temporary_path.c_str());
    return valid;
}

/* Admit publication only after every owner, relation, and count is complete. */
static BOOL
VHO_FHE_Runtime_Production_Finalizer
        (const VHO_FHE_RUNTIME_LOWER_RESULT *aggregate, FILE *diagnostic)
{
    if (aggregate == NULL || !VHO_FHE_runtime_production.initialized ||
        VHO_FHE_runtime_production.processed_owners.size() != 6 ||
        VHO_FHE_runtime_production.schedule.size() != 32 ||
        VHO_FHE_runtime_production.computed_count != 33 ||
        VHO_FHE_runtime_production.promoted_count != 44 ||
        VHO_FHE_runtime_production.dead_count != 126 ||
        VHO_FHE_runtime_production.selector_count != 87 ||
        VHO_FHE_runtime_production.evaluation_count != 87 ||
        aggregate->semantic_gatekeeper_count != 12 ||
        aggregate->runtime_lowering_pass_count != 6 ||
        aggregate->standard_call_count != 174 ||
        aggregate->output_handle_count != 174 ||
        aggregate->status_check_count != 174 ||
        aggregate->error_count != 0 ||
        DSL_FHE_Materialization_Operation_Count() != 114 ||
        !DSL_FHE_Materialization_Image_Validate(diagnostic) ||
        !DSL_IR_Image_Validate_Lowered_Relations(diagnostic) ||
        !DSL_Program_Interface_Image_Validate(diagnostic) ||
        !DSL_Runtime_Interface_Image_Validate(diagnostic))
        return VHO_FHE_Runtime_Production_Report
                   (diagnostic, "CFHELOWER-CHECKPOINT-002",
                    "all-PU runtime lowering is incomplete");
    const std::string schedule_final =
        VHO_FHE_runtime_production.checkpoint_path + ".schedule.json";
    const std::string report_final =
        VHO_FHE_runtime_production.checkpoint_path + ".report.json";
    const std::string schedule_temp = schedule_final + ".tmp";
    const std::string report_temp = report_final + ".tmp";
    sigset_t blocked;
    sigset_t previous;
    sigfillset(&blocked);
    if (sigprocmask(SIG_BLOCK, &blocked, &previous) != 0)
        return VHO_FHE_Runtime_Production_Report
                   (diagnostic, "CFHELOWER-CHECKPOINT-003",
                    "could not protect auxiliary publication");
    BOOL schedule_written =
        VHO_FHE_Runtime_Production_Write_Schedule(schedule_temp);
    BOOL schedule_registered = schedule_written &&
        VHO_FHE_Runtime_Lower_Checkpoint_Register_Artifact
            (schedule_temp.c_str(), schedule_final.c_str());
    BOOL report_written = schedule_registered &&
        VHO_FHE_Runtime_Production_Write_Report(report_temp, aggregate);
    BOOL report_registered = report_written &&
        VHO_FHE_Runtime_Lower_Checkpoint_Register_Artifact
            (report_temp.c_str(), report_final.c_str());
    if (schedule_written && !schedule_registered)
        unlink(schedule_temp.c_str());
    if (report_written && !report_registered)
        unlink(report_temp.c_str());
    BOOL unblocked = sigprocmask(SIG_SETMASK, &previous, NULL) == 0;
    if (!report_registered || !unblocked) {
        return VHO_FHE_Runtime_Production_Report
                   (diagnostic, "CFHELOWER-CHECKPOINT-003",
                    "runtime schedule or report could not be registered");
    }
    fprintf(diagnostic,
            "FHE-SYNC5-LOWERING: pu=6 definitions=32 computed=33 "
            "promoted=44 dead=126 static=87 dynamic=147 calls=174\n");
    return TRUE;
}

/* Clear all borrowed schedule and PU state on success and on abort. */
static void
VHO_FHE_Runtime_Production_Completion (BOOL committed)
{
    (void)committed;
    VHO_FHE_Runtime_Operation_Plan_Reset();
    VHO_FHE_Runtime_Static_Schedule_Reset();
    VHO_FHE_runtime_production = VHO_FHE_RUNTIME_PRODUCTION_STATE();
}

/* Attach the FHE-owned callbacks without changing shared phase order. */
BOOL
VHO_FHE_Register_Default_Runtime_Production (void)
{
    return VHO_FHE_Runtime_Lower_Register_Semantic_Gatekeeper
               (VHO_FHE_Runtime_Production_Gatekeeper) &&
           VHO_FHE_Runtime_Lower_Register_Pass
               (VHO_FHE_Runtime_Production_Pass) &&
           VHO_FHE_Runtime_Lower_Register_Checkpoint_Lifecycle
               (VHO_FHE_Runtime_Production_Finalizer,
                VHO_FHE_Runtime_Production_Completion) &&
           VHO_FHE_Unlowered_Gate_Register_Semantic_Verifier
               (VHO_FHE_Runtime_Production_Final_Verify);
}

namespace {
struct VHO_FHE_RUNTIME_PRODUCTION_REGISTRATION {
    VHO_FHE_RUNTIME_PRODUCTION_REGISTRATION()
    {
        (void)VHO_FHE_Register_Default_Runtime_Production();
    }
};

static VHO_FHE_RUNTIME_PRODUCTION_REGISTRATION
    VHO_FHE_runtime_production_registration;
}
