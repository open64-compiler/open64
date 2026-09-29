/*
 * Contract test for the per-PU FHE materialization phase boundary.
 */

#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "config.h"
#include "config_fhe.h"
#include "config_targ_opt.h"
#include "controls.h"
#include "dsl_builder.h"
#include "dsl_fhe.h"
#include "dwarf_DST_mem.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "fhe_materialize.h"
#include "ir_reader.h"
#include "mempool.h"
#include "pu_info.h"
#include "stab.h"
#include "wn.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void
Signal_Cleanup (INT sig)
{
    (void)sig;
}

const char *
Host_Format_Parm (INT kind, MEM_PTR parm)
{
    (void)kind;
    (void)parm;
    return "";
}

static UINT32 observed_gate_count;
static UINT32 observed_pass_count;
static const char *expected_bootstrap_mode;

static BOOL
Observe_Materialization_Gatekeeper
        (struct pu_info *pu_info, WN *tree,
         const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
    (void)diagnostic;
    ++observed_gate_count;
    return pu_info != NULL && tree != NULL && options != NULL &&
           strcmp(options->bootstrap_mode, expected_bootstrap_mode) == 0 &&
           options->provider_manifest_path == NULL &&
           options->provider_manifest_sha256 == NULL;
}

static BOOL
Observe_Materialization
        (struct pu_info *pu_info, WN **tree,
         const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic,
         VHO_FHE_MATERIALIZE_RESULT *result)
{
    (void)options;
    (void)diagnostic;
    ++observed_pass_count;
    result->context_count = 1;
    result->operation_count = 6;
    result->refresh_count = 1;
    return pu_info != NULL && tree != NULL && *tree != NULL;
}

static void
Initialize_Test_Context (void)
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

static BOOL
Create_FHE_Config (void)
{
    DSL_FHE_COMPILATION_CONFIG_RECORD config;
    DSL_FHE_Compilation_Config_Record_Init(&config);
    config.scheme = DSL_FHE_SCHEME_CKKS;
    config.security_level = DSL_FHE_SECURITY_128_CLASSIC;
    config.multiplicative_depth = 11;
    config.multiplicative_depth_policy = DSL_FHE_POLICY_EXPLICIT;
    config.scale_bits = 56;
    config.first_modulus_bits = 60;
    config.slot_count_policy = DSL_FHE_POLICY_EXPLICIT;
    config.slot_count = 32768;
    config.ring_dimension = 65536;
    config.bootstrap_policy = DSL_FHE_BOOTSTRAP_ON;
    config.backend_policy = DSL_FHE_BACKEND_OPENFHE;
    return DSL_FHE_Intern_Compilation_Config(&config) !=
               DSL_FHE_CONFIG_INVALID_ID;
}

int
main (void)
{
    Initialize_Test_Context();
    if (!DSL_Builder_Begin_Program())
        return 1;
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("fhe_materialize_contract");
    if (pu == NULL || !DSL_Builder_Select_PU(pu) || !Create_FHE_Config())
        return 1;
    WN *tree = PU_Info_tree_ptr(pu);
    VHO_FHE_MATERIALIZE_RESULT result;

    VHO_FHE_Enable_Materialization = FALSE;
    if (!VHO_FHE_Materialize_Program_Unit(pu, &tree, stderr, &result) ||
        result.materialization_pass_count != 0 || result.error_count != 0) {
        fprintf(stderr, "disabled FHE materialization changed the PU\n");
        return 1;
    }

    VHO_FHE_Enable_Materialization = TRUE;
    VHO_FHE_Bootstrap_Mode = (char *)"manual";
    expected_bootstrap_mode = "manual";
    VHO_FHE_Provider_Manifest_Path = NULL;
    VHO_FHE_Provider_Manifest_SHA256 = NULL;
    if (VHO_FHE_Materialize_Program_Unit(pu, &tree, NULL, &result) ||
        result.error_count != 1) {
        fprintf(stderr, "missing FHE materialization callbacks accepted\n");
        return 1;
    }

    if (!VHO_FHE_Materialize_Register_Semantic_Gatekeeper
             (Observe_Materialization_Gatekeeper) ||
        !VHO_FHE_Materialize_Register_Pass(Observe_Materialization) ||
        VHO_FHE_Materialize_Register_Pass(Observe_Materialization)) {
        fprintf(stderr, "FHE materialization registration changed\n");
        return 1;
    }
    observed_gate_count = 0;
    observed_pass_count = 0;
    if (!VHO_FHE_Materialize_Program_Unit(pu, &tree, stderr, &result) ||
        observed_gate_count != 2 || observed_pass_count != 1 ||
        result.semantic_gatekeeper_count != 2 ||
        result.materialization_pass_count != 1 ||
        result.context_count != 1 || result.operation_count != 6 ||
        result.refresh_count != 1 || result.error_count != 0) {
        fprintf(stderr, "FHE materialization phase ordering changed\n");
        return 1;
    }

    VHO_FHE_Bootstrap_Mode = (char *)"off";
    expected_bootstrap_mode = "off";
    observed_gate_count = 0;
    observed_pass_count = 0;
    if (!VHO_FHE_Materialize_Program_Unit(pu, &tree, stderr, &result) ||
        observed_gate_count != 2 || observed_pass_count != 1) {
        fprintf(stderr, "bootstrap=off did not reach the semantic gate\n");
        return 1;
    }

    DSL_BUILDER_PROGRAM_UNIT second_pu =
        DSL_Builder_Create_Minimal_PU("fhe_materialize_second_owner");
    if (second_pu == NULL || !DSL_Builder_Select_PU(second_pu))
        return 1;
    WN *second_tree = PU_Info_tree_ptr(second_pu);
    VHO_FHE_MATERIALIZE_RESULT second_result;
    if (!VHO_FHE_Materialize_Program_Unit
             (second_pu, &second_tree, stderr, &second_result) ||
        second_result.materialization_pass_count != 1 ||
        second_result.context_count != 1 ||
        second_result.operation_count != 6) {
        fprintf(stderr, "second owner-PU materialization failed\n");
        return 1;
    }

    VHO_FHE_MATERIALIZE_RESULT aggregate;
    VHO_FHE_Materialize_Result_Init(&aggregate);
    VHO_FHE_Materialize_Result_Accumulate(&aggregate, &result);
    VHO_FHE_Materialize_Result_Accumulate(&aggregate, &second_result);
    if (!VHO_FHE_Materialize_Checkpoint_Validate
             (2, 2, &aggregate, stderr) ||
        VHO_FHE_Materialize_Checkpoint_Validate
             (3, 2, &aggregate, NULL)) {
        fprintf(stderr, "FHE materialization aggregation changed\n");
        return 1;
    }

    VHO_FHE_Bootstrap_Mode = (char *)"auto";
    if (VHO_FHE_Materialize_Program_Unit(pu, &tree, NULL, &result) ||
        result.error_count != 1) {
        fprintf(stderr, "unauthenticated provider manifest accepted\n");
        return 1;
    }
    VHO_FHE_Provider_Manifest_Path = (char *)"provider.json";
    VHO_FHE_Provider_Manifest_SHA256 = (char *)"abc";
    if (VHO_FHE_Materialize_Program_Unit(pu, &tree, NULL, &result) ||
        result.error_count != 1) {
        fprintf(stderr, "short provider digest accepted\n");
        return 1;
    }

    DSL_Builder_Abort_Program();
    VHO_FHE_Materialize_Reset_Passes();
    printf("FHE materialization phase contract passed\n");
    return 0;
}
