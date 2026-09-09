/*
 * Contract test for the option-controlled FHE conversion phase boundary.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "config.h"
#include "config_fhe.h"
#include "config_targ_opt.h"
#include "controls.h"
#include "dsl_builder.h"
#include "dsl_fhe.h"
#include "dsl_fhe_plan.h"
#include "dwarf_DST_mem.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "fhe_convert.h"
#include "ir_reader.h"
#include "mempool.h"
#include "pu_info.h"
#include "stab.h"
#include "tracing.h"
#include "wn.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void
Signal_Cleanup(INT sig)
{
    (void)sig;
}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
    (void)kind;
    (void)parm;
    return "";
}

static char observed_order[8];
static UINT32 observed_count;
static BOOL observed_strict_o0;

static BOOL
Observe_Semantic_Gatekeeper
        (struct pu_info *pu_info,
         WN *tree,
         const VHO_FHE_CONVERT_OPTIONS *options,
         FILE *diagnostic)
{
    (void)diagnostic;
    observed_order[observed_count++] = 'G';
    observed_strict_o0 = options->strict_o0;
    return pu_info != NULL && tree != NULL;
}

static BOOL
Observe_Conversion
        (struct pu_info *pu_info,
         WN **tree,
         const VHO_FHE_CONVERT_OPTIONS *options,
         FILE *diagnostic,
         VHO_FHE_CONVERT_RESULT *result)
{
    (void)diagnostic;
    observed_order[observed_count++] = 'C';
    observed_strict_o0 = options->strict_o0;
    result->source_disposition_count = 1;
    result->converted_disposition_count = 1;
    return pu_info != NULL && tree != NULL && *tree != NULL;
}

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

static BOOL
Create_FHE_Config(void)
{
    DSL_FHE_COMPILATION_CONFIG_RECORD config;
    DSL_FHE_Compilation_Config_Record_Init(&config);
    config.scheme = DSL_FHE_SCHEME_CKKS;
    config.security_level = DSL_FHE_SECURITY_128_CLASSIC;
    config.multiplicative_depth = 8;
    config.multiplicative_depth_policy = DSL_FHE_POLICY_AUTO;
    config.scale_bits = 50;
    config.first_modulus_bits = 60;
    config.slot_count_policy = DSL_FHE_POLICY_AUTO;
    config.ring_dimension = 16384;
    config.bootstrap_policy = DSL_FHE_BOOTSTRAP_AUTO;
    config.backend_policy = DSL_FHE_BACKEND_OPENFHE;
    return DSL_FHE_Intern_Compilation_Config(&config) !=
               DSL_FHE_CONFIG_INVALID_ID;
}

int
main(void)
{
    Initialize_Test_Context();
    if (!DSL_Builder_Begin_Program())
        return 1;
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("fhe_convert_contract");
    if (pu == NULL || !DSL_Builder_Select_PU(pu) ||
        !Create_FHE_Config())
        return 1;
    WN *tree = PU_Info_tree_ptr(pu);

    VHO_FHE_CONVERT_RESULT result;
    VHO_FHE_Enable_Conversion = FALSE;
    if (!VHO_FHE_Convert_Program_Unit(pu, &tree, stderr, &result) ||
        result.semantic_gatekeeper_count != 0 ||
        result.conversion_pass_count != 0) {
        fprintf(stderr, "disabled FHE conversion changed the PU\n");
        return 1;
    }

    VHO_FHE_Enable_Conversion = TRUE;
    if (VHO_FHE_Convert_Program_Unit(pu, &tree, NULL, &result) ||
        result.error_count != 1) {
        fprintf(stderr, "missing FHE semantic pass was not rejected\n");
        return 1;
    }

    VHO_FHE_Convert_Reset_Passes();
    if (!VHO_FHE_Convert_Register_Semantic_Gatekeeper
             (Observe_Semantic_Gatekeeper) ||
        !VHO_FHE_Convert_Register_Pass(Observe_Conversion) ||
        VHO_FHE_Convert_Register_Pass(Observe_Conversion)) {
        fprintf(stderr, "FHE conversion pass registration changed\n");
        return 1;
    }

    observed_count = 0;
    observed_strict_o0 = FALSE;
    VHO_FHE_Strict_O0 = TRUE;
    if (!VHO_FHE_Convert_Program_Unit(pu, &tree, stderr, &result) ||
        result.semantic_gatekeeper_count != 2 ||
        result.conversion_pass_count != 1 || result.error_count != 0 ||
        result.source_disposition_count != 1 ||
        result.converted_disposition_count != 1 ||
        observed_count != 3 || observed_order[0] != 'G' ||
        observed_order[1] != 'C' || observed_order[2] != 'G' ||
        !observed_strict_o0) {
        fprintf(stderr, "FHE gatekeeper/conversion ordering changed\n");
        return 1;
    }

    const char *trace = getenv("OPEN64_FHE_CONVERT_TRACE");
    if (trace != NULL) {
        FILE *trace_file = fopen(trace, "w");
        if (trace_file == NULL)
            return 1;
        Set_Trace_File_internal(trace_file);
        VHO_FHE_Dump_Before_Conversion = TRUE;
        VHO_FHE_Dump_After_Conversion = TRUE;
        tree = VHO_FHE_Convert_Driver(pu, tree);
        fclose(trace_file);
        Set_Trace_File_internal(stderr);
    }

    DSL_FHE_Image_Reset();
    DSL_FHE_Plan_Image_Reset();
    VHO_FHE_Convert_Reset_Passes();
    if (!VHO_FHE_Convert_Program_Unit(pu, &tree, stderr, &result) ||
        result.semantic_gatekeeper_count != 0 ||
        result.conversion_pass_count != 0) {
        fprintf(stderr, "ordinary WHIRL no-op compatibility changed\n");
        return 1;
    }

    DSL_Builder_Abort_Program();
    printf("FHE conversion phase contract passed\n");
    return 0;
}
