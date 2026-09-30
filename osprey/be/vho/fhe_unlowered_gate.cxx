/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>

#include "fhe_unlowered_gate.h"
#include "dsl_fhe.h"
#include "dsl_fhe_plan.h"
#include "dsl_opcode.h"
#include "wn.h"

static VHO_FHE_UNLOWERED_SEMANTIC_GATE VHO_FHE_unlowered_semantic_gate;

static void
VHO_FHE_Unlowered_Count_Carriers (WN *wn, UINT32 *count)
{
    if (wn == NULL)
        return;
    if (DSL_WN_Is_Native(wn) ||
        (WN_operator(wn) != OPR_COMMENT && DSL_WN_Has_Opcode(wn)))
        ++*count;
    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))
            VHO_FHE_Unlowered_Count_Carriers(stmt, count);
        return;
    }
    for (INT kid = 0; kid < WN_kid_count(wn); ++kid)
        VHO_FHE_Unlowered_Count_Carriers(WN_kid(wn, kid), count);
}

static BOOL
VHO_FHE_Unlowered_Has_FHE_Image (void)
{
    return DSL_FHE_Config_Count() != 0 ||
           DSL_FHE_Plan_Conversion_Disposition_Count() != 0 ||
           DSL_FHE_Materialization_Operation_Count() != 0;
}

BOOL
VHO_FHE_Unlowered_Gate_Register_Semantic_Verifier
        (VHO_FHE_UNLOWERED_SEMANTIC_GATE verifier)
{
    if (verifier == NULL || VHO_FHE_unlowered_semantic_gate != NULL)
        return FALSE;
    VHO_FHE_unlowered_semantic_gate = verifier;
    return TRUE;
}

void
VHO_FHE_Unlowered_Gate_Reset (void)
{
    VHO_FHE_unlowered_semantic_gate = NULL;
}

BOOL
VHO_FHE_Unlowered_Gate_Program_Unit
        (struct pu_info *pu_info, WN *tree, FILE *diagnostic,
         VHO_FHE_UNLOWERED_GATE_RESULT *result)
{
    VHO_FHE_UNLOWERED_GATE_RESULT local_result;
    memset(&local_result, 0, sizeof(local_result));
    if (pu_info == NULL || tree == NULL) {
        ++local_result.error_count;
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHEMID-001: program unit or WHIRL tree is missing\n");
    }
    else {
        VHO_FHE_Unlowered_Count_Carriers
            (tree, &local_result.executable_dsl_carrier_count);
        if (local_result.executable_dsl_carrier_count != 0) {
            ++local_result.error_count;
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "CFHEMID-002: %u executable DSL carrier(s) remain "
                        "before the standard-WHIRL boundary\n",
                        local_result.executable_dsl_carrier_count);
        }
        if (VHO_FHE_Unlowered_Has_FHE_Image()) {
            if (VHO_FHE_unlowered_semantic_gate == NULL) {
                ++local_result.error_count;
                if (diagnostic != NULL)
                    fprintf(diagnostic,
                            "CFHEMID-003: FHE semantic final verifier is not "
                            "registered\n");
            }
            else {
                ++local_result.semantic_gatekeeper_count;
                if (!VHO_FHE_unlowered_semantic_gate
                         (pu_info, tree, diagnostic)) {
                    ++local_result.error_count;
                    if (diagnostic != NULL)
                        fprintf(diagnostic,
                                "CFHEMID-004: FHE semantic final verifier "
                                "rejected the PU\n");
                }
            }
        }
    }
    if (result != NULL)
        *result = local_result;
    return local_result.error_count == 0;
}
