/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef fhe_convert_INCLUDED
#define fhe_convert_INCLUDED

#include <stdio.h>

#include "defs.h"

class WN;
struct pu_info;

typedef struct {
    BOOL strict_o0;
} VHO_FHE_CONVERT_OPTIONS;

typedef struct {
    UINT32 semantic_gatekeeper_count;
    UINT32 conversion_pass_count;
    UINT32 source_disposition_count;
    UINT32 converted_disposition_count;
    UINT32 rewritten_value_count;
    UINT32 folded_batch_norm_count;
    UINT32 approximation_contract_count;
    UINT32 error_count;
} VHO_FHE_CONVERT_RESULT;

typedef BOOL (*VHO_FHE_SEMANTIC_GATEKEEPER)
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 const VHO_FHE_CONVERT_OPTIONS *options,
                                 FILE *diagnostic);

typedef BOOL (*VHO_FHE_CONVERSION_PASS)
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 const VHO_FHE_CONVERT_OPTIONS *options,
                                 FILE *diagnostic,
                                 VHO_FHE_CONVERT_RESULT *result);

extern BOOL VHO_FHE_Convert_Register_Semantic_Gatekeeper
                                (VHO_FHE_SEMANTIC_GATEKEEPER gatekeeper);
extern BOOL VHO_FHE_Convert_Register_Pass
                                (VHO_FHE_CONVERSION_PASS pass);
extern void VHO_FHE_Convert_Reset_Passes (void);
extern BOOL VHO_FHE_Convert_Program_Unit
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 FILE *diagnostic,
                                 VHO_FHE_CONVERT_RESULT *result);
extern void VHO_FHE_Convert_Result_Init
                                (VHO_FHE_CONVERT_RESULT *result);
extern void VHO_FHE_Convert_Result_Accumulate
                                (VHO_FHE_CONVERT_RESULT *aggregate,
                                 const VHO_FHE_CONVERT_RESULT *result);
extern BOOL VHO_FHE_Convert_Checkpoint_Validate
                                (UINT32 expected_pu_count,
                                 UINT32 converted_pu_count,
                                 const VHO_FHE_CONVERT_RESULT *aggregate,
                                 FILE *diagnostic);
extern BOOL VHO_FHE_Convert_Driver_Try
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 VHO_FHE_CONVERT_RESULT *result);
extern WN *VHO_FHE_Convert_Driver_With_Result
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 VHO_FHE_CONVERT_RESULT *result);
extern WN *VHO_FHE_Convert_Driver (struct pu_info *pu_info, WN *tree);

#endif /* fhe_convert_INCLUDED */
