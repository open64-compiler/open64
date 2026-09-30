/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef config_fhe_INCLUDED
#define config_fhe_INCLUDED

extern BOOL VHO_FHE_Enable_Conversion;
extern BOOL VHO_FHE_Enable_Conversion_Set;
extern BOOL VHO_FHE_Strict_O0;
extern BOOL VHO_FHE_Strict_O0_Set;
extern BOOL VHO_FHE_Dump_Before_Conversion;
extern BOOL VHO_FHE_Dump_Before_Conversion_Set;
extern BOOL VHO_FHE_Dump_After_Conversion;
extern BOOL VHO_FHE_Dump_After_Conversion_Set;
extern char *VHO_FHE_Conversion_Checkpoint_Output;
extern char *VHO_FHE_Calibration_Manifest_Path;
extern char *VHO_FHE_Calibration_Manifest_SHA256;
extern BOOL VHO_FHE_Enable_Materialization;
extern BOOL VHO_FHE_Enable_Materialization_Set;
extern char *VHO_FHE_Materialization_Checkpoint_Output;
extern char *VHO_FHE_Bootstrap_Mode;
extern char *VHO_FHE_Provider_Manifest_Path;
extern char *VHO_FHE_Provider_Manifest_SHA256;
extern BOOL VHO_FHE_Enable_Runtime_Lowering;
extern BOOL VHO_FHE_Enable_Runtime_Lowering_Set;
extern char *VHO_FHE_Runtime_Lowering_Checkpoint_Output;

#endif /* config_fhe_INCLUDED */
